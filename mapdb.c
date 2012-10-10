
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <errno.h>
#include <ccan/htable/htable.h>
#include <ccan/alignof/alignof.h>
#include <ccan/likely/likely.h>
#include <ccan/container_of/container_of.h>

#include <l4/types.h>

#include <ukernel/misc.h>
#include <ukernel/util.h>
#include <ukernel/slab.h>
#include <ukernel/trace.h>
#include <ukernel/space.h>
#include <ukernel/mapdb.h>


/* for mapdb dumps on add/remove */
#define TRACE(fmt, ...) TRACE_MSG(TRID_MAPDB, fmt, __VA_ARGS__)


#define GROUP_SIZE (PAGE_SIZE * MAX_ENTRIES_PER_GROUP)
#define GROUP_ADDR(addr) ((addr) & ~(GROUP_SIZE - 1))

/* the largest map_entry that appears here is 4 MiB, i.e. 2^22 bytes. if a 4
 * KiB page within it is replaced, the previous 4-meg entry is divided into at
 * most 10 more (one each of 4k, 8k, 16k, 32k, 64k, 128k, 256k, 512k, 1m, 2m;
 * the original page adds the final 4k).
 *
 * callers to entry_split_and_insert() should reserve at least this many
 * map_entry structs somewhere.
 */
#define MAX_SPLIT 10

/* maximum probe depth in map_entry->children. used by mapdb_add_child(). */
#define MAX_PROBE_DEPTH 16


/* dereferenced map_entry->children entry. */
struct child_ref
{
	struct map_db *child_db;
	struct map_entry *child_entry;

	/* page offset within the deref_child() parent entry. */
	int parent_offset;
};


static size_t rehash_ref_hash(const void *, void *);
static struct map_entry *discontiguate(
	struct map_db *db,
	struct map_group *g,
	L4_Fpage_t range);


static struct kmem_cache *map_group_slab = NULL;
static uint32_t next_ref_id = 1;
static struct htable ref_hash = HTABLE_INITIALIZER(ref_hash,
	rehash_ref_hash, NULL);


/* FIXME: move this into a utility header. many places duplicate it. */
static inline bool int_eq(const void *elem, void *ref) {
	return *(const int *)elem == *(int *)ref;
}


static void dump_map_group(struct map_group *g)
{
#ifndef NDEBUG
	TRACE("%s: group %#lx .. %#lx contains (%d ents, %d alloc):\n",
		__func__, g->start, g->start + GROUP_SIZE - 1,
		g->num_entries, g->num_alloc);
	for(int i=0; i < g->num_entries; i++) {
		struct map_entry *e = &g->entries[i];
		assert(!L4_IsNilFpage(e->range));
		TRACE("  %d: [%#lx .. %#lx] (%c%c%c), pages [%u .. %lu]; nc %u\n", i,
			L4_Address(e->range), L4_Address(e->range) + L4_Size(e->range) - 1,
			CHECK_FLAG(L4_Rights(e->range), L4_Readable) ? 'r' : '-',
			CHECK_FLAG(L4_Rights(e->range), L4_Writable) ? 'w' : '-',
			CHECK_FLAG(L4_Rights(e->range), L4_eXecutable) ? 'x' : '-',
			e->first_page_id,
			e->first_page_id + L4_Size(e->range) / PAGE_SIZE - 1,
			(unsigned)e->num_children);
	}
#endif
}


/* returns false on stale child. */
static bool deref_child(
	struct child_ref *cr,
	struct map_db *home_db,
	const struct map_entry *e,
	int child_ix)
{
	assert(child_ix < e->num_children);

	const L4_Word_t *children = e->num_children > 1 ? e->children : &e->child;
	if(!REF_DEFINED(children[child_ix])) return false;

	L4_Word_t child_addr = REF_ADDR(children[child_ix]);
	uint32_t space_id = REF_SPACE(children[child_ix]);
	assert(space_id != home_db->ref_id);
	struct map_db *db = htable_get(&ref_hash, int_hash(space_id),
		&int_eq, &space_id);
	if(db == NULL) {
		printf("mapdb ref_id %u not found\n", (unsigned)space_id);
		return false;
	}

	struct map_entry *ce = mapdb_probe(db, child_addr);
	if(ce == NULL) return false;

	/* a valid child refers to the home space, and into the home range. */
	if(REF_SPACE(ce->parent) != home_db->ref_id
		|| !BETWEEN(FPAGE_LOW(e->range), FPAGE_HIGH(e->range),
				REF_ADDR(ce->parent)))
	{
		return false;
	}

	/* check that the physical page is the same, too. */
	int child_offset = (child_addr - L4_Address(ce->range)) >> PAGE_BITS;
	cr->parent_offset = ((REF_ADDR(ce->parent) - L4_Address(e->range)) >> PAGE_BITS)
		+ child_offset;
	uint32_t child_page = ce->first_page_id + child_offset,
		parent_page = e->first_page_id + cr->parent_offset;
	if(child_page != parent_page) return false;

	cr->child_db = db;
	cr->child_entry = ce;

	return true;
}


/* returns true if the entry was cleared. */
static bool flush_entry(struct map_entry *e, int access)
{
	const L4_Word_t *children = e->num_children > 1 ? e->children : &e->child;
	for(int j=0; j < e->num_children && REF_DEFINED(children[j]); j++) {
		/* TODO: deref children[j], recur */
	}

	int rwx = L4_Rights(e->range);
	rwx &= ~access;
	L4_Set_Rights(&e->range, rwx);
	if(rwx != 0) return false;
	else {
		/* everything was revoked. */
		e->range = L4_Nilpage;
		if(e->num_children > 1) free(e->children);
		e->child = 0;
		e->num_children = 0;
		return true;
	}
}


/* returns true if the group now has 0 pages in it. */
static bool flush_group(struct map_db *db, struct map_group *g, int access)
{
	int active_maps = 0;
	for(int i=0; i < g->num_entries; i++) {
		struct map_entry *e = &g->entries[i];
		if(!flush_entry(e, access)) active_maps++;
	}
	/* TODO: compress the range when active_maps != 0 */

	return active_maps == 0;
}


static bool cmp_group_addr(const void *cand, void *keyptr)
{
	uintptr_t key = *(uintptr_t *)keyptr;
	const struct map_group *g = cand;
	return g->start == key;
}


static size_t rehash_map_group(const void *ptr, void *priv)
{
	const struct map_group *g = ptr;
	return int_hash(g->start);
}


static size_t rehash_ref_hash(const void *elem, void *priv) {
	const struct map_db *db = elem;
	return int_hash(db->ref_id);
}


int mapdb_init(struct map_db *ptr, struct space *space)
{
	/* FIXME: add a proper interface for enabling/disabling trace IDs.
	 * kernel commandline perhaps?
	 */
#if 0
	// trace_enable(TRID_MAPDB);
	// trace_enable(TRID_IPC);
	// trace_enable(TRID_SCHED);
#endif
	htable_init(&ptr->groups, &rehash_map_group, NULL);
	ptr->space = space;
	ptr->ref_id = next_ref_id++;
	bool ok = htable_add(&ref_hash, int_hash(ptr->ref_id), ptr);
	return ok ? 0 : -ENOMEM;
}


void mapdb_destroy(struct map_db *db)
{
	htable_del(&ref_hash, int_hash(db->ref_id), db);

	struct htable_iter it;
	for(struct map_group *g = htable_first(&db->groups, &it);
		g != NULL;
		g = htable_next(&db->groups, &it))
	{
		flush_group(db, g, 0x7);
		free(g->entries);
		kmem_cache_free(map_group_slab, g);
	}

	htable_clear(&db->groups);
	db->space = NULL;
}


static struct map_group *group_for_addr(struct map_db *db, uintptr_t addr)
{
	uintptr_t key = GROUP_ADDR(addr);
	return htable_get(&db->groups, int_hash(key), &cmp_group_addr, &key);
}


/* TODO: needs thorough testing. */
static struct map_entry *probe_group_addr(struct map_group *g, uintptr_t addr)
{
	if(g->num_entries == 0) return NULL;

	/* common binary search. there's a segment that might handle sparse inputs
	 * correctly in there, but it's quite untested.
	 */
	int imin = 0, imax = g->num_entries - 1;
	int iters = 0;
	while(imax >= imin && ++iters < 100) {
		int probe = (imin + imax) / 2, slide = probe;
		struct map_entry *ent = &g->entries[probe];
#if 0
		if(L4_IsNilFpage(ent->range)) {
			/* sparse storage handling.
			 *
			 * TODO: maybe put this to use one day? the insertion routines
			 * don't currently produce sparse arrays, so this complexity is
			 * pointless.
			 *
			 * NOTE: handling of nil ranges should be added to everywhere that
			 * iterates over map_entries. that might be ugly.
			 */
			for(int i = probe - 1;
				i >= imin && L4_IsNilFpage(ent->range);
				i--)
			{
				assert(i >= imin && i <= imax);
				ent = &g->entries[i];
			}
			for(int i = probe + 1;
				L4_IsNilFpage(ent->range) && i <= imax;
				i++)
			{
				assert(i >= imin && i <= imax);
				ent = &g->entries[i];
			}
			if(L4_IsNilFpage(ent->range)) return NULL;
			slide = ent - &g->entries[0];
		}
#endif
		assert(slide >= 0 && slide < MAX_ENTRIES_PER_GROUP);

		if(addr < L4_Address(ent->range)) {
			imax = MIN(int, probe - 1, slide);
		} else if(addr >= L4_Address(ent->range) + L4_Size(ent->range)) {
			imin = MAX(int, probe + 1, slide);
		} else {
			return ent;
		}
	}
	assert(iters < 100);

	return NULL;
}


static struct map_entry *probe_group_range(struct map_group *g, L4_Fpage_t fpage)
{
	/* when in doubt, use brute force.
	 *
	 * this time it's because 1) the group occupancy bitmap wasn't known to be
	 * a good idea, and 2) it was also broken. this isn't the properly
	 * efficient version; instead, a range-to-range binary search operation
	 * should be written.
	 */
	for(L4_Word_t addr = L4_Address(fpage), lim = addr + L4_Size(fpage);
		addr < lim;
		addr += PAGE_SIZE)
	{
		struct map_entry *e = probe_group_addr(g, addr);
		if(e != NULL) return e;
	}

	return NULL;
}


static void coalesce_entries(
	struct map_group *g,
	struct map_entry *ent)
{
	const L4_Word_t size_mask = 1 << L4_SizeLog2(ent->range);
	bool is_low = (L4_Address(ent->range) & size_mask) == 0;
	int ent_ix = ent - g->entries, oth_ix = ent_ix + (is_low ? 1 : -1);
	struct map_entry *oth = &g->entries[oth_ix];
	if(oth_ix < 0 || oth_ix == g->num_entries
		|| L4_Rights(oth->range) != L4_Rights(ent->range)
		|| L4_SizeLog2(oth->range) != L4_SizeLog2(ent->range))
	{
		/* rejected. */
		return;
	}

	if(is_low) {
		SWAP(struct map_entry *, oth, ent);
		SWAP(int, ent_ix, oth_ix);
	}

	if(LAST_PAGE_ID(oth) + 1 == ent->first_page_id
		&& L4_Address(oth->range) + L4_Size(oth->range) == L4_Address(ent->range))
	{
		TRACE("%s: hit between %#lx:%#lx and %#lx:%#lx\n",
			__func__, L4_Address(oth->range), L4_Size(oth->range),
			L4_Address(ent->range), L4_Size(ent->range));

		oth->range = L4_FpageLog2(L4_Address(oth->range),
			L4_SizeLog2(oth->range) + 1);
		L4_Set_Rights(&oth->range, L4_Rights(ent->range));
		assert(LAST_PAGE_ID(oth) == LAST_PAGE_ID(ent));

		/* move stuff back one step into *ent */
		g->num_entries--;
		for(ent_ix = ent - g->entries; ent_ix < g->num_entries; ent_ix++) {
			g->entries[ent_ix] = g->entries[ent_ix + 1];
		}

		coalesce_entries(g, oth);
	}
}


/* attempts to merge the given parameters of mapdb_add_map() to previously
 * existing items.
 *
 * FIXME: this doesn't account for the bits in @parent !!!
 */
static bool merge_entries(
	struct map_group *g,
	int prev_pos,
	L4_Word_t parent,
	L4_Fpage_t fpage,
	uint32_t first_page_id)
{
	/* try merging. */
	struct map_entry *pred = &g->entries[prev_pos],
		*succ = &g->entries[prev_pos + 1];
	if((L4_Address(pred->range) & (1 << L4_SizeLog2(fpage))) == 0
		&& L4_SizeLog2(pred->range) == L4_SizeLog2(fpage)
		&& LAST_PAGE_ID(pred) + 1 == first_page_id
		&& L4_Address(pred->range) + L4_Size(pred->range) == L4_Address(fpage))
	{
		/* backward merge. */
		int access = L4_Rights(pred->range) | L4_Rights(fpage);
		pred->range = L4_FpageLog2(L4_Address(pred->range),
			L4_SizeLog2(pred->range) + 1);
		L4_Set_Rights(&pred->range, access);
		coalesce_entries(g, pred);
		return true;
	} else if(succ < &g->entries[g->num_entries]
		&& (L4_Address(succ->range) & (1 << L4_SizeLog2(fpage))) != 0
		&& L4_SizeLog2(succ->range) == L4_SizeLog2(fpage)
		&& succ->first_page_id == first_page_id + 1
		&& L4_Address(fpage) + L4_Size(fpage) == L4_Address(succ->range))
	{
		/* forward merge. */
		int access = L4_Rights(succ->range) | L4_Rights(fpage);
		succ->range = L4_FpageLog2(L4_Address(fpage), L4_SizeLog2(fpage) + 1);
		L4_Set_Rights(&succ->range, access);
		succ->first_page_id--;
		assert(succ->first_page_id == first_page_id);
		coalesce_entries(g, succ);
		return true;
	} else {
		return false;
	}
}


/* create a map group with a single map_entry. */
static int add_map_group(
	struct map_db *db,
	L4_Word_t parent,
	L4_Fpage_t fpage,
	uint32_t first_page_id)
{
	struct map_group *g = kmem_cache_zalloc(map_group_slab);
	if(unlikely(g == NULL)) return -ENOMEM;
	g->entries = malloc(sizeof(struct map_entry) * 2);
	if(unlikely(g->entries == NULL)) {
		kmem_cache_free(map_group_slab, g);
		return -ENOMEM;
	}
	g->start = GROUP_ADDR(L4_Address(fpage));
	g->num_alloc = 2;
	g->entries[0] = (struct map_entry){
		.parent = parent, .range = fpage, .first_page_id = first_page_id,
	};
	g->entries[1].range = L4_Nilpage;
	g->num_entries = 1;
	bool ok = htable_add(&db->groups, int_hash(g->start), g);
	if(unlikely(!ok)) {
		free(g->entries);
		kmem_cache_free(map_group_slab, g);
		return -ENOMEM;
	}

	return 0;
}


static int insert_map_entry(
	struct map_db *db,
	struct map_group *g,
	L4_Word_t parent,
	L4_Fpage_t fpage,
	uint32_t first_page_id)
{
	/* TODO: use a clever binary hoppity-skip algorithm here, and recycle that
	 * in the split-placement case in mapdb_add_map().
	 *
	 * NOTE: this stuff requires that entries be tightly packed at the
	 * beginning of g->entries[] . the algorithm won't compact holes to the
	 * right of the right-side ("prev") entry.
	 */
	int prev = -1;
	for(int i=0; i < g->num_entries; i++) {
		L4_Fpage_t e = g->entries[i].range;
		assert(L4_Address(e) + L4_Size(e) - 1 < L4_Address(fpage)
			|| L4_Address(fpage) + L4_Size(fpage) - 1 < L4_Address(e));
		if(L4_Address(e) < L4_Address(fpage)) prev = i; else break;
	}
	if(prev < 0 || !merge_entries(g, prev, parent,
		fpage, first_page_id))
	{
		int dst_pos;
		if(prev + 1 < g->num_alloc
			&& L4_IsNilFpage(g->entries[prev + 1].range))
		{
			dst_pos = prev + 1;
		} else {
			if(g->num_entries + 1 >= g->num_alloc) {
				int next_size = g->num_alloc > 0 ? g->num_alloc * 2 : 2;
				TRACE("resizing group at %p from %d to %d entries\n",
					g, g->num_alloc, next_size);
				struct map_entry *new_ents = realloc(g->entries,
					next_size * sizeof(struct map_entry));
				if(unlikely(new_ents == NULL)) return -ENOMEM;
				for(int i = g->num_alloc; i < next_size; i++) {
					new_ents[i].range = L4_Nilpage;
				}
				g->num_alloc = next_size;
				g->entries = new_ents;
			}
			dst_pos = prev + 1;
			if(dst_pos < g->num_entries) {
				struct map_entry prev_ent = g->entries[dst_pos];
				for(int i=dst_pos + 1; i <= g->num_entries; i++) {
					SWAP(struct map_entry, g->entries[i], prev_ent);
				}
			}
		}
		g->entries[dst_pos] = (struct map_entry){
			.parent = parent, .range = fpage,
			.first_page_id = first_page_id,
		};
		g->num_entries++;
	}

	return 0;
}


/* counterintuitively, if this function finds that "old" matches the page
 * range as well, fpage's rights are added to those in "old" rather than
 * replaced. this is consistent with the behaviour of L4's map operation in
 * all cases.
 */
static void replace_map_entry(
	struct map_db *db,
	struct map_group *g,
	struct map_entry *old,
	L4_Word_t parent,
	L4_Fpage_t fpage,
	uint32_t first_page_id)
{
	if(old->first_page_id == first_page_id) {
		/* matches content, also */
		L4_Set_Rights(&old->range,
			L4_Rights(old->range) | L4_Rights(fpage));
	} else {
		if(old->num_children > 1) {
			/* FIXME: revert these children to referring to this
			 * entry's parent
			 */
			free(old->children);
			old->children = NULL;
		}
		*old = (struct map_entry){
			.parent = parent, .range = fpage,
			.first_page_id = first_page_id,
		};
	}
}


/* this should be split up by case. there are at least four:
 * #1 -- map_group doesn't exist (trivial case)
 * #2 -- entry doesn't exist in map_group (insert case)
 * #3 -- entry exists and matches added map exactly (modify or no-op case)
 * #4 -- entry exists and doesn't match added map (split case)
 */
int mapdb_add_map(
	struct map_db *db,
	L4_Word_t parent,
	L4_Fpage_t fpage,
	uint32_t first_page_id)
{
	TRACE("%s: adding fpage at %#lx, size %#lx, access [%c%c%c], parent %#lx\n",
		__func__, L4_Address(fpage), L4_Size(fpage),
		CHECK_FLAG(L4_Rights(fpage), L4_Readable) ? 'r' : '-',
		CHECK_FLAG(L4_Rights(fpage), L4_Writable) ? 'w' : '-',
		CHECK_FLAG(L4_Rights(fpage), L4_eXecutable) ? 'x' : '-',
		parent);

	/* x86 no-NX hack. this lets the pagefault exception handler do
	 * pre-existing maps correctly.
	 */
	if(CHECK_FLAG(L4_Rights(fpage), L4_Readable)) {
		L4_Set_Rights(&fpage, L4_Rights(fpage) | L4_eXecutable);
	}

	struct map_group *g = group_for_addr(db, L4_Address(fpage));
	if(g == NULL) {
		/* no group. */
		int n = add_map_group(db, parent, fpage, first_page_id);
		if(unlikely(n != 0)) return n;
	} else {
		struct map_entry *old = probe_group_range(g, fpage);
		if(old == NULL) {
			/* not covered. */
			int n = insert_map_entry(db, g, parent, fpage, first_page_id);
			if(unlikely(n != 0)) return n;
		} else if(L4_SizeLog2(old->range) == L4_SizeLog2(fpage)) {
			/* exact match with old entry's form. */
			assert(L4_Address(old->range) == L4_Address(fpage));
			replace_map_entry(db, g, old, parent, fpage, first_page_id);
		} else {
			/* skip the no-op. */
			int page_offs = (L4_Address(fpage) - L4_Address(old->range)) >> PAGE_BITS;
			if(CHECK_FLAG_ALL(L4_Rights(old->range), L4_Rights(fpage))
				&& old->first_page_id + page_offs == first_page_id
				&& REF_SPACE(old->parent) == REF_SPACE(parent)
				&& (REF_ADDR(old->parent) + page_offs * PAGE_SIZE) == REF_ADDR(parent))
			{
				/* no-op, truley */
			} else {
				/* FIXME: apply discontiguate() somehow */
				panic("discontig case not written");
			}
		}
	}

#ifndef NDEBUG
	if(g == NULL) g = group_for_addr(db, L4_Address(fpage));
	dump_map_group(g);
#endif

	return 0;
}


static int grow_children_array(struct map_entry *ent)
{
	/* rehash old children into a larger hash table. if at least one doesn't
	 * succeed due to exceeded probe depth, try with larger tables until
	 * calloc() fails.
	 *
	 * (TODO: this could be exploitable. perhaps the hash should be randomized
	 * per map_entry from a pool seeded with the low 2 bits of rdtsc output.)
	 */
	int new_size = ent->num_children * 2;
	L4_Word_t *new_children;
	bool ok;
	do {
		new_children = calloc(new_size, sizeof(L4_Word_t));
		if(new_children == NULL) return -ENOMEM;

		int mask = new_size - 1,
			depth = MIN(int, new_size, MAX_PROBE_DEPTH);
		ok = true;
		for(int i=0; i < ent->num_children && ok; i++) {
			L4_Word_t c = ent->children[i];
			if(!REF_DEFINED(c)) continue;
			ok = false;
			for(int p = int_hash(c) & mask, end = p + depth - 1;
				p <= end;
				p++)
			{
				if(new_children[p & mask] == 0) {
					new_children[p & mask] = c;
					ok = true;
					break;
				}
			}
		}

		if(!ok) {
			new_size *= 2;
			free(new_children);
		}
	} while(!ok);

	free(ent->children);
	ent->children = new_children;
	ent->num_children = new_size;

	return 0;
}


/* returns 0, -ENOMEM, or -EEXIST.
 *
 * the probe for duplicates is done with a hashed starting position and a
 * maximum depth of MIN(num_children, 16), accessing 2 cache lines when words
 * are 32 bits wide. this means that a crowded bucket can trigger resizing of
 * the entry.
 */
static int mapdb_add_child(struct map_entry *ent, L4_Word_t child)
{
	assert(REF_DEFINED(child));
	if(ent->num_children == 0
		|| (ent->num_children == 1 && !REF_DEFINED(ent->child)))
	{
		ent->child = child;
		ent->num_children = 1;
	} else if(ent->num_children == 1) {
		L4_Word_t *new_children = malloc(sizeof(L4_Word_t) * 2);
		if(new_children == NULL) return -ENOMEM;
		int slot = int_hash(child) & 1;
		new_children[slot] = child;
		new_children[slot ^ 1] = ent->child;
		ent->children = new_children;
		ent->num_children = 2;
	} else {
		L4_Word_t *got = NULL;
		do {
			assert(POPCOUNT(ent->num_children) == 1);
			int mask = ent->num_children - 1, base = int_hash(child) & mask;
			for(int i=0, md = MIN(int, MAX_PROBE_DEPTH, ent->num_children);
				i < md;
				i++)
			{
				int probe = (i + base) & mask;
				L4_Word_t *c = &ent->children[probe];
				if(*c == child) return -EEXIST;
				else if(!REF_DEFINED(*c)) {
					if(got == NULL) got = c;
					if(*c != REF_TOMBSTONE) break;
				}
			}
			if(got == NULL) {
				int n = grow_children_array(ent);
				if(n < 0) return n;
			}
		} while(got == NULL);
		*got = child;
	}

	return 0;
}


/* does mappings of all physical pages inside map_page. skips holes in the
 * sender address space within the mapping (so pages in the receiver won't be
 * unmapped on overlap with empty.)
 *
 * FIXME: should catch and return -ENOMEM from mapdb_add_map() etc.
 */
int mapdb_map_pages(
	struct map_db *from_db,
	struct map_db *to_db,
	L4_Fpage_t map_page,
	L4_Word_t dest_addr)
{
	struct map_entry *first = NULL;
	struct map_group *grp;
	L4_Word_t first_addr = L4_Address(map_page),
		last_addr = L4_Address(map_page) + L4_Size(map_page) - 1;
	do {
		grp = group_for_addr(from_db, first_addr);
		if(grp != NULL) first = probe_group_addr(grp, first_addr);
		/* TODO: would this work? (or fold it into the += right-side.) */
		// else first_addr += GROUP_SIZE - PAGE_SIZE;
	} while(first == NULL && (first_addr += PAGE_SIZE) <= last_addr);

	if(first == NULL) {
		/* no pages; it's a no-op. */
		return 0;
	}

	if(L4_Address(first->range) == L4_Address(map_page)
		&& L4_Size(first->range) >= L4_Size(map_page))
	{
		assert(first_addr == L4_Address(map_page));
		/* the nice, common 1:1 case. (also works with a larger source range; it
		 * just has to start at offset 0. this is a distinct case because it is
		 * virtually always hit for 4-8 KiB pages.)
		 */
		L4_Fpage_t p = L4_FpageLog2(dest_addr, L4_SizeLog2(map_page));
		L4_Set_Rights(&p, L4_Rights(first->range) & L4_Rights(map_page));
		if(L4_Rights(p) != 0) {
			mapdb_add_map(to_db,
				MAPDB_REF(from_db->ref_id, L4_Address(first->range)),
				p, first->first_page_id);
			mapdb_add_child(first, MAPDB_REF(to_db->ref_id, L4_Address(p)));
		}
		return L4_Rights(p);
	} else if(first_addr == L4_Address(map_page)
		&& last_addr < L4_Address(first->range) + L4_Size(first->range))
	{
		/* the "entirely contained inside" case, common for 4KiB pages being
		 * mapped from inside a hugepage
		 */
		int offset = (first_addr - L4_Address(first->range)) >> PAGE_BITS;
		L4_Fpage_t p = L4_FpageLog2(dest_addr, L4_SizeLog2(map_page));
		L4_Set_Rights(&p, L4_Rights(first->range) & L4_Rights(map_page));
		if(L4_Rights(p) != 0) {
			mapdb_add_map(to_db,
				MAPDB_REF(from_db->ref_id,
					L4_Address(first->range) + offset * PAGE_SIZE),
				p, first->first_page_id + offset);
			mapdb_add_child(first, MAPDB_REF(to_db->ref_id, L4_Address(p)));
		}
		return L4_Rights(p);
	} else {
#if 0
		TRACE("%s: first->range %#lx:%#lx; map_page %#lx:%#lx\n", __func__,
			L4_Address(first->range), L4_Size(first->range),
			L4_Address(map_page), L4_Size(map_page));
#endif

		struct map_entry *ent = first;
		L4_Word_t pos = MAX(L4_Word_t, L4_Address(ent->range), first_addr),
			limit = last_addr + 1;
		while(pos < limit && ent != NULL && L4_Address(ent->range) < limit) {
			L4_Word_t start = MAX(L4_Word_t, pos, L4_Address(ent->range)),
				end = MIN(L4_Word_t, limit,
					L4_Address(ent->range) + L4_Size(ent->range));
			int p_offs = (start - L4_Address(ent->range)) >> PAGE_BITS,
				size_log2;
			L4_Word_t r_addr;
			for_page_range(start, end, r_addr, size_log2) {
				/* brute force, one at a time.
				 * should instead do a for_page_range() over the dest range.
				 */
				for(L4_Word_t i = 0;
					i < 1 << size_log2;
					i += PAGE_SIZE, p_offs++)
				{
					L4_Fpage_t p = L4_FpageLog2(
						dest_addr + r_addr - L4_Address(map_page) + i,
						PAGE_BITS);
					L4_Set_Rights(&p, L4_Rights(ent->range) & L4_Rights(map_page));
					if(L4_Rights(p) != 0) {
						mapdb_add_map(to_db,
							MAPDB_REF(from_db->ref_id,
								L4_Address(ent->range) + p_offs * PAGE_SIZE),
							p, ent->first_page_id + p_offs);
						mapdb_add_child(ent,
							MAPDB_REF(to_db->ref_id, L4_Address(p)));
					}
				}
			}

			/* next entry. */
			if(ent < &grp->entries[grp->num_entries - 1]) ent++;
			else {
				/* next group, even */
				uintptr_t g_start = grp->start + GROUP_SIZE;
				grp = NULL;
				for(uintptr_t grp_addr = g_start;
					grp == NULL && grp_addr <= last_addr;
					grp_addr += GROUP_SIZE)
				{
					grp = group_for_addr(from_db, grp_addr);
				}
				ent = grp != NULL ? &grp->entries[0] : NULL;
			}

			pos = end;
		}
	}

	return 0;
}


/* enough space = MSB(size) - 11 map_entries */
static int make_pages_for_range(
	L4_Fpage_t *dst,
	L4_Word_t start,
	L4_Word_t size)
{
	assert(((start | size) & 0xfff) == 0);

	L4_Word_t addr;
	int sizelog2, p = 0;
	for_page_range(start, start + size, addr, sizelog2) {
		dst[p++] = L4_FpageLog2(addr, sizelog2);
	}
	return p;
}


/* blanks appear after *entry. it is updated after group resize. */
static int insert_blanks(
	struct map_group *g,
	int num_to_add,
	struct map_entry **entry)
{
	assert(*entry >= g->entries && *entry < &g->entries[g->num_alloc]);
	struct map_entry *e = *entry;

	int need = g->num_entries + num_to_add,
		num_tail = g->num_entries - (e - g->entries) - 1;
	if(need > g->num_alloc) {
		/* make moar RAMz */
		int e_pos = e - g->entries;
		assert(e_pos < g->num_entries);
		int newsize = g->num_alloc * 2;
		while(newsize < need) newsize *= 2;
		TRACE("%s: resizing group from %u to %d entries\n", __func__,
			g->num_alloc, newsize);
		void *ptr = realloc(g->entries, newsize * sizeof(struct map_entry));
		if(ptr == NULL) return -ENOMEM;
		g->entries = ptr;
		g->num_alloc = newsize;
		e = &g->entries[e_pos];
		*entry = e;
	}
	assert(g->num_alloc >= need);

	if(num_tail > 0) {
		/* poor man's memmove(3) (TODO) */
		TRACE("%s: move %d items to %d (*entry at %d)\n", __func__, num_tail,
			(e + num_to_add + 1) - g->entries, e - g->entries);
		size_t len = sizeof(struct map_entry) * num_tail;
		void *tmp = malloc(len);
		if(tmp == NULL) return -ENOMEM;
		memcpy(tmp, e + 1, len);
		memcpy(e + num_to_add + 1, tmp, len);
		free(tmp);
	}

	return 0;
}


/* find which children in the previously-valid @from are valid, and add those
 * into the corresponding current entries in @g.
 *
 * on alloc failure this leaves entries in an uncertain-but-valid state and
 * returns -ENOMEM. idempotent due to mapdb_add_child() being so.
 */
static int distribute_children(
	struct map_db *local_db,
	struct map_group *g,
	L4_Fpage_t cut,
	const struct map_entry *from)
{
	if(from->num_children == 0) return 0;

	/* TODO: this is nice for one or two children, and less so for more. when
	 * hugepage support is written, this part should sort children by ref_id
	 * when there are enough, and cache the corresponding database to avoid
	 * hash table lookups -- 4M hugepages in a memory server would be mapped
	 * to at least 1024 child pages under full utilization, for instance.
	 */
	const L4_Word_t *children = from->num_children <= 1
		? &from->child : from->children;
	for(int i=0; i < from->num_children; i++) {
		struct child_ref r;
		if(!deref_child(&r, local_db, from, i)) continue;

		L4_Word_t p_addr = L4_Address(from->range) + r.parent_offset;
		struct map_entry *p_ent = mapdb_probe(local_db, p_addr);
		if(p_ent == NULL) {
			/* discard child due to hole made in parent */
			continue;
		}

		int n = mapdb_add_child(p_ent, children[i]);
		if(n == -ENOMEM) return -ENOMEM;
	}

	return 0;
}


static int split_entry(
	struct map_db *db,
	struct map_group *g,
	struct map_entry *e,
	L4_Fpage_t cut)
{
	/* a maximum group has just a single 4 MiB entry. at most this breaks up
	 * into one each of the smaller sizes and two of the smallest. so 11
	 * entries for each potential copy operation is definitely enough, plus
	 * one for the cut itself.
	 */
	L4_Fpage_t pg_buf[23];
	int p = 0;

	L4_Word_t r_first = L4_Address(cut);
	if(L4_Address(e->range) < r_first) {
		/* left side */
		p += make_pages_for_range(&pg_buf[p], L4_Address(e->range),
			r_first - L4_Address(e->range));
	}
	pg_buf[p++] = cut;	/* middle */
	L4_Word_t r_end = L4_Address(cut) + L4_Size(cut);
	if(L4_Address(e->range) + L4_Size(e->range) > r_end) {
		/* right side */
		p += make_pages_for_range(&pg_buf[p], r_end,
			L4_Address(e->range) + L4_Size(e->range) - r_end);
	}
	assert(p > 1);		/* forbid the trivial case */

	int n = insert_blanks(g, p - 1, &e);
	if(unlikely(n < 0)) return n;

	struct map_entry saved = *e;
	L4_Word_t addr_offset = 0;
	for(int i=0; i < p; i++) {
		e[i] = (struct map_entry){
			.range = pg_buf[i],
			.parent = REF_DEFINED(saved.parent)
					? MAPDB_REF(REF_SPACE(saved.parent),
						REF_ADDR(saved.parent) + addr_offset)
					: 0,
			.first_page_id = saved.first_page_id + (addr_offset >> PAGE_BITS),
			.access = saved.access,
			.num_children = 0,
		};
		L4_Set_Rights(&e[i].range, L4_Rights(saved.range));
		TRACE("%s: set ents[%d] = range %#lx:%#lx, parent %#lx, first page %u, rwx %#lx\n",
			__func__, e - g->entries + i, L4_Address(e[i].range), L4_Size(e[i].range),
			e[i].parent, e[i].first_page_id, L4_Rights(e[i].range));
		addr_offset += L4_Size(pg_buf[i]);
	}
	g->num_entries += p - 1;

	if(saved.num_children > 0) {
		distribute_children(db, g, cut, &saved);
	}

	return 0;
}


/* render a map_group's map_entries discontiguous in such a way that entries
 * covered by "range" fit inside it exactly. returns the first entry covered
 * by "range".
 */
static struct map_entry *discontiguate(
	struct map_db *db,
	struct map_group *g,
	L4_Fpage_t range)
{
	int err;
	TRACE("%s: group %#lx, range %#lx:%#lx\n", __func__, g->start,
		L4_Address(range), L4_Size(range));
	/* test the first and last entries in the group that fall within
	 * `range`.
	 */
	struct map_entry *e = probe_group_range(g, range);
	if(e == NULL) return NULL;
//	TRACE("%s: e %#lx:%#lx\n", __func__, L4_Address(e->range),
//		L4_Size(e->range));
	bool new_e = false;
	L4_Word_t r_start = L4_Address(range);
	if(L4_Address(e->range) < r_start
		|| (L4_Address(e->range) == r_start
			&& L4_Size(e->range) > L4_Size(range)))
	{
		err = split_entry(db, g, e, range);
		if(err < 0) goto fail;
		new_e = true;
	}

	/* then the last entry. */
	if(L4_Size(range) > PAGE_SIZE) {
		L4_Word_t r_end = r_start + L4_Size(range);
		struct map_entry *last = probe_group_range(g, L4_FpageLog2(
			L4_Address(range) + L4_Size(range) - PAGE_SIZE, PAGE_BITS));
//		TRACE("%s: last %#lx:%#lx\n", __func__,
//			L4_Address(last->range), L4_Size(last->range));
		/* ... does this work? */
		if(last != NULL
			&& (L4_Address(last->range) != r_start
				|| L4_Size(last->range) != L4_Size(range))
			&& L4_Address(last->range) < r_end)
		{
			err = split_entry(db, g, last, range);
			if(err < 0) goto fail;
			new_e = true;
		}
	}

	if(new_e) {
//		TRACE("%s: dumping the modified group\n", __func__);
//		dump_map_group(g);

		e = probe_group_range(g, range);
		assert(e != NULL);		/* guaranteed by previous "e" */
	}

	return e;

fail:
	/* FIXME: have a proper exit path here */
	panic("split_entry() failed: out of kernel heap");
	return NULL;
}


int mapdb_unmap_fpage(
	struct map_db *db,
	L4_Fpage_t range,
	bool immediate,
	bool recursive,
	bool clear_stored_access)
{
	assert(recursive || immediate);	/* disallows the one-level status read */

	int rwx_seen = 0;

	TRACE("%s: range %#lx:%#lx, %simmediate, %srecursive, ref_id %d\n",
		__func__, L4_Address(range), L4_Size(range),
		!immediate ? "non-" : "", !recursive ? "non-" : "",
		(int)db->ref_id);
	L4_Word_t unmap_rights = L4_Rights(range),
		r_end = L4_Address(range) + L4_Size(range);
	/* this function will only call discontiguate() to modify the structure of
	 * the map_groups it accesses when it might revoke access in the immediate
	 * map_db.
	 */
	const bool modify = immediate && unmap_rights != 0;
	for(L4_Word_t grp_pos = L4_Address(range);
		grp_pos < r_end;
		grp_pos += GROUP_SIZE)
	{
		struct map_group *g = group_for_addr(db, grp_pos);
		if(g == NULL) {
			TRACE("%s: group for %#lx doesn't exist\n", __func__, grp_pos);
			continue;
		}

		struct map_entry *e = NULL;
		if(modify) {
			e = discontiguate(db, g, range);
			/* FIXME: discontiguate() can also fail due to ENOMEM. in this
			 * case the operation should be put to sleep (pending restart on
			 * some condition) and some sort of handler invoked to acquire
			 * more kernel memory to expand the heap, or to release allocated
			 * memory to create sufficiently large free heap segments, in each
			 * case triggering syscall restart.
			 *
			 * (for now the malloc() failure is handled with panic(). that'll
			 * start happening once the kernel heap reaches a couple of
			 * megabytes. the kernel address space limit doesn't matter
			 * because of the tiny amount of free RAM, some of which will be
			 * locked away in page tables and other recreatable structures.
			 * the quick workaround is just to increase the kernel
			 * allocation.)
			 */
		} else {
			e = probe_group_range(g, range);
		}
		if(e == NULL) continue;

		L4_Word_t r_pos = L4_Address(e->range);
		do {
			/* check each native page.
			 *
			 * TODO: extend to support big pages as specified in the related
			 * KIP field.
			 */
			int e_mask = 0;
			do {
				L4_Word_t next = 0;
				int pmask = space_probe_pt_access(&next, db->space, r_pos);
				r_pos += PAGE_SIZE;
				if(pmask >= 0) {
					e_mask |= pmask;
				} else {
					assert(pmask == -ENOENT);
					if(next > r_pos) r_pos = next;
				}
			} while(r_pos < L4_Address(e->range) + L4_Size(e->range)
				&& e_mask != L4_FullyAccessible);

			if(e_mask != 0 && REF_DEFINED(e->parent)) {
				/* FIXME: propagate e_mask to parent */
			}

			rwx_seen |= (e_mask | e->access);

			/* Urist McBreedington cancels store item in stockpile: seeking
			 * infant.
			 */
			for(int i=0; recursive && i < e->num_children; i++) {
				struct child_ref r;
				if(!deref_child(&r, db, e, i)) {
					if(e->num_children < 2) e->child = 0;
					else e->children[i] = REF_TOMBSTONE;
					continue;
				}

				TRACE("deref child %d (%#lx) -> %#lx:%#lx, p_o %d\n",
					i, e->num_children < 2 ? e->child : e->children[i],
					L4_Address(r.child_entry->range),
					L4_Size(r.child_entry->range), r.parent_offset);

				/* intersection with e->range in the child entry. this is for
				 * the case where the parent is large and the child is small,
				 * as happens with hugepages.
				 */
				int size_log2;
				bool sp_changed = false;	/* TODO: track in struct space */
				L4_Word_t address;
				int ric_offs = r.parent_offset * PAGE_SIZE
					+ (L4_Address(e->range) - L4_Address(range));
				for_page_range(
					MAX(L4_Word_t, L4_Address(r.child_entry->range),
						L4_Address(r.child_entry->range) - ric_offs),
					MIN(L4_Word_t, FPAGE_HIGH(r.child_entry->range),
						L4_Address(r.child_entry->range) + L4_Size(range)
							- ric_offs) + 1,
					address, size_log2)
				{
					L4_Fpage_t fp = L4_FpageLog2(address, size_log2);
					L4_Set_Rights(&fp, unmap_rights);
					int pass_rwx = mapdb_unmap_fpage(r.child_db, fp,
						true, true, false);
					/* TODO: instead, call space_put_page() in
					 * mapdb_unmap_fpage() end, or better yet, something that
					 * modifies the MMU-level access bits.
					 */
					for(L4_Word_t a = L4_Address(fp);
						a < L4_Address(fp) + L4_Size(fp);
						a += PAGE_SIZE)
					{
						space_put_page(r.child_db->space, a, 0, 0);
						sp_changed = true;
					}

					rwx_seen |= pass_rwx;
				}
				if(sp_changed) space_commit(r.child_db->space);
			}

			r_pos = L4_Address(e->range) + L4_Size(e->range);
			bool drop = false;
			if(modify) {
				/* ensured by discontiguate() */
				assert(L4_Address(e->range) >= L4_Address(range));
				assert(L4_Address(e->range) + L4_Size(e->range) <= r_end);

				int new_r = L4_Rights(e->range) & ~unmap_rights;
				L4_Set_Rights(&e->range, new_r);
				if(new_r == 0) drop = true;
			}
			if(drop) {
				assert(modify);
				/* TODO: implement memmove(3), use it */
				TRACE("%s: removing entry %#lx:%#lx\n", __func__,
					L4_Address(e->range), L4_Size(e->range));
				int pos = e - g->entries;
				if(pos < g->num_entries - 1) {
					int copy_num = g->num_entries - 1 - pos;
					size_t c_size = copy_num * sizeof(struct map_entry);
					void *tmp = malloc(c_size);
					assert(tmp != NULL);		/* FIXME */
					memcpy(tmp, &g->entries[pos + 1], c_size);
					memcpy(&g->entries[pos], tmp, c_size);
					free(tmp);
				}
				g->num_entries--;
				/* map_group shrinking (but don't go below 8 items) */
				if(g->num_entries <= g->num_alloc / 2 - g->num_alloc / 8
					&& g->num_alloc > 8)
				{
					int e_pos = e - g->entries, newsize = g->num_alloc / 2;
					void *ptr = realloc(g->entries,
						newsize * sizeof(struct map_entry));
					if(ptr != NULL) {
						g->entries = ptr;
						g->num_alloc = newsize;
						e = &g->entries[e_pos];
					}
				}
			}

			e->access = clear_stored_access ? 0 : rwx_seen;
		} while(++e < &g->entries[g->num_entries] && r_pos < r_end);

		dump_map_group(g);
	}

	return rwx_seen;
}


struct map_entry *mapdb_probe(
	struct map_db *db,
	uintptr_t addr)
{
	struct map_group *g = group_for_addr(db, addr);
	if(g == NULL) return NULL;

	assert(addr >= g->start);
	assert(addr < g->start + GROUP_SIZE);
	return probe_group_addr(g, addr);
}


COLD void init_mapdb(void)
{
	map_group_slab = kmem_cache_create("map_group_slab",
		sizeof(struct map_group), ALIGNOF(struct map_group),
		0, NULL, NULL);
}


COLD void mapdb_init_range(
	struct map_db *db,
	uintptr_t start_addr,
	const uint32_t *page_ids,
	unsigned int num_pages,
	int entry_flags)
{
#if 1
	TRACE("%s: start_addr %#lx, num_pages %u (%#x bytes)\n", __func__,
		(L4_Word_t)start_addr, num_pages, num_pages * PAGE_SIZE);
#endif
#ifndef NDEBUG
	unsigned int done = 0;
#endif
	for(uintptr_t g_addr = GROUP_ADDR(start_addr),
				  g_last = GROUP_ADDR(start_addr + num_pages * PAGE_SIZE - 1);
		g_addr <= g_last;
		g_addr += GROUP_SIZE)
	{
		uintptr_t range_start = MAX(uintptr_t, start_addr, g_addr),
			range_end = MIN(uintptr_t, start_addr + num_pages * PAGE_SIZE,
				g_addr + GROUP_SIZE) - 1;
		struct map_group *g = group_for_addr(db, g_addr);
		if(g == NULL) {
			g = kmem_cache_zalloc(map_group_slab);
			if(g == NULL) {
				/* there's no point in handling ENOMEM in early boot. */
				panic("mapdb_init_range() [early boot call] can't allocate map group");
			}
			g->start = g_addr;
			assert((g->start & (GROUP_SIZE - 1)) == 0);
			bool ok = htable_add(&db->groups, int_hash(g->start), g);
			if(!ok) {
				panic("mapdb_init_range() [early boot call] htable_add() failed");
			}
		}

		/* - search page_ids[id_offset ...] for contiguous blocks of
		 *   appropriate alignment (for the current address between [range_start
		 *   .. range_end]);
		 */
		uintptr_t range_pos = range_start;
		int range_len = (range_end - range_start + 1) >> PAGE_BITS;
		while(range_pos <= range_end) {
			int b = ffsl(range_pos);
//			TRACE("%s: range_pos %#lx, b %d\n", __func__, range_pos, b);
			assert(b == 0
				|| __builtin_popcount(range_pos) == 1
				|| ffsl(range_pos & ~(1 << (b - 1))) > b);

			/* 4 MiB at most */
			if(unlikely(b == 0)) b = 22; else b = MIN(int, 22, b - 1);
			int r_ix = (range_pos - range_start) >> PAGE_BITS,
				seg = MIN(int, 1 << (b - 12), range_len - r_ix),
				id_offset = (range_pos - start_addr) >> PAGE_BITS;
			for(int i=1, last = page_ids[id_offset]; i < seg; i++) {
				if(page_ids[id_offset + i] != last + 1) {
					seg = i;
					break;
				}
				last++;
				assert(last == page_ids[id_offset + i]);
			}
			assert(seg > 0);
			int mag = sizeof(int) * 8 - __builtin_clz(seg) - 1 + 12;
			assert(1 << mag <= seg * PAGE_SIZE);
			/* - then pass those one by one to mapdb_add_map(db, fpage, first_id,
			 *   [rwx])
			 */
			L4_Fpage_t page = L4_FpageLog2(range_pos, mag);
			assert((range_pos & (L4_Size(page) - 1)) == 0);
			L4_Set_Rights(&page, L4_FullyAccessible);
			if(mapdb_add_map(db, 0, page, page_ids[id_offset]) < 0) {
				panic("mapdb_init_range() [early boot call] mapdb_add_map() failed");
			}

			range_pos += 1 << mag;
#ifndef NDEBUG
			done += 1 << (mag - 12);
#endif
		}
	}

	assert(done == num_pages);
}
