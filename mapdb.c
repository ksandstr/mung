
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
#include <ukernel/bug.h>
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
	struct map_group *group;
	struct map_entry *child_entry;
};


static size_t rehash_ref_hash(const void *, void *);

static struct map_entry *discontiguate(
	struct map_db *db,
	struct map_group *g,
	L4_Fpage_t range);

static struct map_group *group_for_addr(struct map_db *db, uintptr_t addr);

static struct map_entry *probe_group_addr(struct map_group *g, uintptr_t addr);

static bool deref_child(
	struct child_ref *cr,
	struct map_db *home_db,
	const struct map_entry *e,
	int child_ix);

/* (how come this function is both static, and prefixed?) */
static int mapdb_add_child(struct map_entry *ent, L4_Word_t child);


static struct kmem_cache *map_group_slab = NULL;
static uint32_t next_ref_id = 1;	/* also the kernel space's ID */
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
		TRACE("  %d: [%#lx .. %#lx] (%c%c%c) from %#lx, pages [%u .. %lu]; nc %u\n", i,
			L4_Address(e->range), L4_Address(e->range) + L4_Size(e->range) - 1,
			CHECK_FLAG(L4_Rights(e->range), L4_Readable) ? 'r' : '-',
			CHECK_FLAG(L4_Rights(e->range), L4_Writable) ? 'w' : '-',
			CHECK_FLAG(L4_Rights(e->range), L4_eXecutable) ? 'x' : '-',
			e->parent, e->first_page_id,
			e->first_page_id + L4_Size(e->range) / PAGE_SIZE - 1,
			(unsigned)e->num_children);
	}
#endif
}


static inline struct map_db *find_map_db(uint32_t ref_id)
{
	void *ptr = htable_get(&ref_hash, int_hash(ref_id),
		&int_eq, &ref_id);
	return ptr == NULL ? NULL : container_of(ptr, struct map_db, ref_id);
}


/* as above, but stricter. */
static struct map_db *get_map_db(uint32_t ref_id)
{
	struct map_db *db = find_map_db(ref_id);
	BUG_ON(db == NULL, "parameter must refer to valid map_db");
	return db;
}


/* "don't test this" bits */
#define MOD_NO_CHILD_REFS (1 << 0)	/* don't check child refs */

#ifdef DEBUG_ME_HARDER
#include <ukernel/invariant.h>

/* runtime invariant checks. */
static bool check_mapdb(struct map_db *db, int opts)
{
	INV_CTX;

	/* database-side consistency. */
	struct htable_iter grp_it;
	for(void *grp_ptr = htable_first(&db->groups, &grp_it);
		grp_ptr != NULL;
		grp_ptr = htable_next(&db->groups, &grp_it))
	{
		struct map_group *grp = container_of(grp_ptr,
			struct map_group, start);

		/* for each entry, check that
		 *   - it references a valid entry (one that exists)
		 *   - it is at most as large as the parent
		 *   - the parent has a child reference to it
		 */
		for(int i=0; i < grp->num_entries; i++) {
			const struct map_entry *e = &grp->entries[i];
			inv_ok1(L4_Rights(e->range) != 0);

			if(!REF_DEFINED(e->parent)) continue;
			inv_push("check entry %#lx:%#lx in ref_id %u; ->parent %#lx",
				L4_Address(e->range), L4_Size(e->range), db->ref_id,
				e->parent);
			struct map_db *p_db = find_map_db(REF_SPACE(e->parent));
			inv_ok1(p_db != NULL);

			const struct map_entry *p_e = mapdb_probe(p_db,
				REF_ADDR(e->parent));
			inv_ok1(p_e != NULL);
			inv_log("parent entry %#lx:%#lx in ref_id %u",
				L4_Address(p_e->range), L4_Size(p_e->range), p_db->ref_id);
			inv_ok1(ADDR_IN_FPAGE(p_e->range, REF_ADDR(e->parent)));
			inv_ok1(L4_SizeLog2(e->range) <= L4_SizeLog2(p_e->range));

			if(!CHECK_FLAG(opts, MOD_NO_CHILD_REFS)) {
				bool found = false;
				int n_push = 0;
				const L4_Word_t *p_cs = p_e->num_children > 1
					? p_e->children : &p_e->child;
				for(int j=0; j < p_e->num_children; j++) {
					n_push++;
					inv_log("  child %d = %#lx", j, p_cs[j]);
					if(REF_SPACE(p_cs[j]) != db->ref_id) continue;
					if(ADDR_IN_FPAGE(e->range, REF_ADDR(p_cs[j]))) {
						found = true;

						/* test deref_child() since the loop provides us with
						 * known results.
						 */
						struct child_ref cr;
						bool got_child = deref_child(&cr, p_db, p_e, j);
						inv_ok1(got_child);
						inv_ok1(cr.child_db == db);
						inv_ok1(cr.child_entry == e);
					}
				}
				inv_ok1(found);
			}

			inv_pop();
		}
	}

	/* page table consistency. */
	/* (TODO: iterate over the tables in db->space, check that @db agrees with
	 * pages referenced)
	 */

	return true;

inv_fail:
	return false;
}


static bool check_mapdb_module(int opts)
{
	struct htable_iter it;
	for(void *ptr = htable_first(&ref_hash, &it);
		ptr != NULL;
		ptr = htable_next(&ref_hash, &it))
	{
		struct map_db *db = container_of(ptr, struct map_db, ref_id);
		if(!check_mapdb(db, opts)) return false;
	}

	return true;
}
#else
#define check_mapdb_module(foo) (true)
#define check_mapdb(foo, bar) (true)
#endif


static inline struct map_entry *lookup_ref(
	struct map_db **db_p,
	struct map_group **group_p,
	L4_Word_t ref)
{
	assert(REF_DEFINED(ref));

	struct map_db *db = find_map_db(REF_SPACE(ref));
	if(db == NULL) return NULL;

	/* (same as mapdb_probe(), but we'll keep @g.) */
	struct map_group *g = group_for_addr(db, REF_ADDR(ref));
	if(g == NULL) return NULL;
	assert(REF_ADDR(ref) >= g->start);
	assert(REF_ADDR(ref) < g->start + GROUP_SIZE);

	if(group_p != NULL) *group_p = g;
	if(db_p != NULL) *db_p = db;
	return probe_group_addr(g, REF_ADDR(ref));
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
	struct map_db *db = find_map_db(space_id);
	if(db == NULL) {
		TRACE("%s: mapdb ref_id %u not found\n", __func__,
			(unsigned)space_id);
		return false;
	}

	/* (same as mapdb_probe(), but we'll keep @g.) */
	struct map_group *g = group_for_addr(db, child_addr);
	if(g == NULL) return false;
	assert(child_addr >= g->start);
	assert(child_addr < g->start + GROUP_SIZE);
	struct map_entry *ce = probe_group_addr(g, child_addr);
	if(ce == NULL) {
		TRACE("%s: address %#lx not found in child ref_id %u\n", __func__,
			child_addr, space_id);
		return false;
	}

	/* a valid child refers to the home space, and into the home range. */
	if(REF_SPACE(ce->parent) != home_db->ref_id
		|| !BETWEEN(FPAGE_LOW(e->range), FPAGE_HIGH(e->range),
				REF_ADDR(ce->parent)))
	{
		TRACE("%s: backref %#lx mismatches space %u, or range %#lx .. %#lx\n",
			__func__, ce->parent, home_db->ref_id,
			FPAGE_LOW(e->range), FPAGE_HIGH(e->range));
		return false;
	}

	/* check that the physical page is the same, too. */
	L4_Word_t off_in_parent = REF_ADDR(ce->parent) - L4_Address(e->range);
	uint32_t off_pages = off_in_parent >> PAGE_BITS;
	if(ce->first_page_id != e->first_page_id + off_pages) {
		TRACE("%s: page mismatch (child first %u, parent first %u, offset %u)\n",
			__func__, ce->first_page_id, e->first_page_id, off_pages);
		return false;
	}

	cr->child_db = db;
	cr->group = g;
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

	assert(check_mapdb_module(0));
}


static struct map_group *group_for_addr(struct map_db *db, uintptr_t addr)
{
	uintptr_t key = GROUP_ADDR(addr);
	return htable_get(&db->groups, int_hash(key), &cmp_group_addr, &key);
}


static struct map_entry *probe_group_addr(struct map_group *g, uintptr_t addr)
{
	if(g->num_entries == 0) return NULL;

	/* common binary search. */
	int imin = 0, imax = g->num_entries - 1;
	int iters = 0;
	while(imax >= imin && ++iters < 100) {
		int probe = (imin + imax) / 2, slide = probe;
		struct map_entry *ent = &g->entries[probe];
		/* NOTE: could do sparse storage support here. see commit
		 * 3a617b7947ab4ff1badc51254cf70a41e34d66d1 for a related code
		 * fragment that used to be here.
		 */
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


/* finds the leftmost entry in @g that overlaps @fpage. */
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


/* pass NULL for @limit_parent except to recurse. */
static void coalesce_entries(
	struct map_group *g,
	struct map_entry *ent,
	struct map_entry *limit_parent)
{
	/* this function must check that children are never joined to become
	 * larger than their parents. so the first iteration's parent should act
	 * as a limit; skipping for toplevel mappings (for which this function is
	 * quite rare).
	 */
	if(likely(REF_DEFINED(ent->parent))) {
		if(limit_parent == NULL) {
			limit_parent = lookup_ref(NULL, NULL, ent->parent);
			assert(limit_parent != NULL);
		}

		if(L4_SizeLog2(ent->range) >= L4_SizeLog2(limit_parent->range)) {
			return;
		}
	}

	const L4_Word_t size_mask = 1 << L4_SizeLog2(ent->range);
	bool is_low = (L4_Address(ent->range) & size_mask) == 0;
	int ent_ix = ent - g->entries, oth_ix = ent_ix + (is_low ? 1 : -1);
	struct map_entry *oth = &g->entries[oth_ix];
	if(oth_ix < 0 || oth_ix == g->num_entries
		|| L4_Rights(oth->range) != L4_Rights(ent->range)
		|| L4_SizeLog2(oth->range) != L4_SizeLog2(ent->range)
		|| REF_SPACE(oth->parent) != REF_SPACE(ent->parent))
	{
		/* rejected. */
		return;
	}

	if(is_low) {
		SWAP(struct map_entry *, oth, ent);
		SWAP(int, ent_ix, oth_ix);
	}

	if(LAST_PAGE_ID(oth) + 1 == ent->first_page_id
		&& L4_Address(oth->range) + L4_Size(oth->range) == L4_Address(ent->range)
		&& ((!REF_DEFINED(oth->parent) && !REF_DEFINED(ent->parent))
			|| REF_ADDR(oth->parent) + L4_Size(oth->range) == REF_ADDR(ent->parent)))
	{
		TRACE("%s: hit between %#lx:%#lx and %#lx:%#lx\n",
			__func__, L4_Address(oth->range), L4_Size(oth->range),
			L4_Address(ent->range), L4_Size(ent->range));

		oth->range = L4_FpageLog2(L4_Address(oth->range),
			L4_SizeLog2(oth->range) + 1);
		L4_Set_Rights(&oth->range, L4_Rights(ent->range));
		assert(LAST_PAGE_ID(oth) == LAST_PAGE_ID(ent));
		/* FIXME: handle ENOMEM in all cases! */
		if(ent->num_children == 1 && REF_DEFINED(ent->child)) {
			mapdb_add_child(oth, ent->child);
		} else if(ent->num_children > 1) {
			for(int i=0; i < ent->num_children; i++) {
				if(!REF_DEFINED(ent->children[i])) continue;
				mapdb_add_child(oth, ent->children[i]);
			}
			free(ent->children);
			ent->num_children = 0;
		}

		/* move stuff back one step into *ent */
		g->num_entries--;
		for(ent_ix = ent - g->entries; ent_ix < g->num_entries; ent_ix++) {
			g->entries[ent_ix] = g->entries[ent_ix + 1];
		}

		coalesce_entries(g, oth, limit_parent);
	}
}


/* attempts to record the equivalent of a map_entry with the given parameters
 * by expanding an existing map_entry. returns true on success.
 */
static bool merge_entries(
	struct map_group *g,
	int prev_pos,
	L4_Word_t parent,
	L4_Fpage_t fpage,
	uint32_t first_page_id)
{
	struct map_entry *pred = &g->entries[prev_pos],
		*succ = &g->entries[prev_pos + 1];
	if((L4_Address(pred->range) & (1 << L4_SizeLog2(fpage))) == 0
		&& ((!REF_DEFINED(parent) && !REF_DEFINED(pred->parent))
			|| (REF_SPACE(pred->parent) == REF_SPACE(parent)
				&& REF_ADDR(pred->parent) + L4_Size(pred->range) == REF_ADDR(parent)))
		&& L4_SizeLog2(pred->range) == L4_SizeLog2(fpage)
		&& LAST_PAGE_ID(pred) + 1 == first_page_id
		&& L4_Address(pred->range) + L4_Size(pred->range) == L4_Address(fpage))
	{
		/* backward merge. */
		int access = L4_Rights(pred->range) | L4_Rights(fpage);
		pred->range = L4_FpageLog2(L4_Address(pred->range),
			L4_SizeLog2(pred->range) + 1);
		L4_Set_Rights(&pred->range, access);
		coalesce_entries(g, pred, NULL);
		return true;
	} else if(succ < &g->entries[g->num_entries]
		&& (L4_Address(succ->range) & (1 << L4_SizeLog2(fpage))) != 0
		&& ((!REF_DEFINED(parent) && !REF_DEFINED(pred->parent))
			|| (REF_SPACE(succ->parent) == REF_SPACE(parent)
				&& REF_ADDR(parent) + L4_Size(fpage) == REF_ADDR(succ->parent)))
		&& L4_SizeLog2(succ->range) == L4_SizeLog2(fpage)
		&& succ->first_page_id == first_page_id + 1
		&& L4_Address(fpage) + L4_Size(fpage) == L4_Address(succ->range))
	{
		/* forward merge. */
		int access = L4_Rights(succ->range) | L4_Rights(fpage);
		succ->range = L4_FpageLog2(L4_Address(fpage), L4_SizeLog2(fpage) + 1);
		L4_Set_Rights(&succ->range, access);
		succ->first_page_id--;
		succ->parent = parent;
		assert(succ->first_page_id == first_page_id);
		coalesce_entries(g, succ, NULL);
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
	/* [v1] TODO: use a clever binary hoppity-skip algorithm here, and recycle
	 * that in the split-placement case in mapdb_add_map().
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
	assert(L4_Address(old->range) == L4_Address(fpage));
	assert(L4_SizeLog2(old->range) == L4_SizeLog2(fpage));

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


int mapdb_add_map(
	struct map_db *db,
	L4_Word_t parent,
	L4_Fpage_t fpage,
	uint32_t first_page_id)
{
	assert(check_mapdb_module(0));

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

	/* there are five cases:
	 *
	 * #1 -- map_group doesn't exist (trivial)
	 * #2 -- entry doesn't exist in map_group (insert)
	 * #3 -- entry exists and matches added map exactly (modify or no-op case)
	 * #4 -- entry exists and contains added map (no-op or split case)
	 * #5 -- entry exists and is contained in added map (scan & replace case)
	 */
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
		} else if(L4_SizeLog2(old->range) > L4_SizeLog2(fpage)) {
			/* "contained" case. */
			int page_offs = (L4_Address(fpage) - L4_Address(old->range)) >> PAGE_BITS;
			if(CHECK_FLAG_ALL(L4_Rights(old->range), L4_Rights(fpage))
				&& old->first_page_id + page_offs == first_page_id
				&& REF_SPACE(old->parent) == REF_SPACE(parent)
				&& (REF_ADDR(old->parent) + page_offs * PAGE_SIZE) == REF_ADDR(parent))
			{
				/* contained no-op. the condition is hugely complex, but
				 * should succeed entirely after the first two.
				 */
			} else {
				/* break it up & replace. */
				struct map_entry *ne = discontiguate(db, g, fpage);
				if(unlikely(ne == NULL)) return -ENOMEM;
				replace_map_entry(db, g, ne, parent, fpage, first_page_id);
			}
		} else {
			assert(L4_SizeLog2(old->range) < L4_SizeLog2(fpage));
			/* "shrimp" case.
			 *
			 * TODO: erase entries from here to @fpage's end, recycle one slot
			 * for the added entry, and compress the rest of the group.
			 */
			panic("shrimp case not implemented");
		}
	}

#ifndef NDEBUG
	/* postcondition: the mapping database must contain the indicated
	 * range if it existed in the parent. (could be stricter.)
	 */
	struct map_db *p_db = REF_DEFINED(parent)
		? get_map_db(REF_SPACE(parent)) : NULL;
	for(L4_Word_t addr = L4_Address(fpage), pg = 0;
		REF_DEFINED(parent) && addr < L4_Address(fpage) + L4_Size(fpage);
		addr += PAGE_SIZE, pg++)
	{
		L4_Word_t p_addr = REF_ADDR(parent) + pg * PAGE_SIZE;
		struct map_entry *p_e = mapdb_probe(p_db, p_addr);
		if(p_e == NULL) {
			TRACE("%s/postcond: no parent entry\n", __func__);
			continue;
		}

		struct map_entry *e = mapdb_probe(db, addr);
		assert(e != NULL);
		assert(mapdb_page_id_in_entry(e, addr) == pg + first_page_id);
		assert(mapdb_page_id_in_entry(p_e, p_addr) == pg + first_page_id);
	}
#endif

	assert(check_mapdb_module(MOD_NO_CHILD_REFS));

	return 0;
}


static int grow_children_array(struct map_entry *ent)
{
	/* rehash old children into a larger hash table. if at least one doesn't
	 * succeed due to exceeded probe depth, try with larger tables until
	 * calloc() fails.
	 *
	 * (ISSUE: this could be exploitable. perhaps the hash should be
	 * randomized per map_entry from a pool seeded with the low 2 bits of
	 * rdtsc output.)
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
	TRACE("mapdb: add child %#lx to entry %#lx:%#lx (%p)\n", child,
		L4_Address(ent->range), L4_Size(ent->range), ent);
	if(ent->num_children == 0
		|| (ent->num_children == 1 && !REF_DEFINED(ent->child)))
	{
		ent->child = child;
		ent->num_children = 1;
	} else if(ent->num_children == 1) {
		if(ent->child == child) return -EEXIST;
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
	TRACE("mapdb: ok, num_children now %d\n", ent->num_children);

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
	assert(check_mapdb_module(0));

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

	/* TODO: modify the page tables in mapdb_add_map() at some future time.
	 * right now it leaves them out of sync, i.e. potentially pointing to
	 * other pages entirely, so clear the page tables out and let the pf
	 * handler fill them in.
	 */
	for(L4_Word_t a = 0; a < L4_Size(map_page); a += PAGE_SIZE) {
		space_put_page(to_db->space, dest_addr + a, 0, 0);
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
		L4_Fpage_t p = L4_FpageLog2(dest_addr, L4_SizeLog2(map_page));
		L4_Set_Rights(&p, L4_Rights(first->range) & L4_Rights(map_page));
		if(L4_Rights(p) != 0) {
			int offset = (first_addr - L4_Address(first->range)) >> PAGE_BITS;
			mapdb_add_map(to_db, MAPDB_REF(from_db->ref_id, first_addr),
				p, first->first_page_id + offset);
			mapdb_add_child(first, MAPDB_REF(to_db->ref_id, L4_Address(p)));
		}
		return L4_Rights(p);
	} else {
		struct map_entry *ent = first;
		L4_Word_t pos = MAX(L4_Word_t, L4_Address(ent->range), first_addr),
			limit = last_addr + 1;
		int given = L4_Rights(map_page);
		while(pos < limit && ent != NULL && L4_Address(ent->range) < limit) {
			given &= L4_Rights(ent->range);
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
						/* FIXME: audit both parent and child refs */
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

		return given;
	}
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
	for(int i=0; i < from->num_children; i++) {
		struct child_ref r;
		if(!deref_child(&r, local_db, from, i)) continue;

		L4_Word_t pref_addr = REF_ADDR(r.child_entry->parent);
		struct map_entry *p_ent = mapdb_probe(local_db, pref_addr);
		if(p_ent == NULL) {
			/* discard child due to hole made in parent */
			continue;
		}

		if(L4_SizeLog2(p_ent->range) < L4_SizeLog2(r.child_entry->range)) {
			/* the larger page was split, and it had a child entry that no
			 * longer fits in its parent range.
			 */
			L4_Word_t off = pref_addr - L4_Address(p_ent->range);
			L4_Fpage_t cut = L4_FpageLog2(L4_Address(r.child_entry->range)
				+ off, L4_SizeLog2(p_ent->range));
			struct map_entry *ent = discontiguate(r.child_db, r.group, cut);
			/* FIXME: handle ENOMEM (once discontiguate() returns that) */
			r.child_entry = ent;

			/* block output guarantee, and a required result from
			 * discontiguate(): the child is known to exist.
			 */
			assert(r.child_entry != NULL);
		}

		/* simple case. */
		int n = mapdb_add_child(p_ent, MAPDB_REF(r.child_db->ref_id,
			L4_Address(r.child_entry->range)));
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

	struct map_entry saved = *e, *parent_ent = NULL;
	struct map_db *parent_db = NULL;
	if(REF_DEFINED(saved.parent)) {
		parent_db = find_map_db(REF_SPACE(saved.parent));
	}
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

		/* child references for parents. */
		if(parent_db != NULL) {
			L4_Word_t p_addr = REF_ADDR(e[i].parent);
			if(parent_ent == NULL
				|| !ADDR_IN_FPAGE(parent_ent->range, p_addr))
			{
				parent_ent = mapdb_probe(parent_db, p_addr);
				assert(parent_ent != NULL);
			}
			/* FIXME: catch -ENOMEM */
			mapdb_add_child(parent_ent,
				MAPDB_REF(db->ref_id, L4_Address(e[i].range)));
		}
	}
	g->num_entries += p - 1;

	if(saved.num_children > 0) {
		int n = distribute_children(db, g, cut, &saved);
		if(n < 0) {
			panic("distribute_children() failed in split_entry()");
		}
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
		/* two assumptions:
		 * 1. if the entry would've started from before `range', then it'll
		 *    have been cut up by the earlier call to split_entry().
		 * 2. if the entry ends after `range', it should be split up further.
		 */
		if(last != NULL) {
			/* if the entry would've started from before `range', it'll have
			 * been turned to smaller chunks by the earlier call to
			 * split_entry().
			 */
			assert(L4_Address(last->range) >= r_start);
			/* so we only need test whether the entry at the last page's
			 * position straddles r_end.
			 */
			if(FPAGE_HIGH(last->range) + 1 > r_end) {
				err = split_entry(db, g, last, range);
				if(err < 0) goto fail;
				new_e = true;
			}
		}
	}

#ifndef NDEBUG
	/* discontiguate() guarantees that no entry in `g' overlaps the start or
	 * end of `range'. that's easy enough to check by brute force.
	 */
	struct map_entry *chk_e = probe_group_range(g, range);
	assert(chk_e != NULL);
	while(chk_e < &g->entries[g->num_entries]
		&& L4_Address(chk_e->range) < FPAGE_HIGH(range))
	{
		assert(ADDR_IN_FPAGE(range, FPAGE_LOW(chk_e->range)));
		assert(ADDR_IN_FPAGE(range, FPAGE_HIGH(chk_e->range)));
		chk_e++;
	}
#endif

	if(new_e) {
		e = probe_group_range(g, range);
		assert(e != NULL);		/* guaranteed by previous "e" */
	}

	return e;

fail:
	/* FIXME: have a proper exit path here */
	if(err == -ENOMEM) panic("split_entry() failed: out of kernel heap");
	else panic("split_entry() failed: non-ENOMEM error code");

	return NULL;
}


static int reparent_children(struct map_db *db, struct map_entry *e)
{
	if(e->num_children == 0) return 0;

	struct map_db *parent_db = NULL;
	struct map_entry *parent_entry = NULL;
	if(REF_DEFINED(e->parent)) {
		parent_db = get_map_db(REF_SPACE(e->parent));
		parent_entry = mapdb_probe(parent_db, REF_ADDR(e->parent));
		BUG_ON(parent_entry == NULL, "parent ref must be valid");
	}

	L4_Word_t *cs = e->num_children == 1 ? &e->child : e->children;
	for(int i=0; i < e->num_children; i++) {
		struct child_ref cr;
		if(!deref_child(&cr, db, e, i)) {
			TRACE("%s: child %#lx was stale\n", __func__, cs[i]);
			cs[i] = REF_TOMBSTONE;
			continue;
		}

		if(parent_db != NULL) {
			/* FIXME: almost certainly wrong. the child and parent reference
			 * computations should incorporate the child-in-parent offset (which
			 * should be defined properly).
			 */
			if(L4_SizeLog2(e->range) > L4_SizeLog2(cr.child_entry->range)) {
				/* EXPLODE! */
				panic("massive fuckups!");
			}

			int n = mapdb_add_child(parent_entry, MAPDB_REF(cr.child_db->ref_id,
				L4_Address(cr.child_entry->range)));
			if(unlikely(n != 0 && n != -EEXIST)) {
				/* on failure, this function can be called again with the same
				 * parameters and will reach an equivalent state wrt @e, its
				 * parent, and its children, if successful.
				 */
				return n;
			}
			cr.child_entry->parent = MAPDB_REF(parent_db->ref_id,
				REF_ADDR(e->parent));
		} else {
			/* @e is a toplevel mapping. */
			TRACE("%s: detached second-level mapping %#lx:%#lx in ref %u\n",
				__func__, L4_Address(cr.child_entry->range),
				L4_Size(cr.child_entry->range), cr.child_db->ref_id);
			cr.child_entry->parent = 0;
			coalesce_entries(cr.group, cr.child_entry, NULL);
		}

		cs[i] = REF_TOMBSTONE;	/* idempotency guarantee */
	}

	if(e->num_children > 1) free(e->children);
	e->num_children = 0;

	return 0;
}


int mapdb_unmap_fpage(
	struct map_db *db,
	L4_Fpage_t range,
	bool immediate,
	bool recursive,
	bool clear_stored_access)
{
	assert(recursive || immediate);	/* disallows the one-level status read */
	assert(check_mapdb_module(0));

	bool db_changed = false;		/* TODO: track elsewhere */
	int rwx_seen = 0;

	TRACE("%s: range %#lx:%#lx, %simmediate, %srecursive, ref_id %d\n",
		__func__, L4_Address(range), L4_Size(range),
		!immediate ? "non-" : "", !recursive ? "non-" : "",
		(int)db->ref_id);
	L4_Word_t unmap_rights = L4_Rights(range), r_end = FPAGE_HIGH(range);
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

		do {
			/* check each native page.
			 *
			 * TODO: extend to support big pages as specified in the related
			 * KIP field.
			 *
			 * TODO: only do this if the caller provides a location for the
			 * out-parameter.
			 */
			L4_Word_t r_pos = L4_Address(e->range);
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

			/* dereference children, and call mapdb_unmap_fpage() on them when
			 * their address in @db fits within @range.
			 *
			 * NOTE: do the loop in a deref_children() call to save on mapdb
			 * ref_id lookups when @range is small compared to @e->range.
			 */
			for(int i=0; recursive && i < e->num_children; i++) {
				struct child_ref r;
				if(!deref_child(&r, db, e, i)) {
					if(e->num_children < 2) {
						e->child = 0;
						break;
					} else {
						e->children[i] = REF_TOMBSTONE;
						continue;
					}
				}

				TRACE("deref child %d (%#lx) -> %#lx:%#lx\n",
					i, e->num_children < 2 ? e->child : e->children[i],
					L4_Address(r.child_entry->range),
					L4_Size(r.child_entry->range));

				if(!ADDR_IN_FPAGE(range, REF_ADDR(r.child_entry->parent))) {
					continue;
				}
				int rm_rights = L4_Rights(r.child_entry->range) & unmap_rights;
				if(rm_rights == 0) continue;

				L4_Fpage_t fp = r.child_entry->range;
				L4_Set_Rights(&fp, unmap_rights);
				int pass_rwx = mapdb_unmap_fpage(r.child_db, fp,
					true, true, false);
				if(pass_rwx < 0) {
					/* FIXME: handle ENOMEM */
					printf("%s: failed for child ref %u, fpage %#lx:%#lx\n",
						__func__, r.child_db->ref_id, L4_Address(fp),
						L4_Size(fp));
					panic("recursive unmap failed!");
				}
				rwx_seen |= pass_rwx;
			}

			bool drop = false;
			if(modify) {
				/* ensured by discontiguate() */
				assert(FPAGE_LOW(e->range) >= FPAGE_LOW(range));
				assert(FPAGE_LOW(e->range) < r_end);
				assert(FPAGE_HIGH(e->range) <= r_end);

				int old_access = L4_Rights(e->range),
					new_access = old_access & ~unmap_rights;
				L4_Set_Rights(&e->range, new_access);
				if(new_access == 0) drop = true;
				else if(new_access < old_access) {
					/* TODO: move into a function */
					for(L4_Word_t a = L4_Address(e->range),
								  pgid = e->first_page_id;
						a < L4_Address(e->range) + L4_Size(e->range);
						a += PAGE_SIZE, pgid++)
					{
						TRACE("setting page at %#lx to pgid %lu, access %#x in ref_id %d\n",
							a, pgid, (unsigned)new_access, (int)db->ref_id);
						space_put_page(db->space, a, pgid, new_access);
					}
					db_changed = true;
				}
			}
			if(drop) {
				assert(modify);
				/* TODO: implement memmove(3), use it */
				TRACE("%s: removing entry %#lx:%#lx\n", __func__,
					L4_Address(e->range), L4_Size(e->range));
				int n = reparent_children(db, e);
				if(n < 0) {
					/* ENOMEM can happen because children must be added to a
					 * parent entry, which may return ENOMEM on hash resize.
					 *
					 * FIXME: this is HIGHLY INSUFFICIENT because of changes
					 * made earlier in this function. the function should be
					 * split into a children-gathering stage and three
					 * functional stages, each invoked according to its
					 * corresponding bool parameter.
					 */
					return -ENOMEM;
				}

				/* TODO: move into a function */
				for(L4_Word_t a = L4_Address(e->range);
					a < L4_Address(e->range) + L4_Size(e->range);
					a += PAGE_SIZE)
				{
					TRACE("dropping page at %#lx in ref_id %d\n",
						a, (int)db->ref_id);
					space_put_page(db->space, a, 0, 0);
				}
				db_changed = true;

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

				/* counteract the loop bump */
				e--;
			} else {
				e->access = clear_stored_access ? 0 : rwx_seen;
			}
		} while(++e < &g->entries[g->num_entries] && L4_Address(e->range) < r_end);

		dump_map_group(g);
	}

	assert(unmap_rights == 0 || check_mapdb_module(0));
	if(db_changed) space_commit(db->space);

	/* TODO: move postconditions into another function */
#ifndef NDEBUG
	if(unmap_rights != 0 && immediate) {
		for(L4_Word_t addr = FPAGE_LOW(range);
			addr < FPAGE_HIGH(range);
			addr += PAGE_SIZE)
		{
			struct map_entry *e = mapdb_probe(db, addr);
			assert(e == NULL || !CHECK_FLAG_ALL(L4_Rights(e->range), unmap_rights));
		}
	}
#endif

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
	assert(check_mapdb_module(0));

	TRACE("%s: start_addr %#lx, num_pages %u (%#x bytes)\n", __func__,
		(L4_Word_t)start_addr, num_pages, num_pages * PAGE_SIZE);
	unsigned int done = 0;
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
			done += 1 << (mag - 12);
		}
	}

	assert(done == num_pages);
	assert(check_mapdb_module(0));
}
