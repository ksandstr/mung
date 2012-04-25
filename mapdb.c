
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
#include <ukernel/slab.h>
#include <ukernel/mapdb.h>


#define TRACE_VERBOSE 0		/* 1 for mapdb dumps on add/remove */

#if TRACE_VERBOSE
#define TRACE(fmt, ...) printf(fmt, __VA_ARGS__)
#else
#define TRACE(fmt, ...)
#endif


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


static size_t rehash_ref_hash(const void *, void *);


static struct kmem_cache *map_group_slab = NULL;
static uint32_t next_ref_id = 1;
static struct htable ref_hash = HTABLE_INITIALIZER(ref_hash,
	rehash_ref_hash, NULL);


/* returns true if the entry was cleared. */
static bool flush_entry(struct map_entry *e, int access)
{
	const uintptr_t *children = e->num_children > 1 ? e->children : &e->child;
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


/* TODO: unit test the living crap out of this function, please. right now
 * there's the rudimentary "iters" limit to break it up after 20 iterations
 * (2^20 being 1024^2, one power more than the maximum number of elements in a
 * map_group)
 */
static struct map_entry *find_entry_in_group(
	struct map_group *g,
	uintptr_t addr)
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


/* TODO: move this into an utility header */
static uint32_t mask32_range(int first, int count)
{
	uint32_t mask = (~0u << first);
	if(first + count < 32) mask &= ~(~0u << (first + count));
#ifndef NDEBUG
	for(int i=0; i < 32; i++) {
		bool set = (mask & (1 << i)) != 0;
		assert(set || i < first || i >= first + count);
		assert(!set || (i >= first && i < first + count));
	}
#endif
	return mask;
}


static void add_to_group_occ(struct map_group *g, L4_Fpage_t fpage)
{
	/* +2 for aliasing four 4 KiB pages per bit */
	const int first = (L4_Address(fpage) - g->start) >> (PAGE_BITS + 2),
		count = L4_Size(fpage) >> (PAGE_BITS + 2);
	int done = 0;
	while(done < count) {
		int pos = first + done, limb = pos >> 5, offset = pos & 0x1f,
			seg = MIN(int, 32 - offset, count - done);
		g->occ[limb] |= mask32_range(offset, seg);
		done += seg;
		assert(done == count || ((done + first) & 0x1f) == 0);
	}

#ifndef NDEBUG
	for(int i = first; i < first + count; i++) {
		int limb = i >> 5, offset = i & 0x1f;
		assert((g->occ[limb] & (1 << offset)) != 0);
	}
#endif
}


static struct map_entry *probe_group_bitmap(
	struct map_group *g,
	L4_Fpage_t fpage)
{
	assert(!L4_IsNilFpage(fpage));
	int num_pages = MAX(int, 1, L4_Size(fpage) >> PAGE_BITS);
	if(num_pages <= 4) {
		/* optimized unit case (with aliasing) */
		int pos = (L4_Address(fpage) - g->start) >> (PAGE_BITS + 2),
			limb = pos >> 5, offset = pos & 0x1f;
#if 0
		TRACE("probing pos %d (limb %d, offset %d) in %#x; occ[%d] = %#x\n",
			pos, limb, offset, g->start, limb, g->occ[limb]);
#endif
		if((g->occ[limb] & (1 << offset)) != 0) {
			/* do a final 4-page scan at the aliased range. */
			for(int p=0; p < 4; p++) {
				struct map_entry *e = find_entry_in_group(g,
					L4_Address(fpage) + p * PAGE_SIZE);
				if(e != NULL) return e;
			}
		}
	} else {
		/* other cases in terms of the previous.
		 * TODO: use proper mask tests, bit-magic
		 */
		for(int p=0; p < num_pages; p += 4) {
			struct map_entry *e = probe_group_bitmap(g,
				L4_FpageLog2(L4_Address(fpage) + p * PAGE_SIZE, 14));
			if(e != NULL) return e;
		}
	}

	return NULL;
}


static int entry_split_and_insert(
	struct map_entry *outbuf,
	const struct map_entry *src,
	L4_Fpage_t fpage,
	uint32_t first_page_id)
{
	return 0;
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
		TRACE("%s: hit between %#x:%#x and %#x:%#x\n",
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
 */
static bool merge_entries(
	struct map_group *g,
	int prev_pos,
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


int mapdb_add_map(
	struct map_db *db,
	L4_Fpage_t fpage,
	uint32_t first_page_id)
{
	L4_Word_t addr = L4_Address(fpage);
	TRACE("%s: adding fpage at %#x, size %#x\n", __func__, addr,
		L4_Size(fpage));

	struct map_group *g = group_for_addr(db, addr);
	if(g == NULL) {
		/* trivial case. */
		g = kmem_cache_zalloc(map_group_slab);
		if(unlikely(g == NULL)) return -ENOMEM;
		g->entries = malloc(sizeof(struct map_entry) * 2);
		if(unlikely(g->entries == NULL)) {
			kmem_cache_free(map_group_slab, g);
			return -ENOMEM;
		}
		g->start = GROUP_ADDR(addr);
		g->num_alloc = 2;
		g->entries[0] = (struct map_entry){
			.range = fpage, .first_page_id = first_page_id,
		};
		g->entries[1].range = L4_Nilpage;
		g->num_entries = 1;
		bool ok = htable_add(&db->groups, int_hash(g->start), g);
		if(unlikely(!ok)) {
			free(g->entries);
			kmem_cache_free(map_group_slab, g);
			return -ENOMEM;
		}
		add_to_group_occ(g, fpage);
	} else {
		struct map_entry *old = probe_group_bitmap(g, fpage);
		if(old == NULL) {
			/* TODO: use a clever binary hoppity-skip algorithm here,
			 * recycling it for the split-placement bit in the next case.
			 *
			 * this stuff requires that entries be tightly packed at the
			 * beginning of g->entries[] . the algorithm won't compact holes
			 * to the right of the right-side ("prev") entry.
			 */
			int prev = -1;
			for(int i=0; i < g->num_entries; i++) {
				L4_Fpage_t e = g->entries[i].range;
				assert(L4_Address(e) + L4_Size(e) - 1 < L4_Address(fpage)
					|| L4_Address(fpage) + L4_Size(fpage) - 1 < L4_Address(e));
				if(L4_Address(e) < L4_Address(fpage)) prev = i; else break;
			}
			if(prev < 0 || !merge_entries(g, prev, fpage, first_page_id)) {
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
					.range = fpage, .first_page_id = first_page_id,
				};
				g->num_entries++;
			}
			add_to_group_occ(g, fpage);
		} else {
			struct map_entry split_tmp[MAX_SPLIT];
			int split_count = entry_split_and_insert(split_tmp, old,
				fpage, first_page_id);
			TRACE("%s: ESI returned %d\n", __func__, split_count);
			if(split_count <= 1) {
				/* simple 1:1 replacement, no bitmap update even */
				if(old->num_children > 1) {
					free(old->children);
					old->children = NULL;
				}
				*old = (struct map_entry){
					.range = fpage, .first_page_id = first_page_id,
				};
			} else {
				int dst_pos = old - &g->entries[0];
				printf("would insert %d items at %d\n", split_count, dst_pos);
				panic("N-insert case not written");
			}
		}
	}

#ifndef NDEBUG
	TRACE("%s: group %#x .. %#x contains (%d ents, %d alloc):\n",
		__func__, g->start, g->start + GROUP_SIZE - 1,
		g->num_entries, g->num_alloc);
	for(int i=0; i < g->num_entries; i++) {
		struct map_entry *e = &g->entries[i];
		assert(!L4_IsNilFpage(e->range));
		TRACE("  %d: [%#x .. %#x], pages [%u .. %u]\n", i,
			L4_Address(e->range), L4_Address(e->range) + L4_Size(e->range) - 1,
			e->first_page_id,
			e->first_page_id + L4_Size(e->range) / PAGE_SIZE - 1);
	}
#endif

	return 0;
}


/* does mappings of all physical pages inside map_page. skips holes in the
 * sender address space within the mapping (so pages in the receiver won't be
 * unmapped on overlap with empty.)
 *
 * TODO: should have a "grant" option, removing the maps from the source
 * database.
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
		if(grp != NULL) first = find_entry_in_group(grp, first_addr);
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
		if(L4_Rights(p) != 0) mapdb_add_map(to_db, p, first->first_page_id);
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
			mapdb_add_map(to_db, p, first->first_page_id + offset);
		}
		return L4_Rights(p);
	} else {
#if 0
		printf("%s: first->range %#x:%#x; map_page %#x:%#x\n", __func__,
			L4_Address(first->range), L4_Size(first->range),
			L4_Address(map_page), L4_Size(map_page));
#endif

		const struct map_entry *ent = first;
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
						mapdb_add_map(to_db, p, ent->first_page_id + p_offs);
					}
				}
			}

			/* next entry. */
			if(ent < &grp->entries[grp->num_entries - 1]) ent++;
			else {
				/* next group, even */
				grp = NULL;
				for(uintptr_t grp_addr = grp->start + GROUP_SIZE;
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


struct map_entry *mapdb_probe(
	struct map_db *db,
	uintptr_t addr)
{
	struct map_group *g = group_for_addr(db, addr);
	if(g == NULL) return NULL;

	assert(addr >= g->start);
	assert(addr < g->start + GROUP_SIZE);
	return find_entry_in_group(g, addr);
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
	TRACE("%s: start_addr %#x, num_pages %u (%#x bytes)\n", __func__,
		start_addr, num_pages, num_pages * PAGE_SIZE);
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
//			TRACE("%s: range_pos %#x, b %d\n", __func__, range_pos, b);
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
			if(mapdb_add_map(db, page, page_ids[id_offset]) < 0) {
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
