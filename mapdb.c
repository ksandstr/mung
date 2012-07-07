
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


static size_t rehash_ref_hash(const void *, void *);


static struct kmem_cache *map_group_slab = NULL;
static uint32_t next_ref_id = 1;
static struct htable ref_hash = HTABLE_INITIALIZER(ref_hash,
	rehash_ref_hash, NULL);


static void dump_map_group(struct map_group *g)
{
#ifndef NDEBUG
	TRACE("%s: group %#lx .. %#lx contains (%d ents, %d alloc):\n",
		__func__, g->start, g->start + GROUP_SIZE - 1,
		g->num_entries, g->num_alloc);
	for(int i=0; i < g->num_entries; i++) {
		struct map_entry *e = &g->entries[i];
		assert(!L4_IsNilFpage(e->range));
		TRACE("  %d: [%#lx .. %#lx], pages [%u .. %lu]\n", i,
			L4_Address(e->range), L4_Address(e->range) + L4_Size(e->range) - 1,
			e->first_page_id,
			e->first_page_id + L4_Size(e->range) / PAGE_SIZE - 1);
	}
#endif
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
		TRACE("probing pos %d (limb %d, offset %d) in %#lx; occ[%d] = %#lx\n",
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
	TRACE("%s: adding fpage at %#lx, size %#lx\n", __func__, addr,
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
	dump_map_group(g);
#endif

	return 0;
}


/* does mappings of all physical pages inside map_page. skips holes in the
 * sender address space within the mapping (so pages in the receiver won't be
 * unmapped on overlap with empty.)
 */
int mapdb_map_pages(
	struct map_db *from_db,
	struct map_db *to_db,
	L4_Fpage_t map_page,
	L4_Word_t dest_addr)
{
	struct map_entry *first = NULL;
	struct map_group *grp;
	/* TODO: scan the bitmap instead of repeating binary searches */
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
		TRACE("%s: first->range %#lx:%#lx; map_page %#lx:%#lx\n", __func__,
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


static int split_entry(
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
			/* num_children, child/children left undefined */
		};
		L4_Set_Rights(&e[i].range, L4_Rights(saved.range));
		TRACE("%s: set ents[%d] = range %#lx:%#lx, parent %#lx, first page %u, rwx %#lx\n",
			__func__, e - g->entries + i, L4_Address(e[i].range), L4_Size(e[i].range),
			e[i].parent, e[i].first_page_id, L4_Rights(e[i].range));
		addr_offset += L4_Size(pg_buf[i]);
	}
	g->num_entries += p - 1;

	/* FIXME: redistribute children */

	return 0;
}


/* render a map_group's map_entries discontiguous in such a way that entries
 * covered by "range" fit inside it exactly. returns the first entry covered
 * by "range".
 */
static struct map_entry *discontiguate(struct map_group *g, L4_Fpage_t range)
{
	int err;
	TRACE("%s: group %#lx, range %#lx:%#lx\n", __func__, g->start,
		L4_Address(range), L4_Size(range));
	/* test the first and last entries in the group that fall within
	 * `range`.
	 */
	struct map_entry *e = probe_group_bitmap(g, range);
	if(e == NULL) return NULL;
//	TRACE("%s: e %#lx:%#lx\n", __func__, L4_Address(e->range),
//		L4_Size(e->range));
	bool new_e = false;
	L4_Word_t r_start = L4_Address(range);
	if(L4_Address(e->range) < r_start
		|| (L4_Address(e->range) == r_start
			&& L4_Size(e->range) > L4_Size(range)))
	{
		err = split_entry(g, e, range);
		if(err < 0) goto fail;
		new_e = true;
	}

	/* then the last entry. */
	if(L4_Size(range) > PAGE_SIZE) {
		L4_Word_t r_end = r_start + L4_Size(range);
		struct map_entry *last = probe_group_bitmap(g, L4_FpageLog2(
			L4_Address(range) + L4_Size(range) - PAGE_SIZE, PAGE_BITS));
//		TRACE("%s: last %#lx:%#lx\n", __func__,
//			L4_Address(last->range), L4_Size(last->range));
		/* ... does this work? */
		if(last != NULL
			&& (L4_Address(last->range) != r_start
				|| L4_Size(last->range) != L4_Size(range))
			&& L4_Address(last->range) < r_end)
		{
			err = split_entry(g, last, range);
			if(err < 0) goto fail;
			new_e = true;
		}
	}

	if(new_e) {
//		TRACE("%s: dumping the modified group\n", __func__);
//		dump_map_group(g);

		e = probe_group_bitmap(g, range);
		assert(e != NULL);		/* guaranteed by previous "e" */
	}

	return e;

fail:
	/* FIXME: have a proper exit path here */
	panic("split_entry() failed: out of kernel heap");
	return NULL;
}


int mapdb_unmap_fpage(struct map_db *db, L4_Fpage_t range, bool recursive)
{
	int rwx_seen = 0;

	TRACE("%s: range %#lx:%#lx\n", __func__,
		L4_Address(range), L4_Size(range));
	L4_Word_t r_end = L4_Address(range) + L4_Size(range);
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
		if(L4_Rights(range) != 0) {
			e = discontiguate(g, range);
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
			e = probe_group_bitmap(g, range);
		}
		if(e == NULL) continue;

		L4_Word_t r_pos = L4_Address(e->range);
		do {
			/* properties ensured by discontiguate() */
			assert(L4_Address(e->range) >= L4_Address(range));
			assert(L4_Address(e->range) + L4_Size(e->range) <= r_end);

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
			e->access = 0;

			if(recursive) {
				/* FIXME: examine children: for each, see if they're valid
				 * (and compress them where not), and if the actual address
				 * falls within `range`.
				 */
				panic("recursive unmap not implemented");
			}

			r_pos = L4_Address(e->range) + L4_Size(e->range);
			L4_Set_Rights(&e->range, L4_Rights(e->range) & ~L4_Rights(range));
			if(L4_Rights(e->range) == 0) {
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
