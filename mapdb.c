
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <errno.h>

#include <ccan/htable/htable.h>
#include <ccan/likely/likely.h>
#include <ccan/container_of/container_of.h>

#include <l4/types.h>

#include <ukernel/misc.h>
#include <ukernel/util.h>
#include <ukernel/slab.h>
#include <ukernel/trace.h>
#include <ukernel/space.h>
#include <ukernel/ptab.h>
#include <ukernel/bug.h>
#include <ukernel/mapdb.h>


/* for mapdb dumps on add/remove */
#define TRACE(fmt, ...) TRACE_MSG(TRID_MAPDB, fmt, ##__VA_ARGS__)


#define GROUP_SIZE (PAGE_SIZE * MAX_ENTRIES_PER_GROUP)
#define GROUP_ADDR(addr) ((addr) & ~(GROUP_SIZE - 1))

/* maximum probe depth in map_entry->children. */
#define MAX_PROBE_DEPTH 16


/* dereferenced map_entry->children entry. */
struct child_ref
{
	struct map_db *child_db;
	struct map_group *group;
	struct map_entry *child_entry;
};


static size_t rehash_ref_hash(const void *, void *);

static struct map_entry *fetch_entry(
	struct map_db *db, struct map_group *g,
	L4_Fpage_t range, bool make_exact);

static int insert_empties(
	struct map_group *g,
	int num_to_add,
	struct map_entry **entry);

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
		 *   - if it's special, check that space=0 appears only in sigma0 and
		 *   that space=1 has no children.
		 */
		for(int i=0; i < grp->num_entries; i++) {
			const struct map_entry *e = &grp->entries[i];
			inv_ok1(L4_Rights(e->range) != 0);

			inv_push("check entry %#lx:%#lx in ref_id %u; ->parent %#lx",
				L4_Address(e->range), L4_Size(e->range), db->ref_id,
				e->parent);

			struct map_db *p_db = find_map_db(REF_SPACE(e->parent));
			inv_iff1(REF_DEFINED(e->parent), p_db != NULL);

			inv_iff1(REF_IS_SPECIAL(e->parent), p_db == &kernel_space->mapdb);
			inv_imply1(REF_IS_SPECIAL(e->parent), e->num_children == 0);
			inv_imply1(REF_IS_SPECIAL(e->parent), REF_ADDR(e->parent) == 0);

			const struct map_entry *p_e;
			if(!REF_DEFINED(e->parent)) {
				inv_log("  ... is parentless entry");
				p_e = NULL;
			} else if(REF_IS_SPECIAL(e->parent)) {
				inv_log("  ... is special entry (parent=%#lx)", e->parent);
				p_e = NULL;
			} else {
				inv_ok1(p_db != NULL);
				p_e = mapdb_probe(p_db, REF_ADDR(e->parent));
				inv_ok1(p_e != NULL);
				inv_log("parent entry %#lx:%#lx in ref_id %u",
					L4_Address(p_e->range), L4_Size(p_e->range), p_db->ref_id);
				inv_ok1(ADDR_IN_FPAGE(p_e->range, REF_ADDR(e->parent)));
				inv_ok1(L4_SizeLog2(e->range) <= L4_SizeLog2(p_e->range));
			}

			if(!CHECK_FLAG(opts, MOD_NO_CHILD_REFS) && p_e != NULL) {
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
		|| !ADDR_IN_FPAGE(e->range, REF_ADDR(ce->parent)))
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


int mapdb_init(struct map_db *ptr)
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
		for(int i=0; i < g->num_entries; i++) {
			struct map_entry *e = &g->entries[i];
			/* FIXME: re-parent children of @e */
			if(e->num_children > 1) free(e->children);
		}
		free(g->entries);
		kmem_cache_free(map_group_slab, g);
	}

	htable_clear(&db->groups);

	assert(check_mapdb_module(0));
}


/* used in postcondition asserts. brute force. */
static bool no_addr_in_group(struct map_group *g, uintptr_t addr)
{
	for(int i=0; i < g->num_entries; i++) {
		assert(!ADDR_IN_FPAGE(g->entries[i].range, addr));
	}

	return true;
}


static struct map_group *group_for_addr(struct map_db *db, uintptr_t addr)
{
	uintptr_t key = GROUP_ADDR(addr);
	return htable_get(&db->groups, int_hash(key), &cmp_group_addr, &key);
}


/* finds an entry in @fpage. returns non-negative index into @g->entries on
 * success, and -(n + 1) for an approximate position where @fpage could be
 * inserted if nothing was found.
 */
static int search_group_by_range(struct map_group *g, L4_Fpage_t fpage)
{
	/* binary search. faster than it looks. */
	L4_Word_t fpage_addr = L4_Address(fpage);
	int low = 0, high = g->num_entries - 1, mid = 0;
	while(low <= high) {
		mid = low + (high - low) / 2;
		if(fpage_overlap(g->entries[mid].range, fpage)) {
			return mid;
		} else {
			/* branchless selection. */
			L4_Word_t e_addr = L4_Address(g->entries[mid].range),
				mask = (intptr_t)(e_addr - fpage_addr) >> (WORD_BITS - 1);
			assert(e_addr >= fpage_addr || mask == ~0ul);
			assert(e_addr <  fpage_addr || mask == 0);
			low = (mask & (mid + 1)) | (~mask & low);
			high = (mask & high) | (~mask & (mid - 1));
		}
	}

	assert(no_addr_in_group(g, FPAGE_LOW(fpage)));
	assert(no_addr_in_group(g, FPAGE_HIGH(fpage)));
	assert(mid >= 0);
	return -(mid + 1);
}


/* find first map_entry inside @range, starting from @e (which is typically a
 * return value from probe_group_range()).
 */
static inline struct map_entry *rewind_to_first(
	struct map_group *grp,
	struct map_entry *e,
	L4_Fpage_t range)
{
	assert(L4_SizeLog2(e->range) < L4_SizeLog2(range));
	while(e > &grp->entries[0]
		&& L4_Address(e[-1].range) >= L4_Address(range))
	{
		assert(fpage_overlap(e[-1].range, range));
		e--;
	}
	assert(fpage_overlap(e->range, range));

	return e;
}


/* returns an entry that falls in @fpage; this may not be the left-most one
 * when @fpage.s > retval->range.s .
 *
 * TODO: inspect call sites, replace with search_group_by_range() where
 * appropriate.
 */
static struct map_entry *probe_group_range(struct map_group *g, L4_Fpage_t fpage)
{
	int ix = search_group_by_range(g, fpage);
	return ix >= 0 ? &g->entries[ix] : NULL;
}


static struct map_entry *probe_group_addr(struct map_group *g, uintptr_t addr)
{
	assert(BETWEEN(g->start,
		g->start + MAX_ENTRIES_PER_GROUP * PAGE_SIZE - 1,
		addr));
	if(unlikely(g->num_entries == 0)) return NULL;

	return probe_group_range(g,
		L4_FpageLog2(addr & ~PAGE_MASK, PAGE_BITS));
}


static bool can_merge(
	const struct map_entry *e,
	L4_Word_t parent,
	L4_Fpage_t fpage,
	uint32_t first_page_id)
{
	bool far_side = CHECK_FLAG(L4_Address(fpage), L4_Size(fpage));

	int n_pages = L4_Size(fpage) / PAGE_SIZE;
	assert(POPCOUNT(n_pages) == 1);
	if(e->first_page_id != (first_page_id ^ n_pages)
		|| unlikely(far_side != (e->first_page_id < first_page_id)))
	{
		/* range starts aren't n_pages apart, or are the wrong way around */
		TRACE("%s: rejected for e->first_page_id=%u\n", __func__,
			e->first_page_id);
		return false;
	}
	assert(abs((int)e->first_page_id - (int)first_page_id) == n_pages);

	if(L4_Address(e->range) != (L4_Address(fpage) ^ L4_Size(fpage))
		|| L4_Rights(e->range) != L4_Rights(fpage)
		|| unlikely(L4_SizeLog2(e->range) != L4_SizeLog2(fpage)))
	{
		/* wrong size, or not the neighbour, or rights aren't compatible. */
		TRACE("%s: rejected for e->range=%#lx:%#lx (or rights)\n",
			__func__, L4_Address(e->range), L4_Size(e->range));
		return false;
	}

	if((REF_DEFINED(e->parent) || REF_DEFINED(parent))
		&& (REF_SPACE(e->parent) != REF_SPACE(parent)
			|| REF_ADDR(e->parent) != (REF_ADDR(parent) ^ L4_Size(fpage))
			|| far_side != (REF_ADDR(e->parent) < REF_ADDR(parent))))
	{
		/* either or both have a defined parent; and
		 * they reference different parent spaces, or the addresses aren't
		 * contiguous, or the addresses aren't in the right order.
		 */
		TRACE("%s: rejected for e->parent=%#lx\n", __func__, e->parent);
		return false;
	}

	return true;
}


static void expand_entry(struct map_entry *e)
{
	uint32_t n_pages = L4_Size(e->range) >> PAGE_BITS;
	int rights = L4_Rights(e->range);

	e->parent = MAPDB_REF(REF_SPACE(e->parent),
		REF_ADDR(e->parent) & ~L4_Size(e->range));
	e->first_page_id &= ~n_pages;
	e->range = L4_FpageLog2(L4_Address(e->range) & ~L4_Size(e->range),
		L4_SizeLog2(e->range) + 1);
	L4_Set_Rights(&e->range, rights);
}


/* where possible, merge @e into its compatible neighbour in @g. try to merge
 * the resulting larger entry again, etc.
 */
static void coalesce_entries(struct map_group *g, struct map_entry *e)
{
	bool far_side = CHECK_FLAG(L4_Address(e->range), L4_Size(e->range));
	struct map_entry *oth = &e[far_side ? -1 : 1];
	if(oth >= &g->entries[0] && oth < &g->entries[g->num_entries]
		&& can_merge(oth, e->parent, e->range, e->first_page_id))
	{
		/* always join to the left. */
		if(far_side) SWAP(struct map_entry *, e, oth);
		expand_entry(e);

		/* move children over as well. */
		L4_Word_t *chs = oth->num_children <= 1 ? &oth->child : oth->children;
		for(int i=0; i < oth->num_children; i++) {
			if(!REF_DEFINED(chs[i])) continue;
			int n = mapdb_add_child(e, chs[i]);
			/* FIXME: handle this atomically by pre-growing @e's child array
			 * before expand_entry().
			 */
			if(unlikely(n == -ENOMEM)) {
				panic("ENOMEM in coalesce_entries()");
			}
			/* this loop ignores -EEXIST to fold existing children in. */
		}
		if(oth->num_children > 1) free(oth->children);
		oth->num_children = 0;

		/* NOTE: an iterative version of this function could keep voodoo
		 * indexes about the removed entries' range, and then do a single
		 * larger memmove at the end. multiple joins are so rare that we
		 * don't, though.
		 */
		int oix = oth - &g->entries[0];
		if(oix < --g->num_entries) {
			memmove(&g->entries[oix], &g->entries[oix + 1],
				sizeof(struct map_entry) * (g->num_entries - oix));
		}
		g->entries[g->num_entries].range = L4_Nilpage;

		coalesce_entries(g, e);
	}
}


/* attempts to record the equivalent of a map_entry with the given parameters
 * by expanding an existing map_entry. returns true on success.
 */
static bool merge_into_entry(
	struct map_group *g,
	int prev_pos,
	L4_Word_t parent,
	L4_Fpage_t fpage,
	uint32_t first_page_id)
{
	assert(prev_pos >= -1 && prev_pos < g->num_entries);
	assert(L4_Rights(fpage) != 0);

	/* the "far" page has this bit set and lies further along in the address
	 * space.
	 */
	bool far_side = CHECK_FLAG(L4_Address(fpage), L4_Size(fpage));
	TRACE("%s: far_side=%s, fpage=%#lx:%#lx, parent=%#lx, fpi=%u\n",
		__func__, btos(far_side), L4_Address(fpage), L4_Size(fpage),
		parent, first_page_id);

	struct map_entry *e = &g->entries[far_side ? prev_pos : prev_pos + 1];
	if(e < &g->entries[0] || e >= &g->entries[g->num_entries]) {
		/* head-in-far, or tail-in-near case. */
		TRACE("%s: rejected for head/tail\n", __func__);
		return false;
	}

	if(!can_merge(e, parent, fpage, first_page_id)) return false;

	expand_entry(e);
	TRACE("%s: accepted; range'=%#lx:%#lx, fpi'=%u, parent'=%#lx\n",
		__func__, L4_Address(e->range), L4_Size(e->range),
		e->first_page_id, e->parent);

	/* enforce maximum coalescing. this lets child mappings merge freely
	 * without dereferencing their parent pointers to check the limit.
	 */
	coalesce_entries(g, e);

	return true;
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


/* insert a map_entry for (@parent, @fpage, @first_page_id) into @g. if a
 * compatible map_entry exists already, it'll be extended. otherwise a new
 * entry will be inserted.
 *
 * returns 0 on success, or -ENOMEM on out-of-memory.
 */
static int insert_map_entry(
	struct map_db *db,
	struct map_group *g,
	L4_Word_t parent,
	L4_Fpage_t fpage,
	uint32_t first_page_id)
{
	assert(!L4_IsNilFpage(fpage));
	assert(g != NULL);
	assert(group_for_addr(db, L4_Address(fpage)) == g);
	assert(mapdb_probe(db, L4_Address(fpage)) == NULL);

	/* this happens during boot-up.
	 *
	 * TODO: it should be removed; this function is along every pager's hot
	 * path.
	 */
	if(unlikely(g->num_entries == 0)) {
		if(g->num_alloc == 0) {
			assert(g->entries == NULL);
			g->entries = malloc(2 * sizeof(struct map_entry));
			if(g->entries == NULL) return -ENOMEM;
			g->num_alloc = 2;
		}
		g->entries[0] = (struct map_entry){
			.parent = parent, .range = fpage,
			.first_page_id = first_page_id,
		};
		g->entries[1].range = L4_Nilpage;
		g->num_entries = 1;
		return 0;
	}

	/* find the page's neighbour, or its insert position. */
	L4_Fpage_t search_page = L4_FpageLog2(
		L4_Address(fpage) & ~L4_Size(fpage),
		L4_SizeLog2(fpage) + 1);
	int ix = search_group_by_range(g, search_page);
	if(ix < 0 || !merge_into_entry(g, ix, parent, fpage, first_page_id)) {
		int prev = abs(ix) - 1;		/* (correct for esoteric reasons.) */
		assert(prev < g->num_entries);
		/* scan forward or backward to find the right spot. */
		bool back = false;
		while(prev >= 0
			&& L4_Address(g->entries[prev].range) > L4_Address(fpage))
		{
			prev--;
			back = true;
		}
		while(!back && prev < g->num_entries - 1
			&& L4_Address(g->entries[prev + 1].range) < L4_Address(fpage))
		{
			prev++;
		}
		assert(prev == -1 || !fpage_overlap(g->entries[prev].range, fpage));
		assert(prev == g->num_entries - 1
			|| L4_Address(g->entries[prev + 1].range) > FPAGE_HIGH(fpage));

		struct map_entry *pe = &g->entries[prev];
		int n = insert_empties(g, 1, &pe);
		if(unlikely(n < 0)) {
			assert(n == -ENOMEM);
			return -ENOMEM;
		}
		pe[1] = (struct map_entry){
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


static bool add_map_postcond(
	struct map_db *db,
	L4_Word_t parent,
	L4_Fpage_t map_area,
	uint32_t first_page_id)
{
	/* the mapping database must contain the indicated range if it existed in
	 * the parent. (could be stricter.)
	 */
	int n_pages = L4_Size(map_area) / PAGE_SIZE;
	struct map_db *p_db = REF_DEFINED(parent) && !REF_IS_SPECIAL(parent)
		? get_map_db(REF_SPACE(parent)) : NULL;
	for(L4_Word_t addr = L4_Address(map_area), pg = 0;
		REF_DEFINED(parent) && !REF_IS_SPECIAL(parent) && pg < n_pages;
		addr += PAGE_SIZE, pg++)
	{
		L4_Word_t p_addr = REF_ADDR(parent) + pg * PAGE_SIZE;
		struct map_entry *p_e = mapdb_probe(p_db, p_addr);
		if(p_e == NULL) {
			TRACE("%s: no parent entry\n", __func__);
			continue;
		}

		struct map_entry *e = mapdb_probe(db, addr);
		assert(e != NULL);
		assert(mapdb_page_id_in_entry(e, addr) == pg + first_page_id
			|| REF_IS_SPECIAL(e->parent));
		assert(mapdb_page_id_in_entry(p_e, p_addr) == pg + first_page_id);
	}

	/* check that where map_area is present in @db's page tables, it contains
	 * either blanks or hits along the run from @first_page_id .
	 */
	struct pt_iter it;
	pt_iter_init(&it, SPACE_OF_MAPDB(db));
	for(L4_Word_t addr = FPAGE_LOW(map_area), exp_pgid = first_page_id;
		addr < FPAGE_HIGH(map_area);
		addr += PAGE_SIZE, exp_pgid++)
	{
		/* that one can't be tested for, though. */
		if(exp_pgid == 0) continue;

		bool upper_present;
		uint32_t pgid = pt_get_pgid(&it, &upper_present, addr);
		assert(!upper_present || pgid == exp_pgid);
	}
	pt_iter_destroy(&it);

	return true;
}


int mapdb_add_map(
	struct map_db *db,
	L4_Word_t parent,
	L4_Fpage_t fpage,
	uint32_t first_page_id)
{
	assert(check_mapdb_module(0));

	TRACE("%s: adding fpage=%#lx:%#lx, access=%c%c%c, parent=%#lx\n",
		__func__, L4_Address(fpage), L4_Size(fpage),
		CHECK_FLAG(L4_Rights(fpage), L4_Readable) ? 'r' : '-',
		CHECK_FLAG(L4_Rights(fpage), L4_Writable) ? 'w' : '-',
		CHECK_FLAG(L4_Rights(fpage), L4_eXecutable) ? 'x' : '-',
		parent);

	/* unparented entries may only be created with mapdb_add_map() into
	 * sigma0_space, or while sigma0_space isn't defined. other causes of such
	 * entries are nonrecursive flushes: grants and mappings on top of.
	 */
	assert(REF_DEFINED(parent) || SPACE_OF_MAPDB(db) == sigma0_space
		|| sigma0_space == NULL);
	/* TODO: assert that when @parent is defined and not special, then
	 * @parent's parent isn't a special range
	 */

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
			if(likely(!REF_IS_SPECIAL(old->parent))) {
				replace_map_entry(db, g, old, parent, fpage, first_page_id);
			}
		} else if(L4_SizeLog2(old->range) > L4_SizeLog2(fpage)) {
			/* "contained" case. */
			int page_offs = (L4_Address(fpage) - L4_Address(old->range)) >> PAGE_BITS;
			if(CHECK_FLAG_ALL(L4_Rights(old->range), L4_Rights(fpage))
				&& old->first_page_id + page_offs == first_page_id
				&& REF_SPACE(old->parent) == REF_SPACE(parent)
				&& (REF_ADDR(old->parent) + page_offs * PAGE_SIZE) == REF_ADDR(parent))
			{
				/* contained no-op. the condition is hugely complex, but
				 * should succeed entirely after the first two terms.
				 */
			} else if(unlikely(REF_IS_SPECIAL(old->parent))) {
				/* won't touch a special range. */
			} else {
				/* break it up & replace. */
				struct map_entry *ne = fetch_entry(db, g, fpage, true);
				if(unlikely(ne == NULL)) {
					assert(probe_group_range(g, fpage) != NULL);
					return -ENOMEM;
				}
				replace_map_entry(db, g, ne, parent, fpage, first_page_id);
			}
		} else {
			assert(L4_SizeLog2(old->range) < L4_SizeLog2(fpage));
			/* "shrimp" case.
			 *
			 * TODO: wind "old" back to the leftmost entry covered by @fpage,
			 * erase entries from here to @fpage's end, recycle one slot for
			 * the added entry, and compress the rest of the group.
			 *
			 * FIXME: implement this!
			 */
			panic("shrimp case not implemented");
		}
	}
	struct pt_iter it;
	pt_iter_init(&it, SPACE_OF_MAPDB(db));
	for(int i = 0, l = L4_Size(fpage) / PAGE_SIZE; i < l; i++) {
		pt_set_page(&it, L4_Address(fpage) + i * PAGE_SIZE,
			first_page_id + i, L4_Rights(fpage), false);
	}
	pt_iter_destroy(&it);

	assert(add_map_postcond(db, parent, fpage, first_page_id));
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
 * sender address space within the mapping: therefore pages in the receiver
 * won't be unmapped when the sender's corresponding slot is empty.
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

	/* well this is a bit vile: the map_page > group_size case. recursion
	 * recurs, baby.
	 */
	int n_groups = L4_Size(map_page) / GROUP_SIZE;
	if(unlikely(n_groups > 1)) {
		int given = 0;
		for(int i=0; i < n_groups; i++) {
			L4_Fpage_t fp = L4_Fpage(
				L4_Address(map_page) + i * GROUP_SIZE,
				GROUP_SIZE);
			L4_Set_Rights(&fp, L4_Rights(map_page));
			int n = mapdb_map_pages(from_db, to_db, fp,
				dest_addr + i * GROUP_SIZE);
			if(unlikely(n < 0)) return n;
			given |= n;
		}
		return given;
	}

	/* the "within a single group in @from_db" case. */
	const L4_Word_t first_addr = L4_Address(map_page);
	struct map_group *grp = group_for_addr(from_db, first_addr);
	if(unlikely(grp == NULL)) return 0;
	struct map_entry *first = probe_group_range(grp, map_page);
	if(unlikely(first == NULL)) return 0;
	if(L4_SizeLog2(first->range) < L4_SizeLog2(map_page)) {
		first = rewind_to_first(grp, first, map_page);
	}

	int given;
	if(likely(L4_SizeLog2(map_page) <= L4_SizeLog2(first->range))) {
		/* the simple case: a small fpage being mapped from inside larger or
		 * equal-sized entry.
		 */
		if(unlikely(REF_IS_SPECIAL(first->parent))) return 0;
		given = L4_Rights(first->range) & L4_Rights(map_page);
		if(given != 0) {
			L4_Fpage_t p = L4_FpageLog2(dest_addr, L4_SizeLog2(map_page));
			L4_Set_Rights(&p, given);
			int off = first_addr - L4_Address(first->range);
			mapdb_add_map(to_db, MAPDB_REF(from_db->ref_id, first_addr),
				p, first->first_page_id + (off >> PAGE_BITS));
			mapdb_add_child(first, MAPDB_REF(to_db->ref_id, L4_Address(p)));
		}
	} else {
		/* the complex case: the range is made up out of multiple smaller
		 * entries.
		 */
		struct map_entry *ent = first;
		L4_Word_t limit = FPAGE_HIGH(map_page) + 1;
		given = 0;
		while(L4_Address(ent->range) < limit) {
			int eff = L4_Rights(ent->range) & L4_Rights(map_page);
			if(eff == 0 || unlikely(REF_IS_SPECIAL(ent->parent))) {
				goto next_entry;
			}
			given |= eff;

			const L4_Word_t pos = L4_Address(ent->range) - first_addr;
			L4_Word_t dp_addr;
			int size_log2;
			for_page_range(dest_addr + pos,
				dest_addr + pos + L4_Size(ent->range),
				dp_addr, size_log2)
			{
				L4_Fpage_t p = L4_FpageLog2(dp_addr, size_log2);
				L4_Set_Rights(&p, eff);

				int src_offs = dp_addr - pos - dest_addr;
				mapdb_add_map(to_db,
					MAPDB_REF(from_db->ref_id,
						L4_Address(ent->range) + src_offs),
					p, ent->first_page_id + src_offs / PAGE_SIZE);
				mapdb_add_child(ent, MAPDB_REF(to_db->ref_id, dp_addr));
			}

next_entry:
			if(++ent == &grp->entries[grp->num_entries]) break;
		}
	}

	return given;
}


/* @dst should have room for MSB(size) - 11 fpages; 21 at most. */
static int gen_range_pages(L4_Fpage_t *dst, L4_Word_t start, L4_Word_t size)
{
	assert(((start | size) & PAGE_MASK) == 0);

	L4_Word_t addr;
	int sizelog2, p = 0;
	for_page_range(start, start + size, addr, sizelog2) {
		dst[p++] = L4_FpageLog2(addr, sizelog2);
	}
	return p;
}


/* free items appear after *entry, i.e. the first is at (*entry)[1]. they're
 * left uncleared in the interest of efficiency (since calling this function
 * implies intent to overwrite, anyway). *entry is updated after group resize.
 *
 * return value is 0 on success, and -ENOMEM when realloc() fails. atomic on
 * failure (aside from an abort() inside memmove()).
 *
 * NOTE: the caller must bump g->num_entries as appropriate!
 */
static int insert_empties(
	struct map_group *g,
	int num_to_add,
	struct map_entry **entry)
{
	assert(num_to_add > 0);

	const int e_pos = *entry - g->entries;
	assert(e_pos >= -1 && e_pos < g->num_alloc);

	struct map_entry *e = *entry;
	int need = g->num_entries + num_to_add,
		num_tail = g->num_entries - e_pos - 1;
	if(need > g->num_alloc) {
		/* make moar RAMz */
		int newsize = MAX(int, g->num_alloc, 2) * 2;
		while(newsize < need) newsize *= 2;
		TRACE("%s: resizing group from %u to %d entries\n", __func__,
			g->num_alloc, newsize);
		void *ptr = realloc(g->entries, newsize * sizeof(struct map_entry));
		if(unlikely(ptr == NULL)) return -ENOMEM;
		g->entries = ptr;
		g->num_alloc = newsize;
		e = &g->entries[e_pos];
		*entry = e;
	}
	assert(g->num_alloc >= need);

	if(num_tail > 0) {
		TRACE("%s: move %d items to %d (*entry at %d)\n", __func__, num_tail,
			(e + num_to_add + 1) - g->entries, e - g->entries);
		memmove(e + num_to_add + 1, e + 1,
			sizeof(struct map_entry) * num_tail);
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
			struct map_entry *ent = fetch_entry(r.child_db, r.group,
				cut, true);
			if(unlikely(ent == NULL)) {
				assert(probe_group_range(r.group, cut) != NULL);
				/* FIXME: attempt some kind of atomicity on failure. */
				panic("split_entry() ran out of heap in distribute_children()");
			}

			r.child_entry = ent;

			/* block output guarantee, and a required result from
			 * fetch_entry(..., true): the child is known to exist.
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

	assert(fpage_overlap(cut, e->range));

	L4_Word_t r_first = L4_Address(cut);
	if(L4_Address(e->range) < r_first) {
		/* left side */
		p += gen_range_pages(&pg_buf[p], L4_Address(e->range),
			r_first - L4_Address(e->range));
	}
	pg_buf[p++] = cut;	/* middle */
	L4_Word_t r_end = L4_Address(cut) + L4_Size(cut);
	if(L4_Address(e->range) + L4_Size(e->range) > r_end) {
		/* right side */
		p += gen_range_pages(&pg_buf[p], r_end,
			L4_Address(e->range) + L4_Size(e->range) - r_end);
	}
	assert(p > 1);		/* forbid the trivial case */

	int n = insert_empties(g, p - 1, &e);
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
		/* FIXME: catch -ENOMEM */
		int n = distribute_children(db, g, cut, &saved);
		if(n < 0) {
			panic("distribute_children() failed in split_entry()");
		}
	}

	return 0;
}


/* find the leftmost contained entry in @range. if @range is contained in a
 * greater entry and @make_exact is set, attempt to break the larger entry
 * up to return an exact match for @range; or return NULL on malloc() failure.
 * (the caller must distinguish between "doesn't exist" and malloc-fail
 * explicitly.)
 *
 * returns special ranges as-is regardless of size and @make_exact. this
 * allows things like multi-page KIPs, hugepage UTCB mappings, and so forth.
 */
static struct map_entry *fetch_entry(
	struct map_db *db,
	struct map_group *g,
	L4_Fpage_t range,
	bool make_exact)
{
	assert(L4_Size(range) >= PAGE_SIZE);

	TRACE("%s: group %#lx, range %#lx:%#lx\n", __func__, g->start,
		L4_Address(range), L4_Size(range));

	struct map_entry *e = probe_group_range(g, range);
	if(e == NULL || unlikely(REF_IS_SPECIAL(e->parent))) {
		/* not found, or is special */
		return e;
	}

	int esz = L4_SizeLog2(e->range), rsz = L4_SizeLog2(range);
	if(esz <= rsz) {
		if(esz < rsz) e = rewind_to_first(g, e, range);
	} else if(make_exact) {
		int err = split_entry(db, g, e, range);
		if(unlikely(err < 0)) {
			if(err != -ENOMEM) {
				panic("split_entry() failed: non-ENOMEM error code");
			}
			e = NULL;
		} else {
			/* refetch our thing. */
			e = probe_group_range(g, range);
			assert(e != NULL);
			assert(L4_Address(e->range) == L4_Address(range));
			assert(L4_SizeLog2(e->range) == L4_SizeLog2(range));
		}
	}

	return e;
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
			coalesce_entries(cr.group, cr.child_entry);
		}

		cs[i] = REF_TOMBSTONE;	/* idempotency guarantee */
	}

	if(e->num_children > 1) free(e->children);
	e->num_children = 0;

	return 0;
}


static void clear_pt_range(struct pt_iter *drop_it, L4_Fpage_t range)
{
	L4_Word_t a = L4_Address(range), l = a + L4_Size(range);
	while(a < l) {
		if(pt_clear_page(drop_it, a)) a += PAGE_SIZE;
		else a = (a + PT_UPPER_SIZE) & ~PT_UPPER_MASK;
	}
}


static void set_pt_range_rights(struct pt_iter *mod_it, L4_Fpage_t range)
{
	if(unlikely(!CHECK_FLAG(L4_Rights(range), L4_Readable))) {
		clear_pt_range(mod_it, range);
		return;
	}

	L4_Word_t a = L4_Address(range), l = a + L4_Size(range);
	while(a < l) {
		/* TODO: add an interface that doesn't dig through the page tables
		 * twice. then use it here.
		 */
		bool upper;
		uint32_t pgid = pt_probe(mod_it, &upper, NULL, a);
		if(unlikely(!upper)) a = (a + PT_UPPER_SIZE) & ~PT_UPPER_MASK;
		else {
			pt_set_page(mod_it, a, pgid, L4_Rights(range), false);
			a += PAGE_SIZE;
		}
	}
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

	int rwx_seen = 0;

	/* check "affect special ranges" form */
	const bool drop_special = unlikely(!recursive)
		&& L4_Rights(range) == L4_FullyAccessible
		&& (range.raw & 0xc00) == 0x800;
	assert(drop_special || (L4_Address(range) & 0xc00) == 0);

	/* TODO: only initialize this if the `drop' variable becomes true */
	struct pt_iter mod_it;
	pt_iter_init(&mod_it, SPACE_OF_MAPDB(db));

	TRACE("%s: range %#lx:%#lx, %simmediate, %srecursive, ref_id %d\n",
		__func__, L4_Address(range), L4_Size(range),
		!immediate ? "non-" : "", !recursive ? "non-" : "",
		(int)db->ref_id);
	L4_Word_t unmap_rights = L4_Rights(range), r_end = FPAGE_HIGH(range);
	/* this function will only modify the structure of the map_groups it
	 * accesses when it might revoke access in the immediate map_db. this is
	 * used for fetch_entry().
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

		struct map_entry *e = fetch_entry(db, g, range, modify);
		if(e == NULL && modify) {
			/* distinguish between not-exist and ENOMEM from split_entry(). */
			if(probe_group_range(g, range) != NULL) {
				/* FIXME: implement restartable suspending somewhere along
				 * this function's call chain.
				 */
				panic("malloc() failed in fetch_entry()");
			}
		}
		if(e == NULL || (recursive && REF_IS_SPECIAL(e->parent))) {
			/* nonexistents are skipped. specials can't be influenced by Unmap
			 * and don't contribute to @rwx_seen.
			 */
			continue;
		}

		do {
			if(!REF_IS_SPECIAL(e->parent)) {
				/* check each native page in e->range.
				 *
				 * TODO: only do this if the caller provides a location for
				 * the out-parameter.
				 */
				L4_Word_t r_pos = L4_Address(e->range),
					r_last = r_pos + L4_Size(e->range);
				int e_mask = 0;
				do {
					bool up;
					int pmask = 0;
					pt_probe(&mod_it, &up, &pmask, r_pos);
					if(!up) {
						r_pos = (r_pos + PT_UPPER_SIZE) & ~PT_UPPER_MASK;
					} else {
						r_pos += PAGE_SIZE;
						if(pmask >= 0) e_mask |= pmask;
						else {
							assert(pmask == -ENOENT);
						}
					}
				} while(r_pos < r_last);

				if(e_mask != 0 && REF_DEFINED(e->parent)) {
					/* FIXME: propagate e_mask to parent */
				}

				rwx_seen |= (e_mask | e->access);
			}

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

			const bool special = REF_IS_SPECIAL(e->parent);
			bool drop = special && drop_special;
			if(modify && !special) {
				/* ensured by split_entry() */
				assert(FPAGE_LOW(e->range) >= FPAGE_LOW(range));
				assert(FPAGE_LOW(e->range) < r_end);
				assert(FPAGE_HIGH(e->range) <= r_end);

				int old_rights = L4_Rights(e->range),
					new_rights = old_rights & ~unmap_rights;
				L4_Set_Rights(&e->range, new_rights);
				if(new_rights == 0) drop = true;
				else if(new_rights < old_rights) {
					set_pt_range_rights(&mod_it, e->range);
				}
			}
			if(drop) {
				assert(modify);
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
					goto Enomem;
				}

				clear_pt_range(&mod_it, e->range);

				int pos = e - g->entries;
				if(pos < g->num_entries - 1) {
					int copy_num = g->num_entries - 1 - pos;
					memmove(&g->entries[pos], &g->entries[pos + 1],
						copy_num * sizeof(struct map_entry));
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

end:
	pt_iter_destroy(&mod_it);
	return rwx_seen;

Enomem:
	rwx_seen = -ENOMEM;
	goto end;
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



/* TODO: flick the directory entry out to optimize for VMs that use shadowed
 * page tables; restore it when done.
 */
int mapdb_fill_page_table(struct map_db *db, uintptr_t addr)
{
	addr &= ~PT_UPPER_MASK;

	/* TODO: force this in mapdb.h */
	assert(MAX_ENTRIES_PER_GROUP == 1 << PT_UPPER_WIDTH);
	struct map_group *g = group_for_addr(db, addr);
	if(g == NULL) return 0;

	int n_set = 0;
	struct pt_iter it;
	pt_iter_init(&it, SPACE_OF_MAPDB(db));
	for(int i=0; i < g->num_entries; i++) {
		struct map_entry *e = &g->entries[i];
		int n_pages = L4_Size(e->range) / PAGE_SIZE;
		for(int j=0; j < n_pages; j++) {
			pt_set_page(&it, L4_Address(e->range) + j * PAGE_SIZE,
				e->first_page_id + j, L4_Rights(e->range), true);
			n_set++;
		}
	}
	pt_iter_destroy(&it);

	return n_set;
}


COLD void init_mapdb(void)
{
	map_group_slab = KMEM_CACHE_NEW("map_group_slab", struct map_group);
}


COLD void mapdb_init_range(
	struct map_db *db,
	uintptr_t start_addr,
	const uint32_t *page_ids,
	unsigned int num_pages,
	int entry_flags)
{
	assert(check_mapdb_module(0));

	TRACE("%s: start_addr %#lx, num_pages %u (%#lx bytes)\n", __func__,
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
			L4_Set_Rights(&page, entry_flags & L4_FullyAccessible);
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
