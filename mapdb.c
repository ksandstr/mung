
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdalign.h>
#include <string.h>
#include <assert.h>
#include <errno.h>

#include <ccan/htable/htable.h>
#include <ccan/likely/likely.h>
#include <ccan/container_of/container_of.h>

#include <l4/types.h>

#include <ukernel/bug.h>
#include <ukernel/misc.h>
#include <ukernel/util.h>
#include <ukernel/slab.h>
#include <ukernel/trace.h>
#include <ukernel/space.h>
#include <ukernel/mapdb.h>
#include <ukernel/ptab.h>


/* for mapdb dumps on add/remove */
#define TRACE(fmt, ...) TRACE_MSG(TRID_MAPDB, fmt, ##__VA_ARGS__)

/* the child/parent reference format.
 *
 * - high 10 bits (v1: 9 with x86 PTE) index into the target group in
 *   increments of PAGE_SIZE.
 * - in between there are group ID bits per grp_mask_and.
 * - the low 4 ("MISC") bits vary:
 *   - in map_entry.parent, bit 1 (value 2) is the IS_SPECIAL indicator.
 *     others are undefined, so left at 0;
 *   - in map_entry.{child,children} they are copies of bits 5..1 of the
 *     child's position within the parent group.
 *     - in the child array tombstone, the value is 1. the tombstone is
 *       therefore distinct from NULL but not defined.
 *
 * child references are stored in the <struct map_entry>.children array at a
 * position within MAX_PROBE_DEPTH slots from their REF_HASH() value, computed
 * from all the bits except MISC, the probe index proceeding forward and
 * wrapping at the array's end.
 */
/* constructors */
#define REF_CTOR(ix, grp, misc) ((ix) << 22 | (grp) << grp_mask_shift | (misc))
#define REF_SPECIAL(ix, grp, misc) REF_CTOR((ix), (grp), (misc) | 2)
/* basic accessors */
#define REF_INDEX(ref) (((ref) >> 22) & 0x3ff)
#define REF_GROUP_BITS(ref) ((ref) & grp_mask_and)
#define REF_MISC(ref) ((ref) & (sizeof(struct map_group) - 1))
/* derived accessors */
#define REF_IS_NULL(ref) ((ref) == 0)
#define REF_IS_SPECIAL(ref) CHECK_FLAG(REF_MISC((ref)), 2)
#define REF_DEFINED(ref) (REF_GROUP_BITS(ref) != 0)
#define REF_ADDR(ref) (REF_INDEX(ref) * PAGE_SIZE)
#define REF_GROUP_ID(ref) (REF_GROUP_BITS((ref)) >> grp_mask_shift)
#define REF_HASH(ref) int_hash((ref) & ~0xful)
/* constants */
#define REF_NULL 0
#define REF_TOMBSTONE REF_CTOR(0, 0, 1)

#define GROUP_SIZE (PAGE_SIZE * MAX_ENTRIES_PER_GROUP)
#define GROUP_ADDR(addr) ((addr) & ~(GROUP_SIZE - 1))

/* maximum probe depth in map_entry->children. */
#define MAX_PROBE_DEPTH 8

/* mode bits for unmap_entry_in_group().
 *
 * TODO: export immediate, recursive, and get_access; and change
 * mapdb_unmap_fpage() to accept their mask instead of 3 bools.
 */
#define UM_IMMEDIATE	0x1
#define UM_RECURSIVE	0x2
#define UM_GET_ACCESS	0x4
#define UM_DROP_SPECIAL	0x8
#define UM_RIGHTS		0x70	/* L4_Rights() in mask */


/* dereferenced map_entry->children entry.
 *
 * TODO: replace this with just two out-parameters, since group->space is now
 * there.
 */
struct child_ref
{
	struct map_group *group;
	struct map_entry *child_entry;
};


static struct map_entry *fetch_entry(
	struct map_group *g, L4_Fpage_t range, bool make_exact);

static int insert_empties(
	struct map_group *g,
	int num_to_add,
	struct map_entry **entry);

static struct map_entry *probe_group_addr(struct map_group *g, uintptr_t addr);

static int reparent_children(struct map_group *g, struct map_entry *e);
static bool deref_child(
	struct child_ref *cr,
	struct map_group *home_grp,
	struct map_entry *e,
	int child_ix,
	L4_Fpage_t eff_range);		/* as given to unmap_entry_in_group() */

/* (how come this function is both static, and prefixed?) */
static int mapdb_add_child(struct map_entry *ent, L4_Word_t child);


struct kmem_cache *map_group_slab = NULL;
static struct slab_policy *map_group_policy;
static uintptr_t grp_mask_and, grp_mask_or;
static unsigned short grp_mask_shift;


static void dump_map_group(struct map_group *g)
{
#ifndef NDEBUG
	TRACE("%s: group %#lx .. %#lx contains (%d ents, %lu alloc):\n",
		__func__, MG_START(g), MG_START(g) + GROUP_SIZE - 1,
		MG_N_ENTRIES(g), 1ul << MG_N_ALLOC_LOG2(g));
	for(int i=0; i < MG_N_ENTRIES(g); i++) {
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


/* tests a known-valid pointer for whether the group is also valid.
 *
 * XXX: a reference to a group that's both pointer-valid and alive may still
 * end up pointing to no valid entry at all, or an entry in an unrelated
 * space's group (because of object recycling). caveat lector.
 */
static bool is_group_valid(struct map_group *grp) {
	return grp->space != NULL;
}


static struct map_group *group_for_addr(struct space *sp, uintptr_t addr) {
	uintptr_t key = GROUP_ADDR(addr);
	return htable_get(&sp->ptab_groups, int_hash(key), &cmp_group_addr, &key);
}


/* turn @address (within @g) into a group-id + index pair. */
static inline L4_Word_t addr_to_ref(struct map_group *g, uintptr_t addr)
{
	assert(addr >= MG_START(g) && addr < MG_START(g) + GROUP_SIZE);
	uintptr_t grp_bits = (uintptr_t)g & grp_mask_and,
		ix = (addr - MG_START(g)) >> PAGE_BITS;

	assert(ix >= 0 && ix < GROUP_SIZE / PAGE_SIZE);
	L4_Word_t ref = grp_bits | (ix << 22);
	assert(REF_ADDR(ref) + MG_START(g) == (addr & ~PAGE_MASK));
	assert((REF_GROUP_BITS(ref) | grp_mask_or) == (uintptr_t)g);
	return ref;
}


/* "don't test this" bits.
 * these are used when the partial invariant is known not to pass.
 */
#define MOD_NO_CHILD_REFS (1 << 0)	/* don't check child refs */

#ifdef DEBUG_ME_HARDER
#include <ukernel/invariant.h>

/* runtime invariant checks. */

static bool check_map_entry(
	struct map_group *grp, struct map_entry *e, int opts)
{
	INV_CTX;

	/* for each entry, check that
	 *
	 *   - it references a valid parent entry, or that its ->parent is
	 *     correctly special or undefined
	 *   - it is at most as large as the parent
	 *   - the parent has a child reference to it
	 *     - (unless MOD_NO_CHILD_REFS \in @opts)
	 *   - if it's special, check that space=0 appears only in sigma0 and
	 *     that space=1 has no children.
	 */
	inv_push("check entry %#lx:%#lx in grp=%p; ->parent %#lx",
		L4_Address(e->range), L4_Size(e->range), grp,
		e->parent);

	inv_ok1(L4_Rights(e->range) != 0);
	inv_ok1(L4_Size(e->range) <= GROUP_SIZE);
	inv_ok1(fpage_overlap(L4_Fpage(MG_START(grp), GROUP_SIZE), e->range));

	struct map_group *p_grp = kmem_id2ptr_safe(map_group_policy,
		REF_GROUP_ID(e->parent));
	if(p_grp != NULL && p_grp->space == NULL) {
		inv_log("p_grp=%p had NULL space -- cleared", p_grp);
		p_grp = NULL;
	}
	inv_iff1(REF_DEFINED(e->parent), p_grp != NULL);

	if(p_grp != NULL) {
		L4_ThreadId_t s_name = space_name(p_grp->space);
		inv_log("p_grp=%p, ->space=%p (%lu:%lu)",
			p_grp, p_grp->space, L4_ThreadNo(s_name),
			L4_Version(s_name));
		inv_iff1(REF_IS_SPECIAL(e->parent),
			p_grp->space == kernel_space);
	}
	inv_imply1(REF_IS_SPECIAL(e->parent), e->num_children == 0);
	inv_imply1(REF_IS_SPECIAL(e->parent), REF_ADDR(e->parent) == 0);
	inv_imply1(REF_IS_SPECIAL(e->parent), REF_GROUP_BITS(e->parent) == 0);

	struct map_entry *p_e;
	if(!REF_DEFINED(e->parent)) {
		inv_log("  ... is parentless entry");
		p_e = NULL;
	} else if(REF_IS_SPECIAL(e->parent)) {
		inv_log("  ... is special entry (parent=%#lx)", e->parent);
		p_e = NULL;
	} else {
		inv_ok1(p_grp != NULL);
		p_e = probe_group_addr(p_grp,
			MG_START(p_grp) + REF_ADDR(e->parent));
		inv_ok(p_e != NULL, "must find parent for ref=[%#lx, %#lx]",
			REF_INDEX(e->parent), REF_GROUP_BITS(e->parent));
		inv_log("parent entry %#lx:%#lx in p_grp=%p",
			L4_Address(p_e->range), L4_Size(p_e->range), p_grp);
		inv_ok1(ADDR_IN_FPAGE(p_e->range,
			MG_START(p_grp) + REF_ADDR(e->parent)));
		inv_ok1(L4_SizeLog2(e->range) <= L4_SizeLog2(p_e->range));
	}

	if(!CHECK_FLAG(opts, MOD_NO_CHILD_REFS) && p_e != NULL) {
		assert(p_grp != NULL);
		bool found = false;
		const L4_Word_t *p_cs = p_e->num_children > 1
			? p_e->children : &p_e->child;
		for(int j=0; j < p_e->num_children; j++) {
			inv_log("  child %d = %#lx", j, p_cs[j]);
			if(REF_GROUP_BITS(p_cs[j]) != ((uintptr_t)grp & grp_mask_and)) continue;
			if(ADDR_IN_FPAGE(e->range, MG_START(grp) + REF_ADDR(p_cs[j]))) {
				inv_log("  found matching entry=%#lx:%#lx",
					L4_Address(e->range), L4_Size(e->range));
				inv_ok(!found, "should find child only once");
				found = true;

				/* test deref_child() since the loop provides us with
				 * known results.
				 */
				struct child_ref cr;
				bool got_child = deref_child(&cr, p_grp, p_e, j, p_e->range);
				inv_ok1(got_child);
				inv_ok1(cr.group == grp);
				inv_ok1(cr.child_entry == e);
			}
		}
		inv_ok1(found);
	}

	inv_pop();
	return true;

inv_fail:
	return false;
}


static bool check_map_group(struct map_group *grp, int opts)
{
	INV_CTX;

	/* group spec. */
	inv_ok1(MG_N_ENTRIES(grp) <= MAX_ENTRIES_PER_GROUP);
	inv_ok1(MG_N_ENTRIES(grp) <= 1 << MG_N_ALLOC_LOG2(grp));
	inv_ok1(MG_N_ALLOC_LOG2(grp) <= size_to_shift(MAX_ENTRIES_PER_GROUP));

	for(int i=0; i < MG_N_ENTRIES(grp); i++) {
		struct map_entry *e = &grp->entries[i];

		inv_push("e->range=%#lx:%#lx",
			L4_Address(e->range), L4_Size(e->range));
		/* entries should appear in sorted order. */
		inv_imply1(e > grp->entries,
			FPAGE_HIGH(e[-1].range) < FPAGE_LOW(e->range));

		inv_ok1(check_map_entry(grp, e, opts));

		inv_pop();
	}

	return true;

inv_fail:
	return false;
}


static bool check_mapdb(struct space *sp, int opts)
{
	INV_CTX;

	struct pt_iter pit;
	pt_iter_init(&pit, sp);

	uint32_t *pdir_mem = map_vm_page(sp->pdirs, VM_SYSCALL);
	inv_ok1(pdir_mem != NULL);

	struct htable_iter grp_it;
	for(struct map_group *grp = htable_first(&sp->ptab_groups, &grp_it);
		grp != NULL;
		grp = htable_next(&sp->ptab_groups, &grp_it))
	{
		inv_push("group.start=%#lx .space=%p .n_entries=%d .n_alloc_log2=%lu",
			MG_START(grp), grp->space, MG_N_ENTRIES(grp),
			MG_N_ALLOC_LOG2(grp));

		/* ein Maschinentest, jawohl */
		inv_imply1(grp->ptab_page != NULL,
			pt_upper_present(&pit, MG_START(grp)));

		uint32_t pde = pdir_mem[MG_START(grp) / GROUP_SIZE];
		inv_iff1(grp->ptab_page != NULL, CHECK_FLAG(pde, PDIR_PRESENT));
		inv_imply1(grp->ptab_page != NULL, grp->ptab_page->id == pde >> 12);

		inv_ok1(check_map_group(grp, opts));

		inv_pop();
	}
	pt_iter_destroy(&pit);

	return true;

inv_fail:
	pt_iter_destroy(&pit);
	return false;
}

#else
#define check_mapdb(foo, bar) (true)
#endif


/* fetch the map_group and map_entry referenced in @e->children[@child_ix] and
 * return true, or return false when not found or stale.
 *
 * @home_group is used to decide staleness per group reference and
 * index within entry. @e need not point to inside @home_group->entries .
 */
static bool deref_child(
	struct child_ref *cr,		/* outputs appear in *cr. */
	struct map_group *home_group,
	struct map_entry *e,
	int child_ix,
	L4_Fpage_t eff_range)
{
	assert(child_ix < e->num_children);
	assert(fpage_overlap(eff_range, e->range));
	assert(L4_SizeLog2(eff_range) <= L4_SizeLog2(e->range));

	L4_Word_t *children = e->num_children > 1 ? e->children : &e->child,
		ref = children[child_ix];
	if(!REF_DEFINED(ref)) return false;

	if(eff_range.raw != e->range.raw) {
		/* test the MISC bits. */
		int mask = (~(L4_Size(eff_range) - 1) & (0x1e * PAGE_SIZE))
				>> (PAGE_BITS + 1),
			bits = (eff_range.raw >> (PAGE_BITS + 1)) & mask;
		assert(L4_SizeLog2(eff_range) > PAGE_BITS + 5 || mask != 0);
		if((ref & mask) != bits) {
			TRACE("%s: ref=%#lx excluded by MISC bits (got=%#x, mask=%#x, bits=%#x)\n",
				__func__, ref, (unsigned)REF_MISC(ref), (unsigned)mask,
				(unsigned)bits);
			return false;
		}
	}

	struct map_group *g = kmem_id2ptr_safe(map_group_policy,
		REF_GROUP_ID(ref));
	if(unlikely(g == NULL || !is_group_valid(g))) goto tombstone;
	cr->child_entry = probe_group_addr(g, MG_START(g) + REF_ADDR(ref));
	if(cr->child_entry == NULL) {
		TRACE("%s: no entry at %#lx+%#lx=%#lx\n", __func__,
			MG_START(g), REF_ADDR(ref), MG_START(g) + REF_ADDR(ref));
		goto tombstone;
	}
	cr->group = g;

	/* check the child's parent reference. it should point into @e->range
	 * within @home_group.
	 */
	L4_Word_t c_pt = cr->child_entry->parent;
	if(((uintptr_t)home_group & grp_mask_and) != REF_GROUP_BITS(c_pt)
		|| !ADDR_IN_FPAGE(e->range, MG_START(home_group) + REF_ADDR(c_pt)))
	{
		TRACE("%s: backref %#lx mismatches group %#lx, or range %#lx..%#lx\n",
			__func__, c_pt, (L4_Word_t)home_group & grp_mask_and,
			FPAGE_LOW(e->range), FPAGE_HIGH(e->range));
		goto tombstone;
	}

	/* check that the physical page is the same, too. */
	int pg_off = REF_INDEX(c_pt)
		- (L4_Address(e->range) - MG_START(home_group)) / PAGE_SIZE;
	if(cr->child_entry->first_page_id != e->first_page_id + pg_off) {
		TRACE("%s: page mismatch (child first %u, parent first %u, offset %d)\n",
			__func__, cr->child_entry->first_page_id, e->first_page_id, pg_off);
		goto tombstone;
	}

	return true;

tombstone:
	/* when a child has been very, very naughty. */
	children[child_ix] = REF_TOMBSTONE;
	return false;
}


static size_t rehash_map_group(const void *ptr, void *priv) {
	const struct map_group *g = ptr;
	return int_hash(MG_START(g));
}


void mapdb_init(struct space *sp)
{
	htable_init(&sp->ptab_groups, &rehash_map_group, NULL);
}


void mapdb_destroy(struct space *sp)
{
	struct htable_iter it;
	for(struct map_group *g = htable_first(&sp->ptab_groups, &it);
		g != NULL;
		g = htable_next(&sp->ptab_groups, &it))
	{
		for(int i=0; i < MG_N_ENTRIES(g); i++) {
			struct map_entry *e = &g->entries[i];
			if(e->num_children > 0) {
				reparent_children(g, e);
				if(e->num_children > 1) free(e->children);
			}
		}
		free(g->entries); g->entries = NULL;
		x86_free_ptab(g);

		g->space = NULL;
		assert(!is_group_valid(g));
		kmem_cache_free(map_group_slab, g);
	}
	htable_clear(&sp->ptab_groups);
}


/* used in postcondition asserts. brute force. */
static bool no_addr_in_group(struct map_group *g, uintptr_t addr)
{
	for(int i=0; i < MG_N_ENTRIES(g); i++) {
		assert(!ADDR_IN_FPAGE(g->entries[i].range, addr));
	}

	return true;
}


/* finds an entry in @fpage. returns non-negative index into @g->entries on
 * success, and -(n + 1) for an approximate position where @fpage could be
 * inserted if nothing was found.
 */
static int search_group_by_range(struct map_group *g, L4_Fpage_t fpage)
{
	/* binary search. faster than it looks. */
	L4_Word_t fpage_addr = L4_Address(fpage);
	int low = 0, high = MG_N_ENTRIES(g) - 1, mid = 0;
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


/* NOTE: @addr is _absolute_ because map_entry->range is also absolute. the
 * common error is to pass a REF_ADDR() value, so there's an assert here to
 * catch that.
 */
static struct map_entry *probe_group_addr(struct map_group *g, uintptr_t addr)
{
	assert(BETWEEN(MG_START(g),
		MG_START(g) + MAX_ENTRIES_PER_GROUP * PAGE_SIZE - 1,
		addr));
	if(unlikely(MG_N_ENTRIES(g) == 0)) return NULL;

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
		&& (REF_GROUP_BITS(e->parent) != REF_GROUP_BITS(parent)
			|| REF_ADDR(e->parent) != (REF_ADDR(parent) ^ L4_Size(fpage))
			|| far_side != (REF_ADDR(e->parent) < REF_ADDR(parent))))
	{
		/* either or both have a defined parent; and they reference different
		 * parent groups, or the addresses aren't contiguous, or the addresses
		 * aren't in the right order.
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

	int old_ix = REF_INDEX(e->parent);
	e->parent &= (~n_pages << 22);
	assert(REF_INDEX(e->parent) == (old_ix & ~n_pages));

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
	if(oth >= &g->entries[0] && oth < &g->entries[MG_N_ENTRIES(g)]
		&& can_merge(oth, e->parent, e->range, e->first_page_id))
	{
		/* always join to the left. */
		if(far_side) SWAP(struct map_entry *, e, oth);
		expand_entry(e);

		/* move valid children over as well. these will overwrite (presumably
		 * less or equally valid) children in @e.
		 */
		L4_Word_t *chs = oth->num_children <= 1 ? &oth->child : oth->children;
		for(int i=0; i < oth->num_children; i++) {
			struct child_ref cr;
			if(!deref_child(&cr, g, oth, i, oth->range)) continue;
			int n = mapdb_add_child(e, chs[i]);
			/* FIXME: handle this atomically by pre-growing @e's child array
			 * before expand_entry().
			 */
			if(unlikely(n == -ENOMEM)) {
				panic("ENOMEM in coalesce_entries()");
			}
		}
		if(oth->num_children > 1) free(oth->children);
		oth->num_children = 0;

		/* NOTE: an iterative version of this function could keep voodoo
		 * indexes about the removed entries' range, and then do a single
		 * larger memmove at the end. multiple joins are so rare that we
		 * don't, though.
		 */
		int oix = oth - &g->entries[0];
		if(oix < (--g->addr, MG_N_ENTRIES(g))) {
			memmove(&g->entries[oix], &g->entries[oix + 1],
				sizeof(struct map_entry) * (MG_N_ENTRIES(g) - oix));
		}
		g->entries[MG_N_ENTRIES(g)].range = L4_Nilpage;

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
	assert(prev_pos >= -1 && prev_pos < MG_N_ENTRIES(g));
	assert(L4_Rights(fpage) != 0);

	/* the "far" page has this bit set and lies further along in the address
	 * space.
	 */
	bool far_side = CHECK_FLAG(L4_Address(fpage), L4_Size(fpage));
	TRACE("%s: far_side=%s, fpage=%#lx:%#lx, parent=%#lx, fpi=%u\n",
		__func__, btos(far_side), L4_Address(fpage), L4_Size(fpage),
		parent, first_page_id);

	struct map_entry *e = &g->entries[far_side ? prev_pos : prev_pos + 1];
	if(e < &g->entries[0] || e >= &g->entries[MG_N_ENTRIES(g)]) {
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


/* create a map group with a single map_entry. returns NULL when out of
 * memory.
 */
static struct map_group *add_map_group(
	struct space *sp,
	L4_Word_t parent,
	L4_Fpage_t fpage,
	uint32_t first_page_id)
{
	struct map_group *g = kmem_cache_alloc(map_group_slab);
	if(unlikely(g == NULL)) return NULL;
	assert(!is_group_valid(g));

	*g = (struct map_group){
		.addr = GROUP_ADDR(L4_Address(fpage)) | (1 << 15) | 1,
		.space = sp,
		.entries = malloc(sizeof(struct map_entry) * 2),
		/* .ptab_page is left NULL for lazy creation in the pf handler */
	};
	if(unlikely(g->entries == NULL)) goto Enomem;
	assert(MG_START(g) == GROUP_ADDR(L4_Address(fpage)));
	assert(MG_N_ALLOC_LOG2(g) == 1);
	assert(MG_N_ENTRIES(g) == 1);
	g->entries[0] = (struct map_entry){
		.parent = parent, .range = fpage, .first_page_id = first_page_id,
	};
	g->entries[1].range = L4_Nilpage;

	bool ok = htable_add(&sp->ptab_groups, int_hash(MG_START(g)), g);
	if(unlikely(!ok)) {
		free(g->entries);
		goto Enomem;
	}

	return g;

Enomem:
	g->space = NULL;
	assert(!is_group_valid(g));
	kmem_cache_free(map_group_slab, g);
	return NULL;
}


/* insert a map_entry for (@parent, @fpage, @first_page_id) into @g. if a
 * compatible map_entry exists already, it'll be extended. otherwise a new
 * entry will be inserted.
 *
 * returns 0 on success, or -ENOMEM on out-of-memory.
 */
static int insert_map_entry(
	struct space *sp,
	struct map_group *g,
	L4_Word_t parent,
	L4_Fpage_t fpage,
	uint32_t first_page_id)
{
	assert(!L4_IsNilFpage(fpage));
	assert(g != NULL);
	assert(group_for_addr(sp, L4_Address(fpage)) == g);
	assert(mapdb_probe(sp, L4_Address(fpage)) == NULL);

	/* this happens during boot-up.
	 *
	 * XXX or does it? add a panic() to check it out.
	 *
	 * TODO: it should be removed; this function is along every pager's hot
	 * path.
	 */
	if(unlikely(MG_N_ENTRIES(g) == 0)) {
		if(MG_N_ALLOC_LOG2(g) == 0) {
			assert(g->entries == NULL);
			g->entries = malloc(2 * sizeof(struct map_entry));
			if(unlikely(g->entries == NULL)) return -ENOMEM;
			g->addr |= (1 << 15);
			assert(MG_N_ALLOC_LOG2(g) == 1);
		}
		g->entries[0] = (struct map_entry){
			.parent = parent, .range = fpage,
			.first_page_id = first_page_id,
		};
		g->entries[1].range = L4_Nilpage;
		g->addr |= 1;
		assert(MG_N_ENTRIES(g) == 1);
		return 0;
	}

	/* find the page's neighbour, or its insert position. */
	L4_Fpage_t search_page = L4_FpageLog2(
		L4_Address(fpage) & ~L4_Size(fpage),
		L4_SizeLog2(fpage) + 1);
	int ix = search_group_by_range(g, search_page);
	if(ix < 0 || !merge_into_entry(g, ix, parent, fpage, first_page_id)) {
		int prev = abs(ix) - 1;		/* (correct for esoteric reasons.) */
		assert(prev < MG_N_ENTRIES(g));
		/* scan forward or backward to find the right spot. */
		bool back = false;
		while(prev >= 0
			&& L4_Address(g->entries[prev].range) > L4_Address(fpage))
		{
			prev--;
			back = true;
		}
		while(!back && prev < MG_N_ENTRIES(g) - 1
			&& L4_Address(g->entries[prev + 1].range) < L4_Address(fpage))
		{
			prev++;
		}
		assert(prev == -1 || !fpage_overlap(g->entries[prev].range, fpage));
		assert(prev == MG_N_ENTRIES(g) - 1
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
		assert(MG_N_ENTRIES(g) < 0x7fff);
		g->addr++;
	}

	return 0;
}


/* returns the new @e, making iteration safe if g->entries ends up shrunk
 * by this function. this is supposed to be used in conjunction with
 * reparent_children() to rewrite any child mappings' parent refs.
 */
static struct map_entry *erase_map_entry(
	struct map_group *g,
	struct map_entry *e)
{
	int n_ents = MG_N_ENTRIES(g), nal_shift = MG_N_ALLOC_LOG2(g),
		n_alloc = 1 << nal_shift;
	assert(n_ents > 0);
	assert(e->num_children == 0);	/* see comment */

	int pos = e - g->entries;
	if(pos < n_ents - 1) {
		int copy_num = n_ents - 1 - pos;
		memmove(&g->entries[pos], &g->entries[pos + 1],
			copy_num * sizeof(struct map_entry));
	} else {
		e->range = L4_Nilpage;
	}
	n_ents--;

	/* map_group shrinking (but don't go below 8 items) */
	if(n_ents <= n_alloc / 2 - n_alloc / 8 && n_alloc > 8) {
		int e_pos = e - g->entries;
		n_alloc >>= 1;
		void *ptr = realloc(g->entries, n_alloc * sizeof(struct map_entry));
		if(ptr != NULL) {
			g->entries = ptr;
			g->addr &= ~(0xful << 15);
			g->addr |= (nal_shift - 1) << 15;
			assert(1 << MG_N_ALLOC_LOG2(g) == n_alloc);
			e = &g->entries[e_pos];
		}
	}

	assert(MG_N_ENTRIES(g) > 0);
	g->addr--;
	assert(MG_N_ENTRIES(g) == n_ents);

	return e;
}


/* counterintuitively, if this function finds that "old" matches the page
 * range as well, fpage's rights are added to those in "old" rather than
 * replaced. this is consistent with the behaviour of L4's map operation in
 * all cases.
 */
static void replace_map_entry(
	struct map_group *g,
	struct map_entry *old,
	L4_Word_t parent,
	L4_Fpage_t fpage,
	uint32_t first_page_id)
{
	assert(L4_Address(old->range) == L4_Address(fpage));
	assert(L4_SizeLog2(old->range) == L4_SizeLog2(fpage));

	if(old->first_page_id == first_page_id && old->parent == parent) {
		/* matches content and parentage -> expand rights */
		L4_Set_Rights(&old->range,
			L4_Rights(old->range) | L4_Rights(fpage));
	} else {
		if(old->num_children > 0) {
			/* FIXME: reparent replacee's children */
			printf("%s: %d children were left dangling!\n",
				__func__, (int)old->num_children);
			if(old->num_children > 1) {
				free(old->children);
				old->children = NULL;
			}
		}
		*old = (struct map_entry){
			.parent = parent, .range = fpage,
			.first_page_id = first_page_id,
		};
	}
}


static bool add_map_postcond(
	struct space *sp,
	L4_Word_t parent,
	L4_Fpage_t map_area,
	uint32_t first_page_id)
{
	/* @sp must contain the indicated range if it existed in the parent. */
	struct map_group *p_grp = kmem_id2ptr_safe(map_group_policy,
		REF_GROUP_ID(parent));
	/* @parent may be undefined, or special. */
	assert(p_grp == NULL || is_group_valid(p_grp));
	assert(p_grp != NULL || !REF_DEFINED(parent) || REF_IS_SPECIAL(parent));

	if(REF_DEFINED(parent) && !REF_IS_SPECIAL(parent)) {
		int n_pages = L4_Size(map_area) / PAGE_SIZE;
		for(int i=0; i < n_pages; i++) {
			L4_Word_t off = i * PAGE_SIZE,
				p_addr = MG_START(p_grp) + REF_ADDR(parent) + off,
				addr = L4_Address(map_area) + off;
			struct map_entry *p_e = probe_group_addr(p_grp, p_addr);
			if(p_e == NULL) {
				TRACE("%s: no parent entry\n", __func__);
				continue;
			}

			struct map_entry *e = mapdb_probe(sp, addr);
			assert(e != NULL);
			assert(mapdb_page_id_in_entry(e, addr) == i + first_page_id
				|| REF_IS_SPECIAL(e->parent));
			assert(mapdb_page_id_in_entry(p_e, p_addr) == i + first_page_id);
		}
	}

	/* check that where map_area is present in @sp's page tables, it's either
	 * got missing lowest-level tables or the correct page IDs from
	 * @first_page_id onward.
	 */
	struct pt_iter it;
	pt_iter_init(&it, sp);
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
	struct space *sp,
	struct map_group **fpage_group_p,
	L4_Word_t parent,
	L4_Fpage_t fpage,
	uint32_t first_page_id)
{
	assert(L4_Size(fpage) <= GROUP_SIZE);
	assert(check_mapdb(sp, 0));

	TRACE("%s: adding fpage=%#lx:%#lx, access=%c%c%c, parent=%#lx\n",
		__func__, L4_Address(fpage), L4_Size(fpage),
		CHECK_FLAG(L4_Rights(fpage), L4_Readable) ? 'r' : '-',
		CHECK_FLAG(L4_Rights(fpage), L4_Writable) ? 'w' : '-',
		CHECK_FLAG(L4_Rights(fpage), L4_eXecutable) ? 'x' : '-',
		parent);

	/* mapdb_add_map() will only create unparented entries in sigma0_space, in
	 * any space while sigma0_space hasn't yet been defined, in @sp's UTCB
	 * area, and in @sp's KIP area.
	 *
	 * other causes of such entries are nonrecursive flushes: grants and
	 * mappings on top of.
	 */
	assert(REF_DEFINED(parent)
		|| fpage_overlap(fpage, sp->utcb_area)
		|| fpage_overlap(fpage, sp->kip_area)
		|| sigma0_space == NULL || sp == sigma0_space);
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
	struct map_group *g = group_for_addr(sp, L4_Address(fpage));
	if(g == NULL) {
		/* no group. */
		g = add_map_group(sp, parent, fpage, first_page_id);
		if(fpage_group_p != NULL) *fpage_group_p = g;
		if(g == NULL) return -ENOMEM;
	} else {
		if(fpage_group_p != NULL) *fpage_group_p = g;
		struct map_entry *old = probe_group_range(g, fpage);
		if(old == NULL) {
			/* not covered. */
			int n = insert_map_entry(sp, g, parent, fpage, first_page_id);
			if(unlikely(n != 0)) return n;
		} else if(L4_SizeLog2(old->range) == L4_SizeLog2(fpage)) {
			/* exact match with old entry's form. */
			assert(L4_Address(old->range) == L4_Address(fpage));
			if(likely(!REF_IS_SPECIAL(old->parent))) {
				replace_map_entry(g, old, parent, fpage, first_page_id);
			}
		} else if(L4_SizeLog2(old->range) > L4_SizeLog2(fpage)) {
			/* "contained" case. */
			int page_offs = (L4_Address(fpage) - L4_Address(old->range)) >> PAGE_BITS;
			if(CHECK_FLAG_ALL(L4_Rights(old->range), L4_Rights(fpage))
				&& old->first_page_id + page_offs == first_page_id
				&& REF_GROUP_BITS(old->parent) == REF_GROUP_BITS(parent)
				&& REF_INDEX(old->parent) + page_offs == REF_INDEX(parent))
			{
				/* contained no-op. the condition is hugely complex, but
				 * should succeed entirely after the first two terms.
				 */
			} else if(unlikely(REF_IS_SPECIAL(old->parent))) {
				/* won't touch a special range. */
			} else {
				/* break it up & replace. */
				struct map_entry *ne = fetch_entry(g, fpage, true);
				if(unlikely(ne == NULL)) {
					assert(probe_group_range(g, fpage) != NULL);
					return -ENOMEM;
				}
				replace_map_entry(g, ne, parent, fpage, first_page_id);
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
	pt_iter_init_group(&it, g);
	for(int i = 0, l = L4_Size(fpage) / PAGE_SIZE; i < l; i++) {
		pt_set_page(&it, L4_Address(fpage) + i * PAGE_SIZE,
			first_page_id + i, L4_Rights(fpage));
	}
	pt_iter_destroy(&it);

	assert(add_map_postcond(sp, parent, fpage, first_page_id));
	assert(check_mapdb(sp, MOD_NO_CHILD_REFS));

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
			for(int p = REF_HASH(c) & mask, end = p + depth - 1;
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


/* returns 0 or -ENOMEM.
 *
 * overwrites the first existing entry (i.e. one with equal REF_INDEX and
 * REF_GROUP_BITS) within the probe range until the first unused slot, and
 * replaces the rest with tombstones.
 */
static int mapdb_add_child(struct map_entry *ent, L4_Word_t child)
{
	assert(REF_DEFINED(child));
	TRACE("mapdb: add child %#lx to entry %#lx:%#lx (%p)\n", child,
		L4_Address(ent->range), L4_Size(ent->range), ent);
	L4_Word_t cmp_child = child & ~0xfUL;
	if(ent->num_children == 0
		|| (ent->num_children == 1 && !REF_DEFINED(ent->child))
		|| (ent->num_children == 1 && (ent->child & ~0xfUL) == cmp_child))
	{
		ent->child = child;
		ent->num_children = 1;
	} else if(ent->num_children == 1) {
		assert((ent->child & ~0xfUL) != cmp_child);
		L4_Word_t *new_children = malloc(sizeof(L4_Word_t) * 2);
		if(unlikely(new_children == NULL)) return -ENOMEM;
		int slot = REF_HASH(child) & 1;
		new_children[slot] = child;
		new_children[slot ^ 1] = ent->child;
		ent->children = new_children;
		ent->num_children = 2;
	} else {
		L4_Word_t *got = NULL;
		do {
			assert(POPCOUNT(ent->num_children) == 1);
			int mask = ent->num_children - 1, base = REF_HASH(child) & mask;
			for(int i=0, md = MIN(int, MAX_PROBE_DEPTH, ent->num_children);
				i < md;
				i++)
			{
				int probe = (i + base) & mask;
				L4_Word_t *c = &ent->children[probe];
				if(!REF_DEFINED(*c)) {
					if(got == NULL) got = c;
					if(*c == REF_NULL) break;
				} else if((*c & ~0xfUL) == cmp_child) {
					/* overwrite the first, tombstone the rest. */
					if(got == NULL) got = c; else *c = REF_TOMBSTONE;
				}
			}
			if(got == NULL) {
				int n = grow_children_array(ent);
				if(unlikely(n < 0)) return n;
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
	struct space *from_space,
	struct space *to_space,
	L4_Fpage_t map_page,
	L4_Word_t dest_addr)
{
	assert(check_mapdb(from_space, 0));
	assert(check_mapdb(to_space, 0));

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
			int n = mapdb_map_pages(from_space, to_space, fp,
				dest_addr + i * GROUP_SIZE);
			if(unlikely(n < 0)) return n;
			given |= n;
		}
		return given;
	}

	/* the "within a single group in @from_space" case. */
	const L4_Word_t first_addr = L4_Address(map_page);
	struct map_group *grp = group_for_addr(from_space, first_addr);
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
			L4_Word_t parent = addr_to_ref(grp, first_addr);
			struct map_group *dstgrp = NULL;
			mapdb_add_map(to_space, &dstgrp, parent, p,
				first->first_page_id + (off >> PAGE_BITS));
			L4_Word_t child = addr_to_ref(dstgrp, L4_Address(p))
				| ((REF_INDEX(parent) >> 1) & 0xf);
			mapdb_add_child(first, child);
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
				/* TODO: this is nigh-equal to the sequence in the non-complex
				 * case. see if they can be merged into an
				 * add_map_and_child().
				 */
				L4_Word_t parent = addr_to_ref(grp,
					L4_Address(ent->range) + src_offs);
				struct map_group *dstgrp = NULL;
				mapdb_add_map(to_space, &dstgrp, parent, p,
					ent->first_page_id + src_offs / PAGE_SIZE);
				L4_Word_t child = addr_to_ref(dstgrp, dp_addr)
					| ((REF_INDEX(parent) >> 1) & 0xf);
				mapdb_add_child(ent, child);
			}

next_entry:
			if(++ent == &grp->entries[MG_N_ENTRIES(grp)]) break;
		}
	}

	assert(check_mapdb(from_space, 0));
	assert(check_mapdb(to_space, 0));

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
 * NOTE: the caller must bump MG_N_ENTRIES(g) as appropriate!
 */
static int insert_empties(
	struct map_group *g,
	int num_to_add,
	struct map_entry **entry)
{
	assert(num_to_add > 0);

	const int e_pos = *entry - g->entries;
	assert(e_pos >= -1 && e_pos < (1 << MG_N_ALLOC_LOG2(g)));

	struct map_entry *e = *entry;
	int need = MG_N_ENTRIES(g) + num_to_add,
		num_tail = MG_N_ENTRIES(g) - e_pos - 1;
	if(need > (1 << MG_N_ALLOC_LOG2(g))) {
		/* make moar RAMz */
		int newlog2 = MG_N_ALLOC_LOG2(g) + 1;
		while((1 << newlog2) < need) newlog2++;
		TRACE("%s: resizing group from %lu to %d entries\n", __func__,
			1ul << MG_N_ALLOC_LOG2(g), 1 << newlog2);
		void *ptr = realloc(g->entries,
			(1 << newlog2) * sizeof(struct map_entry));
		if(unlikely(ptr == NULL)) return -ENOMEM;
		g->entries = ptr;
		g->addr &= ~(0xfUL << 15);
		g->addr |= newlog2 << 15;
		assert(MG_N_ALLOC_LOG2(g) == newlog2);

		e = &g->entries[e_pos];
		*entry = e;
	}
	assert((1 << MG_N_ALLOC_LOG2(g)) >= need);

	if(num_tail > 0) {
		TRACE("%s: move %d items to %d (*entry at %d)\n", __func__, num_tail,
			(e + num_to_add + 1) - g->entries, e - g->entries);
		memmove(e + num_to_add + 1, e + 1,
			sizeof(struct map_entry) * num_tail);
	}

	return 0;
}


/* dereference @from's children, and add the valid ones into entries within
 * @g. it's assumed that @from is a copy of a larger map_entry that was
 * previously in @g->entries, which has since been substituted with smaller
 * entries such as from split_entry().
 *
 * this function is used to move a larger map_entry's numerous children into
 * the smaller, cut-up entries that split_entry() creates.
 *
 * on alloc failure this leaves entries in an uncertain-but-valid state and
 * returns -ENOMEM. idempotent due to mapdb_add_child() being so.
 */
static int distribute_children(struct map_group *g, struct map_entry *from)
{
	if(from->num_children == 0) return 0;

	for(int i=0; i < from->num_children; i++) {
		struct child_ref r;
		if(!deref_child(&r, g, from, i, from->range)) continue;

		L4_Word_t pref_addr = MG_START(g) + REF_ADDR(r.child_entry->parent);
		/* FIXME: this is stupid, isn't it? probe_group_addr() would
		 * suffice.
		 */
		struct map_entry *p_ent = mapdb_probe(g->space, pref_addr);
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
			struct map_entry *ent = fetch_entry(r.group, cut, true);
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
		L4_Word_t child = addr_to_ref(r.group,
			L4_Address(r.child_entry->range))
				| ((REF_INDEX(r.child_entry->parent) >> 1) & 0xf);
		int n = mapdb_add_child(p_ent, child);
		if(unlikely(n == -ENOMEM)) return -ENOMEM;
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
	struct map_group *parent_g = NULL;
	if(REF_DEFINED(saved.parent)) {
		/* dereference the parent group unsafely, since it's always guaranteed
		 * to stay valid.
		 */
		parent_g = (void *)((saved.parent & grp_mask_and) | grp_mask_or);
		assert(parent_g == kmem_id2ptr_safe(map_group_policy,
			REF_GROUP_ID(saved.parent)));

		parent_ent = probe_group_addr(parent_g,
			MG_START(parent_g) + REF_ADDR(saved.parent));
		if(unlikely(parent_ent == NULL)) {
			panic("parent reference was invalid (no entry found)!");
		}
	}
	L4_Word_t addr_offset = 0;
	for(int i=0; i < p; i++) {
		L4_Word_t p_ref = 0;
		assert(REF_DEFINED(saved.parent) == (parent_g != NULL));
		if(likely(parent_g != NULL)) {
			p_ref = addr_to_ref(parent_g,
				MG_START(parent_g) + REF_ADDR(saved.parent) + addr_offset);
		}
		e[i] = (struct map_entry){
			.range = pg_buf[i],
			.parent = p_ref,
			.first_page_id = saved.first_page_id + (addr_offset >> PAGE_BITS),
			.access = saved.access,
			.num_children = 0,
		};
		L4_Set_Rights(&e[i].range, L4_Rights(saved.range));
		TRACE("%s: set ents[%d] = range %#lx:%#lx, parent %#lx, first page %u, rwx %#lx\n",
			__func__, e - g->entries + i, L4_Address(e[i].range), L4_Size(e[i].range),
			e[i].parent, e[i].first_page_id, L4_Rights(e[i].range));
		addr_offset += L4_Size(pg_buf[i]);

		/* add a new child reference to the parent. the way split_entry() is
		 * defined, the old reference will always get overwritten during this
		 * part of the loop, which should update its MISC bits.
		 */
		if(likely(parent_g != NULL)) {
			L4_Word_t child = addr_to_ref(g, L4_Address(e[i].range))
				| ((REF_INDEX(p_ref) >> 1) & 0xf);
			/* FIXME: catch -ENOMEM */
			mapdb_add_child(parent_ent, child);
		}
	}
	assert(MG_N_ENTRIES(g) + p - 1 <= 1024);
	g->addr += p - 1;

	if(saved.num_children > 0) {
		/* FIXME: catch -ENOMEM */
		int n = distribute_children(g, &saved);
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
	struct map_group *g,
	L4_Fpage_t range,
	bool make_exact)
{
	assert(L4_Size(range) >= PAGE_SIZE);

	TRACE("%s: group %#lx, range %#lx:%#lx\n", __func__, MG_START(g),
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
		int err = split_entry(g, e, range);
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


/* moves children of @e (which is inside @g) to be children of the entry in
 * @e->parent.
 *
 * returns 0 on success, or -ENOMEM on failure, leaving a partially-completed
 * state behind. calling the function again with the exact same parameters
 * will resume the operation and complete into the same final state.
 */
static int reparent_children(struct map_group *g, struct map_entry *e)
{
	if(e->num_children == 0) return 0;

	struct map_group *parent_g = NULL;
	struct map_entry *parent_entry = NULL;
	int p_offs = 0;
	if(likely(REF_DEFINED(e->parent))) {
		parent_g = (void *)((e->parent & grp_mask_and) | grp_mask_or);
		assert(parent_g == kmem_id2ptr_safe(map_group_policy,
			REF_GROUP_ID(e->parent)));
		parent_entry = probe_group_addr(parent_g,
			MG_START(parent_g) + REF_ADDR(e->parent));
		BUG_ON(parent_entry == NULL,
			"parent ref=(%#lx, %#lx, %#lx) must be valid",
			REF_INDEX(e->parent), REF_GROUP_BITS(e->parent),
			REF_MISC(e->parent));
		p_offs = L4_Address(parent_entry->range) - MG_START(parent_g);
	}

	L4_Word_t *cs = e->num_children == 1 ? &e->child : e->children;
	for(int i=0; i < e->num_children; i++) {
		struct child_ref cr;
		if(!deref_child(&cr, g, e, i, e->range)) continue;

		if(parent_g != NULL) {
			/* compute cr.child_entry's location in parent_g from @e's
			 * position in parent_entry.
			 */
			int child_in_e = REF_ADDR(cr.child_entry->parent)
					- (L4_Address(e->range) - MG_START(g)),
				e_in_parent = REF_ADDR(e->parent) - p_offs,
				child_offs = child_in_e + e_in_parent;
			assert(child_in_e >= 0 && e_in_parent >= 0);
			assert(cr.child_entry->first_page_id
				== parent_entry->first_page_id + child_offs / PAGE_SIZE);

			L4_Word_t parent = addr_to_ref(parent_g,
					L4_Address(parent_entry->range) + child_offs),
				child = addr_to_ref(cr.group,
						L4_Address(cr.child_entry->range))
					| ((REF_INDEX(parent) >> 1) & 0xf);
			int n = mapdb_add_child(parent_entry, child);
			if(unlikely(n < 0)) {
				/* idempotent after re-do due to the child-overwriting part.
				 * therefore it's OK to simply return on failed malloc.
				 */
				return n;
			}

			/* make the new ref look alive in deref_child(). */
			cr.child_entry->parent = parent;
		} else {
			/* @e is a toplevel mapping. */
			TRACE("%s: detached second-level mapping %#lx:%#lx in grp %#lx\n",
				__func__, L4_Address(cr.child_entry->range),
				L4_Size(cr.child_entry->range),
				(L4_Word_t)cr.group & grp_mask_and);
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
		bool upper;
		uint32_t pgid = pt_probe(mod_it, &upper, NULL, a);
		if(unlikely(!upper)) a = (a + PT_UPPER_SIZE) & ~PT_UPPER_MASK;
		else {
			pt_set_page(mod_it, a, pgid, L4_Rights(range));
			a += PAGE_SIZE;
		}
	}
}


/* the core of mapdb_unmap_fpage(). this avoids a hashtable lookup for every
 * recursion, instead using the group pointer from a prior deref_child()
 * operation. @eff_range is supplied to avoid a modifying fetch_entry() in
 * non-flushing Unmap into a larger entry; it restricts child matches and
 * access reading.
 *
 * *@e_p will retain its value's offset in @g->entries.
 */
static int unmap_entry_in_group(
	struct map_group *g,
	struct map_entry **e_p,
	int mode,
	L4_Fpage_t eff_range)
{
	struct map_entry *e = *e_p;
	assert(GROUP_ADDR(L4_Address(e->range)) == MG_START(g));
	assert(fpage_overlap(eff_range, e->range));
	assert(L4_SizeLog2(eff_range) <= L4_SizeLog2(e->range));

	const bool get_access = CHECK_FLAG(mode, UM_GET_ACCESS),
		recursive = CHECK_FLAG(mode, UM_RECURSIVE),
		immediate = CHECK_FLAG(mode, UM_IMMEDIATE),
		drop_special = CHECK_FLAG(mode, UM_DROP_SPECIAL);

	struct pt_iter mod_it;
	pt_iter_init_group(&mod_it, g);

	int unmap_rights = (mode & UM_RIGHTS) >> 4;
	assert(unmap_rights != L4_NoAccess || get_access);

	TRACE("%s: g=%#lx (%p), e=%#lx:%#lx (eff=%#lx:%#lx),\n"
		"    immediate=%s, recursive=%s, get_access=%s, drop_special=%s\n",
		__func__, MG_START(g), g, L4_Address(e->range), L4_Size(e->range),
		L4_Address(eff_range), L4_Size(eff_range), btos(immediate),
		btos(recursive), btos(get_access), btos(drop_special));

	int rwx_seen = 0;
	if(get_access && !REF_IS_SPECIAL(e->parent)) {
		/* check each native page in e->range.
		 *
		 * TODO: move access fetching into a proper function.
		 * TODO: ... and implement the side/up access buffer design
		 * TODO: ... then reconcile @eff_range with the other pages' access
		 * bits in a smaller @e->range
		 */
		L4_Word_t r_pos = L4_Address(eff_range),
			r_last = r_pos + L4_Size(eff_range);
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

	/* dereference children and recur.
	 *
	 * TODO: depending on the mapping structure, this can end up generating a
	 * potentially infinite series of invocation frames in kernel space,
	 * overflowing the measly one-page stack. so instead it should use a
	 * non-recursive breadth-first algorithm.
	 */
	int next_mode = (mode & ~UM_DROP_SPECIAL) | UM_IMMEDIATE;
	for(int i=0; recursive && i < e->num_children; i++) {
		struct child_ref r;
		if(!deref_child(&r, g, e, i, eff_range)) {
			if(e->num_children < 2) {
				e->child = 0;
				break;
			} else {
				continue;
			}
		}

		TRACE("deref child %d (%#lx) -> %#lx:%#lx\n",
			i, e->num_children < 2 ? e->child : e->children[i],
			L4_Address(r.child_entry->range),
			L4_Size(r.child_entry->range));

		L4_Word_t paddr = MG_START(g) + REF_ADDR(r.child_entry->parent);
		if(!ADDR_IN_FPAGE(eff_range, paddr)) continue;
		int rm_rights = L4_Rights(r.child_entry->range) & unmap_rights;
		if(rm_rights == 0) continue;

		int n = unmap_entry_in_group(r.group, &r.child_entry, next_mode,
			r.child_entry->range);
		if(unlikely(n < 0)) {
			assert(n == -ENOMEM);
			rwx_seen = n;
			goto end;
		}
		rwx_seen |= n;
	}

	const bool modify = immediate && unmap_rights != 0,
		special = REF_IS_SPECIAL(e->parent);
	bool drop = special && drop_special;	/* else-case */
	if(modify && !special) {
		assert(!drop);
		int old_rights = L4_Rights(e->range),
			new_rights = old_rights & ~unmap_rights;
		L4_Set_Rights(&e->range, new_rights);
		if(new_rights == 0) drop = true;
		else if(new_rights < old_rights) {
			set_pt_range_rights(&mod_it, e->range);
		}
	}
	if(drop) {
		assert(modify);		/* fails on UM_DROP_SPECIAL mis-spec */
		TRACE("%s: removing entry %#lx:%#lx\n", __func__,
			L4_Address(e->range), L4_Size(e->range));
		int n = reparent_children(g, e);
		if(unlikely(n < 0)) {
			/* ENOMEM can happen because children must be added to a
			 * parent entry, which may return ENOMEM on hash resize.
			 *
			 * FIXME: this is HIGHLY INSUFFICIENT because of changes
			 * made earlier in this function. the function should be
			 * split into a children-gathering stage and three
			 * functional stages, each invoked according to its
			 * corresponding bool parameter.
			 */
			assert(n == -ENOMEM);
			rwx_seen = n;
			goto end;
		}

		clear_pt_range(&mod_it, e->range);
		e = erase_map_entry(g, e);
		e--;	/* counteract the caller's loop bump. */
		*e_p = e;
	} else {
		e->access = get_access ? 0 : rwx_seen;
	}

end:
	pt_iter_destroy(&mod_it);
	return rwx_seen;
}


int mapdb_unmap_fpage(
	struct space *sp,
	L4_Fpage_t range,
	bool immediate,
	bool recursive,
	bool get_access)
{
	assert(recursive || immediate);	/* disallows the one-level status read */
	assert(check_mapdb(sp, 0));

	TRACE("%s: range %#lx:%#lx, %simmediate, %srecursive, get_access=%s\n",
		__func__, L4_Address(range), L4_Size(range),
		!immediate ? "non-" : "", !recursive ? "non-" : "",
		btos(get_access));

	L4_Word_t unmap_rights = L4_Rights(range);
	if(unmap_rights == L4_NoAccess && !get_access) return 0;	/* no-op */

	/* slightly bad, but much like mapdb_map_pages(), it's the only reasonable
	 * thing to do.
	 */
	int n_groups = L4_Size(range) / GROUP_SIZE;
	if(unlikely(n_groups > 1)) {
		int acc = 0;
		for(int i=0; i < n_groups; i++) {
			L4_Fpage_t fp = L4_Fpage(
				L4_Address(range) + i * GROUP_SIZE, GROUP_SIZE);
			L4_Set_Rights(&fp, L4_Rights(range));
			int n = mapdb_unmap_fpage(sp, fp, immediate, recursive,
				get_access);
			if(unlikely(n < 0)) return n;
			acc |= n;
		}
		return acc;
	}

	struct map_group *g = group_for_addr(sp, L4_Address(range));
	if(g == NULL) {
		/* invisible steering wheel!!! */
		return 0;
	}

	/* this function will only modify the map_group it accesses when it might
	 * revoke access in the immediate space. this is passed to fetch_entry().
	 * to contrast, unmap_entry_in_group() always receives immediate entries
	 * as they are and never calls fetch_entry(..., true).
	 */
	const bool modify = immediate && unmap_rights != 0;
	struct map_entry *e = fetch_entry(g, range, modify);
	if(e == NULL && modify) {
		/* distinguish between not-exist and ENOMEM from split_entry(). */
		if(probe_group_range(g, range) != NULL) {
			/* FIXME: implement restartable suspending somewhere along
			 * this function's call chain.
			 */
			panic("malloc() failed in fetch_entry()");
		}
	}
	if(e == NULL) {
		/* empty ranges are empty. */
		return 0;
	}

	/* the "affect special ranges" form */
	const bool drop_special = !recursive
		&& L4_Rights(range) == L4_FullyAccessible
		&& (range.raw & 0xc00) == 0x800;
	assert(drop_special || (L4_Address(range) & 0xc00) == 0);

	int rwx_seen = 0, mode = L4_Rights(range) << 4;
	if(immediate) mode |= UM_IMMEDIATE;
	if(recursive) mode |= UM_RECURSIVE;
	if(get_access) mode |= UM_GET_ACCESS;
	if(drop_special) mode |= UM_DROP_SPECIAL;
	const L4_Word_t r_end = FPAGE_HIGH(range);
	do {
		if(likely(!REF_IS_SPECIAL(e->parent)) || drop_special) {
			int n = unmap_entry_in_group(g, &e, mode,
				L4_SizeLog2(range) < L4_SizeLog2(e->range) ? range : e->range);
			if(unlikely(n < 0)) return n;
			rwx_seen |= n;
		}
	} while(++e < &g->entries[MG_N_ENTRIES(g)] && L4_Address(e->range) < r_end);

	dump_map_group(g);
	assert(unmap_rights == 0 || check_mapdb(sp, 0));
	return rwx_seen;
}


struct map_entry *mapdb_probe(struct space *sp, uintptr_t addr)
{
	struct map_group *g = group_for_addr(sp, addr);
	if(g == NULL) return NULL;

	assert(addr >= MG_START(g));
	assert(addr < MG_START(g) + GROUP_SIZE);
	return probe_group_addr(g, addr);
}


int mapdb_fill_page_table(struct space *sp, uintptr_t addr)
{
	addr &= ~PT_UPPER_MASK;

	assert(MAX_ENTRIES_PER_GROUP == 1 << PT_UPPER_WIDTH);
	struct map_group *g = group_for_addr(sp, addr);
	if(g == NULL) return 0;

	if(g->ptab_page == NULL) {
		int n = x86_alloc_ptab(g);
		if(n < 0) return n;
	}

	int n_set = 0;
	struct pt_iter it;
	pt_iter_init_group(&it, g);
	assert(pt_upper_present(&it, addr));
	for(int i=0; i < MG_N_ENTRIES(g); i++) {
		struct map_entry *e = &g->entries[i];
		int n_pages = L4_Size(e->range) / PAGE_SIZE;
		for(int j=0; j < n_pages; j++) {
			pt_set_page(&it, L4_Address(e->range) + j * PAGE_SIZE,
				e->first_page_id + j, L4_Rights(e->range));
			n_set++;
		}
	}
	pt_iter_destroy(&it);

	return n_set;
}


COLD void init_mapdb(void)
{
	/* TODO: add a proper interface for enabling/disabling trace IDs.
	 * kernel commandline perhaps?
	 */
#if 0
	static bool first = true;
	if(first) {
		first = false;
		// trace_enable(TRID_MAPDB);
		// trace_enable(TRID_IPC);
		// trace_enable(TRID_SCHED);
	}
#endif

	/* 18 bits for the group ID + 4 bits for its content = 8 MiB of address
	 * space.
	 *
	 * this gives up to 2^18 = 512K map_groups, enough to track ~680 address
	 * spaces that populate at least one page for every 4M of space up to the
	 * 3G mark, or to give 16k concurrent address spaces 128M of active range
	 * at once.
	 */
	struct slab_policy *pol = kmem_policy_align(1 << 22,
		sizeof(struct map_group));
	map_group_slab = kmem_cache_create("map_group_slab",
		sizeof(struct map_group), 1 << size_to_shift(sizeof(struct map_group)),
		KMEM_POLICY, NULL, NULL);
	kmem_cache_set_policy(map_group_slab, pol);
	kmem_get_align_masks(pol, &grp_mask_and, &grp_mask_or);
	assert(POPCOUNT(grp_mask_and) == 18);
	grp_mask_shift = ffsl(grp_mask_and) - 1;
	map_group_policy = pol;
}


/* FIXME: change the interface to this function. passing a group of page_ids
 * is stupid because of the decode op down there.
 *
 * FIXME: there's also likely a good division of labour between
 * mapdb_init_range() and mapdb_add_map(), somewhere.
 */
COLD void mapdb_init_range(
	struct space *sp,
	uintptr_t start_addr,
	const uint32_t *page_ids,
	unsigned int num_pages,
	int entry_flags)
{
	assert(check_mapdb(sp, 0));

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
			/* - then pass those one by one to mapdb_add_map(db, fpage,
			 *   first_id, [rwx]); this creates groups and entries.
			 */
			L4_Fpage_t page = L4_FpageLog2(range_pos, mag);
			assert((range_pos & (L4_Size(page) - 1)) == 0);
			L4_Set_Rights(&page, entry_flags & L4_FullyAccessible);
			int n = mapdb_add_map(sp, NULL, 0, page, page_ids[id_offset]);
			if(n < 0) {
				printf("!!! n=%d\n", n);
				panic("mapdb_init_range() [early boot call] failed");
			}

			range_pos += 1 << mag;
			done += 1 << (mag - 12);
		}
	}

	assert(done == num_pages);
	assert(check_mapdb(sp, 0));
}
