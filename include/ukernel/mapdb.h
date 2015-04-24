#ifndef SEEN_UKERNEL_MAPDB_H
#define SEEN_UKERNEL_MAPDB_H

#include <stdint.h>
#include <assert.h>

#include <l4/types.h>
#include <ukernel/mm.h>


/* TODO: change this per pagetable format, i.e. derive it from constants in
 * <ukernel/ptab.h> .
 */
#define MAX_ENTRIES_PER_GROUP 1024	/* 4 MiB in 4 KiB pages */

/* map_group->addr accessors. note that MG_FLAGS() doesn't shift. */
#define MG_START(grp) ((grp)->addr & ~(PAGE_SIZE * MAX_ENTRIES_PER_GROUP - 1))
#define MG_N_ALLOC_LOG2(grp) (((grp)->addr >> 15) & 0xf)
#define MG_N_ENTRIES(grp) (int)((grp)->addr & 0x7fff)
#define MG_FLAGS(grp) ((grp)->addr & 0x380000)

/* MG_FLAGS() bits. */
#define MGF_ROOT	0x080000	/* kernel_space only; no entries present. */
#define MGF__SPARE1	0x100000
#define MGF__SPARE2	0x200000


struct space;


struct map_entry
{
	/* @range == L4_nilpage is used to indicate an empty slot in map_group.
	 *
	 * an entry's @range is always at most as large as its parent's entry.
	 * toplevel entries' size is limited by group size alone.
	 */
	L4_Fpage_t range;		/* incl. L4 permission bits */
	uint32_t first_page_id;

	/* special values:
	 *   - when !defined, there is no parent. this appears at boot in sigma0,
	 *   and entries whose immediate last parent has been granted away (e.g.
	 *   as granted by sigma0)
	 *   - when misc & 2, the entry represents a kernel special range such as
	 *   the UTCB or KIP area, and is not subject to unmapping, access
	 *   querying, mapping on top of, or map/granting out of.
	 */
	L4_Word_t parent;

	uint16_t access;	/* temporarily wack (FIXME) */

	/* there may be fewer actual children of this map_entry; num_children only
	 * tracks whether "child" or "children" is to be used, and how many words
	 * are allocated under "children" when num_children > 1.
	 *
	 * empties may show up as a !defined ref, or a reference to a map_group
	 * that either doesn't exist, isn't valid, has no map_entry at that index,
	 * or the mapping doesn't reference the correct physical page in this
	 * map_entry. invalid children will be replaced with a tombstone at
	 * dereference.
	 *
	 * "children" is also used to verify that the previous mapping was made
	 * from this entry at this address, as required by the L4.X2 criteria on
	 * when a map/grant extends the privileges of an existing mapping.
	 */
	uint16_t num_children;	/* (at most 4096; upper 4 bits redundant) */
	union {
		L4_Word_t child;		/* when num_children <= 1 */
		L4_Word_t *children;	/* otherwise (malloc()'d) */
	};
};


struct map_group
{
	/* `addr' encodes the group's start address at its architecture-dependent
	 * size. the 21/22 lowest bits contain, high to low: flags [3 bits],
	 * n_alloc_log2 [4 bits], and n_entries [15 bits]. use MG_START(),
	 * MG_N_ALLOC_LOG2(), MG_N_ENTRIES(), and MG_FLAGS() to extract the named
	 * component.
	 *
	 * XXX: on x86, this encoding leaves 5 or 6 of the high bits of
	 * `n_entries' for other uses.
	 *
	 * `n_entries' gives the number of valid entries under `entries'.
	 * 1 << n_alloc_log2 gives the amount of space under `entries' iff
	 * n_alloc_log2 > 0.
	 *
	 * n_alloc_log2 == 0 is not valid.
	 *
	 * 1 << n_alloc_log2 <= MAX_ENTRIES_PER_GROUP.
	 *
	 * TODO: move these into an invariant check in mapdb.c .
	 */
	L4_Word_t addr;
	struct map_entry *entries;
	struct page *ptab_page;
	struct space *space;
};


extern struct kmem_cache *map_group_slab;	/* for space_finalize_kernel() */

extern void init_mapdb(void);

extern void mapdb_init(struct space *sp);
extern void mapdb_destroy(struct space *sp);

/* on success, returns OR mask of rights that would've been granted by this
 * mapping operation (which doesn't happen in the rights extension case, but
 * is ignored). special ranges may not be mapped from, or over, using this
 * function; instead, such maps are ignored and the return value set to 0.
 *
 * on failure, returns negative errno.
 */
extern int mapdb_map_pages(
	struct space *from,
	struct space *to,
	L4_Fpage_t src_page,
	L4_Word_t dest_addr);

/* revokes access rights for bits given and in the mappings covered by @fpage.
 * if a covered mapping ends up with no rights at all, it is removed and its
 * children are moved to its parent (if any remain after recursion).
 *
 * if @immediate is true, the mappings in @db covered by @fpage will be
 * affected. thus triples of (@immediate, @recursive, @get_access) correspond
 * to L4.X2 unmap operations like this:
 *   (true, true, true) = flush
 *   (true, false, false) = grant
 *   (false, true, true) = unmap
 *   (false, false, _) = one-level access read (not used)
 *   (true, true, false) = flush w/o access read (not used)
 *   (true, false, true) = grant w/ access read (nonsensical)
 *   (false, true, false) = unmap w/o access read (not used)
 *
 * returns the inclusive mask of access bits of the physical pages actually
 * covered by @fpage since the last call to mapdb_unmap_fpage(). if @recursive
 * is set, this also covers child mappings' access bits. destructive
 * examination of page tables is compensated by storing access bits in struct
 * map_entry as appropriate to recursion. (see comment above the field
 * declaration.)
 *
 * special mappings (KIP, UTCB pages) aren't affected by mapdb_unmap_fpage()
 * in its regular forms. however, when (@fpage.address & 0xc00) == 0x800 &&
 * !@recursive && @fpage.rights == full, special mappings will be removed if
 * they're contained within @fpage, and fragmented if they are larger.
 * regardless of this setting, the access bits of special mappings don't
 * contribute to the return value. when the special form isn't used, the
 * denormal bits in @fpage.address must be zero.
 *
 * if @get_access is set, stored access bits will be recursively retrieved
 * for @fpage and cleared in @db's corresponding entries. if not, access bits
 * will be neither examined nor altered.
 *
 * notably, !@get_access && @fpage.rights == 0 is a no-op.
 */
extern int mapdb_unmap_fpage(
	struct space *db,
	L4_Fpage_t fpage,
	bool immediate,
	bool recursive,
	bool get_access);

static inline void mapdb_erase_special(struct space *db, L4_Fpage_t fpage) {
	fpage = L4_FpageLog2(L4_Address(fpage) | 0x800, L4_SizeLog2(fpage));
	L4_Set_Rights(&fpage, L4_FullyAccessible);
	mapdb_unmap_fpage(db, fpage, true, false, false);
}


/* get-cmp function for struct space's ptab_groups. */
static inline bool cmp_group_addr(const void *cand, void *keyptr) {
	const struct map_group *g = cand;
	return MG_START(g) == *(uint32_t *)keyptr;
}


/* access from the pagefault handler. returns NULL when the entry doesn't
 * exist.
 */
extern struct map_entry *mapdb_probe(struct space *db, uintptr_t addr);

/* map_entry accessor */
static inline uint32_t mapdb_page_id_in_entry(
	const struct map_entry *m,
	uintptr_t addr)
{
	assert(!L4_IsNilFpage(m->range));
	return m->first_page_id + ((addr - L4_Address(m->range)) >> PAGE_BITS);
}

/* writes 1 << PT_UPPER_WIDTH entries into the page table using pt_*()
 * primitives. doesn't alter positions that aren't present. returns number of
 * last-level table entries written.
 */
extern int mapdb_fill_page_table(struct space *db, uintptr_t addr);


/* add mapping as indicated for @fpage, referencing @parent. this is a
 * low-level call that can only be used safely from outside of mapdb.c by
 * setting @parent to 2 or 0 (special and root mappings, respectively);
 * anything else will introduce an invalid parent reference and blow the
 * invariants.
 *
 * completely unatomic on out-of-memory; the caller is supposed to suspend and
 * re-start once malloc has a chance of succeeding.
 *
 * L4_Size(@fpage) may be at most GROUP_SIZE.
 *
 * *@fpage_group_p will be filled in with the map_group pointer for @fpage in
 * @sp iff @fpage_group_p != NULL.
 *
 * returns either a mask of rights given (on any component of @fpage), or
 * -ENOMEM. (FIXME: actually always returns 0. no idea what this comment was.)
 */
extern int mapdb_add_map(
	struct space *sp,
	struct map_group **fpage_group_p,
	L4_Word_t parent,		/* 0 for immutables */
	L4_Fpage_t fpage,
	uint32_t first_page_id);


/* kernel-mode initialization */
extern void mapdb_init_range(
	struct space *ptr,
	uintptr_t start_addr,
	const uint32_t *page_ids,
	unsigned int num_pages,
	int entry_flags);		/* L4 Fpage access bits */


#endif
