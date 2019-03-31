#ifndef SEEN_UKERNEL_MAPDB_H
#define SEEN_UKERNEL_MAPDB_H

#include <stdint.h>
#include <assert.h>

#include <l4/types.h>
#include <ukernel/mm.h>
#include <ukernel/rangealloc.h>


/* TODO: change this per pagetable format, i.e. derive it from constants in
 * <ukernel/ptab.h> .
 */
#define PAGES_PER_GROUP 1024	/* 4 MiB in 4 KiB pages */
#define GROUP_SIZE (PAGE_SIZE * PAGES_PER_GROUP)
#define GROUP_MASK (GROUP_SIZE - 1)

/* map_group->addr accessors. note that MG_FLAGS() doesn't shift. */
#define MG_START(grp) ((grp)->addr & ~GROUP_MASK)
#define MG_N_MAPS(grp) (((grp)->addr & 0x3ff) + 1)
#define MG_N_ALLOC_LOG2(grp) (((grp)->addr >> 10) & 0x1f)

/* @mode flags for mapdb_unmap(). */
#define UM_IMMEDIATE	0x1
#define UM_RECURSIVE	0x2
#define UM_GET_ACCESS	0x4


struct space;


/* per-space tracking structure corresponding 1:1 to the 2M/4M last-level page
 * table. page frame IDs are stored in the page table.
 *
 * "side" access bits are stored in the 3 ignored bits of 32-bit/PAE page
 * table entries. see pt_probe() spec in <ukernel/ptab.h> about that.
 */
struct map_group
{
	/* the group's vm address is addr & ~GROUP_MASK. lowest 10 bits are the
	 * number of active maps in this group minus one. bits 15..11 are
	 * n_alloc_log2. bits 20..16 left spare. use MG_START(), MG_N_MAPS(), and
	 * MG_N_ALLOC_LOG2() to extract the named field.
	 *
	 * n_alloc_log2 == 0 indicates that the group has no child references, and
	 * that `children' and `c_aux' are invalid.
	 */
	L4_Word_t addr;

	struct page *ptab_page;
	struct space *space;

	uint32_t *parent;	/* per map; [GROUP_SIZE / PAGE_SIZE]. */

	/* child reference storage. these have length 1 << MG_N_ALLOC_LOG2(grp)
	 * and are allocated consecutively, with the malloc() return value used
	 * for `children'. see mapdb.c for details of format etc.
	 */
	uint32_t *children;
	uint16_t *c_aux;
};


extern struct rangealloc *map_group_ra;		/* for space_finalize_kernel() */

extern void init_mapdb(void);

extern void mapdb_init(struct space *sp);
extern void mapdb_destroy(struct space *sp);

/* returns 0 if a map_group can't be found for @address, -ENOENT if the map's
 * parent group can't be found, or a positive rights mask otherwise. fills
 * *@pgid_p with the page frame number when not NULL. from non-mapdb.c
 * callers, @g must be NULL.
 */
extern int mapdb_query(
	uint32_t *pgid_p,
	struct space *sp, struct map_group *g, uintptr_t address);

/* map page rights covered by @src_page in @from into @to at @dest_addr.
 * @dest_addr must be aligned to @src_page.size .
 *
 * for holes in @src_page, the corresponding mapping in the destination range
 * is removed with the same semantics as in the non-hole case. exempt pages
 * may not be mapped from, or over, using this function; instead, maps
 * involving exempt pages are ignored 0 is returned.
 *
 * on success, returns the inclusive mask of rights granted from component
 * pages of @src_page in @from. on failure, returns negative errno.
 */
extern int mapdb_map(
	struct space *from, L4_Fpage_t src_page,
	struct space *to, L4_Word_t dest_addr);

/* revokes access rights for bits given and in the pages covered by @fpage.
 * if a page ends up with no rights at all, it is removed and its children are
 * moved to its parent (if any remain after recursion).
 *
 * the @mode parameter's immediate, recursive, and get_access flags correspond
 * to L4.X2 unmap variations like this:
 *   (true, true, true) = flush
 *   (true, false, false) = grant
 *   (false, true, true) = unmap
 *   (false, false, _) = one-level access read (not used)
 *   (true, true, false) = flush w/o access read (not used)
 *   (true, false, true) = grant w/ access read (nonsensical)
 *   (false, true, false) = unmap w/o access read (not used)
 *
 * returns the inclusive mask of access bits of the physical pages actually
 * covered by @fpage since the last call to mapdb_unmap() iff access querying
 * was requested in @mode. if recursion is enabled, this includes child
 * mappings' access bits. destructive examination of page tables is
 * compensated by storing access bits in mapdb structures as appropriate to
 * recursion.
 *
 * exempt pages (KIP, UTCB pages) aren't affected by mapdb_unmap() in its
 * regular forms. however, when (@fpage.address & 0xc00) == 0x800 &&
 * !recursive && @fpage.rights == full, exempt pages within @fpage will be
 * removed. regardless of this setting, the access bits of exempt pages don't
 * contribute to the return value. when the special form isn't used, the
 * denormal bits in @fpage.address must be zero.
 */
extern int mapdb_unmap(struct space *db, L4_Fpage_t fpage, unsigned mode);

/* special interface for removing KIP, UTCB, and other exempt pages. */
static inline int mapdb_erase_exempt(struct space *sp, L4_Fpage_t fpage)
{
	fpage = L4_FpageLog2(L4_Address(fpage) | 0x800, L4_SizeLog2(fpage));
	L4_Set_Rights(&fpage, L4_FullyAccessible);
	return mapdb_unmap(sp, fpage, UM_IMMEDIATE);
}


/* ptab_groups lookup assist for ptab_*.c and space.c . */
extern struct map_group *find_group(struct space *sp, uintptr_t addr);


/* add parentless maps covering @fpage in @sp. this is used for adding memory
 * to sigma0 on bootup, and for placing UTCB, KIP, and other exempt pages in
 * an address space.
 *
 * L4_Size(@fpage) may be at most GROUP_SIZE. @is_exempt should be true for
 * pages that mapdb will ignore when mapping out of, and will refuse to unmap,
 * flush, or overwrite; and false for ordinary root maps.
 *
 * returns negative errno on failure, idempotent on retry; or nonnegative on
 * success.
 */
extern int mapdb_put(
	struct space *sp, L4_Fpage_t fpage, uint32_t first_page_id,
	bool is_exempt);

#endif
