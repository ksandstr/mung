
/* somewhat abstracted access to page table structures. the microkernel will
 * only support exactly one page table format, selected at compile time.
 */

#ifndef SEEN_UKERNEL_PTAB_H
#define SEEN_UKERNEL_PTAB_H

#include <stdint.h>
#include <stdbool.h>
#include <alloca.h>


struct space;
struct map_group;

/* the following constants are defined per page table format:
 *
 * PT_UPPER_BITS, PT_UPPER_SIZE, PT_UPPER_MASK, PT_UPPER_WIDTH  (L4_Word_t)
 *   the size and # of bits occupied by the upper-level table entry, i.e. the
 *   one that points to the lowest-level tables. mask includes all lower-order
 *   bits. each page in the upper table contains 1 << width entries.
 *
 * FIXME: consider replacing each of those with something cleaner.
 */

#define PT_UPPER_WIDTH 10
#define PT_UPPER_BITS 22
#define PT_UPPER_SIZE (1 << PT_UPPER_BITS)
#define PT_UPPER_MASK (PT_UPPER_SIZE - 1)


/* iteration context for efficient querying in loops. must be provided for
 * even one-off callsites, but see PTI_SPACE() and PTI_GROUP(). invalidated by
 * any call to space.c .
 */
typedef union pt_iter_u {
	struct map_group *grp;
	struct space *sp;
} pt_iter_t;


/* initialization. ..._group() dereferences @grp to get its first page table
 * pointer. there is no destructor.
 */
extern void pt_iter_init(pt_iter_t *it, struct space *sp);
extern void pt_iter_init_group(pt_iter_t *it, struct map_group *grp);

/* temporary iterators backed with alloca(), giving purists something to cry
 * about.
 */
#define PTI_SPACE(sp) \
	({ pt_iter_t *__it = alloca(sizeof *__it); \
	   pt_iter_init(__it, (sp)); \
	   __it; })
#define PTI_GROUP(grp) \
	({ pt_iter_t *__it = alloca(sizeof *__it); \
	   pt_iter_init_group(__it, (grp)); \
	   __it; })

extern struct space *pti_space(const pt_iter_t *iter);

/* examine a pagetable entry for a given address. returns 0 when not present.
 * if there's a chance that physical page #0 would actually become mapped at
 * this address, the caller should disambiguate with pt_page_present().
 *
 * if @access_p != NULL, the access mask is copied and cleared according to
 * @is_up. if @is_up is true, the access mask and @below will be OR'd into the
 * "side" bits in each PTE; otherwise, "side" bits will be stored in
 * *@side_access_p and also cleared.
 *
 * if @access_p == NULL, any access masks won't be affected.
 */
extern uint32_t pt_probe(
	pt_iter_t *it,
	int *access_p, int *side_access_p,
	uintptr_t addr, bool is_up, int below);

/* write a pagetable entry. rights mask per L4_Rights(). clears "side".
 *
 * NOTE: this cannot be used to clear a page table entry; use pt_clear_page
 * instead.
 *
 * returns false when accessing an address where the page table cannot be
 * found, and true otherwise. a looping caller may then skip to (addr +
 * PT_UPPER_SIZE) & ~PT_UPPER_MASK, i.e. to the start of the next upper-level
 * structure, with equivalent results.
 */
extern bool pt_set_page(
	pt_iter_t *it,
	uintptr_t addr, uint32_t pgid, int rights);

/* change rights of a page table entry. retains "side", frame number, and
 * access bits.
 */
extern void pt_set_rights(pt_iter_t *it, uintptr_t addr, int rights);

/* clears a page table entry, clearing every field. */
extern void pt_clear_page(pt_iter_t *it, uintptr_t addr);

extern bool pt_page_present(pt_iter_t *it, uintptr_t addr);


#endif
