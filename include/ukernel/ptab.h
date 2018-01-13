
/* somewhat abstracted access to page table structures. the microkernel will
 * only support exactly one page table format, selected at compile time.
 *
 * prototypes are "static inline" because of dirty preprocessor tricks; see
 * pt_i386.c for example.
 */

#ifndef SEEN_UKERNEL_PTAB_H
#define SEEN_UKERNEL_PTAB_H

#include <stdint.h>
#include <stdbool.h>


struct space;

/* the following constants are defined per page table format:
 *
 * PT_UPPER_BITS, PT_UPPER_SIZE, PT_UPPER_MASK, PT_UPPER_WIDTH  (L4_Word_t)
 *   the size and # of bits occupied by the upper-level table entry, i.e. the
 *   one that points to the lowest-level tables. mask includes all lower-order
 *   bits. each page in the upper table contains 1 << width entries.
 */


/* iteration context. retains metadata pointers for pagetable pages, or
 * address space reservations, or whatever. these are connected to a
 * particular address space and must not be retained across any space_*()
 * call.
 */
struct pt_iter;


static inline void pt_iter_init(struct pt_iter *iter, struct space *sp);
static inline void pt_iter_init_group(
	struct pt_iter *iter, struct map_group *grp);
static inline void pt_iter_destroy(struct pt_iter *iter);

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
static inline uint32_t pt_probe(
	struct pt_iter *iter,
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
static inline bool pt_set_page(
	struct pt_iter *iter,
	uintptr_t addr, uint32_t pgid, int rights);

/* change rights of a page table entry. retains "side", frame number, and
 * access bits.
 */
static inline void pt_set_rights(
	struct pt_iter *iter, uintptr_t addr, int rights);

/* clears a page table entry, clearing every field. */
static inline void pt_clear_page(struct pt_iter *it, uintptr_t addr);

static inline bool pt_page_present(struct pt_iter *iter, uintptr_t addr);


/* TODO: switch according to pagetable format. */
#undef IN_PTAB_IMPL
#define IN_PTAB_IMPL 1
#include "pt_i386.c"
#undef IN_PTAB_IMPL


#endif
