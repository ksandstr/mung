
/* somewhat abstracted access to page table structures. the microkernel will
 * only support exactly one page table format which is selected at compile
 * time.
 *
 * prototypes are "static inline" because everything here is implemented
 * inline in the per-format source (e.g. pt_i386.c).
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
 * particular address space, and must not be retained across any space_*()
 * call.
 */
struct pt_iter;


static inline void pt_iter_init(struct pt_iter *iter, struct space *sp);
static inline void pt_iter_destroy(struct pt_iter *iter);

/* read a single page ID at a given address. returns 0 when not present. */
static inline uint32_t pt_get_pgid(
	struct pt_iter *iter,
	bool *upper_present_p,		/* optional */
	uintptr_t addr);

/* test whether an upper-level directory entry is present. */
static inline bool pt_upper_present(
	const struct pt_iter *iter,
	uintptr_t addr);


/* TODO: switch according to pagetable format. */
#undef IN_PTAB_IMPL
#define IN_PTAB_IMPL 1
#include "pt_i386.c"
#undef IN_PTAB_IMPL


#endif
