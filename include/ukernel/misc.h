/* quasi-runtime functions for the microkernel.
 *
 * TODO: move the utility things into <ukernel/util.h>. this header should be
 * for miscellaneous kernel exports, and not be included from e.g. sigma0 or
 * mbiloader.
 */

#ifndef SEEN_UKERNEL_MISC_H
#define SEEN_UKERNEL_MISC_H

#include <stdint.h>
#include <ccan/compiler/compiler.h>

#include <l4/types.h>

/* TODO: remove this at some point. */
#include <ukernel/util.h>


/* from kmain.c */

extern uint8_t syscall_stack[];
/* should only be read with interrupts disabled! */
extern uint64_t *global_timer_count;

extern void NORETURN panic(const char *message);
extern uint64_t read_global_timer(void);

#endif
