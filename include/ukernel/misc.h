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


#define SYSCALL __attribute__((regparm(3)))


struct thread;


/* from kmain.c */

extern uint8_t syscall_stack[];
/* should only be read with interrupts disabled! */
extern uint64_t global_timer_count;		/* timer ticks */
extern uint64_t *systemclock_p;			/* microseconds */
extern struct thread *s0_pager;

extern void NORETURN panic(const char *message);

/* return the values of global_timer_count, and *systemclock_p, disabling
 * interrupts around the read operation.
 */
extern uint64_t read_global_timer(void);
extern uint64_t ksystemclock(void);

#endif
