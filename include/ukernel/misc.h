/* quasi-runtime functions for the microkernel.
 *
 * TODO: move the utility things into <ukernel/util.h>. this header should be
 * for miscellaneous kernel exports, and not be included from e.g. sigma0 or
 * mbiloader.
 */

#ifndef SEEN_UKERNEL_MISC_H
#define SEEN_UKERNEL_MISC_H

#include <stdint.h>
#include <stdnoreturn.h>

#include <l4/types.h>

/* TODO: remove this at some point. */
#include <ukernel/util.h>


#define SYSCALL __attribute__((regparm(3), noinline))
#define KERNEL_STACK_SIZE (16 * 1024)

#define NOT_REACHED __not_reached(__FILE__, __LINE__, __func__)


struct thread;


/* from kmain.c */

/* should only be read with interrupts disabled! */
extern uint64_t global_timer_count;		/* timer ticks */
extern uint64_t *systemclock_p;			/* microseconds */

extern void *syscall_stack;

extern void noreturn panic(const char *message);
extern void noreturn __not_reached(
	const char *file, int line, const char *func);

/* return the values of global_timer_count, and *systemclock_p, disabling
 * interrupts around the read operation.
 */
extern uint64_t read_global_timer(void);
extern uint64_t ksystemclock(void);

#endif
