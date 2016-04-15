
/* FIXME: this is so tiny that it could just as well be merged into
 * <ukernel/sched.h>.
 */

#ifndef SEEN_UKERNEL_KTH_H
#define SEEN_UKERNEL_KTH_H

#include <stdbool.h>

#include <ukernel/thread.h>


struct thread;

extern void kth_init(void);
extern struct thread *kth_start(void (*function)(void *), void *parameter);

/* returns true if switched, false if not. */
extern bool kth_yield(void);


#endif
