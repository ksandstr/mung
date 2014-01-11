
#ifndef SEEN_UKERNEL_SCHED_H
#define SEEN_UKERNEL_SCHED_H

#include <stdint.h>
#include <stdbool.h>
#include <ccan/compiler/compiler.h>

#include <ukernel/x86.h>
#include <ukernel/misc.h>
#include <ukernel/thread.h>


/* global data from sched.c */

/* accessed from the timer interrupt. r/w only with irqs off. */
extern uint64_t preempt_timer_count;
extern uint64_t task_switch_time;		/* ms */
extern int preempt_task_pri;
extern bool preempt_delayed;
/* set per CPU rather early, used to signal preemption scheduling */
extern L4_Word_t *scheduler_mr1;


/* initializes the scheduler.
 *
 * TODO: should be generalized to accept a per-CPU structure, and initialize
 * the scheduling bits in that.
 */
extern void init_sched(struct thread *current);

/* the "return_to_*" family exits the current interrupt or exception context
 * and resumes execution in the x86 frame given. "current_thread" is updated.
 */
extern NORETURN void return_to_scheduler(void);
/* same, but invokes send-and-wait ipc first and if successful, schedules the
 * target. source is the current thread.
 */
extern NORETURN void return_to_ipc(struct thread *target);

/* very rare; invoked in thread self-deletion after the current thread has
 * been destroyed. basically a variant of return_to_scheduler() but doesn't
 * reference current at all.
 */
extern NORETURN void return_from_dead(void);

/* ISRs that don't call return_to_*() should instead call return_from_exn() as
 * their last line. it disables interrupts until the interrupt flag is
 * reloaded by IRET.
 */
extern void return_from_exn(void);

extern struct thread *get_current_thread(void);

/* switches from current thread, which must be an userspace thread. caller
 * should save the exception context.
 */
extern NORETURN void switch_thread_u2u(struct thread *next);

/* returns clock value that fits thread->wakeup_time */
extern uint64_t wakeup_at(L4_Time_t period);


/* scheduling queues */
extern void sq_insert_thread(struct thread *t);
extern void sq_remove_thread(struct thread *t);

static inline void sq_update_thread(struct thread *t) {
	sq_remove_thread(t);
	sq_insert_thread(t);
}

extern const char *sched_status_str(struct thread *t);

/* returns true if "other" will preempt "self"'s current quantum, counted from
 * switch_time_us.
 */
static inline bool preempted_by(
	struct thread *self,
	uint64_t switch_at_us,
	struct thread *other)
{
	return other->pri > self->pri
		&& other->wakeup_time <= switch_at_us + self->quantum;
}

extern SYSCALL L4_Word_t sys_schedule(
	L4_ThreadId_t dest_tid,
	L4_Word_t prioctl,
	L4_Word_t *timectl_p,
	L4_Word_t procctl,
	L4_Word_t preemptctl);
extern SYSCALL void sys_threadswitch(L4_ThreadId_t target);

/* switches away from a kernel thread.
 * returns false when no thread was activated.
 */
extern bool schedule(void);

/* where CPU-starting threads go to cause proper scheduling */
extern NORETURN void scheduler_loop(struct thread *self);

#endif
