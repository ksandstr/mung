
#ifndef SEEN_UKERNEL_SCHED_H
#define SEEN_UKERNEL_SCHED_H

#include <stdint.h>
#include <stdbool.h>
#include <ccan/compiler/compiler.h>

#include <ukernel/x86.h>
#include <ukernel/thread.h>


/* the "return_to_*" family exits the current interrupt or exception context
 * and resumes execution in the x86 frame given. "current_thread" is updated.
 */
extern NORETURN void return_to_scheduler(void);
/* same, but invokes send-and-wait ipc first and if successful, schedules the
 * target. source is the current thread.
 */
extern NORETURN void return_to_ipc(struct thread *target);

/* NOTE: doesn't yield */
extern void yield(struct thread *to);

extern struct thread *get_current_thread(void);

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
extern bool preempted_by(
	struct thread *self,
	uint64_t switch_at_us,
	struct thread *other);

extern void sys_schedule(struct x86_exregs *regs);
extern void sys_threadswitch(struct x86_exregs *regs);

/* switches away from a kernel thread.
 * returns false when no thread was activated.
 */
extern bool schedule(void);

/* called by the kthread wrapper function. */
extern NORETURN void end_kthread(void);


/* where CPU-starting threads go to cause proper scheduling */
extern NORETURN void scheduler_loop(struct thread *self);


#endif
