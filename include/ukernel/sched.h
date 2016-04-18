
#ifndef SEEN_UKERNEL_SCHED_H
#define SEEN_UKERNEL_SCHED_H

#include <stdint.h>
#include <stdbool.h>
#include <ccan/compiler/compiler.h>

#include <ukernel/x86.h>
#include <ukernel/misc.h>
#include <ukernel/thread.h>
#include <ukernel/interrupt.h>


/* preempt_status flags */
#define PS_PENDING 0x80	/* preemption occurred */
#define PS_DELAYED 0x40	/* preemption was delayed until limit */


/* global data from sched.c */

/* accessed from the timer interrupt. r/w only with irqs off. */
extern uint64_t preempt_timer_count;
extern uint64_t task_switch_time;		/* ms */
extern int preempt_task_pri;
extern int preempt_status;		/* see PS_* */

/* set from thread_sleep() */
extern bool kernel_preempt_pending;
extern struct thread *kernel_preempt_to;


/* the "return_to_*" family leaves the currently-running thread, exits the
 * current interrupt or exception context, and resumes execution in the x86
 * frame indicated. "current_thread" is updated.
 */
extern NORETURN void return_to_scheduler(void);

/* same, but used to exit after sending a pf/exn/tqe message. caller should've
 * done the ipc_user() dance beforehand. source is the current thread.
 * @msg_utcb is ipc_user()'s return value.
 */
extern NORETURN void return_to_ipc(void *msg_utcb, struct thread *target);

/* returns on failure. caller should fall back to return_to_scheduler(). */
extern void return_to_other(struct thread *next);

/* ISRs that don't call return_to_*() should instead call return_from_exn() as
 * their last line. it disables interrupts until the interrupt flag is
 * reloaded by IRET.
 */
extern void return_from_exn(void);

/* see comment for check_preempt() */
extern void return_to_preempt(void);

/* called from ipc.c to un-do a part of the IPC partner chain due to a
 * successful reply send-phase. parameters implied from current_thread and its
 * ->u0.partner .
 */
extern NORETURN void return_to_partner(void);

/* returns true when it's useful for the caller to prepare the current thread
 * for scheduling out of, and then call return_to_preempt(). unless preemption
 * was delayed, the latter will do a non-local exit.
 */
extern bool check_preempt(void);


/* distinct function for easier transition to SMP. (it'll be under
 * per-processor %fs then.)
 */
static inline struct thread *get_current_thread(void) {
	extern struct thread *current_thread;
	return current_thread;
}

/* calls scheduler, replacing the current kernel context. used after a
 * save_user_ex() or save_user_regs(), or when the current thread has
 * committed suicide via ThreadControl.
 */
extern NORETURN void exit_to_scheduler(struct thread *prev);

/* switches into target thread. caller should save exception context. */
extern NORETURN void exit_to_thread(struct thread *next);

/* for the self-deleting ThreadControl edge case: required before
 * exit_to_scheduler(NULL).
 */
extern void leaving_thread(struct thread *current);

/* interface for task switching in ipc.c . these establish an IPC partnership
 * between @src and @dst, updating the scheduling structures accordingly and
 * letting sys_ipc() and sys_lipc() return to the receiver without going
 * through the scheduler.
 *
 * precond: @src->status == RECV_WAIT (as R_RECV is only noise)
 *          @src == get_current_thread()
 * postcond: @dst->status == RUNNING
 *           @dst == get_current_thread()
 *
 * the _quick() variant assumes that sender's receive phase won't time out,
 * avoiding a trip through the scheduling queue. the _timeout() variant will
 * not accept L4_Never as the third parameter.
 *
 * if @src->u0.partner == @dest, these functions set @src->u0.partner to NULL
 * and perform other bookkeeping related to partner scheduling.
 */
extern void sched_ipc_handoff_quick(struct thread *src, struct thread *dst);
extern void sched_ipc_handoff_timeout(
	struct thread *src, struct thread *dst, L4_Time_t timeout);

/* returns clock value that fits thread->wakeup_time */
extern uint64_t wakeup_at(L4_Time_t period);


/* scheduling queues */
extern void sq_insert_thread(struct thread *t);
extern void sq_remove_thread(struct thread *t);

static inline void sq_update_thread(struct thread *t) {
	sq_remove_thread(t);
	sq_insert_thread(t);
}

/* called from thread_sleep()'s wake branch, and the ->R_RECV Ipc state
 * transition.
 */
extern void might_preempt(struct thread *t);

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

#endif
