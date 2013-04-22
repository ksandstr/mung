
#ifndef SEEN_UKERNEL_THREAD_H
#define SEEN_UKERNEL_THREAD_H

#include <stdint.h>
#include <stdbool.h>
#include <ccan/list/list.h>
#include <ccan/compiler/compiler.h>

#include <l4/types.h>
#include <ukernel/mm.h>
#include <ukernel/rbtree.h>
#include <ukernel/hook.h>
#include <ukernel/util.h>
#include <ukernel/x86.h>
#include <ukernel/guard.h>


typedef L4_Word_t thread_id;

#define TID_VERSION_BITS 14
#define TID_VERSION_MASK ((1 << TID_VERSION_BITS) - 1)
#define THREAD_ID(num, version) ((num) << TID_VERSION_BITS | ((version) & TID_VERSION_MASK))
#define TID_THREADNUM(tid) ((tid) >> TID_VERSION_BITS)
#define TID_VERSION(tid) ((tid) & TID_VERSION_MASK)

/* values of <struct thread>.flags */
#define TF_HALT		0x1		/* after IPC completion, go to TS_STOPPED */
#define TF_SENDER	0x2		/* is the sender of a suspended typed transfer */

/* thread states (<struct thread>.status); see also sched_status_str() */
#define TS_STOPPED 0
#define TS_DEAD 1		/* (kthread only) thread waits for pruning */
#define TS_RUNNING 2
#define TS_READY 3		/* ready to execute */
#define TS_R_RECV 4		/* (user only) ready to do IPC receive phase */
#define TS_SEND_WAIT 5
#define TS_RECV_WAIT 6
#define TS_XFER 7		/* ready to resume suspended typed IPC */


/* TODO: clean these up. */
#define IS_READY(st) ((st) == TS_READY || (st) == TS_R_RECV || (st) == TS_XFER)
#define IS_IPC_WAIT(st) (st == TS_SEND_WAIT || st == TS_RECV_WAIT)
#define IS_IPC(st) (IS_IPC_WAIT(st) || st == TS_R_RECV || st == TS_XFER)
#define IS_SCHED(thread) ((thread)->status >= TS_RUNNING)

/* (requires inclusion of <ukernel/space.h>) */
#define IS_KERNEL_THREAD(thread) ((thread)->space == kernel_space)

/* let's leave this at 128. interrupts also get UTCBs as interrupt IPCs are
 * done that way.
 */
#define NUM_KERNEL_THREADS 128


struct space;
struct ipc_state;		/* private to ipc.c */

struct thread
{
	GUARD_MEMBER(sched_rb_0);
	/* sched_rb is in a scheduling queue whenever the thread is not TS_STOPPED
	 * or TS_DEAD. this can be tested with IS_SCHED().
	 */
	struct rb_node sched_rb;
	GUARD_MEMBER(sched_rb_1);
	uint64_t wakeup_time;		/* absolute microseconds since epoch */

	thread_id id;
	/* TODO: alter ipc.c to go to TS_STOPPED after IPC completion when TF_HALT
	 * is set
	 */
	uint8_t flags;
	uint8_t status;

	uint8_t pri, sens_pri;
	uint16_t max_delay;
	L4_Time_t ts_len;
	uint32_t quantum;			/* # of µs left (goes up to 1h 6m) */
	uint64_t total_quantum;		/* # of µs left */

	/* parameters of the ongoing IPC operation
	 * (i.e. copies of TCR values at time of IPC syscall)
	 */
	L4_Time_t send_timeout, recv_timeout;
	/* successful active or passive receive stores the apparent sender's
	 * global TID in ipc_from. (actual sender is stored in the TCR.)
	 */
	L4_ThreadId_t ipc_from, ipc_to;

	/* the IPC mechanism arranges for post_exn_call to be called front-to-back
	 * once either the IPC in progress succeeds in its send and receive half
	 * both, or fails in either. hook functions should call hook_detach() as
	 * desired.
	 *
	 * @code is 0 on success and nonzero on failure. @dataptr isn't defined.
	 * the containing thread can be referenced with container_of().
	 *
	 * this hook informs kernel-generated IPC chains (i.e. page faults,
	 * exceptions, string transfer pagefaults, and breath-of-life) of timeout
	 * and abort events. on success, exactly one hook function must put the
	 * thread in question in the correct state that follows from success; on
	 * failure, hook functions must leave the thread state alone (per def'n of
	 * post_exn_fail()).
	 *
	 * when the hook is empty, no kernel-generated IPC is occurring. threads
	 * can only be scheduled when this is true.
	 */
	struct hook post_exn_call;

	struct space *space;
	struct list_node space_link;
	int utcb_pos;				/* offset in space's UTCB region */
	int utcb_ptr_seg;			/* segment descriptor index for %gs */
	L4_ThreadId_t scheduler;

	struct page *stack_page;

	struct x86_exregs ctx;

	/* saved IPC registers. at most 14 (acceptor, tag, 12 MRs for exception
	 * message) for x86.
	 *
	 * NOTE: before a thread is activated, saved_regs[0] is used for the pager
	 * value set by ThreadControl before a thread's pager TCR is available.
	 * NOTE #2: buffer registers are stored in a reverse order at
	 * saved_regs[saved_mrs .. saved_brs - 1], i.e. high number to low. this
	 * is due to the spec's UTCB layout.
	 * NOTE #3: send_prexfer_fault() and its hook function keep the original
	 * ipc_from & ipc_to values in saved_regs[12, 13] respectively, as it's
	 * known that set_pf_msg() only uses the first four.
	 */
	uint8_t saved_mrs, saved_brs;
	L4_Word_t saved_regs[14];

	/* context of the IPC state machine. emitted by do_typed_transfer()
	 * whenever a string transfer becomes complex and doesn't immediately
	 * abort. when this field is not NULL, threads coming out of RECV_WAIT
	 * (for pager replies) transition into SENDING/RECVING per the presence or
	 * absence, respectively, of the TF_SENDER bit in @flags.
	 */
	struct ipc_state *ipc;

	struct list_node dead_link;	/* link in dead_thread_list */
};


/* keyed by int_hash(thread->id), members are <struct thread *> */
extern struct htable thread_hash;

extern struct thread *init_threading(thread_id boot_tid);
extern struct thread *create_kthread(
	void (*function)(void *),
	void *parameter);

extern void thread_set_space(struct thread *t, struct space *sp);
/* finds by thread ID, ignores version. */
extern struct thread *thread_find(thread_id tid);


extern L4_Word_t sys_exregs(
	L4_ThreadId_t dest,
	L4_Word_t *control_p,
	L4_Word_t *sp_p, L4_Word_t *ip_p, L4_Word_t *flags_p,
	L4_Word_t *udh_p,
	L4_ThreadId_t *pager_p);

extern void sys_threadcontrol(struct x86_exregs *regs);


/* thread of tid's threadnum must not exist already. caller handles
 * ThreadControl semantics.
 */
extern struct thread *thread_new(thread_id tid);

extern void thread_set_spip(struct thread *t, L4_Word_t sp, L4_Word_t ip);
/* returns false on some error (generally when out of GDT slots). */
extern bool thread_set_utcb(struct thread *t, L4_Word_t start);

/* schedules a stopped thread with a cleared TF_HALT bit. this status only
 * occurs in freshly-created threads.
 *
 * requires that status == STOPPED && TF_HALT is set.
 */
extern void thread_start(struct thread *t);

/* sets TF_HALT and, if status \in {READY,RUNNING}, changes it to STOPPED and
 * removes the thread from scheduling. IPC operation is not affected. a halted
 * thread may therefore generate page faults if a string transfer starts due
 * to an active sender.
 *
 * requires that TF_HALT is clear.
 *
 * as a special case, when a kernel thread halts itself, this function calls
 * schedule() on its behalf.
 */
extern void thread_halt(struct thread *t);

/* clears TF_HALT and sets status to READY if it was STOPPED before.
 * requires that TF_HALT is set.
 */
extern void thread_resume(struct thread *t);

/* effects an IPC failure (timeout, abort, cancel) state transition. doesn't
 * change vregs or call the post-exception hooks, just does the status &
 * scheduling bits.
 *
 * requires status \in {RECV_WAIT, SEND_WAIT, R_RECV}.
 */
extern void thread_ipc_fail(struct thread *t);


/* sets a thread's wakeup timer and updates its scheduling. if "period" is
 * L4_ZeroTime, the thread wakes up immediately.
 *
 * (see thread_wake() for the period = L4_Never case.)
 *
 * requires that status \in {RECV_WAIT, SEND_WAIT, XFER}.
 */
extern void thread_sleep(struct thread *t, L4_Time_t period);

/* interrupts an IPC wait (incl. R_RECV state) and sets state to STOPPED or
 * READY, depending on TF_HALT as you'd expect; or retains it as XFER if it
 * was already XFER. this is a low-level function: it doesn't set error codes
 * or invoke exception IPC callbacks. it's exactly equivalent to
 * thread_sleep(t, L4_ZeroTime), but this is better documentation.
 *
 * requires that status \in {RECV_WAIT, SEND_WAIT}.
 */
#define thread_wake(t) thread_sleep((t), L4_ZeroTime)

/* used by exception handlers to save previous IPC registers & automagically
 * restore them on IPC completion.
 */
extern void save_ipc_regs(struct thread *t, int mrs, int brs);

/* these return false for ordinary IPC (with return values etc), and true for
 * exception IPC (with a full frame restore). they don't care about kernel
 * threads.
 *
 * calls to post_exn_fail() should co-occur with something that brings @t out
 * of IPC sleep, i.e. thread_ipc_fail(), thread_wake() or similar, regardless
 * of the return value.
 *
 * when post_exn_ok() returns true the thread'll have changed state according
 * to the kernel-side IPC flow, and should not be put to sleep or woken up by
 * the caller.
 */
extern bool post_exn_fail(struct thread *t);
extern bool post_exn_ok(struct thread *t);


/* complicated accessors */
extern PURE void *thread_get_utcb(struct thread *t);
extern struct thread *thread_get_pager(struct thread *t, void *utcb);
extern void thread_save_ctx(struct thread *t, const struct x86_exregs *regs);


/* for htable */
extern size_t hash_thread_by_id(const void *threadptr, void *dataptr);


/* defined in context-32.S etc. */

/* swap_context() is the soft yield. it re-/stores only those registers
 * that're preserved over a SysV x86 function call.
 */
extern void swap_context(
	struct x86_exregs *store,
	const struct x86_exregs *load);

extern void swap_to_ring3(
	struct x86_exregs *store,
	const struct x86_exregs *load,
	int gs_selector);

extern NORETURN void iret_to_scheduler(
	const struct x86_exregs *sched_ctx);

#endif
