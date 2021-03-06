
#ifndef SEEN_UKERNEL_THREAD_H
#define SEEN_UKERNEL_THREAD_H

#include <stdint.h>
#include <stdbool.h>
#include <assert.h>

#include <ccan/likely/likely.h>
#include <ccan/compiler/compiler.h>

#include <l4/types.h>
#include <l4/vregs.h>

#include <ukernel/mm.h>
#include <ukernel/rbtree.h>
#include <ukernel/hook.h>
#include <ukernel/util.h>
#include <ukernel/x86.h>
#include <ukernel/rangealloc.h>
#include <ukernel/misc.h>


typedef L4_Word_t thread_id;

#define TID_VERSION_BITS 14
#define TID_VERSION_MASK ((1 << TID_VERSION_BITS) - 1)
#define THREAD_ID(num, version) ((num) << TID_VERSION_BITS | ((version) & TID_VERSION_MASK))
#define TID_THREADNUM(tid) ((tid) >> TID_VERSION_BITS)
#define TID_VERSION(tid) ((tid) & TID_VERSION_MASK)

/* values of <struct thread>.flags */
#define TF_HALT		0x1	/* after IPC completion, go to TS_STOPPED */
#define TF_SENDER	0x2	/* is the sender of a suspended typed transfer */
#define TF_INTR		0x4	/* interrupt receiver */
#define TF_PRE_RECV	0x8	/* halted in TS_R_RECV (restored on resume) */
#define TF_SYSCALL	0x10 /* may do fast exit (for Ipc, Lipc, ThreadSwitch) */
#define TF_REDIR	0x20 /* has ever been made redirector of any space */
#define TF_REDIR_WAIT 0x40 /* in SEND_WAIT but redirector blocked */
#define TF_PREEMPT	0x80 /* sends a preempt exception at reschedule */
#define TF_FROM_TURN 0x100 /* FromSpec turned by propagation */

/* thread states (<struct thread>.status); see also sched_status_str() */
#define TS_STOPPED 0
#define TS__UNUSED 1	/* unused, reserved for asm reasons */
#define TS_RUNNING 2
#define TS_READY 3		/* ready to execute */
#define TS_R_RECV 4		/* (user only) ready to do IPC receive phase */
#define TS_SEND_WAIT 5
#define TS_RECV_WAIT 6
#define TS_XFER 7		/* ready to proceed with typed IPC */


/* TODO: clean these up. */
#define IS_READY(st) ((st) == TS_READY || (st) == TS_R_RECV || (st) == TS_XFER)
#define IS_IPC_WAIT(st) (st == TS_SEND_WAIT || st == TS_RECV_WAIT)
#define IS_IPC(st) (IS_IPC_WAIT(st) || st == TS_R_RECV || st == TS_XFER)
#define IS_SCHED(thread) ((thread)->status >= TS_RUNNING)

/* allocation of implementation-defined UTCB fields.
 * one is used for %gs:0, and two more to return ECX and EDX via SYSEXIT and
 * to pass EBX and EBP via SYSENTER.
 *
 * position 0 is trampled by L4_LoadMR(0, ...), so it cannot be used.
 */
#define TCR_SYSEXIT_ECX (-1)
#define TCR_SYSENTER_EBP (-1)
#define TCR_SYSEXIT_EDX (-2)
#define TCR_SYSENTER_EBX (-2)
#define TCR_UTCB_PTR (-3)


struct space;
struct utcb_page;
struct ipc_state;		/* defined in <ukernel/ipc.h> */
struct saved_regs;		/* private to thread.c */

/* validity criteria in thread_find(): space != NULL. */
struct thread
{
	/* the fields up to and including "ctx" are accessed from the fast
	 * Ipc/Lipc path and should remain at the head of <struct thread>.
	 */
	struct space *space;

	uint16_t flags;
	uint8_t status, pri, sens_pri;

	struct x86_ctx ctx;
	void *fpu_context;

	/* sched_rb is in a scheduling queue whenever the thread is not TS_STOPPED
	 * or TS_DEAD. this can be tested with IS_SCHED().
	 */
	struct rb_node sched_rb;
	uint64_t wakeup_time;		/* absolute microseconds since epoch */

	thread_id id;
	uint16_t max_delay;
	L4_Time_t ts_len;
	uint32_t quantum;			/* # of µs left (goes up to 1h 6m) */

	/* # of µs left before message to scheduler. ~0 for infinite. threads with
	 * a zero total_quantum will not schedule until total_quantum is reset.
	 */
	uint64_t total_quantum;

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
	 * the containing thread can be referenced with container_of(). @param
	 * is the sender thread on success of the reply half, and NULL otherwise
	 * (i.e. errors and one-way kernel IPC).
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

	/* inactive threads have space != NULL && utcb_pos < 0, and haven't been
	 * added with space_add_thread().
	 */
	struct utcb_page *utcb_page;
	int utcb_pos;				/* offset in space's UTCB region */
	int utcb_ptr_seg;			/* segment descriptor index for %gs */
	L4_ThreadId_t scheduler;

	union {
		/* entry in `sendwait_hash' of ipc.c, keyed by int_hash(dest_tid.raw).
		 * valid in TS_SEND_WAIT. present in sendwait_hash iff $status ==
		 * TS_SEND_WAIT ∧ TF_HALT \notin ->flags$.
		 */
		struct {
			L4_ThreadId_t dest_tid;		/* global ID */
			L4_ThreadId_t send_tid;		/* ->id or vs, may be local */
		} ipc_wait;
		/* valid when $TF_PREEMPT ∈ ->flags$. */
		L4_Clock_t preempt_clock;
	} u2;

	union {
		/* key into `redir_wait' in ipc.c. implies TF_REDIR_WAIT when not
		 * nilthread, but may be nilthread even when a thread is in SEND_WAIT
		 * with REDIR_WAIT set. when not nilthread, thread is present in
		 * `redir_wait'.
		 */
		L4_ThreadId_t waited_redir;
	} u1;

	union {
		L4_ThreadId_t pager;		/* before activation */
		struct thread *partner;		/* after activation */
	} u0;

	/* context of the typed IPC state machine. emitted by do_typed_transfer()
	 * whenever a string transfer becomes complex and doesn't immediately
	 * abort. when this field is not NULL, threads coming out of RECV_WAIT
	 * (for pager replies) transition into SENDING/RECVING per the presence or
	 * absence, respectively, of the TF_SENDER bit in @flags.
	 */
	struct ipc_state *ipc;
	/* saved copies of ipc_from, ipc_to during string-transfer pagefault */
	L4_ThreadId_t orig_ipc_from, orig_ipc_to;
};


extern struct rangealloc *thread_ra;
extern void init_threading(void);


extern SYSCALL L4_Word_t sys_exregs(
	L4_ThreadId_t dest,
	L4_Word_t *control_p,
	L4_Word_t *sp_p, L4_Word_t *ip_p, L4_Word_t *flags_p,
	L4_Word_t *udh_p,
	L4_ThreadId_t *pager_p);

extern SYSCALL L4_Word_t sys_threadcontrol(
	L4_ThreadId_t dest_tid,
	L4_ThreadId_t pager,
	L4_ThreadId_t scheduler,
	L4_ThreadId_t spacespec,
	void *utcb_loc);


/* thread of tid's threadnum must not exist already. caller handles
 * ThreadControl semantics.
 */
extern struct thread *thread_new(thread_id tid);

extern void thread_set_spip(struct thread *t, L4_Word_t sp, L4_Word_t ip);
/* returns 0 on success; -ENOMEM on out of memory or GDT slots; and -EEXIST
 * when @start is already occupied by another thread in @t->space.
 */
extern int thread_set_utcb(struct thread *t, L4_Word_t start);

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
 * change vregs, toss the string transfer state, or call the post-exception
 * hooks; just does the status, send-wait (via cancel_ipc_from()) & scheduling
 * bits.
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
 * restore them on IPC completion. restoring happens when @t next receives a
 * message as part of post_exn_{fail,ok}() processing at the back of the hook.
 *
 * @n_mrs indicates how many registers the caller wants stored. the function
 * may store more.
 */
extern void save_ipc_regs(struct thread *t, void *utcb, int n_mrs);

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
extern bool post_exn_ok(struct thread *t, struct thread *sender);


/* complicated accessors */
extern PURE void *thread_get_utcb(struct thread *t);
extern PURE L4_ThreadId_t get_local_id(struct thread *t);

/* find thread by global ID. if thread is found but version doesn't match, or
 * if thread isn't found, returns NULL.
 */
extern struct thread *thread_get(L4_ThreadId_t tid);

/* eqv to thread_get() for global IDs, resolves local IDs in ref_space */
extern struct thread *resolve_tid_spec(
	struct space *ref_space,
	L4_ThreadId_t tid);

/* useful for debug only. */
extern bool thread_is_valid(const struct thread *t);

/* very unsafe, but about as fast as it gets. useful where thread_get() would
 * be used without a null-check.
 */
#ifdef NDEBUG
#define thread_get_fast(tid) \
	((struct thread *)ra_id2ptr(thread_ra, L4_ThreadNo((tid))))
#else
static inline struct thread *thread_get_fast(L4_ThreadId_t tid) {
	assert(L4_IsGlobalId(tid));
	struct thread *t = ra_id2ptr(thread_ra, L4_ThreadNo(tid));
	assert(t->id == tid.raw);
	return t;
}
#endif


static inline struct thread *get_tcr_thread(
	struct thread *t, void *utcb, int tcr)
{
	assert(utcb != NULL);
	L4_ThreadId_t tid = { .raw = L4_VREG(utcb, tcr) };
	return resolve_tid_spec(t->space, tid);
}


#endif
