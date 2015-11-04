
#ifndef SEEN_UKERNEL_THREAD_H
#define SEEN_UKERNEL_THREAD_H

#include <stdint.h>
#include <stdbool.h>
#include <assert.h>

#include <ccan/list/list.h>
#include <ccan/compiler/compiler.h>

#include <l4/types.h>
#include <l4/vregs.h>

#include <ukernel/mm.h>
#include <ukernel/rbtree.h>
#include <ukernel/hook.h>
#include <ukernel/util.h>
#include <ukernel/x86.h>
#include <ukernel/slab.h>
#include <ukernel/guard.h>
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

/* there's need for a few kernel threads: the boot (scheduler) thread, the
 * sigma0 pager, and possibly the ones used by the kernel's self-tests. a
 * larger ID-space reservation won't hurt, however, so 256 it is.
 */
#define NUM_KERNEL_THREADS 256

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

struct thread
{
	/* the fields up to and including "ctx" are accessed from the fast
	 * Ipc/Lipc path and should remain at the head of <struct thread>.
	 */
	struct space *space;

	uint8_t flags;
	uint8_t status, pri, sens_pri;

	struct x86_exregs ctx;
	void *fpu_context;

	GUARD_MEMBER(sched_rb_0);
	/* sched_rb is in a scheduling queue whenever the thread is not TS_STOPPED
	 * or TS_DEAD. this can be tested with IS_SCHED().
	 */
	struct rb_node sched_rb;
	GUARD_MEMBER(sched_rb_1);
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
	/* TCRs and MRs saved under exception IPC. */
	struct saved_regs *regs;

	/* inactive threads have space != NULL && utcb_pos < 0, and haven't been
	 * added with space_add_thread().
	 */
	struct utcb_page *utcb_page;
	int utcb_pos;				/* offset in space's UTCB region */
	int utcb_ptr_seg;			/* segment descriptor index for %gs */
	L4_ThreadId_t scheduler;

	union {
		struct page *stack_page;	/* kth stack page */

		/* key into `redir_wait' in ipc.c. implies TF_REDIR_WAIT when not
		 * nilthread, but may be nilthread even when a thread is in SEND_WAIT
		 * with REDIR_WAIT set. when not nilthread, thread is present in
		 * `redir_wait'.
		 *
		 * non-kth only. (enforced by forbidding SpaceControl on
		 * kernel_space.)
		 */
		L4_ThreadId_t waited_redir;
	} u1;

	union {
		L4_ThreadId_t pager;		/* before activation */
		struct thread *partner;		/* non-kth after activation */
		struct list_node dead_link;	/* kth after termination */
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


/* keyed by int_hash(thread->id), members are <struct thread *> */
extern struct htable thread_hash;

/* allocates <struct thread> after init_threading() */
extern struct kmem_cache *thread_slab;


extern void init_threading(void);

/* finds by thread ID, ignores version. */
extern struct thread *thread_find(thread_id tid);


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

/* (re)constructs a thread. initializes all fields and adds it to thread_hash.
 * return value is true when the latter succeeds.
 */
extern bool thread_ctor(struct thread *t, thread_id tid);

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
 * restore them on IPC completion. the restoring happens when @t next receives
 * a message as part of post_exn_{fail,ok}() processing at the back of the
 * hook; the handler detaches itself and resets t->u0.regs .
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
extern bool post_exn_ok(struct thread *t);


/* complicated accessors */
extern PURE void *thread_get_utcb(struct thread *t);
extern PURE L4_ThreadId_t get_local_id(struct thread *t);
extern void thread_save_ctx(struct thread *t, const struct x86_exregs *regs);

/* compares version bits for global IDs, resolves locak IDs in ref_space */
extern struct thread *resolve_tid_spec(
	struct space *ref_space,
	L4_ThreadId_t tid);

static inline struct thread *get_tcr_thread(
	struct thread *t,
	void *utcb,
	int tcr)
{
	assert(utcb != NULL);
	L4_ThreadId_t tid = { .raw = L4_VREG(utcb, tcr) };
	return L4_IsNilThread(tid) ? NULL : resolve_tid_spec(t->space, tid);
}

/* legacy accessors, to be removed */
#define thread_get_pager(t, utcb) get_tcr_thread((t), (utcb), L4_TCR_PAGER)
#define thread_get_exnh(t, utcb) get_tcr_thread((t), (utcb), L4_TCR_EXCEPTIONHANDLER)


/* for htable */
extern size_t hash_thread_by_id(const void *threadptr, void *dataptr);


/* low-level context switching from context-32.S etc. */

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

/* NOTE: the sysexit_*() family don't fill in TCR_SYSEXIT_E[CD]X! */
extern void sysexit_from_kth(
	struct x86_exregs *store,
	const struct x86_exregs *load,
	int gs_selector);

extern NORETURN void sysexit_to_ring3(
	const struct x86_exregs *userctx,
	int gs_selector);

extern NORETURN void iret_to_scheduler(const struct x86_exregs *sched_ctx);

#endif
