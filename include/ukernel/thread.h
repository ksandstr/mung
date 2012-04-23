
#ifndef SEEN_UKERNEL_THREAD_H
#define SEEN_UKERNEL_THREAD_H

#include <stdint.h>
#include <stdbool.h>
#include <ccan/list/list.h>
#include <ccan/compiler/compiler.h>

#include <l4/types.h>
#include <ukernel/mm.h>
#include <ukernel/util.h>
#include <ukernel/x86.h>


typedef uint32_t thread_id;

#define TID_VERSION_BITS 14
#define TID_VERSION_MASK ((1 << TID_VERSION_BITS) - 1)
#define THREAD_ID(num, version) ((num) << TID_VERSION_BITS | ((version) & TID_VERSION_MASK))
#define TID_THREADNUM(tid) ((tid) >> TID_VERSION_BITS)
#define TID_VERSION(tid) ((tid) & TID_VERSION_MASK)

/* (requires inclusion of <ukernel/space.h>) */
#define IS_KERNEL_THREAD(thread) ((thread)->space == kernel_space)
#define IS_READY(st) ((st) == TS_READY || (st) == TS_R_RECV)
#define IS_IPC(st) ((st) == TS_SEND_WAIT || (st) == TS_RECV_WAIT \
	|| (st) == TS_R_RECV)

/* let's leave this at 128. interrupts also get UTCBs as interrupt IPCs are
 * done that way.
 */
#define NUM_KERNEL_THREADS 128


struct space;

/* swap_context() is the soft yield. it re-/stores only those registers
 * that're preserved over a SysV x86 function call.
 */
struct x86_context
{
	/* eax, ebx, ecx, edx, esi, edi, ebp, esp, eip, eflags */
	uint32_t regs[10];
	/* TODO: others? */
} __attribute__((packed));


enum thread_state {
	TS_STOPPED,
	TS_RUNNING,
	TS_DEAD,
	TS_SEND_WAIT,
	TS_RECV_WAIT,

	/* ready threads. */
	TS_READY,		/* ready to execute in user or kernel */
	TS_R_RECV,		/* ready to do IPC receive side in kernel */

	/* inactive ("pre-active") threads */
	TS_INACTIVE,
};


struct thread
{
	struct list_node link;		/* in the appropriate queue (sleep, ready) */
	thread_id id;
	enum thread_state status;
	L4_Time_t send_timeout, recv_timeout;
	/* "from" is altered on receive. */
	L4_ThreadId_t ipc_from, ipc_to;

	/* the IPC mechanism arranges for post_exn_call() to be called once either
	 * the correct thread replies to an exception IPC (page faults,
	 * exceptions, breath-of-life), or the IPC receive is aborted (but then
	 * only if "exn_priv" is not NULL). in the former case "thread" will refer
	 * to the receiving thread, and in the latter it will be NULL.
	 *
	 * the callback function is supposed to clear post_exn_call, unless the
	 * next IPC reception is also supposed to trigger it.
	 */
	void (*post_exn_call)(struct thread *thread, void *priv);
	void *exn_priv;

	struct space *space;
	struct list_node space_link;
	int utcb_pos;				/* offset in space's UTCB region */
	int utcb_ptr_seg;			/* segment descriptor index for %gs */
	L4_ThreadId_t scheduler;

	struct page *stack_page;

	struct x86_context ctx;

	/* saved IPC registers. restored by a cold path in return_to_ipc(). at
	 * most 13 (acceptor, 12 MRs for exception message) for x86.
	 * (would be 21 for amd64.)
	 */
	uint8_t saved_mrs, saved_brs;
	L4_Word_t saved_regs[13];
};


extern struct thread *init_threading(thread_id boot_tid);
extern struct thread *create_kthread(
	void (*function)(void *),
	void *parameter);

/* NOTE: doesn't yield */
extern void yield(struct thread *to);

/* alters the x86 exception frame to return to the scheduler rather than
 * userspace or a kernel thread.
 */
struct x86_exregs;
extern NORETURN void return_to_scheduler(struct x86_exregs *regs);
/* same, but invokes send-and-wait ipc first and if successful, schedules the
 * target. source is the current thread.
 */
extern NORETURN void return_to_ipc(
	struct x86_exregs *regs,
	struct thread *target);


extern void thread_save_exregs(
	struct thread *t,
	const struct x86_exregs *regs);

extern struct thread *get_current_thread(void);

extern void thread_set_space(struct thread *t, struct space *sp);
/* finds by thread ID, ignores version. */
extern struct thread *thread_find(thread_id tid);


extern L4_Word_t sys_exregs(
	L4_ThreadId_t dest,
	L4_Word_t *control_p,
	L4_Word_t *sp_p, L4_Word_t *ip_p, L4_Word_t *flags_p,
	L4_Word_t *udh_p,
	L4_ThreadId_t *pager_p);

extern void sys_threadswitch(struct x86_exregs *regs);
extern void sys_threadcontrol(struct x86_exregs *regs);


/* thread of tid's threadnum must not exist already. caller handles
 * ThreadControl semantics.
 */
extern struct thread *thread_new(thread_id tid);

extern void thread_set_spip(struct thread *t, L4_Word_t sp, L4_Word_t ip);
extern void thread_set_utcb(struct thread *t, L4_Word_t start);
extern void thread_start(struct thread *t);

extern void save_ipc_regs(struct thread *t, int mrs, int brs);

/* complicated accessors */
extern PURE void *thread_get_utcb(struct thread *t);


/* for htable */
extern size_t hash_thread_by_id(const void *threadptr, void *dataptr);


/* defined in sched.c */
extern struct thread *current_thread, *scheduler_thread;

extern void sys_schedule(struct x86_exregs *regs);

/* switches away from a kernel thread.
 * returns false when no thread was activated.
 */
extern bool schedule(void);

extern NORETURN void end_kthread(void);


/* defined in context-32.S etc. */

extern void swap_context(
	struct x86_context *store,
	const struct x86_context *load);

extern void swap_to_ring3(
	struct x86_context *store,
	const struct x86_context *load,
	int gs_selector);

extern NORETURN void iret_to_scheduler(const struct x86_context *sched_ctx);

#endif
