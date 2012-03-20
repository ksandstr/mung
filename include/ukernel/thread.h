
#ifndef SEEN_UKERNEL_THREAD_H
#define SEEN_UKERNEL_THREAD_H

#include <stdint.h>
#include <stdbool.h>
#include <ccan/list/list.h>
#include <ccan/compiler/compiler.h>

#include <l4/types.h>
#include <ukernel/mm.h>
#include <ukernel/misc.h>


typedef uint32_t thread_id;

#define TID_VERSION_BITS 14
#define TID_VERSION_MASK ((1 << TID_VERSION_BITS) - 1)
#define THREAD_ID(num, version) ((num) << TID_VERSION_BITS | ((version) & TID_VERSION_MASK))
#define TID_THREADNUM(tid) ((tid) >> TID_VERSION_BITS)
#define TID_VERSION(tid) ((tid) & TID_VERSION_MASK)

/* (requires inclusion of <ukernel/space.h>) */
#define IS_KERNEL_THREAD(thread) ((thread)->space == kernel_space)

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
};


struct thread
{
	struct list_node link;		/* in the appropriate queue (sleep, ready) */
	thread_id id;
	enum thread_state status;
	L4_Time_t send_timeout, recv_timeout;
	/* "from" is altered on receive. */
	L4_ThreadId_t ipc_from, ipc_to;

	struct space *space;
	struct list_node space_link;
	int utcb_pos;				/* offset in space's UTCB region */
	int utcb_ptr_seg;			/* segment descriptor index for %gs */

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

/* switches away from a kernel thread.
 * returns false when no thread was activated.
 */
extern bool schedule(void);

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
