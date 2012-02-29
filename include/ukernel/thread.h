
#ifndef SEEN_UKERNEL_THREAD_H
#define SEEN_UKERNEL_THREAD_H

#include <stdint.h>
#include <stdbool.h>
#include <ccan/list/list.h>

#include <ukernel/mm.h>


typedef uint32_t thread_id;

#define TID_VERSION_BITS 14
#define TID_VERSION_MASK ((1 << TID_VERSION_BITS) - 1)
#define THREAD_ID(num, version) ((num) << TID_VERSION_BITS | ((version) & TID_VERSION_MASK))
#define TID_THREADNUM(tid) ((tid) >> TID_VERSION_BITS)
#define TID_VERSION(tid) ((tid) & TID_VERSION_MASK)

/* (requires inclusion of <ukernel/space.h>) */
#define IS_KERNEL_THREAD(thread) ((thread)->space == kernel_space)


struct space;

/* swap_context() is the soft yield. it re-/stores only those registers
 * that're preserved over a SysV x86 function call.
 */
struct x86_context
{
	/* eax, ebx, ecx, edx, esi, edi, ebp, esp, eip (user only) */
	uint32_t regs[9];
	/* TODO: others? */
} __attribute__((packed));


enum thread_state {
	TS_STOPPED,
	TS_RUNNING,
	TS_READY,
	TS_DEAD,
};


struct thread
{
	struct list_node link;		/* in the appropriate queue (sleep, ready) */
	thread_id id;				/* full TID */
	enum thread_state status;

	struct space *space;
	struct list_node space_link;

	struct page *stack_page;

	struct x86_context ctx;
};


extern struct thread *current_thread;


extern struct thread *init_threading(thread_id boot_tid);
extern struct thread *create_kthread(
	thread_id tid,
	void (*function)(void *),
	void *parameter);

/* NOTE: doesn't yield */
extern void yield(struct thread *to);

/* returns false when no thread was activated. */
extern bool schedule(void);

static inline struct thread *get_current_thread(void) {
	return current_thread;
}

extern void thread_set_space(struct thread *t, struct space *sp);
/* finds by thread ID, ignores version. */
extern struct thread *thread_find(thread_id tid);

/* thread of tid's threadnum must not exist already. caller handles
 * ThreadControl semantics.
 */
extern struct thread *thread_new(thread_id tid);

extern void thread_set_spip(struct thread *t, uintptr_t sp, uintptr_t ip);
extern void thread_start(struct thread *t);


/* for htable */
extern size_t hash_thread_by_id(const void *threadptr, void *dataptr);


/* defined in context-32.S etc. */

extern void swap_context(
	struct x86_context *store,
	const struct x86_context *load);

extern void swap_to_ring3(
	struct x86_context *store,
	const struct x86_context *load);

#endif
