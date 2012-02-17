
#ifndef SEEN_UKERNEL_THREAD_H
#define SEEN_UKERNEL_THREAD_H

#include <stdint.h>
#include <ccan/list/list.h>

#include <ukernel/mm.h>


/* swap_context() is the soft yield. it re-/stores only those registers
 * that're preserved over a SysV x86 function call.
 */
struct x86_context
{
	/* eax, ebx, ecx, edx, esi, edi, ebp, esp */
	uint32_t regs[8];
	/* TODO: others? */
} __attribute__((packed));


enum thread_state {
	TS_RUNNING = 0,
	TS_READY,
	TS_DEAD,
};


struct thread
{
	struct list_node link;
	struct page *stack_page;
	enum thread_state status;
	int id;

	struct x86_context ctx;
};


extern struct thread *current_thread;


extern struct thread *init_threading(void);
extern struct thread *create_thread(
	void (*function)(void *),
	void *parameter);

extern void yield(struct thread *to);

static inline struct thread *get_current_thread(void) {
	return current_thread;
}


/* defined in context-32.S etc. */

extern void swap_context(
	struct x86_context *store,
	const struct x86_context *load);

#endif
