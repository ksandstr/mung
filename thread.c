
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <assert.h>
#include <ccan/alignof/alignof.h>
#include <ccan/list/list.h>

#include <ukernel/mm.h>
#include <ukernel/slab.h>
#include <ukernel/thread.h>
#include <ukernel/misc.h>


struct thread *current_thread = NULL;

static struct kmem_cache *thread_slab = NULL;
static struct list_head thread_list = LIST_HEAD_INIT(thread_list),
	dead_thread_list = LIST_HEAD_INIT(dead_thread_list);
static int next_thread_id = 1;


struct thread *init_threading(void)
{
	assert(thread_slab == NULL);
	thread_slab = kmem_cache_create("thread_slab", sizeof(struct thread),
		ALIGNOF(struct thread), 0, NULL, NULL);

	struct thread *boot = kmem_cache_alloc(thread_slab);
	boot->stack_page = NULL;
	boot->id = 0;
	boot->status = TS_RUNNING;

	list_add(&thread_list, &boot->link);
	current_thread = boot;

	return boot;
}


static void end_thread(void)
{
	/* must find a thread to exit to. if none is available, sleep on interrupt
	 * until that changes.
	 */
	bool found = false;
	struct thread *next = NULL, *self = get_current_thread();
	printf("%s: thread %d terminating\n", __func__, self->id);
	do {
		list_for_each(&thread_list, next, link) {
			if(next->status == TS_READY) {
				found = true;
				break;
			}
		}
		if(!found) {
			/* assuming an interrupt can make threads runnable again. */
			asm volatile ("hlt");
		}
	} while(!found);

	list_del_from(&thread_list, &self->link);
	list_add(&dead_thread_list, &self->link);
	self->status = TS_DEAD;
	next->status = TS_RUNNING;
	current_thread = next;
	swap_context(&self->ctx, &next->ctx);

	panic("swap_context() in end_thread() returned???");
}


void yield(struct thread *to)
{
	struct thread *self = get_current_thread();
	assert(self->status == TS_RUNNING);

	/* find next ready thread. */
	struct thread *next = NULL;
	bool found = false;
	list_for_each(&thread_list, next, link) {
		if(next->status == TS_READY) {
			found = true;
			break;
		}
	}
	if(found) {
		self->status = TS_READY;
		next->status = TS_RUNNING;
		current_thread = next;
		swap_context(&self->ctx, &next->ctx);

		assert(current_thread == self);
		assert(self->status == TS_RUNNING);
	}
}


static void thread_wrapper(void (*function)(void *), void *parameter)
{
	(*function)(parameter);
	end_thread();
	for(;;) yield(NULL);
}


struct thread *create_thread(
	void (*function)(void *),
	void *parameter)
{
	struct thread *t = NULL;
	if(list_empty(&dead_thread_list)) {
		struct thread *t = kmem_cache_alloc(thread_slab);
		t->stack_page = get_kern_page();
	} else {
		t = container_of(dead_thread_list.n.next, struct thread, link);
		list_del_from(&dead_thread_list, &t->link);

		/* TODO: could dispose other threads here. */
	}
	t->id = next_thread_id++;
	t->status = TS_READY;

	void **stk_top = t->stack_page->vm_addr + PAGE_SIZE - 16;
	stk_top[0] = &thread_wrapper;
	stk_top[1] = (void *)0xdeadbeef;
	stk_top[2] = function;
	stk_top[3] = parameter;
	t->ctx.regs[7] = (uintptr_t)stk_top;

	list_add(&thread_list, &t->link);

	return t;
}
