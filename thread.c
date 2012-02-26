
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <assert.h>
#include <ccan/alignof/alignof.h>
#include <ccan/likely/likely.h>
#include <ccan/list/list.h>
#include <ccan/htable/htable.h>

#include <ukernel/mm.h>
#include <ukernel/slab.h>
#include <ukernel/misc.h>
#include <ukernel/space.h>
#include <ukernel/thread.h>


struct thread *current_thread = NULL;

static struct kmem_cache *thread_slab = NULL;
static struct htable thread_hash = HTABLE_INITIALIZER(thread_hash,
	hash_thread_by_id, NULL);
static struct list_head thread_list = LIST_HEAD_INIT(thread_list),
	dead_thread_list = LIST_HEAD_INIT(dead_thread_list);


struct thread *init_threading(thread_id boot_tid)
{
	assert(thread_slab == NULL);
	thread_slab = kmem_cache_create("thread_slab", sizeof(struct thread),
		ALIGNOF(struct thread), 0, NULL, NULL);

	struct thread *boot = kmem_cache_alloc(thread_slab);
	boot->stack_page = NULL;
	boot->id = boot_tid;
	boot->status = TS_RUNNING;
	list_add(&thread_list, &boot->link);
	htable_add(&thread_hash, int_hash(TID_THREADNUM(boot->id)), boot);

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
	printf("%s: thread %d:%d terminating\n", __func__, TID_THREADNUM(self->id),
		TID_VERSION(self->id));
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
	htable_del(&thread_hash, int_hash(TID_THREADNUM(self->id)), self);
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
	thread_id tid,
	void (*function)(void *),
	void *parameter)
{
	assert((tid & TID_VERSION_MASK) != 0);
	/* ThreadControl semantics are handled by caller. */
	assert(thread_find(tid) == NULL);

	if(unlikely(thread_slab == NULL)) {
		panic("create_thread() called before init_threading()");
	}

	struct thread *t = NULL;
	if(list_empty(&dead_thread_list)) {
		t = kmem_cache_alloc(thread_slab);
		t->stack_page = get_kern_page();
	} else {
		t = container_of(dead_thread_list.n.next, struct thread, link);
		list_del_from(&dead_thread_list, &t->link);

		/* TODO: could dispose other threads here. */
	}
	t->id = tid;
	t->status = TS_READY;

	void **stk_top = t->stack_page->vm_addr + PAGE_SIZE - 16;
	stk_top[0] = &thread_wrapper;
	stk_top[1] = (void *)0xdeadbeef;
	stk_top[2] = function;
	stk_top[3] = parameter;
	t->ctx.regs[7] = (uintptr_t)stk_top;

	list_add(&thread_list, &t->link);
	htable_add(&thread_hash, int_hash(TID_THREADNUM(t->id)), t);

	return t;
}


void thread_set_space(struct thread *t, struct space *sp)
{
	if(t->space != NULL) {
		list_del_from(&t->space->threads, &t->space_link);
		t->space = NULL;
	}
	space_add_thread(sp, t);
}


static bool cmp_thread_to_id(const void *cand, void *ptr)
{
	const struct thread *t = cand;
	thread_id *tid = ptr;
	return TID_THREADNUM(t->id) == TID_THREADNUM(*tid);
}


struct thread *thread_find(thread_id tid)
{
	struct thread *t = htable_get(&thread_hash, int_hash(TID_THREADNUM(tid)),
		&cmp_thread_to_id, &tid);
	return t;
}


size_t hash_thread_by_id(const void *ptr, void *dataptr) {
	const struct thread *t = ptr;
	return int_hash(TID_THREADNUM(t->id));
}
