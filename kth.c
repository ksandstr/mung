
/* tricky cooperative unsafe kernel thread module. no provision for joining
 * threads, or for knowing whether a kth's <struct thread> is still valid or
 * not once at least one threadswitch has occurred from one kthread to
 * another.
 *
 * burn, baby, burn.
 */

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <ccan/compiler/compiler.h>
#include <ccan/list/list.h>
#include <ccan/htable/htable.h>
#include <ccan/likely/likely.h>

#include <ukernel/misc.h>
#include <ukernel/thread.h>
#include <ukernel/kip.h>
#include <ukernel/sched.h>
#include <ukernel/space.h>
#include <ukernel/kth.h>


static int next_kthread_num = -1;
static struct list_head dead_thread_list = LIST_HEAD_INIT(dead_thread_list);


static COLD void init_kthread_ctx(struct thread *t, L4_Word_t sp, L4_Word_t ip)
{
	t->ctx = (struct x86_ctx){
		.r.esp = sp,
		.eip = ip,
		/* IOPL 0 (supervisor), interrupts enabled. also a reserved, constant
		 * bit is set.
		 */
		.eflags = (0 << 12) | (1 << 9) | (1 << 1),
	};
}


/* FIXME: this function is entirely untested. there should be a kernel
 * self-test mode to fix that even if end_kthread() is never used in anger.
 */
static NORETURN void end_kthread(void)
{
	struct thread *self = get_current_thread();
	assert(IS_KERNEL_THREAD(self));

	space_remove_thread(self->space, self);
	htable_del(&thread_hash, int_hash(TID_THREADNUM(self->id)), self);
	list_add(&dead_thread_list, &self->u0.dead_link);
	self->status = TS_DEAD;
	sq_remove_thread(self);
	schedule();

	panic("schedule() returned to dead thread???");
}


static void thread_wrapper(void (*function)(void *), void *parameter)
{
	(*function)(parameter);
	end_kthread();
}


struct thread *kth_start(void (*function)(void *), void *parameter)
{
	assert(next_kthread_num > 0);	/* must be past init */
	L4_ThreadId_t tid = { .raw = THREAD_ID(next_kthread_num++, 1) };
	assert(L4_ThreadNo(tid) > last_int_threadno());
	assert(L4_ThreadNo(tid) < first_user_threadno());	/* out of kthreads */

	struct thread *t = list_pop(&dead_thread_list, struct thread,
		u0.dead_link);
	if(t != NULL) {
		struct page *stack_page = t->u1.stack_page;	/* stomped by ctor */
		if(!thread_ctor(t, tid.raw)) {
			printf("%s: ctor failed for %lu:%lu\n", __func__,
				L4_ThreadNo(tid), L4_Version(tid));
			return NULL;
		}
		assert(stack_page != NULL);
		t->u1.stack_page = stack_page;
		if(t->u1.stack_page->vm_addr == NULL) {
			/* TODO: map it in somewhere */
			panic("arrrrrgggghhhh!");
		}
	} else {
		t = thread_new(tid.raw);
		assert(t->u1.stack_page == NULL);
		/* TODO: account for this somehow? */
		t->u1.stack_page = get_kern_page(0);
	}
	assert(t->u1.stack_page->vm_addr != NULL);

	t->space = kernel_space;
	bool ok = thread_set_utcb(t, L4_Address(kernel_space->utcb_area)
		+ (TID_THREADNUM(tid.raw) - 1 - last_int_threadno()) * UTCB_SIZE);
	if(!ok) {
		free_kern_page(t->u1.stack_page); t->u1.stack_page = NULL;
		space_remove_thread(kernel_space, t);
		kmem_cache_free(thread_slab, t);
		return NULL;
	}

	void **stk_top = t->u1.stack_page->vm_addr + PAGE_SIZE - 32;
	stk_top[0] = function;
	stk_top[1] = parameter;
	t->ctx = (struct x86_ctx){
		.r.esp = (L4_Word_t)stk_top - 8,	/* fake return address bump, twice */
		.eip = (L4_Word_t)&thread_wrapper,
		/* IOPL 0 (supervisor), interrupts enabled. also a reserved, constant
		 * bit is set.
		 */
		.eflags = (0 << 12) | (1 << 9) | (1 << 1),
	};
	t->pri = 250;
	t->sens_pri = 250;
	thread_start(t);

	return t;
}


bool kth_yield(void)
{
	struct thread *current = get_current_thread();
	current->status = TS_READY;
	return schedule();
}


COLD struct thread *kth_init(L4_ThreadId_t boot_tid)
{
	assert(next_kthread_num < 0);
	next_kthread_num = L4_ThreadNo(boot_tid) + 1;

	struct thread *boot = kmem_cache_zalloc(thread_slab);
	init_kthread_ctx(boot, 0xdeadface, 0xabadc0de);
	boot->space = kernel_space;
	boot->utcb_pos = -1;
	boot->utcb_page = NULL;
	boot->utcb_ptr_seg = 0;
	thread_set_utcb(boot, L4_Address(kernel_space->utcb_area));
	boot->u1.stack_page = NULL;
	boot->id = boot_tid.raw;
	boot->status = TS_RUNNING;
	boot->flags = 0;
	boot->pri = 0xff;
	boot->sens_pri = 0xff;
	boot->max_delay = 0;
	boot->ts_len = L4_TimePeriod(10000);
	boot->quantum = ~0ul;
	boot->total_quantum = ~(uint64_t)0;
	htable_add(&thread_hash, int_hash(TID_THREADNUM(boot->id)), boot);
	sq_insert_thread(boot);

	return boot;
}
