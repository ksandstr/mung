
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
#include <ccan/htable/htable.h>
#include <ccan/likely/likely.h>

#include <ukernel/misc.h>
#include <ukernel/thread.h>
#include <ukernel/kip.h>
#include <ukernel/sched.h>
#include <ukernel/space.h>
#include <ukernel/kth.h>


static int next_kthread_num = -1;


static COLD void init_kthread_ctx(struct thread *t, L4_Word_t sp, L4_Word_t ip)
{
	int dsel = (is_kernel_high ? SEG_KERNEL_DATA_HIGH : SEG_KERNEL_DATA) << 3,
		csel = (is_kernel_high ? SEG_KERNEL_CODE_HIGH : SEG_KERNEL_CODE) << 3;
	t->ctx = (struct x86_exregs){
		.esp = sp, .eip = ip,
		/* IOPL 0 (supervisor), interrupts enabled. also a reserved, constant
		 * bit is set.
		 */
		.eflags = (0 << 12) | (1 << 9) | (1 << 1),
		.es = dsel, .ds = dsel, .ss = dsel,
		.cs = csel,
	};
}


/* FIXME: this function is entirely untested. there should be a kernel
 * self-test mode to fix that even if end_kthread() is never used in anger.
 */
static NORETURN void end_kthread(void)
{
	struct thread *self = get_current_thread();
	assert(IS_KERNEL_THREAD(self));

	list_del_from(&self->space->threads, &self->space_link);
	htable_del(&thread_hash, int_hash(TID_THREADNUM(self->id)), self);
	assert(self->u0.regs == NULL);
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

	struct thread *t = thread_new(tid.raw);
	if(t->stack_page == NULL) {
		/* TODO: account for this somehow? */
		t->stack_page = get_kern_page(0);
	} else {
		/* TODO: make t->stack_page->vm_addr valid
		 *
		 * FIXME: ... this panic() is valid because as of right now, kthreads
		 * never exit. which means end_kthread() is completely untested, too.
		 */
		panic("arrrrrgggghhhh!");
	}
	space_add_thread(kernel_space, t);
	bool ok = thread_set_utcb(t, L4_Address(kernel_space->utcb_area)
		+ (TID_THREADNUM(tid.raw) - 1 - last_int_threadno()) * UTCB_SIZE);
	if(!ok) {
		/* TODO: free t->stack_page */
		space_remove_thread(kernel_space, t);
		kmem_cache_free(thread_slab, t);
		return NULL;
	}

	void **stk_top = t->stack_page->vm_addr + PAGE_SIZE - 32;
	stk_top[0] = function;
	stk_top[1] = parameter;
	int dsel = (is_kernel_high ? SEG_KERNEL_DATA_HIGH : SEG_KERNEL_DATA) << 3,
		csel = (is_kernel_high ? SEG_KERNEL_CODE_HIGH : SEG_KERNEL_CODE) << 3;
	t->ctx = (struct x86_exregs){
		.esp = (L4_Word_t)stk_top - 8,	/* fake return address bump, twice */
		.eip = (L4_Word_t)&thread_wrapper,
		/* IOPL 0 (supervisor), interrupts enabled. also a reserved, constant
		 * bit is set.
		 */
		.eflags = (0 << 12) | (1 << 9) | (1 << 1),
		.es = dsel, .ds = dsel, .ss = dsel,
		.cs = csel,
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
	GUARD_INIT(boot, sched_rb_0);
	GUARD_INIT(boot, sched_rb_1);
	boot->stack_page = NULL;
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


#include <ukernel/ktest.h>

#if KTEST
static void returning_kth(void *param) {
	diag("%s running", __func__);
	*(bool *)param = true;
}


START_TEST(t_basics)
{
	plan_tests(2);

	bool *signal = malloc(sizeof(bool));
	*signal = false;
	diag("starting kthread");
	struct thread *t = kth_start(&returning_kth, signal);
	diag("t=%p", t);
	bool sched_ok = true;
	int calls = 0;
	while(!*signal && calls++ < 500) {
		sched_ok = sched_ok & kth_yield();
	}
	if(!ok(calls == 1, "called kth_yield() once")) {
		diag("calls=%d", calls);
	}
	ok(sched_ok, "scheduling selected another thread");

	free(signal);
}
END_TEST


void ktest_kth(void)
{
	RUN(t_basics);
}
#endif
