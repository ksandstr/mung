
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <assert.h>
#include <ccan/alignof/alignof.h>
#include <ccan/likely/likely.h>
#include <ccan/list/list.h>
#include <ccan/htable/htable.h>
#include <ccan/compiler/compiler.h>

#include <l4/types.h>
#include <l4/vregs.h>
#include <ukernel/x86.h>
#include <ukernel/mm.h>
#include <ukernel/slab.h>
#include <ukernel/ipc.h>
#include <ukernel/misc.h>
#include <ukernel/space.h>
#include <ukernel/thread.h>


#define IS_READY(st) ((st) == TS_READY || (st) == TS_R_RECV)


static struct thread *volatile current_thread = NULL;
static struct thread *scheduler_thread = NULL;

static struct kmem_cache *thread_slab = NULL;
static struct htable thread_hash = HTABLE_INITIALIZER(thread_hash,
	hash_thread_by_id, NULL);
static struct list_head thread_list = LIST_HEAD_INIT(thread_list),
	dead_thread_list = LIST_HEAD_INIT(dead_thread_list);

static int next_kthread_num = 24;


COLD struct thread *init_threading(thread_id boot_tid)
{
	assert(thread_slab == NULL);
	thread_slab = kmem_cache_create("thread_slab", sizeof(struct thread),
		ALIGNOF(struct thread), 0, NULL, NULL);

	struct thread *boot = kmem_cache_alloc(thread_slab);
	boot->stack_page = NULL;
	boot->id = boot_tid;
	boot->status = TS_RUNNING;
	list_add_tail(&thread_list, &boot->link);
	htable_add(&thread_hash, int_hash(TID_THREADNUM(boot->id)), boot);

	current_thread = boot;
	scheduler_thread = boot;

	return boot;
}


static struct thread *schedule_next_thread(struct thread *current)
{
	/* well, that's simple. */
	struct thread *next = NULL;
	list_for_each(&thread_list, next, link) {
		if(next != current && IS_READY(next->status)) return next;
	}

	return NULL;
}


static void end_thread(void)
{
	struct thread *self = get_current_thread();
	printf("%s: thread %d:%d terminating\n", __func__, TID_THREADNUM(self->id),
		TID_VERSION(self->id));

	list_del_from(&thread_list, &self->link);
	htable_del(&thread_hash, int_hash(TID_THREADNUM(self->id)), self);
	list_add(&dead_thread_list, &self->link);
	self->status = TS_DEAD;
	schedule();

	/* schedule() won't return to this thread due to it having been moved to
	 * dead_thread_list.
	 */

	panic("swap_context() in end_thread() returned???");
}


bool schedule(void)
{
	struct thread *self = get_current_thread();
	assert(self->space == kernel_space);
	assert(self->status != TS_RUNNING);

	/* find next ready thread. */
	struct thread *next = schedule_next_thread(self);
	assert(next != self);		/* by schedule_next_thread() def */
	if(next == NULL) return false;

	if(next->status == TS_R_RECV) {
		assert(!IS_KERNEL_THREAD(next));
		if(!ipc_recv_half(next)) {
			/* try again (passive receive) */
			assert(next->status == TS_RECV_WAIT);
			return schedule();
		}
	}

	printf("%s: %c-%c: %d:%d -> %d:%d\n", __func__,
		self->space == kernel_space ? 'K' : 'U',
		next->space == kernel_space ? 'K' : 'U',
		TID_THREADNUM(self->id), TID_VERSION(self->id),
		TID_THREADNUM(next->id), TID_VERSION(next->id));

	assert(self->space != NULL);
	assert(next->space != NULL);

	next->status = TS_RUNNING;
	current_thread = next;
	if(self->space != next->space) {
		if(unlikely(self->space == kernel_space)) {
			/* switch from kernel initial space */
			asm volatile ("movl %0, %%cr3"
				:: "a" (next->space->pdirs->id << 12)
				: "memory");
			swap_to_ring3(&self->ctx, &next->ctx);	/* go go goblin balls! */
		} else {
			/* from one userspace process to another */
			panic("WORP");
		}
	} else {
		/* intra-space switch. */
		if(self->space == kernel_space) {
			swap_context(&self->ctx, &next->ctx);
		} else {
			panic("WNAR");
		}
	}

	printf("%s: returned to %d:%d\n", __func__,
		TID_THREADNUM(current_thread->id), TID_VERSION(current_thread->id));

	assert(current_thread == self);
	assert(self->status == TS_RUNNING);

	return true;
}


void return_to_scheduler(struct x86_exregs *regs)
{
	struct thread *self = get_current_thread(),
		*next = scheduler_thread;
	assert(scheduler_thread != NULL);
	assert(self != scheduler_thread);

	printf("%s: %c-%c: %d:%d -> %d:%d\n", __func__,
		self->space == kernel_space ? 'K' : 'U',
		next->space == kernel_space ? 'K' : 'U',
		TID_THREADNUM(self->id), TID_VERSION(self->id),
		TID_THREADNUM(next->id), TID_VERSION(next->id));

	assert(self->status != TS_RUNNING);
	assert(next->status != TS_RUNNING);
	next->status = TS_RUNNING;
	current_thread = next;

#if 0
	printf("%s: returning to scheduler (kernel ip 0x%x, sp 0x%x)\n",
		__func__, next->ctx.regs[8], next->ctx.regs[7]);
#endif

	assert((next->ctx.regs[9] & (1 << 14)) == 0);
	iret_to_scheduler(&next->ctx);
}


void return_to_ipc(struct x86_exregs *regs, struct thread *target)
{
	ipc_simple(target);

	/* schedule the target next. */
	list_del_from(&thread_list, &target->link);
	list_add(&thread_list, &target->link);

	return_to_scheduler(regs);
}


void yield(struct thread *t)
{
	get_current_thread()->status = TS_READY;
	schedule();
}


void thread_save_exregs(
	struct thread *t,
	const struct x86_exregs *regs)
{
	t->ctx.regs[0] = regs->eax;
	t->ctx.regs[1] = regs->ebx;
	t->ctx.regs[2] = regs->ecx;
	t->ctx.regs[3] = regs->edx;
	t->ctx.regs[4] = regs->esi;
	t->ctx.regs[5] = regs->edi;
	t->ctx.regs[6] = regs->ebp;
	t->ctx.regs[7] = regs->esp;
	t->ctx.regs[8] = regs->eip;
	t->ctx.regs[9] = regs->eflags;
}


void save_ipc_regs(struct thread *t, int mrs, int brs)
{
	assert(t->saved_mrs == 0 && t->saved_brs == 0);
	assert(mrs >= 1 && brs >= 0);
	assert(mrs + brs < sizeof(t->saved_regs) / sizeof(t->saved_regs[0]));

	t->saved_mrs = mrs;
	t->saved_brs = brs;
	void *utcb = thread_get_utcb(t);
	memcpy(t->saved_regs, &L4_VREG(utcb, L4_TCR_MR(0)),
		sizeof(L4_Word_t) * mrs);
	memcpy(&t->saved_regs[mrs], &L4_VREG(utcb, L4_TCR_BR(0)),
		sizeof(L4_Word_t) * brs);
}


static void thread_wrapper(void (*function)(void *), void *parameter)
{
	(*function)(parameter);
	end_thread();
	for(;;) yield(NULL);
}


struct thread *thread_new(thread_id tid)
{
	assert(thread_find(tid) == NULL);
	if(unlikely(thread_slab == NULL)) {
		panic("thread_new() called before init_threading()");
	}

	struct thread *t;
	if(list_empty(&dead_thread_list)) {
		t = kmem_cache_alloc(thread_slab);
		t->stack_page = get_kern_page();
	} else {
		t = container_of(dead_thread_list.n.next, struct thread, link);
		list_del_from(&dead_thread_list, &t->link);
	}
	*t = (struct thread){
		.id = tid,
		.status = TS_STOPPED,
		.utcb_pos = -1,
	};

	list_add(&thread_list, &t->link);
	htable_add(&thread_hash, int_hash(TID_THREADNUM(t->id)), t);

	return t;
}


struct thread *create_kthread(
	void (*function)(void *),
	void *parameter)
{
	L4_ThreadId_t tid = { .raw = THREAD_ID(next_kthread_num++, 1) };
	assert(TID_THREADNUM(tid.raw) < NUM_KERNEL_THREADS);

	struct thread *t = thread_new(tid.raw);
	if(t->stack_page == NULL) {
		/* TODO: account for this somehow? */
		t->stack_page = get_kern_page();
	} else {
		/* TODO: make t->stack_page->vm_addr valid */
		panic("arrrrrgggghhhh!");
	}
	space_add_thread(kernel_space, t);
	thread_set_utcb(t, L4_Address(kernel_space->utcb_area)
		+ TID_THREADNUM(tid.raw) * UTCB_SIZE);

	/* switching into kernel threads ignores EIP (ctx.regs[8]) in favour of
	 * the ones saved on stack.
	 */
	void **stk_top = t->stack_page->vm_addr + PAGE_SIZE - 16;
	stk_top[0] = &thread_wrapper;
	stk_top[1] = (void *)0xdeadbeef;
	stk_top[2] = function;
	stk_top[3] = parameter;
	t->ctx.regs[7] = (uintptr_t)stk_top;
	t->status = TS_READY;

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


void thread_set_spip(struct thread *t, uintptr_t sp, uintptr_t ip)
{
	assert(!IS_KERNEL_THREAD(t));
	assert(t->status != TS_RUNNING);

	t->ctx.regs[7] = sp;
	t->ctx.regs[8] = ip;
}


void thread_set_utcb(struct thread *t, L4_Word_t start)
{
	assert(t->space != NULL);
	assert(!L4_IsNilFpage(t->space->utcb_area));
	assert((start & (UTCB_SIZE - 1)) == 0);

	struct space *sp = t->space;
	int new_pos = (start - L4_Address(sp->utcb_area)) / UTCB_SIZE;
	if(unlikely(sp->utcb_pages == NULL)) {
		sp->utcb_pages = calloc(sizeof(struct page *),
			NUM_UTCB_PAGES(sp->utcb_area));
	}

	/* (could call a space_ensure_utcb() function or something, but why.) */
	int page = new_pos / UTCB_PER_PAGE;
	assert(page < NUM_UTCB_PAGES(sp->utcb_area));
	if(sp->utcb_pages[page] == NULL) {
		struct page *p = get_kern_page();
		sp->utcb_pages[page] = p;
		/* TODO: list "p" somewhere? */
	}
	if(new_pos != t->utcb_pos) {
		int offset = new_pos - (page * UTCB_PER_PAGE);
		assert(sp->utcb_pages[page]->vm_addr != NULL);
		void *pos = sp->utcb_pages[page]->vm_addr + offset * UTCB_SIZE;
		memset(pos, 0, UTCB_SIZE);
	}

	t->utcb_pos = new_pos;
}


void thread_start(struct thread *t)
{
	t->status = TS_READY;
	list_add(&thread_list, &t->link);
}


void *thread_get_utcb(struct thread *t)
{
	assert(t->space != NULL);
	assert(t->utcb_pos >= 0);
	assert(t->utcb_pos < NUM_UTCB_PAGES(t->space->utcb_area));

	int page_ix = t->utcb_pos / UTCB_PER_PAGE,
		offset = t->utcb_pos & (UTCB_PER_PAGE - 1);
	struct page *p = t->space->utcb_pages[page_ix];
	if(unlikely(p->vm_addr == NULL)) {
		panic("UTCB page not mapped in kernel?? HALP");
	}
	return p->vm_addr + offset * UTCB_SIZE;
}


struct thread *get_current_thread(void) {
	return current_thread;
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
