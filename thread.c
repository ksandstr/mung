
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
#include <ukernel/gdt.h>
#include <ukernel/thread.h>


/* also referenced by sched.c . thread_list, dead_thread_list should be moved
 * into a per-cpu scheduler state structure anyway; the latter is only used by
 * terminated kthreads.
 */
struct htable thread_hash = HTABLE_INITIALIZER(thread_hash,
	hash_thread_by_id, NULL);
struct list_head thread_list = LIST_HEAD_INIT(thread_list),
	dead_thread_list = LIST_HEAD_INIT(dead_thread_list);

static struct kmem_cache *thread_slab = NULL;
static int next_kthread_num = 24;


COLD struct thread *init_threading(thread_id boot_tid)
{
	assert(thread_slab == NULL);
	thread_slab = kmem_cache_create("thread_slab", sizeof(struct thread),
		ALIGNOF(struct thread), 0, NULL, NULL);

	struct thread *boot = kmem_cache_zalloc(thread_slab);
	boot->stack_page = NULL;
	boot->id = boot_tid;
	boot->status = TS_RUNNING;
	list_add_tail(&thread_list, &boot->link);
	htable_add(&thread_hash, int_hash(TID_THREADNUM(boot->id)), boot);

	current_thread = boot;
	scheduler_thread = boot;

	return boot;
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
	end_kthread();
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
		t->stack_page = get_kern_page(0);
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
		t->stack_page = get_kern_page(0);
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

	if(t->utcb_ptr_seg != 0) {
		release_gdt_ptr_seg(L4_Address(sp->utcb_area)
			+ t->utcb_pos * UTCB_SIZE + 256, t->utcb_ptr_seg);
		t->utcb_ptr_seg = 0;
	}

	int new_pos = (start - L4_Address(sp->utcb_area)) / UTCB_SIZE;
	if(sp->utcb_pages == NULL) {
		sp->utcb_pages = calloc(sizeof(struct page *),
			NUM_UTCB_PAGES(sp->utcb_area));
	}

	/* (could call a space_ensure_utcb() function or something, but why.) */
	int page = new_pos / UTCB_PER_PAGE;
	assert(page < NUM_UTCB_PAGES(sp->utcb_area));
	if(sp->utcb_pages[page] == NULL) {
		struct page *p = get_kern_page(0);
		sp->utcb_pages[page] = p;
		/* TODO: list "p" somewhere? */
		if(likely(sp != kernel_space)) {
			space_put_page(sp, L4_Address(sp->utcb_area) + page * PAGE_SIZE,
				sp->utcb_pages[page]->id, L4_ReadWriteOnly);
			space_commit(sp);
		}
	}
	if(new_pos != t->utcb_pos) {
		int offset = new_pos - (page * UTCB_PER_PAGE);
		assert(sp->utcb_pages[page]->vm_addr != NULL);
		void *utcb_mem = sp->utcb_pages[page]->vm_addr + offset * UTCB_SIZE;
		memset(utcb_mem, 0, UTCB_SIZE);
		L4_VREG(utcb_mem + 256, L4_TCR_MYGLOBALID) = t->id;
		*(L4_Word_t *)(utcb_mem + 256 - 4) = start + 256;
	}

	t->utcb_pos = new_pos;
	assert(start == L4_Address(sp->utcb_area) + UTCB_SIZE * t->utcb_pos);
	if(likely(sp != kernel_space)) {
		t->utcb_ptr_seg = reserve_gdt_ptr_seg(start + 256 - 4);
	}
}


void thread_start(struct thread *t) {
	t->status = TS_READY;
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
	/* the UTCB pointer starts with the kernel-defined MR0 slot, and has at
	 * least 200 bytes available at negative offsets.
	 */
	return p->vm_addr + offset * UTCB_SIZE + 256;
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
