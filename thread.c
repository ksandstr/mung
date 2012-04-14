
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
			L4_Fpage_t u_page = L4_FpageLog2(L4_Address(sp->utcb_area)
				+ page * PAGE_SIZE, PAGE_BITS);
			L4_Set_Rights(&u_page, L4_FullyAccessible);
			mapdb_add_map(&sp->mapdb, u_page, sp->utcb_pages[page]->id);
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
	assert(p->vm_addr != NULL);
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


/* system calls */

L4_Word_t sys_exregs(
	L4_ThreadId_t dest,
	L4_Word_t *control_p,
	L4_Word_t *sp_p, L4_Word_t *ip_p, L4_Word_t *flags_p,
	L4_Word_t *udh_p,
	L4_ThreadId_t *pager_p)
{
	struct thread *current = get_current_thread(), *dest_thread = NULL;

	if(!L4_IsNilThread(dest)) {
		if(dest.local.X.zeros == 0) {
			dest_thread = space_find_local_thread(current->space, dest.local);
		} else {
			dest_thread = thread_find(dest.global.raw);
		}
	}
	if(dest_thread == NULL || dest_thread->utcb_pos < 0
		|| dest_thread->space != current->space)
	{
		L4_VREG(thread_get_utcb(current), L4_TCR_ERRORCODE) = 2;
		return L4_nilthread.raw;
	}

	L4_ThreadId_t result;
	if(dest.local.X.zeros != 0) {
		result.local.raw = L4_Address(dest_thread->space->utcb_area)
			+ dest_thread->utcb_pos * UTCB_SIZE + 256;
		assert(result.local.X.zeros == 0);
	} else {
		result.global.raw = dest_thread->id;
		assert(result.local.X.zeros != 0);
	}

	if(*control_p != 0) {
		printf("%s: control is 0x%#x, but no flags are implemented\n",
			__func__, *control_p);
	}

	return result.raw;
}


void sys_threadswitch(struct x86_exregs *regs)
{
	L4_ThreadId_t target = { .raw = regs->eax };
	printf("%s: called; target %d:%d\n", __func__,
		TID_THREADNUM(target.raw), TID_VERSION(target.raw));
}


void sys_schedule(struct x86_exregs *regs)
{
	L4_ThreadId_t dest = { .raw = regs->eax };
	L4_Word_t timectl = regs->edx, procctl = regs->esi, prioctl = regs->ecx,
		preemptctl = regs->edi, result, old_timectl;

	printf("%s: called; dest %d:%d, procctl %#x, prioctl %#x\n", __func__,
		TID_THREADNUM(dest.raw), TID_VERSION(dest.raw), (unsigned)procctl,
		(unsigned)prioctl);
	printf("%s: ... timectl %#x, preemptctl %#x\n", __func__,
		(unsigned)timectl, (unsigned)preemptctl);
	old_timectl = 0;
	result = 0;

	regs->eax = result;
	regs->edx = old_timectl;
}


void sys_threadcontrol(struct x86_exregs *regs)
{
	L4_ThreadId_t dest_tid = { .raw = regs->eax },
		pager = { .raw = regs->ecx },
		scheduler = { .raw = regs->edx },
		spacespec = { .raw = regs->esi };
	L4_Word_t utcb_loc = regs->edi, result;
	struct thread *current = get_current_thread();
	/* TODO: check thread privilege */
	void *utcb = thread_get_utcb(current);

	printf("%s: called; dest %d:%d, pager %d:%d, scheduler %d:%d, space %d:%d\n",
		__func__,
		TID_THREADNUM(dest_tid.raw), TID_VERSION(dest_tid.raw),
		TID_THREADNUM(pager.raw), TID_VERSION(pager.raw),
		TID_THREADNUM(scheduler.raw), TID_VERSION(scheduler.raw),
		TID_THREADNUM(spacespec.raw), TID_VERSION(spacespec.raw));
	printf("%s: utcb_loc %p\n", __func__, (void *)utcb_loc);
	result = 0;

	L4_Word_t ec;
	if(TID_VERSION(dest_tid.raw) == 0) {
		ec = 2;		/* "unavailable thread" */
		goto end;
	}
	if(!L4_IsNilThread(spacespec)
		&& TID_THREADNUM(spacespec.raw) < NUM_KERNEL_THREADS)
	{
		ec = 3;		/* invalid space specifier */
		goto end;
	}

	struct thread *dest = thread_find(dest_tid.raw);
	bool is_new = false;
	if(!L4_IsNilThread(spacespec) && dest == NULL) {
		/* thread creation */
		if(L4_IsNilThread(scheduler)) {
			ec = 4;		/* invalid scheduler */
			goto end;
		}
		if(utcb_loc == ~0ul) {
			ec = 6;		/* invalid UTCB location */
			goto end;
		}

		printf("creating thread %d:%d\n", TID_THREADNUM(dest_tid.raw),
			TID_VERSION(dest_tid.raw));
		dest = thread_new(dest_tid.raw);
		if(dest == NULL) {
			ec = 8;		/* "out of memory" */
			goto end;
		}
		struct space *sp = space_find(spacespec.raw);
		if(sp == NULL) sp = space_new();
		space_add_thread(sp, dest);
		is_new = true;
	} else if(L4_IsNilThread(spacespec) && dest != NULL) {
		/* thread/space deletion */
		ec = 1;
	} else if(!L4_IsNilThread(spacespec) && dest != NULL) {
		/* modification only. (rest shared with creation.) */
		if(spacespec.raw != dest_tid.raw) {
			struct space *to_sp = space_find(spacespec.raw);
			if(to_sp != dest->space) {
				/* FIXME: needs a way to abort ongoing IPCs, and to remove
				 * threads from spaces.
				 */
				panic("TODO: movement of threads between spaces");
			}
		}
	} else {
		/* parameter fuckup. */
		ec = 2;
		goto end;
	}

	assert(dest != NULL);
	struct space *sp = dest->space;

	if(utcb_loc != ~0ul) {
		/* set utcb_pos. */
		if(utcb_loc < L4_Address(sp->utcb_area)
			|| utcb_loc + UTCB_SIZE > L4_Address(sp->utcb_area) + L4_Size(sp->utcb_area)
			|| (utcb_loc & (UTCB_SIZE - 1)) != 0)
		{
			ec = 6;
			goto end;
		}
		thread_set_utcb(dest, utcb_loc);
	}

	if(!L4_IsNilThread(scheduler)) dest->scheduler = scheduler;
	if(!L4_IsNilThread(pager)) {
		void *dest_utcb = thread_get_utcb(dest);
		L4_VREG(dest_utcb, L4_TCR_PAGER) = pager.raw;
		if(is_new) {
			printf("TODO: should make thread %d:%d wait for breath-of-life\n",
				TID_THREADNUM(dest->id), TID_VERSION(dest->id));
		}
	}

	result = 1;

end:
	L4_VREG(utcb, L4_TCR_ERRORCODE) = ec;
	regs->eax = result;
}
