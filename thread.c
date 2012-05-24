
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


#define TRACE_VERBOSE 0		/* 1 for exregs, threadctl, threadswitch prints */

#if TRACE_VERBOSE
#define TRACE(fmt, ...) printf(fmt, __VA_ARGS__)
#else
#define TRACE(fmt, ...)
#endif


struct htable thread_hash = HTABLE_INITIALIZER(thread_hash,
	hash_thread_by_id, NULL);

/* NOTE: is dead_thread_list referenced by any other module? it's only for
 * dead kthreads after all.
 */
struct list_head dead_thread_list = LIST_HEAD_INIT(dead_thread_list);

static struct kmem_cache *thread_slab = NULL;
static int next_kthread_num = 24;


static void init_kthread_ctx(struct thread *t, L4_Word_t sp, L4_Word_t ip)
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


COLD struct thread *init_threading(thread_id boot_tid)
{
	assert(thread_slab == NULL);
	thread_slab = kmem_cache_create("thread_slab", sizeof(struct thread),
		ALIGNOF(struct thread), 0, NULL, NULL);

	struct thread *boot = kmem_cache_zalloc(thread_slab);
	init_kthread_ctx(boot, 0xdeadf123, 0xdeade123);
	boot->stack_page = NULL;
	boot->id = boot_tid;
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

	current_thread = boot;
	scheduler_thread = boot;

	return boot;
}


void yield(struct thread *t)
{
	struct thread *current = get_current_thread();
	current->status = TS_READY;
	assert(current->wakeup_time < read_global_timer() * 1000);

	/* TODO: switch to "t" */

	schedule();
}


static void restore_saved_regs(struct thread *t, void *priv)
{
	if(t == NULL) return;
	assert(t->saved_mrs > 0 || t->saved_brs > 0);

	void *utcb = thread_get_utcb(t);
	memcpy(&L4_VREG(utcb, L4_TCR_MR(0)), t->saved_regs,
		sizeof(L4_Word_t) * (int)t->saved_mrs);
	memcpy(&L4_VREG(utcb, L4_TCR_BR(0)), &t->saved_regs[t->saved_mrs],
		sizeof(L4_Word_t) * (int)t->saved_brs);
	t->saved_mrs = 0;
	t->saved_brs = 0;

	t->post_exn_call = NULL;
}


void save_ipc_regs(struct thread *t, int mrs, int brs)
{
	assert(t->post_exn_call == NULL);
	assert(t->saved_mrs == 0 && t->saved_brs == 0);
	assert(mrs >= 1 && brs >= 0);
	assert(mrs + brs <= sizeof(t->saved_regs) / sizeof(t->saved_regs[0]));

	t->saved_mrs = mrs;
	t->saved_brs = brs;
	void *utcb = thread_get_utcb(t);
	memcpy(t->saved_regs, &L4_VREG(utcb, L4_TCR_MR(0)),
		sizeof(L4_Word_t) * mrs);
	memcpy(&t->saved_regs[mrs], &L4_VREG(utcb, L4_TCR_BR(0)),
		sizeof(L4_Word_t) * brs);

	t->post_exn_call = &restore_saved_regs;
	t->exn_priv = NULL;
}


bool post_exn_ok(struct thread *t)
{
	if(t->post_exn_call != NULL) {
		(*t->post_exn_call)(t, t->exn_priv);
		return true;
	} else {
		return false;
	}
}


bool post_exn_fail(struct thread *t)
{
	if(t->post_exn_call != NULL) {
		if(t->exn_priv != NULL) {
			/* (this is fancy so that the function doesn't need an exn_priv
			 * just to clear the callback.)
			 */
			void (*fn)(struct thread *, void *) = t->post_exn_call;
			t->post_exn_call = NULL;
			(*fn)(NULL, t->exn_priv);
		}
		return true;
	} else {
		return false;
	}
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
		t = container_of(dead_thread_list.n.next, struct thread,
			dead_link);
		list_del_from(&dead_thread_list, &t->dead_link);
	}
	*t = (struct thread){
		.id = tid,
		.status = TS_STOPPED,
		.utcb_pos = -1,
		.pri = 100, .sens_pri = 100,
		.max_delay = 0,
		.ts_len = L4_TimePeriod(10000),		/* 10 ms */
		.quantum = 0,
		.total_quantum = 0,

		/* x86 malarkey for non-kernel threads. */
		.ctx = {
			/* IOPL 3 (peon), interrupts enabled. also a reserved, constant
			 * bit is set.
			 */
			.eflags = (3 << 12) | (1 << 9) | (1 << 1),
			.es = SEG_USER_DATA << 3 | 0x3,
			.ds = SEG_USER_DATA << 3 | 0x3,
			.cs = SEG_USER_CODE << 3 | 0x3,
			.ss = SEG_USER_DATA << 3 | 0x3,
		},
	};

	htable_add(&thread_hash, int_hash(TID_THREADNUM(t->id)), t);

	return t;
}


static void thread_destroy(struct thread *t)
{
	assert(t->status == TS_DEAD || t->status == TS_STOPPED);
	assert(t->post_exn_call == NULL);

	struct space *sp = t->space;

	if(t->utcb_ptr_seg != 0) {
		release_gdt_ptr_seg(L4_Address(sp->utcb_area)
			+ t->utcb_pos * UTCB_SIZE + 256 - 4, t->utcb_ptr_seg);
		t->utcb_ptr_seg = 0;
	}

	if(t->stack_page != NULL) {
		free_kern_page(t->stack_page);
		t->stack_page = NULL;
	}

	space_remove_thread(sp, t);
	bool ok = htable_del(&thread_hash, int_hash(TID_THREADNUM(t->id)), t);
	if(unlikely(!ok)) {
		printf("WARNING: %s: thread %d:%d not found in thread_hash\n",
			__func__, TID_THREADNUM(t->id), TID_VERSION(t->id));
	}
	kmem_cache_free(thread_slab, t);
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

	t->ctx.esp = sp;
	t->ctx.eip = ip;
}


void thread_set_utcb(struct thread *t, L4_Word_t start)
{
	assert(t->space != NULL);
	assert(!L4_IsNilFpage(t->space->utcb_area));
	assert((start & (UTCB_SIZE - 1)) == 0);

	struct space *sp = t->space;

	if(t->utcb_ptr_seg != 0) {
		release_gdt_ptr_seg(L4_Address(sp->utcb_area)
			+ t->utcb_pos * UTCB_SIZE + 256 - 4, t->utcb_ptr_seg);
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
		assert(t->utcb_ptr_seg == 0);
		t->utcb_ptr_seg = reserve_gdt_ptr_seg(start + 256 - 4);
	}
}


void thread_start(struct thread *t)
{
	assert(t->status == TS_STOPPED);

	t->status = TS_READY;
	t->wakeup_time = 0;
	sq_insert_thread(t);
}


void thread_stop(struct thread *t)
{
	assert(t->status != TS_STOPPED);

	/* interrupt IPC in progress */
	switch(t->status) {
		case TS_R_RECV:
		case TS_RECV_WAIT:
		case TS_SEND_WAIT:
			post_exn_fail(t);
			break;

		/* TODO: string transfer send/recv states */

		default:
			break;
	}

	sq_remove_thread(t);
	t->status = TS_STOPPED;

	if(t == get_current_thread()) {
		if(IS_KERNEL_THREAD(t)) schedule();
		/* otherwise, rely on the caller to invoke the scheduler */
	}
}


uint64_t wakeup_at(L4_Time_t period)
{
	if(period.raw == L4_ZeroTime.raw) return 0;
	else if(period.raw == L4_Never.raw) return ~(uint64_t)0;
	else return read_global_timer() * 1000 + time_in_us(period);
}


void thread_sleep(struct thread *t, L4_Time_t period)
{
	assert(t->status == TS_SEND_WAIT || t->status == TS_RECV_WAIT);

	if(period.raw != L4_ZeroTime.raw && period.raw != L4_Never.raw) {
		TRACE("%s: sleeping thread %d:%d for %llu microseconds\n", __func__,
			TID_THREADNUM(t->id), TID_VERSION(t->id), time_in_us(period));
	}
	t->wakeup_time = wakeup_at(period);
	if(period.raw == L4_ZeroTime.raw) {
		/* extreme napping */
		t->status = TS_READY;
	}
	sq_update_thread(t);
}


void thread_wake(struct thread *t)
{
	assert(t->status != TS_STOPPED);
	assert(t->status != TS_DEAD);

	t->status = TS_READY;
	t->wakeup_time = 0;
	sq_update_thread(t);
}


void *thread_get_utcb(struct thread *t)
{
	assert(t->space != NULL);
	assert(t->utcb_pos >= 0);
	assert(t->utcb_pos < NUM_UTCB_PAGES(t->space->utcb_area) * UTCB_PER_PAGE);

	int page_ix = t->utcb_pos / UTCB_PER_PAGE,
		offset = t->utcb_pos & (UTCB_PER_PAGE - 1);
	struct page *p = t->space->utcb_pages[page_ix];
	assert(p->vm_addr != NULL);
	/* the UTCB pointer starts with the kernel-defined MR0 slot, and has at
	 * least 200 bytes available at negative offsets.
	 */
	return p->vm_addr + offset * UTCB_SIZE + 256;
}


void thread_save_ctx(struct thread *t, const struct x86_exregs *regs)
{
	size_t flen = x86_frame_len(regs);
	memcpy(&t->ctx, regs, flen);
	if(flen < sizeof(*regs)) {
		assert(IS_KERNEL_THREAD(t));
		t->ctx.ss = regs->ds;
		t->ctx.esp = (L4_Word_t)regs + flen;
#if 0
		printf("%s: saved kernel thread %d:%d: eip %#x, esp %#x\n", __func__,
			TID_THREADNUM(t->id), TID_VERSION(t->id),
			t->ctx.eip, t->ctx.esp);
#endif
	} else {
		assert(!IS_KERNEL_THREAD(t));
#if 0
		printf("%s: saved user thread %d:%d: eip %#x, esp %#x\n", __func__,
			TID_THREADNUM(t->id), TID_VERSION(t->id),
			t->ctx.eip, t->ctx.esp);
#endif
	}
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


static void receive_breath_of_life(struct thread *t, void *priv)
{
	if(t == NULL) return;

	assert(t->post_exn_call == &receive_breath_of_life);
	t->post_exn_call = NULL;
	t->exn_priv = NULL;

	void *utcb = thread_get_utcb(t);
	L4_MsgTag_t tag = { .raw = L4_VREG(utcb, L4_TCR_MR(0)) };
	TRACE("%s: in thread %d:%d, tag %#x\n", __func__,
		TID_THREADNUM(t->id), TID_VERSION(t->id),
		tag.raw);
	if(tag.X.u != 2 || tag.X.t != 0) return;

	L4_Word_t ip = L4_VREG(utcb, L4_TCR_MR(1)),
		sp = L4_VREG(utcb, L4_TCR_MR(2));
	TRACE("%s: setting sp %#x, ip %#x\n", __func__, sp, ip);
	thread_set_spip(t, sp, ip);
	/* the exception IPC mechanism starts the thread. */
}


/* system calls */

/* exregs control bitmasks (W RCdh pufi sSRH) */
#define CTL_H 0x001
#define CTL_R 0x002
#define CTL_S 0x004
#define CTL_s 0x008
#define CTL_i 0x010
#define CTL_f 0x020
#define CTL_u 0x040
#define CTL_p 0x080
#define CTL_h 0x100
#define CTL_d 0x200
#define CTL_XFER_MASK (0x1c00)


L4_Word_t sys_exregs(
	L4_ThreadId_t dest,
	L4_Word_t *control_p,
	L4_Word_t *sp_p, L4_Word_t *ip_p, L4_Word_t *flags_p,
	L4_Word_t *udh_p,
	L4_ThreadId_t *pager_p)
{
	struct thread *current = get_current_thread(), *dest_thread = NULL;

#if 0
	printf("%s: called from %d:%d on %d:%d (state %d); control %#x (", __func__,
		TID_THREADNUM(current->id), TID_VERSION(current->id),
		TID_THREADNUM(dest.raw), TID_VERSION(dest.raw), current->status,
		*control_p);
	const char *ctl_chars = "HRSsifuphd";
	for(int i=0; ctl_chars[i] != '\0'; i++) {
		if(CHECK_FLAG(*control_p, 1 << i)) printf("%c", ctl_chars[i]);
	}
	printf(")\n");
#endif

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

	if(unlikely(CHECK_FLAG_ANY(*control_p, CTL_XFER_MASK))) {
		TRACE("%s: control transfer items are not supported by this microkernel\n",
			__func__);
		goto fail;
	}

	void *dest_utcb;
	if(CHECK_FLAG_ANY(*control_p, CTL_R | CTL_S | CTL_p | CTL_u | CTL_d)) {
		dest_utcb = thread_get_utcb(dest_thread);
	} else {
		dest_utcb = NULL;
	}

	L4_Word_t ctl_in = *control_p;
	if(CHECK_FLAG(ctl_in, CTL_R)) {
		/* abort receive. */
		/* TODO: check for the "currently receiving" state */
		/* ... and move this into ipc.c, & also the one for the send side */
		int state = dest_thread->status;
		if(state == TS_R_RECV || state == TS_RECV_WAIT) {
			bool halted = CHECK_FLAG(dest_thread->flags, TF_HALT);
			if(halted) thread_stop(dest_thread);
			else thread_wake(dest_thread);
			dest_thread->ipc_from = L4_nilthread;
			/* "canceled in receive phase" */
			assert(dest_utcb != NULL);
			L4_VREG(dest_utcb, L4_TCR_ERRORCODE) = 1 | (3 << 1);
			L4_VREG(dest_utcb, L4_TCR_MR(0)) = (L4_MsgTag_t){ .X.flags = 0x8 }.raw;

			/* that covers exceptions, too. */
			if(dest_thread->post_exn_call != NULL) {
				(*dest_thread->post_exn_call)(NULL,
					dest_thread->exn_priv);
				dest_thread->post_exn_call = NULL;
				dest_thread->exn_priv = NULL;
			}

			TRACE("%s: aborted receive\n", __func__);
		}
		ctl_in &= ~CTL_R;
	}

	if(CHECK_FLAG(ctl_in, CTL_S)) {
		/* abort send. */
		/* TODO: check for the "currently sending" state */
		if(dest_thread->status == TS_SEND_WAIT) {
			bool halted = CHECK_FLAG(dest_thread->flags, TF_HALT);
			if(halted) thread_stop(dest_thread);
			else thread_wake(dest_thread);

			dest_thread->ipc_from = L4_nilthread;
			/* "canceled in send phase" */
			assert(dest_utcb != NULL);
			L4_VREG(dest_utcb, L4_TCR_ERRORCODE) = 0 | (3 << 1);
			L4_VREG(dest_utcb, L4_TCR_MR(0)) = (L4_MsgTag_t){ .X.flags = 0x8 }.raw;

			TRACE("%s: aborted send\n", __func__);
		}
		ctl_in &= ~CTL_S;
	}

	if(CHECK_FLAG(ctl_in, CTL_h)) {
		int state = dest_thread->status;
		if(!CHECK_FLAG(ctl_in, CTL_H)) {
			dest_thread->flags &= ~TF_HALT;
			if(state == TS_STOPPED) {
				TRACE("%s: starting halted thread\n", __func__);
				thread_start(dest_thread);
			}
		} else {
			dest_thread->flags |= TF_HALT;
			if(IS_READY(dest_thread->status) || state == TS_RUNNING) {
				thread_stop(dest_thread);
				TRACE("%s: stopped running thread\n", __func__);
			}
		}

		ctl_in &= ~(CTL_H | CTL_h);
	}

	/* register-setting control bits. */
	const L4_Word_t regset_mask = CTL_p | CTL_u | CTL_f | CTL_i | CTL_s;
	if(CHECK_FLAG_ANY(ctl_in, regset_mask)) {
		if(CHECK_FLAG(ctl_in, CTL_p)) {
			assert(dest_utcb != NULL);
			L4_VREG(dest_utcb, L4_TCR_PAGER) = pager_p->raw;
		}
		if(CHECK_FLAG(ctl_in, CTL_u)) {
			assert(dest_utcb != NULL);
			L4_VREG(dest_utcb, L4_TCR_USERDEFINEDHANDLE) = *udh_p;
		}
		if(CHECK_FLAG(ctl_in, CTL_f)) dest_thread->ctx.eflags = *flags_p;
		if(CHECK_FLAG(ctl_in, CTL_i)) dest_thread->ctx.eip = *ip_p;
		if(CHECK_FLAG(ctl_in, CTL_s)) dest_thread->ctx.esp = *sp_p;

		ctl_in &= ~regset_mask;
	}

	if(CHECK_FLAG(ctl_in, CTL_d)) {
		/* readout */
		TRACE("%s: my brain cannot handle readouts. poor me\n", __func__);
		panic("hfasdjfa skhfd jahskdjf lhakjs");
		ctl_in &= ~CTL_d;
	}

	if(unlikely(ctl_in != 0)) {
		printf("%s: unhandled ExchangeRegister control bits %#x\n",
			__func__, ctl_in);
		goto fail;
	}

end:
	return result.raw;

fail:
	result = L4_nilthread;
	/* HAIL SATAN errday */
	L4_VREG(thread_get_utcb(current), L4_TCR_ERRORCODE) = 666;
	goto end;
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

	TRACE("%s: called; dest %d:%d, pager %d:%d, scheduler %d:%d, space %d:%d\n",
		__func__,
		TID_THREADNUM(dest_tid.raw), TID_VERSION(dest_tid.raw),
		TID_THREADNUM(pager.raw), TID_VERSION(pager.raw),
		TID_THREADNUM(scheduler.raw), TID_VERSION(scheduler.raw),
		TID_THREADNUM(spacespec.raw), TID_VERSION(spacespec.raw));
	TRACE("%s: utcb_loc %p\n", __func__, (void *)utcb_loc);
	result = 0;

	L4_Word_t ec = 0;
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

		TRACE("%s: creating thread %d:%d\n", __func__,
			TID_THREADNUM(dest_tid.raw), TID_VERSION(dest_tid.raw));
		dest = thread_new(dest_tid.raw);
		if(dest == NULL) {
			ec = 8;		/* "out of memory" */
			goto end;
		}
		struct space *sp = space_find(spacespec.raw);
		if(sp == NULL) sp = space_new();
		space_add_thread(sp, dest);
	} else if(L4_IsNilThread(spacespec) && dest != NULL) {
		/* thread/space deletion */
		thread_stop(dest);
		thread_destroy(dest);
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
		if(dest->status == TS_STOPPED && !L4_IsNilThread(pager)) {
			dest->ipc_from = pager;
			dest->ipc_to = L4_nilthread;
			dest->recv_timeout = L4_Never;
			dest->wakeup_time = ~(uint64_t)0;
			dest->status = TS_R_RECV;
			sq_insert_thread(dest);
			dest->post_exn_call = &receive_breath_of_life;
			dest->exn_priv = NULL;

			L4_VREG(dest_utcb, L4_TCR_EXCEPTIONHANDLER) = L4_nilthread.raw;
		}
	}

	result = 1;

end:
	L4_VREG(utcb, L4_TCR_ERRORCODE) = ec;
	regs->eax = result;
}
