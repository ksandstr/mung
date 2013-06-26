
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
#include <ukernel/sched.h>
#include <ukernel/trace.h>
#include <ukernel/bug.h>
#include <ukernel/kip.h>
#include <ukernel/thread.h>


/* for exregs, threadctl, threadswitch prints */
#define TRACE(fmt, ...) TRACE_MSG(TRID_THREAD, fmt, __VA_ARGS__)


struct htable thread_hash = HTABLE_INITIALIZER(thread_hash,
	hash_thread_by_id, NULL);

/* NOTE: is dead_thread_list referenced by any other module? it's only for
 * dead kthreads after all.
 */
struct list_head dead_thread_list = LIST_HEAD_INIT(dead_thread_list);

static struct kmem_cache *thread_slab = NULL;
static int next_kthread_num = 24;


#ifndef NDEBUG
#include <ukernel/invariant.h>

static bool check_thread(int opt, struct thread *t)
{
	INV_CTX;
	inv_push("thread %lu:%lu (%p)", TID_THREADNUM(t->id),
		TID_VERSION(t->id), t);
	inv_ok1(GUARD_CHECK(t, sched_rb_0));
	inv_ok1(GUARD_CHECK(t, sched_rb_1));
	inv_pop();
	return true;

inv_fail:
	return false;
}


static bool check_thread_module(int opt)
{
	struct htable_iter it;
	for(void *ptr = htable_first(&thread_hash, &it);
		ptr != NULL;
		ptr = htable_next(&thread_hash, &it))
	{
		if(!check_thread(opt, (struct thread *)ptr)) return false;
	}

	return true;
}
#endif


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
	GUARD_INIT(boot, sched_rb_0);
	GUARD_INIT(boot, sched_rb_1);
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

	assert(check_thread_module(0));

	return boot;
}


static void restore_saved_regs(struct hook *hook, uintptr_t code, void *priv)
{
	struct thread *t = container_of(hook, struct thread, post_exn_call);

	/* TODO: make this a TRACE(). it's useful for catching mis-nesting of
	 * restore_saved_regs() and save_ipc_regs(), i.e. spots where
	 * post_exn_{ok,fail}() isn't being called properly.
	 */
#if 0
	printf("%s: called for %lu:%lu (%d MRs, %d BRs) on %s\n", __func__,
		TID_THREADNUM(t->id), TID_VERSION(t->id),
		(int)t->saved_mrs, (int)t->saved_brs,
		code != 0 ? "IPC fail" : "IPC success");
#endif
	assert(t->saved_mrs > 0 || t->saved_brs > 0);

	void *utcb = thread_get_utcb(t);
	memcpy(&L4_VREG(utcb, L4_TCR_MR(0)), t->saved_regs,
		sizeof(L4_Word_t) * (int)t->saved_mrs);
	memcpy(&L4_VREG(utcb, L4_TCR_BR(t->saved_brs - 1)),
		&t->saved_regs[t->saved_mrs], sizeof(L4_Word_t) * (int)t->saved_brs);
	t->saved_mrs = 0;
	t->saved_brs = 0;

	hook_detach(hook);
}


void save_ipc_regs(struct thread *t, int mrs, int brs)
{
	/* TODO: see above */
#if 0
	if(t->saved_mrs != 0 || t->saved_brs != 0) {
		printf("%s: called for %lu:%lu (%d MRs, %d BRs) from %p\n", __func__,
			TID_THREADNUM(t->id), TID_VERSION(t->id), mrs, brs,
			__builtin_return_address(0));
	}
#endif
	assert(t->saved_mrs == 0 && t->saved_brs == 0);
	assert(mrs >= 1 && brs >= 0);
	assert(mrs + brs <= sizeof(t->saved_regs) / sizeof(t->saved_regs[0]));

	t->saved_mrs = mrs;
	t->saved_brs = brs;
	void *utcb = thread_get_utcb(t);
	memcpy(t->saved_regs, &L4_VREG(utcb, L4_TCR_MR(0)),
		sizeof(L4_Word_t) * mrs);
	memcpy(&t->saved_regs[mrs], &L4_VREG(utcb, L4_TCR_BR(brs - 1)),
		sizeof(L4_Word_t) * brs);

	hook_push_back(&t->post_exn_call, &restore_saved_regs, NULL);
}


bool post_exn_ok(struct thread *t)
{
	int num = hook_call_front(&t->post_exn_call, -1, false, 0);
	/* kernel IPC chains either keep shitting, or get off the pot. */
	assert(num == 0 || t->ipc != NULL || IS_READY(t->status));
	return num > 0;
}


bool post_exn_fail(struct thread *t)
{
	int num = hook_call_front(&t->post_exn_call, -1, false, 1);
	return num > 0;
}


static void thread_wrapper(void (*function)(void *), void *parameter)
{
	(*function)(parameter);
	end_kthread();
}


struct thread *thread_new(thread_id tid)
{
	/* keep thread_new() calls after init_threading() */
	assert(thread_slab != NULL);

	/* must be a currently not-existing thread number */
	assert(thread_find(tid) == NULL);

	struct thread *t = list_top(&dead_thread_list, struct thread, dead_link);
	if(t == NULL) {
		t = kmem_cache_alloc(thread_slab);
	} else {
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

	GUARD_INIT(t, sched_rb_0);
	GUARD_INIT(t, sched_rb_1);
	hook_init(&t->post_exn_call, NULL);
	htable_add(&thread_hash, int_hash(TID_THREADNUM(t->id)), t);

	return t;
}


static void thread_destroy(struct thread *t)
{
	assert(t->status == TS_DEAD || t->status == TS_STOPPED);
	assert(hook_empty(&t->post_exn_call));

	struct space *sp = t->space;
	if(t->utcb_ptr_seg != 0) {
		release_gdt_ptr_seg(L4_Address(sp->utcb_area)
			+ t->utcb_pos * UTCB_SIZE + 256 - 4, t->utcb_ptr_seg);
		t->utcb_ptr_seg = 0;
	}
	space_remove_thread(sp, t);

	if(unlikely(t->stack_page != NULL)) {
		assert(IS_KERNEL_THREAD(t));
		free_kern_page(t->stack_page);
		t->stack_page = NULL;
	}

	cop_killa(t);
	htable_del(&thread_hash, int_hash(TID_THREADNUM(t->id)), t);
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
	bool ok = thread_set_utcb(t, L4_Address(kernel_space->utcb_area)
		+ TID_THREADNUM(tid.raw) * UTCB_SIZE);
	if(unlikely(!ok)) {
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


void thread_set_space(struct thread *t, struct space *sp)
{
	if(t->space != NULL) {
		assert(t->space == sp);
		space_remove_thread(sp, t);
	}
	space_add_thread(sp, t);
}


void thread_set_spip(struct thread *t, L4_Word_t sp, L4_Word_t ip)
{
	assert(!IS_KERNEL_THREAD(t));
	assert(t->status != TS_RUNNING);

	t->ctx.esp = sp;
	t->ctx.eip = ip;
}


/* FIXME: check return values from calloc(), mapdb_add_map(), etc */
bool thread_set_utcb(struct thread *t, L4_Word_t start)
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
			mapdb_add_map(&sp->mapdb, 0, u_page, sp->utcb_pages[page]->id);
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
		if(unlikely(t->utcb_ptr_seg < 0)) {
			t->utcb_ptr_seg = 0;
			return false;
		}
	}

	return true;
}


void thread_start(struct thread *t)
{
	/* "freshly created" */
	assert(t->status == TS_STOPPED && !CHECK_FLAG(t->flags, TF_HALT));

	t->status = TS_READY;
	t->wakeup_time = 0;
	sq_insert_thread(t);
}


void thread_halt(struct thread *t)
{
	TRACE("%s: called for %lu:%lu from %p\n", __func__,
		TID_THREADNUM(t->id), TID_VERSION(t->id), __builtin_return_address(0));

	assert(!CHECK_FLAG(t->flags, TF_HALT));
	assert(t->status != TS_DEAD);

	t->flags |= TF_HALT;
	if(t->status == TS_READY || t->status == TS_RUNNING) {
		t->status = TS_STOPPED;
		sq_remove_thread(t);

		if(t == get_current_thread()) {
			if(IS_KERNEL_THREAD(t)) schedule();
			/* otherwise, rely on the caller to invoke the scheduler */
		}
	}
}


void thread_resume(struct thread *t)
{
	assert(CHECK_FLAG(t->flags, TF_HALT));

	t->flags &= ~TF_HALT;
	if(t->status == TS_STOPPED) {
		t->status = TS_READY;
		t->wakeup_time = 0;
		sq_insert_thread(t);
	}
}


uint64_t wakeup_at(L4_Time_t t)
{
	if(t.raw == L4_ZeroTime.raw) return 0;
	else if(t.raw == L4_Never.raw) return ~(uint64_t)0;
	else {
		L4_Clock_t base = { .raw = ksystemclock() };
		if(L4_IsTimePoint_NP(t)) {
			if(pt_is_valid(base, t)) {
				return L4_PointClock_NP(base, t).raw;
			} else {
				/* like it was ZeroTime. */
				return 0;
			}
		} else {
			return base.raw + time_in_us(t);
		}
	}
}


void thread_sleep(struct thread *t, L4_Time_t period)
{
	/* NOTE: this function was merged with thread_wake(), which asserted
	 * against {STOPPED, DEAD} rather than for the IPC wait states. callers
	 * should handle R_RECV separately as it doesn't instantly promote to
	 * RECV_WAIT.
	 */

#ifndef NDEBUG
	if(period.raw != L4_ZeroTime.raw && period.raw != L4_Never.raw) {
		TRACE("%s: sleeping thread %lu:%lu for %llu microseconds\n", __func__,
			TID_THREADNUM(t->id), TID_VERSION(t->id), time_in_us(period));
	} else if(period.raw == L4_Never.raw) {
		TRACE("%s: sleeping thread %lu:%lu for good\n", __func__,
			TID_THREADNUM(t->id), TID_VERSION(t->id));
	}
#endif

	if(period.raw == L4_ZeroTime.raw) {
		if(CHECK_FLAG(t->flags, TF_HALT)) {
			t->status = TS_STOPPED;
			sq_remove_thread(t);
		} else {
			/* extreme napping */
			if(t->status != TS_XFER) t->status = TS_READY;
			t->wakeup_time = 0;
			sq_update_thread(t);
		}
	} else {
#ifndef NDEBUG
		if(!IS_IPC_WAIT(t->status) && t->status != TS_XFER) {
			printf("%s: thread %lu:%lu status is %s\n", __func__,
				TID_THREADNUM(t->id), TID_VERSION(t->id),
				sched_status_str(t));
		}
#endif

		assert(IS_IPC_WAIT(t->status) || t->status == TS_XFER);
		t->wakeup_time = wakeup_at(period);
		sq_update_thread(t);
	}
}


void thread_ipc_fail(struct thread *t)
{
	assert(t->status == TS_RECV_WAIT
		|| t->status == TS_SEND_WAIT
		|| t->status == TS_R_RECV
		|| (t->status == TS_XFER && t->ipc != NULL));

	if(t->status == TS_SEND_WAIT) {
		/* (NOTE: also, $t->ipc != NULL \implies \
		 * Myself \notin \dom sendwait\_hash$, so this isn't actually
		 * required. abort_thread_ipc() accepts that, though.)
		 */
		abort_thread_ipc(t);
	}

	if(CHECK_FLAG(t->flags, TF_HALT)) {
		t->status = TS_STOPPED;
		sq_remove_thread(t);
	} else {
		t->status = TS_READY;
		if(t->wakeup_time > 0) {
			t->wakeup_time = 0;
			sq_update_thread(t);
		}
	}
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


static struct thread *get_tcr_thread(struct thread *t, void *utcb, int tcr)
{
	assert(utcb != NULL);
	L4_ThreadId_t tid = { .raw = L4_VREG(utcb, tcr) };
	return L4_IsNilThread(tid) ? NULL : resolve_tid_spec(t->space, tid);
}


struct thread *thread_get_pager(struct thread *t, void *utcb) {
	return get_tcr_thread(t, utcb, L4_TCR_PAGER);
}


struct thread *thread_get_exnh(struct thread *t, void *utcb) {
	return get_tcr_thread(t, utcb, L4_TCR_EXCEPTIONHANDLER);
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
		printf("%s: saved kernel thread %d:%d: eip %#lx, esp %#lx\n", __func__,
			TID_THREADNUM(t->id), TID_VERSION(t->id),
			t->ctx.eip, t->ctx.esp);
#endif
	} else {
		assert(!IS_KERNEL_THREAD(t));
#if 0
		printf("%s: saved user thread %d:%d: eip %#lx, esp %#lx\n", __func__,
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


struct thread *thread_find(thread_id tid) {
	return htable_get(&thread_hash, int_hash(TID_THREADNUM(tid)),
		&cmp_thread_to_id, &tid);
}


struct thread *resolve_tid_spec(
	struct space *ref_space,
	L4_ThreadId_t tid)
{
	if(L4_IsLocalId(tid)) {
		return space_find_local_thread(ref_space, tid.local);
	} else {
		struct thread *t = thread_find(tid.raw);
		return t != NULL && t->id == tid.raw ? t : NULL;
	}
}


size_t hash_thread_by_id(const void *ptr, void *dataptr) {
	const struct thread *t = ptr;
	return int_hash(TID_THREADNUM(t->id));
}


static void receive_breath_of_life(
	struct hook *hook,
	uintptr_t code,
	void *priv)
{
	hook_detach(hook);

	if(code == 0) {
		struct thread *t = container_of(hook, struct thread, post_exn_call);
		void *utcb = thread_get_utcb(t);
		L4_MsgTag_t tag = { .raw = L4_VREG(utcb, L4_TCR_MR(0)) };
		TRACE("%s: in thread %lu:%lu, tag %#lx\n", __func__,
			TID_THREADNUM(t->id), TID_VERSION(t->id),
			tag.raw);
		if(tag.X.u != 2 || tag.X.t != 0) return;

		L4_Word_t ip = L4_VREG(utcb, L4_TCR_MR(1)),
			sp = L4_VREG(utcb, L4_TCR_MR(2));
		TRACE("%s: setting sp %#lx, ip %#lx\n", __func__, sp, ip);
		thread_set_spip(t, sp, ip);

		thread_wake(t);
	}
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
	assert(check_thread_module(0));

	struct thread *current = get_current_thread();

#ifndef NDEBUG
	if(trace_is_enabled(TRID_THREAD)) {
		char ctl_buf[16];
		int cp = 0;
		const char *ctl_chars = "HRSsifuphd";
		for(int i=0; ctl_chars[i] != '\0'; i++) {
			if(CHECK_FLAG(*control_p, 1 << i)) ctl_buf[cp++] = ctl_chars[i];
		}
		ctl_buf[cp] = '\0';
		/* NOTE: this doesn't handle local TIDs at all well. */
		TRACE("%s: called from %lu:%lu on %lu:%lu; control %#lx (%s)\n",
			__func__,
			TID_THREADNUM(current->id), TID_VERSION(current->id),
			TID_THREADNUM(dest.raw), TID_VERSION(dest.raw),
			*control_p, ctl_buf);
	}
#endif

	struct thread *dest_thread;
	if(L4_IsNilThread(dest)
		|| (dest_thread = resolve_tid_spec(current->space, dest)) == NULL
		|| dest_thread->space != current->space
		|| dest_thread->utcb_pos < 0)
	{
		L4_VREG(thread_get_utcb(current), L4_TCR_ERRORCODE) = 2;
		return L4_nilthread.raw;
	}

	L4_ThreadId_t result;
	if(L4_IsGlobalId(dest)) {
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

	L4_Word_t ctl_in = *control_p, sp_in = *sp_p, ip_in = *ip_p,
		flags_in = *flags_p, udh_in = *udh_p;
	L4_ThreadId_t pager_in = *pager_p;

	if(CHECK_FLAG(ctl_in, CTL_d)) {
		/* readout */
		ctl_in &= ~CTL_d;

		*control_p = dest_thread->status == TS_STOPPED ? 1 : 0;	/* "H"alt */
		/* S, R never set because string transfers aren't implemented yet. */

		*sp_p = dest_thread->ctx.esp;
		*ip_p = dest_thread->ctx.eip;
		*flags_p = dest_thread->ctx.eflags;
		*udh_p = L4_VREG(dest_utcb, L4_TCR_USERDEFINEDHANDLE);
		pager_p->raw = L4_VREG(dest_utcb, L4_TCR_PAGER);

		TRACE("%s: delivered ExchangeRegisters values\n", __func__);
	}

	if(CHECK_FLAG(ctl_in, CTL_R)) {
		/* abort receive. */
		/* TODO: check for the "currently receiving" state */
		/* ... and move this into ipc.c, & also the one for the send side */
		int state = dest_thread->status;
		if(state == TS_R_RECV || state == TS_RECV_WAIT) {
			thread_ipc_fail(dest_thread);
			dest_thread->ipc_from = L4_nilthread;
			/* "canceled in receive phase", but only communicate it for
			 * non-exception IPC.
			 */
			if(!post_exn_fail(dest_thread)) {
				assert(dest_utcb != NULL);
				L4_VREG(dest_utcb, L4_TCR_ERRORCODE) = 1 | (3 << 1);
				L4_VREG(dest_utcb, L4_TCR_MR(0)) = (L4_MsgTag_t){ .X.flags = 0x8 }.raw;
			}

			TRACE("%s: aborted receive\n", __func__);
		}
		ctl_in &= ~CTL_R;
	}

	if(CHECK_FLAG(ctl_in, CTL_S)) {
		/* abort send. */
		/* TODO: check for the "currently sending" state */
		if(dest_thread->status == TS_SEND_WAIT) {
			thread_ipc_fail(dest_thread);

			dest_thread->ipc_from = L4_nilthread;
			/* "canceled in send phase", but see comment above */
			if(!post_exn_fail(dest_thread)) {
				assert(dest_utcb != NULL);
				L4_VREG(dest_utcb, L4_TCR_ERRORCODE) = 0 | (3 << 1);
				L4_VREG(dest_utcb, L4_TCR_MR(0)) = (L4_MsgTag_t){ .X.flags = 0x8 }.raw;
			}

			TRACE("%s: aborted send\n", __func__);
		}
		ctl_in &= ~CTL_S;
	}

	if(CHECK_FLAG(ctl_in, CTL_h)) {
		int state = dest_thread->status;
		if(!CHECK_FLAG(ctl_in, CTL_H)) {
			if(!CHECK_FLAG(dest_thread->flags, TF_HALT)
				&& state == TS_STOPPED)
			{
				TRACE("%s: starting fresh thread\n", __func__);
				thread_start(dest_thread);
			} else if(CHECK_FLAG(dest_thread->flags, TF_HALT)) {
				TRACE("%s: calling thread_resume() on state %d\n",
					__func__, state);
				thread_resume(dest_thread);
			}
		} else {
			if(!CHECK_FLAG(dest_thread->flags, TF_HALT)) {
				thread_halt(dest_thread);
				assert(dest_thread->status == TS_STOPPED
					|| dest_thread->status == TS_RECV_WAIT
					|| dest_thread->status == TS_SEND_WAIT);
				TRACE("%s: halted thread\n", __func__);
			}
		}

		ctl_in &= ~(CTL_H | CTL_h);
	}

	/* register-setting control bits. */
	const L4_Word_t regset_mask = CTL_p | CTL_u | CTL_f | CTL_i | CTL_s;
	if(CHECK_FLAG_ANY(ctl_in, regset_mask)) {
		if(CHECK_FLAG(ctl_in, CTL_p)) {
			assert(dest_utcb != NULL);
			L4_VREG(dest_utcb, L4_TCR_PAGER) = pager_in.raw;
		}
		if(CHECK_FLAG(ctl_in, CTL_u)) {
			assert(dest_utcb != NULL);
			L4_VREG(dest_utcb, L4_TCR_USERDEFINEDHANDLE) = udh_in;
		}
		if(CHECK_FLAG(ctl_in, CTL_f)) dest_thread->ctx.eflags = flags_in;
		if(CHECK_FLAG(ctl_in, CTL_i)) dest_thread->ctx.eip = ip_in;
		if(CHECK_FLAG(ctl_in, CTL_s)) dest_thread->ctx.esp = sp_in;

		ctl_in &= ~regset_mask;
	}

	if(unlikely(ctl_in != 0)) {
		printf("%s: unhandled ExchangeRegister control bits %#lx\n",
			__func__, ctl_in);
		goto fail;
	}

end:
	assert(check_thread_module(0));
	return result.raw;

fail:
	result = L4_nilthread;
	/* HAIL SATAN errday */
	L4_VREG(thread_get_utcb(current), L4_TCR_ERRORCODE) = 666;
	goto end;
}


/* TODO: make this function atomic on error? it seems a proper microkernel
 * should be like that.
 */
void sys_threadcontrol(struct x86_exregs *regs)
{
	assert(check_thread_module(0));

	L4_ThreadId_t dest_tid = { .raw = regs->eax },
		pager = { .raw = regs->ecx },
		scheduler = { .raw = regs->edx },
		spacespec = { .raw = regs->esi };
	L4_Word_t utcb_loc = regs->edi, result = 0, ec = 0;
	struct thread *current = get_current_thread();
	void *utcb = thread_get_utcb(current);
	if(unlikely(!CHECK_FLAG(current->space->flags, SF_PRIVILEGE))) {
		ec = 1;		/* "no privilege" */
		goto end;
	}

	TRACE("%s: called; dest %lu:%lu, pager %lu:%lu, scheduler %lu:%lu, space %lu:%lu\n",
		__func__,
		L4_ThreadNo(dest_tid), L4_Version(dest_tid),
		L4_ThreadNo(pager), L4_Version(pager),
		L4_ThreadNo(scheduler), L4_Version(scheduler),
		L4_ThreadNo(spacespec), L4_Version(spacespec));
	TRACE("%s: utcb_loc %p\n", __func__, (void *)utcb_loc);

	if(unlikely(L4_IsLocalId(dest_tid)
		|| L4_ThreadNo(dest_tid) < first_user_threadno()))
	{
		ec = 2;		/* "unavailable thread" */
		goto end;
	}
	if(!L4_IsNilThread(spacespec)
		&& unlikely(L4_ThreadNo(spacespec) < NUM_KERNEL_THREADS
			|| L4_Version(spacespec) == 0))
	{
		goto invd_space;
	}

	struct thread *dest = thread_find(dest_tid.raw);
	assert(dest == NULL || dest->space != NULL);
	if(!L4_IsNilThread(spacespec) && dest == NULL) {
		/* thread creation */
		if(unlikely(L4_IsNilThread(scheduler))) goto invd_sched;

		/* validate the scheduler before space/thread creation. */
		struct thread *sched = resolve_tid_spec(current->space, scheduler);
		if(unlikely(sched == NULL)) goto invd_sched;

		TRACE("%s: creating thread %lu:%lu\n", __func__,
			L4_ThreadNo(dest_tid), L4_Version(dest_tid));
		struct space *sp = space_find(spacespec.raw);
		bool new_space = false;
		if(sp == NULL && spacespec.raw != dest_tid.raw) goto invd_space;
		else if(sp == NULL) {
			sp = space_new();
			if(sp == NULL) goto out_of_mem;
			new_space = true;
		}

		dest = thread_new(dest_tid.raw);
		if(dest == NULL) {
			if(new_space) space_free(sp);
			goto out_of_mem;
		}
		space_add_thread(sp, dest);

		/* resolves local TID, avoids second call to resolve_tid_spec() */
		dest->scheduler.raw = sched->id;
		scheduler = L4_nilthread;
	} else if(L4_IsNilThread(spacespec) && dest != NULL) {
		/* thread/space deletion */
		TRACE("%s: deleting thread %lu:%lu (ptr %p)\n", __func__,
			L4_ThreadNo(dest_tid), L4_Version(dest_tid),
			dest);
		/* (could maintain dest->ipc_to to indicate if there's a waiting send
		 * phase.)
		 */
		if(dest->status == TS_SEND_WAIT || dest->status == TS_STOPPED) {
			abort_thread_ipc(dest);
		}
		if(!CHECK_FLAG(dest->flags, TF_HALT)) thread_halt(dest);
		abort_waiting_ipc(dest, 2 << 1);	/* "lost partner" */
		post_exn_fail(dest);
		if(dest->status != TS_STOPPED) {
			dest->status = TS_STOPPED;
			sq_remove_thread(dest);
		}
		thread_destroy(dest);
		goto dead;
	} else if(!L4_IsNilThread(spacespec) && dest != NULL) {
		/* modification only. (rest shared with creation.) */
		TRACE("%s: modifying thread %lu:%lu (ptr %p)\n", __func__,
			L4_ThreadNo(dest_tid), L4_Version(dest_tid),
			dest);

		if(dest->id != dest_tid.raw) {
			/* version field and IPC stomp */
			TRACE("%s: overwrite TID version to %lu:%lu\n", __func__,
				L4_ThreadNo(dest_tid), L4_Version(dest_tid));
			assert(L4_ThreadNo(dest_tid) == TID_THREADNUM(dest->id));
			assert(L4_Version(dest_tid) != TID_VERSION(dest->id));
			if(IS_IPC(dest->status)) abort_thread_ipc(dest);	/* "from" IPC */
			/* cancel sends to the now-stale TID.
			 * TODO: this should handle preemption vs. current_thread
			 */
			abort_waiting_ipc(dest, 3 << 1);
			dest->id = dest_tid.raw;
		}

		if(spacespec.raw != dest_tid.raw) {
			struct space *to_sp = space_find(spacespec.raw);
			if(unlikely(to_sp == NULL)) goto invd_space;
			else if(to_sp != dest->space) {
				/* FIXME: combine this with the TID stomp */
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
	void *dest_utcb = NULL;
	if(!L4_IsNilThread(scheduler)) {
		struct thread *sched = resolve_tid_spec(current->space, scheduler);
		if(unlikely(sched == NULL)) goto invd_sched;
		dest->scheduler.raw = sched->id;
	}

	if(utcb_loc != ~0ul) {
		bool created = dest->utcb_pos < 0;

		/* set utcb_pos. */
		if(utcb_loc < L4_Address(sp->utcb_area)
			|| utcb_loc + UTCB_SIZE > L4_Address(sp->utcb_area) + L4_Size(sp->utcb_area)
			|| (utcb_loc & (UTCB_SIZE - 1)) != 0)
		{
			ec = 6;		/* "bad UTCB location" */
			goto end;
		}
		bool ok = thread_set_utcb(dest, utcb_loc);
		if(!ok) goto out_of_mem;

		if(created) {
			dest_utcb = thread_get_utcb(dest);
			L4_VREG(dest_utcb, L4_TCR_PAGER) = dest->saved_regs[0];
		}
	}

	if(!L4_IsNilThread(pager)) {
		if(dest->utcb_pos >= 0) {
			dest_utcb = thread_get_utcb(dest);
			L4_VREG(dest_utcb, L4_TCR_PAGER) = pager.raw;
		} else {
			dest->saved_regs[0] = pager.raw;
		}
	}

	/* activation condition. */
	if(dest->utcb_pos >= 0 && dest->space != NULL) {
		if(dest_utcb == NULL) dest_utcb = thread_get_utcb(dest);
		assert(L4_IsNilThread(pager)
			|| pager.raw == L4_VREG(dest_utcb, L4_TCR_PAGER));
		pager.raw = L4_VREG(dest_utcb, L4_TCR_PAGER);
		if(!L4_IsNilThread(pager) && dest->status == TS_STOPPED) {
			dest->ipc_from = pager;
			dest->ipc_to = L4_nilthread;
			dest->recv_timeout = L4_Never;
			dest->wakeup_time = ~(uint64_t)0;
			dest->status = TS_R_RECV;
			sq_insert_thread(dest);
			hook_push_back(&dest->post_exn_call, &receive_breath_of_life,
				NULL);

			L4_VREG(dest_utcb, L4_TCR_EXCEPTIONHANDLER) = L4_nilthread.raw;
		}
	}

	assert(!L4_IsNilThread(dest->scheduler));
	assert(L4_IsGlobalId(dest->scheduler));

dead:
	result = 1;

end:
	L4_VREG(utcb, L4_TCR_ERRORCODE) = ec;
	regs->eax = result;

	assert(check_thread_module(0));
	return;

out_of_mem:
	ec = 8;
	goto end;

invd_space:
	ec = 3;
	goto end;

invd_sched:
	ec = 4;
	goto end;
}
