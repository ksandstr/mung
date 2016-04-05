
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>

#include <ccan/likely/likely.h>
#include <ccan/htable/htable.h>
#include <ccan/compiler/compiler.h>

#include <l4/types.h>
#include <l4/thread.h>
#include <l4/vregs.h>
#include <l4/kip.h>

#include <ukernel/x86.h>
#include <ukernel/interrupt.h>
#include <ukernel/mm.h>
#include <ukernel/slab.h>
#include <ukernel/ipc.h>
#include <ukernel/misc.h>
#include <ukernel/space.h>
#include <ukernel/gdt.h>
#include <ukernel/kip.h>
#include <ukernel/sched.h>
#include <ukernel/trace.h>
#include <ukernel/bug.h>
#include <ukernel/kip.h>
#include <ukernel/ktest.h>
#include <ukernel/thread.h>


/* for exregs, threadctl, threadswitch prints */
#define TRACE(fmt, ...) TRACE_MSG(TRID_THREAD, fmt, __VA_ARGS__)

/* the exception message, rounded up to cacheline size */
#define NUM_SAVED_REGS 14


/* stored TCRs for an exception originator */
struct saved_regs
{
	L4_ThreadId_t va_sender, ir;
	L4_Word_t mrs[NUM_SAVED_REGS];
};


/* interrupt routing state. */
struct interrupt
{
	struct thread *pager;	/* set in ThreadControl */
	bool pending, delivered;
};


/* only accessible with interrupts disabled. */
static short num_ints = 0, num_async_words = 0;
static struct interrupt *int_table = NULL;
static L4_Word_t *int_async_table = NULL;	/* bitfield, 0..num_ints-1 */

/* control variables used by scheduler_loop() etc. */
volatile bool kernel_irq_ok = false, kernel_irq_deferred = false;

struct htable thread_hash = HTABLE_INITIALIZER(thread_hash,
	hash_thread_by_id, NULL);

struct kmem_cache *thread_slab = NULL;
static struct kmem_cache *saved_regs_slab = NULL;


#ifdef DEBUG_ME_HARDER
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
#else
static bool check_thread(int a, struct thread *b) { return true; }
static bool check_thread_module(int a) { return true; }
#endif


COLD void init_threading(void)
{
	num_ints = last_int_threadno() + 1;
	int_table = malloc(num_ints * sizeof(struct interrupt));
	for(int i=0; i < num_ints; i++) {
		int_table[i] = (struct interrupt){
			.pager = NULL, .pending = false,
		};
	}
	const int wordsize = sizeof(L4_Word_t) * 8;
	num_async_words = (num_ints + wordsize - 1) / wordsize;
	int_async_table = malloc(num_async_words * sizeof(L4_Word_t));
	for(int i=0; i < num_async_words; i++) int_async_table[0] = 0;

	assert(thread_slab == NULL);
	thread_slab = KMEM_CACHE_NEW("thread_slab", struct thread);
	saved_regs_slab = KMEM_CACHE_NEW("saved_regs_slab", struct saved_regs);

	assert(check_thread_module(0));
}


static void restore_saved_regs(
	struct hook *hook,
	void *param, uintptr_t code, void *priv)
{
	struct thread *t = container_of(hook, struct thread, post_exn_call);
	assert(t->utcb_pos >= 0 && !IS_KERNEL_THREAD(t));
	void *utcb = thread_get_utcb(t);

	struct saved_regs *sr = priv;
	L4_VREG(utcb, L4_TCR_VA_SENDER) = sr->va_sender.raw;
	L4_VREG(utcb, L4_TCR_INTENDEDRECEIVER) = sr->ir.raw;
	memcpy(&L4_VREG(utcb, L4_TCR_MR(0)), sr->mrs,
		sizeof(L4_Word_t) * NUM_SAVED_REGS);
	kmem_cache_free(saved_regs_slab, sr);

	hook_detach(hook);
}


void save_ipc_regs(struct thread *t, void *utcb, int n_regs)
{
	assert(n_regs >= 0 && n_regs <= NUM_SAVED_REGS);
	assert(t->utcb_pos >= 0 && !IS_KERNEL_THREAD(t));

	struct saved_regs *sr = kmem_cache_alloc(saved_regs_slab);
	sr->va_sender.raw = L4_VREG(utcb, L4_TCR_VA_SENDER);
	sr->ir.raw = L4_VREG(utcb, L4_TCR_INTENDEDRECEIVER);
	memcpy(sr->mrs, &L4_VREG(utcb, L4_TCR_MR(0)),
		sizeof(L4_Word_t) * NUM_SAVED_REGS);
	hook_push_back(&t->post_exn_call, &restore_saved_regs, sr);
}


bool post_exn_ok(struct thread *t, struct thread *sender)
{
	int num = hook_call_front(&t->post_exn_call, sender, 0);
	/* kernel IPC chains either keep shitting, or get off the pot. */
	assert(num == 0 || t->ipc != NULL || IS_READY(t->status)
		|| (CHECK_FLAG(t->flags, TF_HALT) && t->status == TS_STOPPED));
	return num > 0;
}


bool post_exn_fail(struct thread *t)
{
	int num = hook_call_front(&t->post_exn_call, NULL, 1);
	return num > 0;
}


struct thread *thread_new(thread_id tid)
{
	/* keep thread_new() calls after init_threading() */
	assert(thread_slab != NULL);

	struct thread *t = kmem_cache_alloc(thread_slab);
	if(thread_ctor(t, tid)) {
		return t;
	} else {
		kmem_cache_free(thread_slab, t);
		return NULL;
	}
}


bool thread_ctor(struct thread *t, thread_id tid)
{
	assert(thread_find(tid) == NULL);
	assert(TID_THREADNUM(tid) > last_int_threadno());

	*t = (struct thread){
		.id = tid,
		.status = TS_STOPPED,
		.utcb_pos = -1,
		.pri = 100, .sens_pri = 100,
		.max_delay = 0,
		.ts_len = L4_TimePeriod(10000),		/* 10 ms */
		.quantum = 0,
		.total_quantum = ~(uint64_t)0,		/* useful default */

		/* x86 malarkey for non-kernel threads. */
		.ctx = {
			/* IOPL 3 (peon), interrupts enabled. also a reserved, constant
			 * bit is set.
			 */
			.eflags = (3 << 12) | (1 << 9) | (1 << 1),
		},
	};

	GUARD_INIT(t, sched_rb_0);
	GUARD_INIT(t, sched_rb_1);
	hook_init(&t->post_exn_call, NULL);
	return htable_add(&thread_hash, int_hash(TID_THREADNUM(t->id)), t);
}


static void thread_destroy(struct thread *t)
{
	assert(t->status == TS_DEAD || t->status == TS_STOPPED);
	assert(hook_empty(&t->post_exn_call));
	assert(!IS_KERNEL_THREAD(t));

	struct space *sp = t->space;
	if(t->utcb_ptr_seg != 0) {
		release_gdt_ptr_seg(L4_Address(sp->utcb_area)
				+ t->utcb_pos * UTCB_SIZE + 256 + TCR_UTCB_PTR * 4,
			t->utcb_ptr_seg);
		t->utcb_ptr_seg = 0;
	}
	if(t->utcb_pos >= 0) {
		assert(t->space != NULL);
		space_remove_thread(sp, t);
	}

	cop_killa(t);
	htable_del(&thread_hash, int_hash(TID_THREADNUM(t->id)), t);
	kmem_cache_free(thread_slab, t);
}


void thread_set_spip(struct thread *t, L4_Word_t sp, L4_Word_t ip)
{
	assert(!IS_KERNEL_THREAD(t));
	assert(t->status != TS_RUNNING);

	t->ctx.r.esp = sp;
	t->ctx.eip = ip;
}


static void copy_tcrs(void *dst, const void *src)
{
	/* all the user-writable TCRs. */
	static const int vregs[] = {
		L4_TCR_VA_SENDER,
		L4_TCR_XFERTIMEOUTS,
		L4_TCR_COP_PREEMPT,
		L4_TCR_EXCEPTIONHANDLER,
		L4_TCR_PAGER,
		L4_TCR_USERDEFINEDHANDLE,
		L4_TCR_THREADWORD0,
		L4_TCR_THREADWORD1,
	};

	for(int i=0; i < sizeof(vregs) / sizeof(vregs[0]); i++) {
		L4_VREG(dst, vregs[i]) = L4_VREG(src, vregs[i]);
	}
}


/* FIXME: this is only atomic on space_get_utcb_page() failure, and not on
 * reserved_gdt_ptr_seg() failure. that's bad.
 */
bool thread_set_utcb(struct thread *t, L4_Word_t start)
{
	assert(t->space != NULL);
	assert(t->utcb_pos < 0 || t->utcb_page != NULL);
	assert(!L4_IsNilFpage(t->space->utcb_area));
	assert((start & (UTCB_SIZE - 1)) == 0);

	struct space *sp = t->space;

	int new_pos = (start - L4_Address(sp->utcb_area)) / UTCB_SIZE,
		page = new_pos / UTCB_PER_PAGE;
	assert(page < NUM_UTCB_PAGES(sp->utcb_area));
	struct utcb_page *up = space_get_utcb_page(sp, page);
	if(up == NULL) return false;
	const uint16_t hold_mask = 1 << UTCB_PER_PAGE;
	assert(!CHECK_FLAG(up->occmap, hold_mask));
	up->occmap |= hold_mask;	/* hold despite space_remove_thread() */

	if(t->utcb_ptr_seg != 0) {
		release_gdt_ptr_seg(L4_Address(sp->utcb_area)
				+ t->utcb_pos * UTCB_SIZE + 256 + TCR_UTCB_PTR * 4,
			t->utcb_ptr_seg);
		t->utcb_ptr_seg = 0;
	}
	assert(t->utcb_ptr_seg == 0);

	if(new_pos != t->utcb_pos) {
		/* FIXME: allocate old_utcb in the heap & copy TCRs via it to avoid
		 * the old one vanishing when it's the last in the old UTCB page.
		 */
		void *old_utcb = NULL;
		if(t->utcb_pos >= 0) {
			old_utcb = thread_get_utcb(t);
			space_remove_thread(sp, t);
		}
		t->utcb_page = up;
		t->utcb_pos = new_pos;
		space_add_thread(sp, t);
		up->occmap &= ~hold_mask;

		int offset = new_pos - (page * UTCB_PER_PAGE);
		assert(up->pg->vm_addr != NULL);
		void *utcb_mem = up->pg->vm_addr + offset * UTCB_SIZE;
		memset(utcb_mem, 0, UTCB_SIZE);
		if(old_utcb != NULL) {
			copy_tcrs(utcb_mem + 256, old_utcb);
		}
		L4_VREG(utcb_mem + 256, L4_TCR_MYGLOBALID) = t->id;
		*(L4_Word_t *)(utcb_mem + 256 + TCR_UTCB_PTR * 4) = start + 256;

		assert(thread_get_utcb(t) == utcb_mem + 256);
	}

	assert(start == L4_Address(sp->utcb_area) + UTCB_SIZE * t->utcb_pos);
	if(likely(sp != kernel_space)) {
		assert(t->utcb_ptr_seg == 0);
		t->utcb_ptr_seg = reserve_gdt_ptr_seg(start + 256 + TCR_UTCB_PTR * 4);
		if(unlikely(t->utcb_ptr_seg < 0)) {
			t->utcb_ptr_seg = 0;
			return false;
		}
	}

	return true;
}


void thread_start(struct thread *t)
{
	/* thread_start() is for freshly-created threads only. unhalting goes
	 * through thread_resume().
	 */
	assert(t->status == TS_STOPPED && !CHECK_FLAG(t->flags, TF_HALT));

	t->status = TS_READY;
	t->wakeup_time = 0;
	sq_insert_thread(t);
}


void thread_halt(struct thread *t)
{
	assert(!IS_KERNEL_THREAD(t));

	TRACE("%s: called for %lu:%lu from %p\n", __func__,
		TID_THREADNUM(t->id), TID_VERSION(t->id), __builtin_return_address(0));

	assert(!CHECK_FLAG_ANY(t->flags, TF_HALT | TF_PRE_RECV));
	assert(t->status != TS_DEAD);

	t->flags |= TF_HALT;
	if(t->status == TS_READY
		|| t->status == TS_RUNNING
		|| t->status == TS_R_RECV)
	{
		if(t->status == TS_R_RECV) t->flags |= TF_PRE_RECV;
		int old_status = t->status;
		t->status = TS_STOPPED;
		if(old_status != TS_READY || t->total_quantum > 0) {
			sq_remove_thread(t);
		}
	}
}


void thread_resume(struct thread *t)
{
	assert(CHECK_FLAG(t->flags, TF_HALT));

	int old = t->flags;
	t->flags &= ~(TF_HALT | TF_PRE_RECV);
	if(t->status == TS_STOPPED) {
		t->status = CHECK_FLAG(old, TF_PRE_RECV) ? TS_R_RECV : TS_READY;
		if(t->status != TS_R_RECV) {
			/* it shouldn't be lost in R_RECV. */
			t->wakeup_time = 0;
		}
		if(t->total_quantum > 0) sq_insert_thread(t);
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

	assert(L4_IsTimePeriod_NP(period));

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

			if(t->status == TS_READY) {
				might_preempt(t);
			}
		}
	} else {
#if 0
#ifndef NDEBUG
		if(!IS_IPC_WAIT(t->status) && t->status != TS_XFER) {
			printf("%s: thread %lu:%lu status is %s\n", __func__,
				TID_THREADNUM(t->id), TID_VERSION(t->id),
				sched_status_str(t));
		}
#endif
#endif
		assert(IS_IPC_WAIT(t->status) || t->status == TS_XFER);

		t->wakeup_time = wakeup_at(period);
		sq_update_thread(t);
	}
}


/* postcond: @t->status == TS_READY || @t->status == TS_STOPPED
 * (depending on TF_HALT)
 */
void thread_ipc_fail(struct thread *t)
{
	assert(t->status == TS_RECV_WAIT
		|| t->status == TS_SEND_WAIT
		|| t->status == TS_R_RECV
		|| (t->status == TS_XFER && t->ipc != NULL));

	if(t->status == TS_SEND_WAIT) {
		/* (NOTE: also, $t->ipc != NULL \implies \
		 * Myself \notin \dom sendwait\_hash$, so this isn't actually
		 * required. cancel_ipc_from() accepts that, though.)
		 */
		cancel_ipc_from(t);
	}

	if(CHECK_FLAG(t->flags, TF_HALT)) {
		t->status = TS_STOPPED;
		sq_remove_thread(t);
	} else {
		if(CHECK_FLAG(t->flags, TF_REDIR_WAIT)) {
			remove_redir_wait(t);
		}
		t->status = TS_READY;
		if(t->wakeup_time > 0) {
			t->wakeup_time = 0;
			sq_update_thread(t);
		}
	}

	t->flags &= ~TF_REDIR_WAIT;
}


void *thread_get_utcb(struct thread *t)
{
	assert(t != NULL);
	assert(t->space != NULL);
	assert(t->utcb_pos >= 0);
	assert(t->utcb_pos < NUM_UTCB_PAGES(t->space->utcb_area) * UTCB_PER_PAGE);

	int page_ix = t->utcb_pos / UTCB_PER_PAGE,
		offset = t->utcb_pos & (UTCB_PER_PAGE - 1);
	struct utcb_page *up = t->utcb_page;
	assert(up->pos == page_ix);
	assert(CHECK_FLAG(up->occmap, 1 << offset));
	struct page *p = up->pg;
	assert(p != NULL);
	assert(p->vm_addr != NULL);

	/* TODO: change the "256" that appears all over UTCB-related code to
	 * UTCB_SIZE / 2, which it properly is.
	 */
	return p->vm_addr + offset * UTCB_SIZE + 256;
}


void thread_save_ctx(struct thread *t, const struct x86_exregs *regs)
{
	size_t flen = x86_frame_len(regs);
	t->ctx.r = regs->r;
	t->ctx.r.esp = regs->esp;
	t->ctx.eip = regs->eip;
	t->ctx.eflags = regs->eflags;
	if(flen < sizeof(*regs)) {
		assert(IS_KERNEL_THREAD(t));
		t->ctx.r.esp = (L4_Word_t)regs + flen;
		TRACE("%s: saved kernel thread %lu:%lu: eip %#lx, esp %#lx\n", __func__,
			TID_THREADNUM(t->id), TID_VERSION(t->id),
			t->ctx.eip, t->ctx.r.esp);
	} else {
		assert(!IS_KERNEL_THREAD(t));
		TRACE("%s: saved user thread %lu:%lu: eip %#lx, esp %#lx\n", __func__,
			TID_THREADNUM(t->id), TID_VERSION(t->id),
			t->ctx.eip, t->ctx.r.esp);
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
	void *param, uintptr_t code, void *priv)
{
	hook_detach(hook);

	if(code == 0) {
		struct thread *t = container_of(hook, struct thread, post_exn_call),
			*sender = param;
		void *utcb = thread_get_utcb(sender);
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


L4_ThreadId_t get_local_id(struct thread *t)
{
	assert(t->space != NULL);
	assert(t->utcb_pos >= 0);
	L4_ThreadId_t result = {
		.raw = L4_Address(t->space->utcb_area)
			+ t->utcb_pos * UTCB_SIZE + 256,
	};
	assert(L4_IsLocalId(result));
	return result;
}


static inline bool selftest_enabled(void) {
#ifdef ENABLE_SELFTEST
	return true;
#else
	return false;
#endif
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


SYSCALL L4_Word_t sys_exregs(
	L4_ThreadId_t dest,
	L4_Word_t *control_p,
	L4_Word_t *sp_p, L4_Word_t *ip_p, L4_Word_t *flags_p,
	L4_Word_t *udh_p,
	L4_ThreadId_t *pager_p)
{
	assert(check_thread_module(0));	/* NB: adds 15k cycles to benchmark */

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
		if(unlikely(selftest_enabled()
			&& !L4_IsNilThread(dest) && dest_thread != NULL
			&& L4_ThreadNo(dest) == first_user_threadno()
			&& CHECK_FLAG(current->space->flags, SF_PRIVILEGE)))
		{
			/* succeed, but return only pager. do nothing else. */
			pager_p->raw = L4_VREG(thread_get_utcb(dest_thread),
				L4_TCR_PAGER);
			assert(dest_thread->space != current->space);
			return dest_thread->id;
		} else {
			/* threadspec was invalid, or not present, or in a different
			 * space, or not active.
			 */
			L4_VREG(thread_get_utcb(current), L4_TCR_ERRORCODE) = 2;
			return L4_nilthread.raw;
		}
	}

	L4_ThreadId_t result;
	if(L4_IsGlobalId(dest)) {
		result = get_local_id(dest_thread);
		assert(result.local.X.zeros == 0);
	} else {
		result.global.raw = dest_thread->id;
		assert(result.local.X.zeros != 0);
	}

	L4_Word_t ctl_out = 0;
	if(CHECK_FLAG(dest_thread->flags, TF_HALT)) ctl_out |= 1;	/* H bit */
	switch(dest_thread->status) {
		case TS_RECV_WAIT:
		case TS_R_RECV:
			ctl_out |= 2;	/* R bit */
			break;
		case TS_SEND_WAIT:
			ctl_out |= 4;	/* S bit */
			break;
		case TS_XFER:
			assert(dest_thread->ipc != NULL);
			ctl_out |= dest_thread == dest_thread->ipc->from ? 4 : 2;
			break;
	}

	/* fast exit for L4_{Local,Global}IdOf() */
	if(*control_p == 0) goto end;

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

		*sp_p = dest_thread->ctx.r.esp;
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
		if(CHECK_FLAG(ctl_in, CTL_f)) {
			dest_thread->ctx.eflags = x86_clean_eflags(
				dest_thread->ctx.eflags, flags_in);
		}
		if(CHECK_FLAG(ctl_in, CTL_i)) {
			/* no longer returning from a simple system call, so must use full
			 * context reload.
			 */
			dest_thread->flags &= ~TF_SYSCALL;
			dest_thread->ctx.eip = ip_in;
		}
		if(CHECK_FLAG(ctl_in, CTL_s)) dest_thread->ctx.r.esp = sp_in;

		ctl_in &= ~regset_mask;
	}

	if(unlikely(ctl_in != 0)) {
		printf("%s: unhandled ExchangeRegister control bits %#lx\n",
			__func__, ctl_in);
		goto fail;
	}

end:
	*control_p = ctl_out;
	TRACE("%s: returning control'=%#lx\n", __func__, *control_p);
	/* NB: this line adds about 6k cycles to the exregs benchmark on core2. */
	assert(dest_thread == NULL || check_thread(0, dest_thread));
	return result.raw;

fail:
	result = L4_nilthread;
	/* HAIL SATAN errday */
	L4_VREG(thread_get_utcb(current), L4_TCR_ERRORCODE) = 666;
	goto end;
}


#define int_disable(irq) (*global_pic.mask_irq)((irq))
/* NOTE: "active high", "edge sensitive" is the default. there should be some
 * ACPI shenanigans for recognizing legacy devices that instead do active-low
 * signaling.
 */
#define int_enable(irq) (*global_pic.unmask_irq)((irq), true, false)


static L4_Word_t interrupt_ctl(
	L4_Word_t *ec_p,
	struct thread *current,
	L4_ThreadId_t dest_tid,
	L4_ThreadId_t pager)
{
	int intnum = L4_ThreadNo(dest_tid);
	assert(L4_Version(dest_tid) == 1);
	assert(intnum < num_ints);

	struct thread *pgt = thread_find(pager.raw);
	if(pager.raw != dest_tid.raw && pgt == NULL) {
		*ec_p = 2;		/* "unavailable thread", when pager isn't valid */
		return 0;
	}
	assert(pgt != NULL || pager.raw == dest_tid.raw);

	/* TODO: add mechanism for clearing TF_INTR also */
	x86_irq_disable();
	struct interrupt *it = &int_table[intnum];
	it->pager = pgt;
	it->pending = false;
	it->delivered = false;

	if(pgt == NULL) {
		int_disable(intnum);
	} else {
		/* enable it. */
		pgt->flags |= TF_INTR;
		int_enable(intnum);
	}
	x86_irq_enable();

	*ec_p = 0;
	return 1;
}


/* returns true if t->status was changed. caller should set
 * int_table[ivec].delivered in that case to avoid double delivery.
 */
static bool send_int_ipc(int ivec, struct thread *t, bool kernel_irq)
{
	assert(t != NULL);

	if(!CHECK_FLAG(t->flags, TF_INTR)) return false;	/* nope.jpg */

	if(kernel_irq && !kernel_irq_ok) {
		/* deferred signaling for modification safety wrt kernel code. */
		int limb = ivec / (sizeof(L4_Word_t) * 8),
			bit = ivec & (sizeof(L4_Word_t) * 8 - 1);
		int_async_table[limb] |= 1ul << bit;
		kernel_irq_deferred = true;
		return false;
	} else if(t->status != TS_RECV_WAIT
		|| (t->ipc_from.raw != L4_anythread.raw
			&& t->ipc_from.raw != L4_GlobalId(ivec, 1).raw))
	{
		/* active signaling can't happen because the recipient isn't waiting.
		 * this one will be received with int_poll().
		 */
		return false;
	} else {
		void *utcb = thread_get_utcb(t);
		L4_VREG(utcb, L4_TCR_MR(0)) = (L4_MsgTag_t){ .X.label = 0xfff0 }.raw;
		t->ipc_from = L4_GlobalId(ivec, 1);
		set_ipc_return_regs(&t->ctx.r, t, utcb);
		thread_wake(t);
		return true;
	}
}


void int_latent(void)
{
	assert(x86_irq_is_enabled());
	assert(kernel_irq_deferred);
	assert(!kernel_irq_ok);

	struct thread *ts[num_ints];
	uint8_t vecs[num_ints];
	int num_vecs = 0;

	x86_irq_disable();
	for(int i=0; i < num_async_words; i++) {
		while(int_async_table[i] != 0) {
			int b = ffsl(int_async_table[i]) - 1,
				v = i * sizeof(L4_Word_t) * 8 + b;
			assert(CHECK_FLAG(int_async_table[i], 1 << b));
			assert(v < num_ints);
			assert(num_vecs < num_ints);
			assert(!int_table[v].delivered);

			int_async_table[i] &= ~(1ul << v);
			int_table[v].delivered = true;
			vecs[num_vecs] = v;
			ts[num_vecs] = int_table[v].pager;
			num_vecs++;
		}
	}
	kernel_irq_deferred = false;
	x86_irq_enable();

	if(num_vecs > 0) {
		int no_deliver = 0;
		for(int i=0; i < num_vecs; i++) {
			if(ts[i] == NULL) continue;
			if(!send_int_ipc(vecs[i], ts[i], false)) {
				no_deliver++;
				ts[i] = NULL;
			}
		}

		if(no_deliver > 0) {
			/* go clear the delivery bits for the ones that failed. */
			x86_irq_disable();
			for(int i=0; i < num_vecs; i++) {
				if(ts[i] == NULL) int_table[vecs[i]].delivered = false;
			}
			x86_irq_enable();
		}
	}
}


/* called from ThreadControl in the deletion & version stomp cases */
static void int_kick(struct thread *t)
{
	assert(x86_irq_is_enabled());
	assert(CHECK_FLAG(t->flags, TF_INTR));

	/* brute force, but acceptable because interrupts are usually few. */
	x86_irq_disable();
	for(int i=0; i < num_ints; i++) {
		struct interrupt *it = &int_table[i];
		if(it->pager != t) continue;
		it->pager = NULL;
		it->pending = false;
		it->delivered = false;
		int_disable(i);
	}
	x86_irq_enable();

	/* no longer applicable. */
	t->flags &= ~TF_INTR;
}


/* called from irq.c with interrupts disabled */
bool int_trigger(int intnum, bool in_kernel)
{
	assert(!x86_irq_is_enabled());
	assert(intnum < num_ints);
	struct interrupt *it = &int_table[intnum];
	if(unlikely(it->pager == NULL)) return false;

	if(likely(!it->pending)) {
		it->pending = true;
		if(send_int_ipc(intnum, it->pager, in_kernel)) it->delivered = true;
		int_disable(intnum);
	}

	return true;
}


/* called when a recipient doesn't ReplyWait to the interrupt.
 *
 * when @intnum == -1, always selects lowest number first. returns -1 when no
 * interrupt is pending; and non-negative when an interrupt is pending.
 */
int int_poll(struct thread *t, int intnum)
{
	assert(x86_irq_is_enabled());
	assert(intnum == -1 || intnum < num_ints);

	x86_irq_disable();
	int retval = -1;
	if(intnum >= 0) {
		struct interrupt *it = &int_table[intnum];
		if(it->pager == t && it->pending && !it->delivered) {
			retval = intnum;
		}
	} else {
		for(int i=0; i < num_ints; i++) {
			struct interrupt *it = &int_table[i];
			if(it->pager == t && it->pending && !it->delivered) {
				retval = i;
				break;
			}
		}
	}
	if(retval >= 0) int_table[retval].delivered = true;
	x86_irq_enable();

	return retval;
}


/* called from ipc_send_half() on reply to interrupt thread */
int int_clear(int intnum, struct thread *sender)
{
	assert(x86_irq_is_enabled());
	assert(sender != NULL);
	assert(intnum < num_ints);

	struct interrupt *it = &int_table[intnum];
	if(unlikely(sender != it->pager)) {
		return 2;	/* non-existing partner */
	} else {
		x86_irq_disable();
		if(unlikely(!it->pending || !it->delivered)) {
			/* can't clear an undelivered interrupt. the IPC wait isn't
			 * significant either.
			 *
			 * NOTE: this violates a send timeout if one is specified, even
			 * L4_Never. that's completely fine; interrupts don't get a
			 * "signal when clearable" mechanism.
			 */
			x86_irq_enable();
			return 1;		/* timeout */
		}

		it->pending = false;
		it->delivered = false;
		int_enable(intnum);
		x86_irq_enable();
		return 0;
	}
}


static void drop_redir_to(struct thread *t)
{
	if(CHECK_FLAG(t->flags, TF_REDIR)) {
		space_remove_redirector(t);
		t->flags &= ~TF_REDIR;
	}
}


/* FIXME: make this function atomic on error! */
SYSCALL L4_Word_t sys_threadcontrol(
	L4_ThreadId_t dest_tid,
	L4_ThreadId_t pager,
	L4_ThreadId_t scheduler,
	L4_ThreadId_t spacespec,
	void *utcblocation)
{
	assert(check_thread_module(0));

	L4_Word_t ec = 0, result = 0, utcb_loc = (L4_Word_t)utcblocation;
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

	/* interrupt control. */
	if(dest_tid.raw == spacespec.raw
		&& TID_VERSION(dest_tid.raw) == 1
		&& TID_THREADNUM(dest_tid.raw) <= last_int_threadno())
	{
		TRACE("%s: calling interrupt_ctl()\n", __func__);
		result = interrupt_ctl(&ec, current, dest_tid, pager);
		goto end;
	}

	if(unlikely(L4_IsLocalId(dest_tid)
		|| L4_ThreadNo(dest_tid) < first_user_threadno()))
	{
		TRACE("%s: dest_tid unavailable to user\n", __func__);
		goto unav_thread;
	}
	if(!L4_IsNilThread(spacespec)
		&& unlikely(L4_IsLocalId(spacespec)
			|| L4_ThreadNo(spacespec) < first_user_threadno()))
	{
		TRACE("%s: spacespec=%lu:%lu is invalid\n", __func__,
			L4_ThreadNo(spacespec), L4_Version(spacespec));
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

		struct space *sp = space_find(spacespec.raw);
		bool new_space = false;
		if(sp == NULL) {
			if(spacespec.raw != dest_tid.raw) goto invd_space;
			sp = space_new();
			if(sp == NULL) goto out_of_mem;
			new_space = true;
		}

		dest = thread_new(dest_tid.raw);
		if(dest == NULL) {
			if(new_space) space_free(sp);
			goto out_of_mem;
		}
		dest->space = sp;
		assert(dest->utcb_pos < 0);
		assert(dest->utcb_page == NULL);

		/* resolves local TID, avoids second call to resolve_tid_spec() */
		dest->scheduler.raw = sched->id;
		scheduler = L4_nilthread;
	} else if(L4_IsNilThread(spacespec)) {
		/* thread/space deletion */
		if(dest == NULL) {
			/* idempotence: return success when spacespec = nil, and thread
			 * doesn't exist.
			 */
			goto dead;
		}

		drop_redir_to(dest);
		cancel_ipc_from(dest);
		if(!CHECK_FLAG(dest->flags, TF_HALT)) thread_halt(dest);
		if(CHECK_FLAG(dest->flags, TF_INTR)) int_kick(dest);
		post_exn_fail(dest);
		if(dest->status != TS_STOPPED) {
			int old_status = dest->status;
			dest->status = TS_STOPPED;
			if(old_status != TS_READY || dest->total_quantum > 0) {
				sq_remove_thread(dest);
			}
		}
		L4_ThreadId_t dead_tid = { .raw = dest->id };
		thread_destroy(dest);
		cancel_ipc_to(dead_tid, 2 << 1);	/* "lost partner" */
		goto dead;
	} else if(!L4_IsNilThread(spacespec) && dest != NULL) {
		/* modification only. (rest shared with creation, so the thread is
		 * left at STOPPED and will await a starting ExchangeRegisters.)
		 */
		L4_ThreadId_t old_tid = { .raw = dest->id };
		if(dest->id != dest_tid.raw) {
			/* version field and IPC stomp */
			assert(L4_ThreadNo(dest_tid) == TID_THREADNUM(dest->id));
			assert(L4_Version(dest_tid) != TID_VERSION(dest->id));
			/* version bits changed, invalidating redir TIDs */
			drop_redir_to(dest);
			/* fail sends to the moribund TID & break propagation where it's
			 * used for VirtualSender
			 */
			cancel_ipc_from(dest);
			L4_ThreadId_t stale_tid = { .raw = dest->id };
			dest->id = dest_tid.raw;
			if(dest->utcb_pos >= 0) {
				void *utcb = thread_get_utcb(dest);
				L4_VREG(utcb, L4_TCR_MYGLOBALID) = dest->id;
			}
			cancel_ipc_to(stale_tid, 2 << 1);	/* "lost partner" */

			if(CHECK_FLAG(dest->flags, TF_INTR)) int_kick(dest);

			if(dest->status != TS_READY || dest->total_quantum > 0) {
				if(dest->status > 1) sq_remove_thread(dest);
			}
			dest->status = TS_STOPPED;
			dest->flags = 0;

			assert(resolve_tid_spec(get_current_thread()->space,
				dest_tid) == dest);
		}

		if(spacespec.raw != dest_tid.raw
			&& spacespec.raw != old_tid.raw)
		{
			struct space *to_sp = space_find(spacespec.raw);
			if(unlikely(to_sp == NULL)) goto invd_space;
			else if(to_sp != dest->space) {
				/* FIXME: combine this with the TID stomp */
				panic("TODO: movement of threads between spaces");
			}
		}
	} else {
		/* parameter fuckup. */
		goto unav_thread;
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
			L4_VREG(dest_utcb, L4_TCR_PAGER) = dest->u0.pager.raw;
			dest->u0.pager = L4_nilthread;
		}
	}

	if(!L4_IsNilThread(pager)) {
		if(dest->utcb_pos >= 0) {
			dest_utcb = thread_get_utcb(dest);
			L4_VREG(dest_utcb, L4_TCR_PAGER) = pager.raw;
		} else {
			dest->u0.pager = pager;
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
			hook_push_back(&dest->post_exn_call, &receive_breath_of_life,
				NULL);
			L4_VREG(dest_utcb, L4_TCR_EXCEPTIONHANDLER) = L4_nilthread.raw;

			assert(dest->u0.partner == NULL);
			sq_insert_thread(dest);
		}
	}

	assert(!L4_IsNilThread(dest->scheduler));
	assert(L4_IsGlobalId(dest->scheduler));

	result = 1;

end:
	if(result == 0) {
		L4_VREG(utcb, L4_TCR_ERRORCODE) = ec;
	}

	assert(check_thread_module(0));
	return result;

dead:
	if(unlikely(dest == current)) {
		assert(check_thread_module(0));
		return_from_dead();
	}
	result = 1;
	goto end;

out_of_mem:
	ec = 8;
	goto end;

unav_thread:
	ec = L4_ERROR_INVALID_THREAD;
	goto end;

invd_space:
	ec = 3;
	goto end;

invd_sched:
	ec = 4;
	goto end;
}
