
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>
#include <errno.h>

#include <ccan/likely/likely.h>
#include <ccan/compiler/compiler.h>

#include <l4/types.h>
#include <l4/thread.h>
#include <l4/vregs.h>
#include <l4/kip.h>

#include <ukernel/x86.h>
#include <ukernel/interrupt.h>
#include <ukernel/mm.h>
#include <ukernel/slab.h>
#include <ukernel/rangealloc.h>
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


static struct thread *thread_find(thread_id tid);
static struct thread *int_trigger(int irqn);


/* only accessible with interrupts disabled. */
static short num_ints = 0;
static struct interrupt *int_table = NULL;

static struct kmem_cache *saved_regs_slab = NULL;
struct rangealloc *thread_ra = NULL;

/* burner threads for tno=0 .. num_ints */
static int num_static;
static struct thread **static_threads;


#ifdef DEBUG_ME_HARDER
#include <ukernel/invariant.h>

static bool check_thread(int opt, struct thread *t)
{
	INV_CTX;
	inv_push("thread %lu:%lu (%p)", TID_THREADNUM(t->id),
		TID_VERSION(t->id), t);

	inv_ok1(t->space != NULL);
	/* TODO: verify presence in utcb_page slot. */
	inv_iff1(t->utcb_page != NULL, t->utcb_pos >= 0);
	inv_iff1(t->utcb_pos >= 0, t->utcb_ptr_seg > 0);

	/* the halt bit */
	inv_imply1(CHECK_FLAG(t->flags, TF_HALT) && t->status != TS_STOPPED,
		IS_IPC_WAIT(t->status));

	/* rangealloc interactions */
	inv_ok1(ra_alloc(thread_ra, TID_THREADNUM(t->id)) == NULL);
	inv_ok1(ra_id2ptr(thread_ra, TID_THREADNUM(t->id)) == t);
	inv_ok1(ra_ptr2id(thread_ra, t) == TID_THREADNUM(t->id));

	inv_pop();
	return true;

inv_fail:
	return false;
}


static bool check_thread_module(int opt)
{
	struct ra_iter it;
	for(struct thread *t = ra_first(thread_ra, &it);
		t != NULL;
		t = ra_next(thread_ra, &it))
	{
		if(t->space == NULL) continue;	/* burners etc. */
		if(!check_thread(opt, t)) return false;
	}

	return true;
}
#else
static bool check_thread(int a, struct thread *b) { return true; }
static bool check_thread_module(int a) { return true; }
#endif


COLD void init_threading(void)
{
	assert(thread_ra == NULL);

	num_ints = last_int_threadno() + 1;
	int_table = malloc(num_ints * sizeof(struct interrupt));
	for(int i=0; i < num_ints; i++) {
		int_table[i] = (struct interrupt){
			.pager = NULL, .pending = false,
		};
	}

	saved_regs_slab = KMEM_CACHE_NEW("saved_regs_slab", struct saved_regs);

	/* room for the full 256k threads of 32-bit L4.X2. */
	assert(thread_ra == NULL);
	thread_ra = RA_NEW(struct thread, 1 << 18);

	num_static = first_user_threadno() - 1;
	if(num_static == 0) {
		static_threads = NULL;
	} else {
		static_threads = malloc(num_static * sizeof(struct thread *));
		for(int i=0; i < num_static; i++) {
			struct thread *t = ra_zalloc(thread_ra, i);
			BUG_ON(t == NULL, "static_threads[%d]", i);
			static_threads[i] = t;
		}
#ifndef NDEBUG
		printf("thread_ra: allocated %d burners.\n", num_static);
#endif
	}

	assert(check_thread_module(0));
}


static void restore_saved_regs(
	struct hook *hook,
	void *param, uintptr_t code,
	struct saved_regs *sr)
{
	struct thread *t = container_of(hook, struct thread, post_exn_call);
	assert(t->utcb_pos >= 0);
	void *utcb = thread_get_utcb(t);

	L4_VREG(utcb, L4_TCR_VA_SENDER) = sr->va_sender.raw;
	L4_VREG(utcb, L4_TCR_INTENDEDRECEIVER) = sr->ir.raw;
	memcpy(&L4_VREG(utcb, L4_TCR_MR(0)), sr->mrs,
		sizeof(L4_Word_t) * NUM_SAVED_REGS);
	kmem_cache_free(saved_regs_slab, sr);

	hook_detach(hook);
}


/* FIXME: handle OOM from kmem_cache_alloc(), hook_push_back()! */
void save_ipc_regs(struct thread *t, void *utcb, int n_regs)
{
	assert(n_regs >= 0 && n_regs <= NUM_SAVED_REGS);
	assert(t->utcb_pos >= 0);

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
	assert(thread_ra != NULL);	/* must have been initialized */
	assert(thread_find(tid) == NULL);
	assert(TID_THREADNUM(tid) > last_int_threadno());

	struct thread *t = ra_alloc(thread_ra, TID_THREADNUM(tid));
	if(unlikely(t == NULL)) return NULL;

	assert(ra_ptr2id(thread_ra, t) == TID_THREADNUM(tid));
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
			/* IOPL 0 (noblman swerve) */
			.eflags = (0 << 12) | X86_DEFAULT_USER_EFLAGS,
		},
	};
	hook_init(&t->post_exn_call, NULL);

	return t;
}


static void thread_destroy(struct thread *t)
{
	assert(t->status == TS_STOPPED);
	assert(hook_empty(&t->post_exn_call));

	if(unlikely(t == get_current_thread())) leaving_thread(t);

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
	t->space = NULL;
	ra_free(thread_ra, t);
}


void thread_set_spip(struct thread *t, L4_Word_t sp, L4_Word_t ip)
{
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


/* FIXME: this isn't atomic on reserved_gdt_ptr_seg() failure. */
int thread_set_utcb(struct thread *t, L4_Word_t start)
{
	assert(t->space != NULL);
	assert(t->utcb_pos < 0 || t->utcb_page != NULL);
	assert(!L4_IsNilFpage(t->space->utcb_area));
	assert((start & (UTCB_SIZE - 1)) == 0);

	struct space *sp = t->space;

	int new_pos = (start - L4_Address(sp->utcb_area)) / UTCB_SIZE;
	if(new_pos == t->utcb_pos) return 0;	/* no effect */
	int page = new_pos / UTCB_PER_PAGE, slot = new_pos % UTCB_PER_PAGE;
	assert(page < NUM_UTCB_PAGES(sp->utcb_area));
	struct utcb_page *up = space_get_utcb_page(sp, page);
	if(up == NULL) return -ENOMEM;
	if(CHECK_FLAG(up->occmap, 1 << slot)) {
		/* invalid destination: thread present. */
		return -EEXIST;
	}
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

	/* TODO: allocate old_utcb in the heap & copy TCRs via it to work around
	 * the old one vanishing when it's the last in the old UTCB page. then
	 * remove the hold_mask trickery, which leaves empty utcb pages in the
	 * tree. (very minor.)
	 */
	void *old_utcb = NULL;
	if(t->utcb_pos >= 0) {
		old_utcb = thread_get_utcb(t);
		space_remove_thread(sp, t);
	}
	t->utcb_page = up;
	t->utcb_pos = new_pos;
	space_add_thread(sp, t);
	assert(CHECK_FLAG(up->occmap, hold_mask));
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

	assert(start == L4_Address(sp->utcb_area) + UTCB_SIZE * t->utcb_pos);
	if(likely(sp != kernel_space)) {
		assert(t->utcb_ptr_seg == 0);
		t->utcb_ptr_seg = reserve_gdt_ptr_seg(start + 256 + TCR_UTCB_PTR * 4);
		if(unlikely(t->utcb_ptr_seg < 0)) {
			t->utcb_ptr_seg = 0;
			return -ENOMEM;
		}
	}

	return 0;
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
	TRACE("%s: called for %lu:%lu from %p\n", __func__,
		TID_THREADNUM(t->id), TID_VERSION(t->id), __builtin_return_address(0));

	assert(!CHECK_FLAG_ANY(t->flags, TF_HALT | TF_PRE_RECV));

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
	} else if(t->status == TS_SEND_WAIT) {
		/* render @t ineligible for active receive */
		/* TODO: should redirect-blocked sendwait also put the redir_wait
		 * entry on ice? or is it enough to have redirectors not pick up for
		 * TF_HALTed redir_wait entries?
		 */
		remove_send_wait(t);
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
	} else if(t->status == TS_SEND_WAIT) {
		/* FIXME: check return value and pass it on. it'll be tough to change
		 * all callsites of thread_resume(), but maybe it can be done.
		 */
		insert_send_wait(t);
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
		|| t->status == TS_XFER);
	assert(t->ipc == NULL);

	if(t->status == TS_SEND_WAIT || t->status == TS_RECV_WAIT) {
		/* (NOTE: also, $t->ipc != NULL \implies \ Myself \notin \dom
		 * sendwait\_hash$ (and the same for recvwait_hash), so this isn't
		 * actually required. cancel_ipc_from() accepts that, though.)
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


/* look for a thread number, ignoring the version number. used from
 * sys_threadcontrol(), thread_new(), and thread_get().
 *
 * this is possibly not as fast as it could be, however, it's quicker than the
 * hashtable search of ra_id2ptr_safe() and accesses fewer cachelines over
 * time. if at some point pt_get_pgid() loses its htable lookup (i.e. the
 * kernel gets some way of quickly accessing its own pagetables), that method
 * should be used instead of catch_pf().
 */
static struct thread *thread_find(thread_id tid)
{
	if(unlikely(catch_pf() != 0)) return NULL;

	uintptr_t ptr = tid >> (14 - thread_ra->id_shift);
	ptr &= thread_ra->and_mask;
	ptr |= thread_ra->or_mask;
	struct thread *t = (struct thread *)ptr;
	assert(t == ra_id2ptr(thread_ra, TID_THREADNUM(tid)));
	if(unlikely(*(struct space *volatile *)&t->space == NULL)) t = NULL;
	assert(t == NULL
		|| TID_THREADNUM(t->id) == TID_THREADNUM(tid));
	uncatch_pf();
	return t;
}


struct thread *thread_get(L4_ThreadId_t tid)
{
	assert(L4_IsGlobalId(tid));
	struct thread *t = thread_find(tid.raw);
	if(t != NULL && t->id != tid.raw) t = NULL;
	return t;
}


struct thread *resolve_tid_spec(struct space *ref_space, L4_ThreadId_t tid)
{
	if(L4_IsNilThread(tid)) return NULL;
	else if(L4_IsLocalId(tid)) {
		return space_find_local_thread(ref_space, tid.local);
	} else {
		return thread_get(tid);
	}
}


bool thread_is_valid(const struct thread *t)
{
	L4_ThreadId_t tid = L4_GlobalId(ra_ptr2id(thread_ra, t), 1);
	struct thread *cand = thread_find(tid.raw);
	assert(cand == NULL || cand == t);
	return cand != NULL;
}


static void receive_breath_of_life(
	struct hook *hook, void *sender, uintptr_t code, struct thread *t)
{
	assert(t == container_of(hook, struct thread, post_exn_call));
	hook_detach(hook);

	if(code == 0) {
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
				set_ipc_error_thread(dest_thread, 7);
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
				set_ipc_error_thread(dest_thread, 6);
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
	assert(dest_thread == NULL || check_thread(0, dest_thread));
	assert(check_thread_module(0));
	return result.raw;

fail:
	result = L4_nilthread;
	/* HAIL SATAN errday */
	L4_VREG(thread_get_utcb(current), L4_TCR_ERRORCODE) = 666;
	goto end;
}


/* NOTE: "active high", "edge sensitive" is the default. there should be some
 * ACPI shenanigans for recognizing legacy devices that instead do active-low
 * signaling.
 */
static void int_enable(int irq)
{
	x86_irq_disable();
	set_irq_handler(irq, &int_trigger, IHF_AUTOMASK);
	unmask_irq(irq, 0);
	x86_irq_enable();
}


static void int_disable(int irq)
{
	x86_irq_disable();
	mask_irq(irq);
	set_irq_handler(irq, NULL, 0);
	x86_irq_enable();
}


static void int_unmask(int irq)
{
	x86_irq_disable();
	unmask_irq(irq, 0);
	x86_irq_enable();
}


static L4_Word_t interrupt_ctl(
	L4_Word_t *ec_p,
	struct thread *current, L4_ThreadId_t dest_tid, L4_ThreadId_t pager)
{
	int intnum = L4_ThreadNo(dest_tid);
	assert(L4_Version(dest_tid) == 1);
	assert(intnum < num_ints);

	struct thread *pgt = thread_get(pager);
	if(pager.raw != dest_tid.raw && pgt == NULL) {
		*ec_p = 2;		/* "unavailable thread", when pager isn't valid */
		return 0;
	}
	assert(pgt != NULL || pager.raw == dest_tid.raw);

	/* TODO: add mechanism for clearing TF_INTR also */
	if(pgt == NULL) int_disable(intnum);

	struct interrupt *it = &int_table[intnum];
	it->pager = pgt;
	it->pending = false;
	it->delivered = false;

	if(pgt != NULL) {
		pgt->flags |= TF_INTR;
		int_enable(intnum);
	}

	*ec_p = 0;
	return 1;
}


/* returns true if t->status was changed. caller should set
 * int_table[@intnum].delivered in that case to avoid double delivery.
 */
static bool send_int_ipc(int intnum, struct thread *t)
{
	assert(t != NULL);

	if(!CHECK_FLAG(t->flags, TF_INTR)) return false;	/* nope.jpg */

	if((t->status != TS_RECV_WAIT && t->status != TS_R_RECV)
		|| (t->ipc_from.raw != L4_anythread.raw
			&& t->ipc_from.raw != L4_GlobalId(intnum, 1).raw))
	{
		/* active signaling can't happen because the recipient isn't waiting.
		 * this one will be received with int_poll().
		 */
		return false;
	} else {
		void *utcb = thread_get_utcb(t);
		L4_VREG(utcb, L4_TCR_MR(0)) = (L4_MsgTag_t){ .X.label = 0xfff0 }.raw;
		t->ipc_from = L4_GlobalId(intnum, 1);
		set_ipc_return_regs(&t->ctx.r, t, utcb);
		thread_wake(t);
		return true;
	}
}


/* called from ThreadControl in the deletion & version stomp cases */
static void int_kick(struct thread *t)
{
	assert(CHECK_FLAG(t->flags, TF_INTR));

	/* brute force, but acceptable because interrupts are usually few. */
	for(int i=0; i < num_ints; i++) {
		struct interrupt *it = &int_table[i];
		if(it->pager == t) {
			it->pager = NULL;
			it->pending = false;
			it->delivered = false;
			int_disable(i);
		}
	}

	/* no longer applicable. */
	t->flags &= ~TF_INTR;
}


static struct thread *int_trigger(int intnum)
{
	assert(intnum >= 0 && intnum < num_ints);

	struct interrupt *it = &int_table[intnum];
	if(likely(it->pager != NULL) && likely(!it->pending)) {
		it->pending = true;
		if(send_int_ipc(intnum, it->pager)) {
			it->delivered = true;
			return it->pager;
		}
	}

	return get_current_thread();
}


/* called in ipc_recv_half() to deliver interrupt IPC in a case where the
 * interrupt occurred, and the handler thread wasn't waiting.
 *
 * when @intnum == -1: returns -1 when no interrupt is pending and
 * non-negative int# otherwise, always selecting lowest number first.
 */
int int_poll(struct thread *t, int intnum)
{
	assert(intnum == -1 || intnum < num_ints);

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

	return retval;
}


/* called from ipc_send_half() on reply to interrupt thread */
int int_clear(int intnum, struct thread *sender)
{
	assert(sender != NULL);
	assert(intnum < num_ints);

	struct interrupt *it = &int_table[intnum];
	if(unlikely(sender != it->pager)) {
		return 2;	/* non-existing partner */
	} else if(unlikely(!it->pending || !it->delivered)) {
		/* can't clear an undelivered interrupt. the IPC wait isn't
		 * significant either.
		 *
		 * NOTE: this violates a send timeout if one is specified, even
		 * L4_Never. that's completely fine; interrupts don't get a "signal
		 * when clearable" mechanism.
		 */
		return 1;		/* timeout */
	} else {
		it->pending = false;
		it->delivered = false;
		int_unmask(intnum);
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


/* TODO: this doesn't clean up properly in various cases, such as thread
 * motion between address spaces.
 */
SYSCALL L4_Word_t sys_threadcontrol(
	L4_ThreadId_t dest_tid,
	L4_ThreadId_t pager, L4_ThreadId_t scheduler, L4_ThreadId_t spacespec,
	void *utcblocation)
{
	assert(check_thread_module(0));

	L4_Word_t ec = 0, result = 0, utcb_loc = (L4_Word_t)utcblocation;
	bool created = false;
	struct thread *current = get_current_thread();
	void *utcb = thread_get_utcb(current);
	if(unlikely(~current->space->flags & SF_PRIVILEGE)) goto no_priv;

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
	struct thread *sched = resolve_tid_spec(current->space, scheduler);
	if(!L4_IsNilThread(spacespec) && dest == NULL) {
		/* thread creation */
		if(L4_IsNilThread(scheduler) || sched == NULL) goto invd_sched;

		bool new_space = false;
		struct space *sp = space_find(spacespec.raw);
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
		created = true;
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
		L4_ThreadId_t dead_tid = { .raw = dest->id };
		cancel_pending_receive(dest->space, dead_tid.global,
			dest->utcb_pos >= 0 ? get_local_id(dest).local
				: L4_nilthread.local, 5);
		if(~dest->flags & TF_HALT) thread_halt(dest);
		if(dest->flags & TF_INTR) int_kick(dest);
		post_exn_fail(dest);
		if(dest->status != TS_STOPPED) {
			int old_status = dest->status;
			dest->status = TS_STOPPED;
			if(old_status != TS_READY || dest->total_quantum > 0) {
				sq_remove_thread(dest);
			}
		}
		x86_irq_disable();
		if(preempt_thread == dest) {
			preempt_thread = NULL;
			preempt_timer_count = ~0ull;
		}
		x86_irq_enable();
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
			cancel_pending_receive(dest->space, stale_tid.global,
				dest->utcb_pos >= 0 ? get_local_id(dest).local
					: L4_nilthread.local, 5);
			dest->id = dest_tid.raw;
			if(dest->utcb_pos >= 0) {
				void *utcb = thread_get_utcb(dest);
				L4_VREG(utcb, L4_TCR_MYGLOBALID) = dest->id;
				if(!post_exn_fail(dest)) {
					/* userspace Ipc status is "canceled in receive phase". */
					L4_VREG(utcb, L4_TCR_ERRORCODE) = 1 | (3 << 1);
					L4_VREG(utcb, L4_TCR_MR(0)) =
						(L4_MsgTag_t){ .X.flags = 0x8 }.raw;
				}
			}
			cancel_ipc_to(stale_tid, 2 << 1);	/* "lost partner" */
			assert(hook_empty(&dest->post_exn_call));

			if(dest->flags & TF_INTR) int_kick(dest);

			if(dest->status != TS_READY || dest->total_quantum > 0) {
				if(dest->status > 1) sq_remove_thread(dest);
			}
			dest->status = TS_STOPPED;
			dest->flags = 0;
			dest->ctx = (struct x86_ctx){ .eflags = X86_DEFAULT_USER_EFLAGS };

			assert(resolve_tid_spec(get_current_thread()->space,
				dest_tid) == dest);
		}

		/* TODO: move this up to make failure behave atomically. */
		if(spacespec.raw != dest_tid.raw && spacespec.raw != old_tid.raw) {
			struct space *to_sp = space_find(spacespec.raw);
			if(unlikely(to_sp == NULL)) goto invd_space;
			else if(to_sp != dest->space) {
				/* TODO: implement it */
				panic("TODO: movement of threads between spaces");
			}
		}
	} else {
		/* parameter fuckup. */
		goto unav_thread;
	}

	assert(dest != NULL);
	assert(dest->space != NULL);
	struct space *sp = dest->space;
	void *dest_utcb = NULL;
	if(!L4_IsNilThread(scheduler)) {
		if(unlikely(sched == NULL)) goto invd_sched;
		dest->scheduler.raw = sched->id;
	}

	int old_utcb_pos = dest->utcb_pos;
	if(utcb_loc != ~0ul) {
		assert(!L4_IsNilThread(spacespec));
		bool first = dest->utcb_pos < 0;

		/* set utcb_pos. */
		if(!ADDR_IN_FPAGE(sp->utcb_area, utcb_loc)
			|| (utcb_loc & (UTCB_SIZE - 1)) != 0)
		{
			goto bad_utcb;
		}

		int n = thread_set_utcb(dest, utcb_loc);
		if(n < 0) {
			if(n == -ENOMEM) goto out_of_mem;
			if(n == -EEXIST) goto bad_utcb;
			NOT_REACHED;
		}

		if(first) {
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
	if(old_utcb_pos < 0 && dest->utcb_pos >= 0) {
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
			/* FIXME: handle OOM! */
			hook_push_back(&dest->post_exn_call, &receive_breath_of_life, dest);
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
		if(created) thread_destroy(dest);
	}

	assert(check_thread_module(0));
	return result;

dead:
	if(unlikely(dest == current)) {
		assert(check_thread_module(0));
		exit_to_scheduler(NULL);
	}
	result = 1;
	goto end;

no_priv: ec = L4_ERROR_NO_PRIVILEGE; goto end;
out_of_mem: ec = L4_ERROR_NO_MEM; goto end;
unav_thread: ec = L4_ERROR_INVALID_THREAD; goto end;
invd_space: ec = L4_ERROR_INVALID_SPACE; goto end;
invd_sched: ec = L4_ERROR_INVALID_SCHEDULER; goto end;
bad_utcb: ec = L4_ERROR_UTCB_AREA; goto end;
}
