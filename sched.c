
#include <stdio.h>
#include <stdlib.h>
#include <ccan/list/list.h>
#include <ccan/likely/likely.h>

#include <l4/types.h>
#include <l4/thread.h>
#include <l4/schedule.h>
#include <l4/vregs.h>

#include <ukernel/rbtree.h>
#include <ukernel/util.h>
#include <ukernel/ipc.h>
#include <ukernel/space.h>
#include <ukernel/kip.h>
#include <ukernel/bug.h>
#include <ukernel/misc.h>
#include <ukernel/util.h>
#include <ukernel/thread.h>
#include <ukernel/trace.h>
#include <ukernel/hook.h>
#include <ukernel/bug.h>
#include <ukernel/interrupt.h>
#include <ukernel/cpu.h>
#include <ukernel/config.h>
#include <ukernel/sched.h>


#define TRACE(fmt, ...) TRACE_MSG(TRID_SCHED, fmt, ##__VA_ARGS__)

static struct thread *send_preempt_fault(struct thread *t, L4_Clock_t at);
static struct thread *send_preempt_exception(struct thread *t);


static struct rb_root sched_tree = { };

/* per-schedqueue attributes. */

/* wakeup time in microseconds since boot. applicable iff
 * current_thread->u0.partner != NULL.
 */
static uint64_t sched_chain_timeout = 0;

struct thread *current_thread = NULL;

/* these control the timer interrupt. write with irqs disabled only. */
struct thread *preempt_thread = NULL;
uint64_t preempt_timer_count = ~(uint64_t)0;
uint64_t task_switch_time = 0;
int preempt_status = 0;

/* pre-emption of threads while inside the kernel. */
struct thread *kernel_preempt_to = NULL;


static inline uint64_t runnable_at(const struct thread *t) {
	if(t->status == TS_R_RECV) return 0; else return t->wakeup_time;
}


/* this establishes a strict ordering between threads in order of
 *   - wakeup time (ascending)
 *   - priority (descending)
 *   - thread number (ascending, unique)
 *
 * therefore collisions can only occur when a thread is being inserted into
 * the tree twice.
 */
static inline int sq_cmp(const struct thread *a, const struct thread *b)
{
	if(a == b) return 0;

	/* while it'd be nice to convert this to a subtraction as well,
	 * wakeup_time may be ~0ull for a "never wake up" time. once these are no
	 * longer seen in the sched queue, replace this with a stanza like the
	 * ones below.
	 */
	uint64_t wa = runnable_at(a), wb = runnable_at(b);
	if(wa < wb) return -1;
	else if(wa > wb) return 1;

	/* descending priority order */
	int n = (int)b->pri - (int)a->pri;
	assert(n >= 0 || a->pri > b->pri);
	assert(n <= 0 || a->pri < b->pri);
	if(n != 0) return n;

	n = TID_THREADNUM(a->id) - TID_THREADNUM(b->id);
	assert(n >= 0 || TID_THREADNUM(a->id) < TID_THREADNUM(b->id));
	assert(n <= 0 || TID_THREADNUM(a->id) > TID_THREADNUM(b->id));
	return n;
}


static inline struct thread *sq_insert_thread_helper(
	struct rb_root *sq,
	struct thread *t)
{
	struct rb_node **p = &sq->rb_node, *parent = NULL;

	while(*p != NULL) {
		parent = *p;
		struct thread *other = rb_entry(parent, struct thread, sched_rb);

		int v = sq_cmp(t, other);
		if(v < 0) p = &(*p)->rb_left;
		else if(v > 0) p = &(*p)->rb_right;
		else return other;
	}

	rb_link_node(&t->sched_rb, parent, p);
	return NULL;
}


const char *sched_status_str(struct thread *t)
{
	int status = t->status;
	static const char *table[] = {
		[TS_STOPPED] = "stopped",
		[TS__UNUSED] = "***unused***",
		[TS_RUNNING] = "running",
		[TS_READY] = "ready",
		[TS_R_RECV] = "r_recv",
		[TS_SEND_WAIT] = "send_wait",
		[TS_RECV_WAIT] = "recv_wait",
		[TS_XFER] = "xfer",
		[0x42] = "***dummy***",
	};
	assert(status >= 0 && status < (sizeof(table) / sizeof(table[0])));
	return table[status];
}


static bool in_sq(struct thread *t)
{
#ifdef NDEBUG
	return true;
#else
	bool found = false;
	RB_FOREACH(node, &sched_tree) {
		if(node == &t->sched_rb) {
			found = true;
			break;
		}
	}
	return found;
#endif
}


void sq_insert_thread(struct thread *t)
{
	assert(t->status != TS_STOPPED);

	struct thread *dupe = sq_insert_thread_helper(&sched_tree, t);
	BUG_ON(dupe != NULL, "thread %lu:%lu already in tree",
		TID_THREADNUM(t->id), TID_VERSION(t->id));
	rb_insert_color(&t->sched_rb, &sched_tree);
}


void sq_remove_thread(struct thread *t)
{
#ifndef NDEBUG
	if(!in_sq(t)) {
		printf("%s: called from %p\n", __func__,
			__builtin_return_address(0));
	}
#endif
	assert(in_sq(t));

	rb_erase(&t->sched_rb, &sched_tree);
}


/* TODO: this should clearly be somewhere else. */
static bool closed_wait(struct thread *waiter, struct thread *sender)
{
	return (!L4_IsLocalId(waiter->ipc_from)
			&& waiter->ipc_from.raw == sender->id)
		|| (waiter->space == sender->space
			&& waiter->ipc_from.raw == get_local_id(sender).raw);
}


/* module invariant checks. */
#ifndef DEBUG_ME_HARDER
#define check_sched_module() true
#else

#include <ukernel/invariant.h>

/* check that @at is a valid wakeup time per all conversions of L4_Time_t to
 * raw microseconds.
 */
static bool is_wakeup_valid(uint64_t at)
{
	/* Never, ZeroTime */
	if(at == ~0ull || at == 0) return true;

	/* others. a relative L4_Time_t gives times up to 2^31 * 1023 µs, which is
	 * 610h 14m ~35.772s; an absolute L4_Time_t can put a wakeup at most
	 * ~33.5s from the current time, so we'll test only the longer option.
	 */
	uint64_t now = ksystemclock();
	int64_t diff = at - now;
	return diff <= (1ull << 31) * 1023;
}


static bool check_sched_module(void)
{
	INV_CTX;

	/* TODO: check states in the scheduling queue as well */

	/* invariants of <struct thread> (via IpcPartnerThread). */
	struct ra_iter it;
	for(struct thread *s = ra_first(thread_ra, &it);
		s != NULL;
		s = ra_next(thread_ra, &it))
	{
		if(s->space == NULL) continue;	/* burners etc. */
		inv_push("s=%lu:%lu", TID_THREADNUM(s->id), TID_VERSION(s->id));
		struct thread *t = s->u0.partner;
		if(t == NULL) {
			/* s ∉ \ran partner */
		} else {
			inv_ok(t->utcb_ptr_seg > 0, "partner thread must be active");
			inv_push("t=%lu:%lu; ->ipc_from=%lu:%lu (%#lx), ->status=%s",
				TID_THREADNUM(t->id), TID_VERSION(t->id),
				L4_ThreadNo(t->ipc_from), L4_Version(t->ipc_from),
				t->ipc_from.raw, sched_status_str(t));
			/* (t → s) ∈ partner */
			inv_ok1(t != s);
			/* $∃s: THREAD @ s.FromSpec = t.id$ is implied by this test. */
			inv_ok1(t->ipc_from.raw == s->id
				|| (t->space == s->space
					&& t->ipc_from.raw == get_local_id(s).raw));
			inv_ok1(t->status == TS_RECV_WAIT || t->status == TS_R_RECV);
			inv_pop();
		}
		inv_pop();
	}

	/* invariants of sched_chain_timeout wrt current_thread */
	if(current_thread != NULL && current_thread->u0.partner != NULL) {
		inv_ok1(is_wakeup_valid(sched_chain_timeout));

		int degree = 1;
		for(struct thread *t = current_thread->u0.partner;
			t != NULL;
			t = t->u0.partner, degree++)
		{
			/* FIXME: our vsnprintf() doesn't support long long types. it
			 * should. once it does, fix this shit.
			 */
			inv_push("partner (degree=%d), wakeup=%#lx", degree,
				(L4_Word_t)t->wakeup_time);
			inv_ok1(sched_chain_timeout <= t->wakeup_time);
			inv_pop();
		}
	}

	return true;

inv_fail:
	return false;
}

#endif


struct thread *sched_resolve_next(
	struct thread *current, void *cur_utcb,
	struct thread *next,
	struct thread *event)
{
	if(next == NULL) return event;
	if(event == NULL || event->pri <= next->pri) return next;

	assert(event != next);	/* implied by null and priority tests above */

	if(next == current
		&& event->pri <= current->sens_pri
		&& current->max_delay > 0
		&& CHECK_FLAG(L4_VREG(cur_utcb, L4_TCR_COP_PREEMPT), 0x40))
	{
		/* delay preemption. */
		L4_VREG(cur_utcb, L4_TCR_COP_PREEMPT) |= 0x80;	/* set "I" */
		return current;
	}

	/* switch. */
	return event;
}


void might_preempt(struct thread *t)
{
	struct thread *current = get_current_thread();
	TRACE("%s: t=%p, current=%p\n", __func__, t, current);
	if(current == NULL) return;
	if(t->pri > current->pri
		&& (kernel_preempt_to == NULL
			|| t->pri > kernel_preempt_to->pri)
		/* don't let a closed waiter pre-empt its waitee */
		&& (t->status != TS_R_RECV || !closed_wait(t, current)))
	{
		/* check the delay-preemption flag */
		L4_Word_t *ctl;
		if(current->max_delay > 0 && current->sens_pri >= t->pri
			&& (ctl = &L4_VREG(thread_get_utcb(current), L4_TCR_COP_PREEMPT),
				CHECK_FLAG(*ctl, 0x40)))
		{
			/* delay preemption. */
			uint64_t now = ksystemclock();
			int q_rem = MAX(int, 0,
					current->quantum - (now - task_switch_time * 1000)),
				pe_after = MIN(int, current->max_delay, q_rem);

			/* only set this delayed task switch if a previous preemptor's
			 * delay target is further out than this one's. this can happen
			 * iff current->max_delay or current->quantum becomes larger in
			 * between; the former may happen because of an opportune Schedule
			 * call and the latter due to timeslice borrowing.
			 */
			if(pe_after > 0
				&& (preempt_timer_count <= task_switch_time
					|| (preempt_thread != NULL && t->pri > preempt_thread->pri)
					|| preempt_timer_count * 1000 - now > pe_after))
			{
				*ctl |= 0x80;
				uint64_t ct = (now + pe_after) / 1000;
				x86_irq_disable_push();
				preempt_thread = t;
				preempt_timer_count = ct;
				preempt_status |= PS_DELAYED;
				x86_irq_restore();
			} else {
				assert(pe_after == 0 || preempt_timer_count * 1000 > now);
			}
			kernel_preempt_to = NULL;
		} else {
			/* make it snappy. */
			kernel_preempt_to = t;
		}
	}
}


/* simple IPC timeout. signaled to exactly one thread. */
static void timeout_ipc(struct thread *t)
{
	TRACE("%s: t=%lu:%lu\n", __func__,
		TID_THREADNUM(t->id), TID_VERSION(t->id));

	assert(hook_empty(&t->post_exn_call));
	assert(check_sched_module());

	bool is_send = t->status == TS_SEND_WAIT;
	/* break partnership on receive timeout. */
	if(!is_send) {
		assert(!L4_IsNilThread(t->ipc_from));
		struct thread *s = resolve_tid_spec(t->space, t->ipc_from);
		if(s != NULL && s->u0.partner == t) {
			TRACE("%s: breaking partnership w/ s=%lu:%lu\n", __func__,
				TID_THREADNUM(s->id), TID_VERSION(s->id));
			s->u0.partner = NULL;
		}
	}
	thread_ipc_fail(t);
	set_ipc_error_thread(t, (1 << 1) | (is_send ? 0 : 1));
	assert(check_sched_module());
}


static void desched_ready(
	struct hook *h,
	void *param, uintptr_t code, void *unused)
{
	struct thread *self = container_of(h, struct thread, post_exn_call);

	if(unlikely(code != 0)) {
		TRACE("%s: ipc failed (code=%lu, status=%s)\n", __func__,
			(unsigned long)code, sched_status_str(self));
		/* should come out of SEND_WAIT. */
		self->status = TS_READY;
	}

	/* put thread in tq-stop: remove from sched if ready, leave as is if
	 * stopped
	 */
	assert(self->status == TS_READY || self->status == TS_STOPPED);
	if(self->status == TS_READY) {
		sq_remove_thread(self);
	}
	hook_detach(h);
}


static bool send_tq_ipc(
	struct thread *sender, struct thread *dest,
	L4_Clock_t body)
{
	void *sender_utcb = thread_get_utcb(sender);
	void *msg = ipc_user((L4_MsgTag_t){ .X.label = 0xffd0, .X.u = 2 },
		sender, sender_utcb, &dest, 3);
	L4_VREG(msg, L4_TCR_MR(1)) = body.raw;
	L4_VREG(msg, L4_TCR_MR(2)) = body.raw >> sizeof(L4_Word_t) * 8;
	sender->ipc_from = L4_nilthread;
	return ipc_user_complete_oneway(sender, msg, &dest);
}


/* returns next thread that's up for scheduling, or NULL for none. */
static struct thread *enter_tq0_state(struct thread *self)
{
	struct thread *sched = L4_IsNilThread(self->scheduler)
		? NULL : thread_get(self->scheduler);
	if(sched == NULL) {
		printf("WARNING: no scheduler for %lu:%lu! thread stopped.\n",
			TID_THREADNUM(self->id), TID_VERSION(self->id));
		thread_halt(self);
	} else {
		L4_Clock_t sw_at = { .raw = ksystemclock() };
		hook_push_front(&self->post_exn_call, &desched_ready, NULL);
		if(!send_tq_ipc(self, sched, sw_at)) {
			assert(self->status == TS_SEND_WAIT);
			assert(in_sq(self));
			sched = NULL;
		} else {
			assert(self->status == TS_READY);
			assert(!in_sq(self));
		}
	}

	return sched;
}


/* dock a thread's quantums. not called for context switch on
 * self-deletion. may cause @self to halt or become READY outside the
 * scheduling queue (pending total_quantum refill).
 */
void leaving_thread(struct thread *self)
{
	assert(get_current_thread() == self);
	assert(self->status != TS_RUNNING);

	uint32_t passed = (read_global_timer() - task_switch_time) * 1000;
	if(passed > self->quantum) self->quantum = 0;
	else self->quantum -= passed;

	if(self->total_quantum != ~(uint64_t)0) {
		if(passed > self->total_quantum) self->total_quantum = 0;
		else self->total_quantum -= passed;

		if(self->total_quantum == 0 && IS_IPC(self->status)) {
			/* thread entered an IPC state before the kernel could notice that
			 * its total_quantum had run out. defer the message until next
			 * enter.
			 *
			 * FIXME: this is quite unlikely to happen in practice, and very
			 * very difficult to test as well!
			 */
			TRACE("%s: tq=0 ipc from %lu:%lu delayed because of %s via %p, %p\n",
				__func__, TID_THREADNUM(self->id), TID_VERSION(self->id),
				sched_status_str(self), __builtin_return_address(0),
				__builtin_return_address(1));
			self->total_quantum = 1;
		} else if(self->total_quantum == 0) {
			TRACE("%s: total_quantum ran out\n", __func__);
			enter_tq0_state(self);
		}
	}

	current_thread = NULL;
}


/* translate scheduling events into preëmption events according to existing
 * state.
 */
struct thread *on_preempt(int vec_num)
{
	x86_irq_disable_push();

	struct thread *next, *cand = preempt_thread,
		*current = get_current_thread();
	preempt_thread = NULL;
	if(current == NULL) {
		next = cand;
		TRACE("%s: straight thread switch\n", __func__);
		goto end;
	}
	void *cur_utcb = thread_get_utcb(current);
	L4_Word_t *ctl_p = &L4_VREG(cur_utcb, L4_TCR_COP_PREEMPT);

	int old_preempt_status = preempt_status,
		old_preemptflags = *ctl_p;
	if(CHECK_FLAG(preempt_status, PS_DELAYED)
		|| (CHECK_FLAG_ALL(*ctl_p, 0x60)
			&& current->max_delay == 0
			&& cand != NULL && cand->pri <= current->sens_pri))
	{
		/* previously-delayed preëmption's max_delay was hit; or preëmpted,
		 * set to signal and delay, is sensitive to cand.pri, but max_delay=0.
		 */
		assert(cand != NULL);		/* FIXME: this is wrong. */
		TRACE("%s: current=%lu:%lu hit max preempt delay; next=%lu:%lu\n",
			__func__, TID_THREADNUM(current->id), TID_VERSION(current->id),
			TID_THREADNUM(cand->id), TID_VERSION(cand->id));
		assert(cand->pri > current->pri);
		assert(cand->pri <= current->sens_pri);

		preempt_status = 0;
		*ctl_p &= ~0xc0;	/* clear I, d */
		if(!CHECK_FLAG(*ctl_p, 0x20)) {
			/* silent but delayed. */
			TRACE("%s: SBD.\n", __func__);
			next = sched_resolve_next(current, cur_utcb, current, cand);
		} else {
			/* send exception IPC immediately & resolve preemption of the
			 * existing candidate.
			 */
			TRACE("%s: sending preëmpt exception\n", __func__);
			struct thread *exh = send_preempt_exception(current);
			next = sched_resolve_next(current, cur_utcb, cand, exh);
			if(next == exh) {
				TRACE("%s: ... activating exh=%lu:%lu over cand=%lu:%lu\n",
					__func__, TID_THREADNUM(exh->id), TID_VERSION(exh->id),
					TID_THREADNUM(cand->id), TID_VERSION(cand->id));
			}
			assert(IS_IPC(current->status));
		}
		assert(next != current);
	} else {
		assert(current->status == TS_RUNNING);
		next = sched_resolve_next(current, cur_utcb, current, cand);
	}

	if(CHECK_FLAG(*ctl_p, 0x80)) {
		/* async preemption of @current was delayed. */
		assert(next == current);
		assert(!CHECK_FLAG(old_preemptflags, 0x80));

		uint64_t now = ksystemclock();
		int q_rem = MAX(int, 0,
				current->quantum - (now - task_switch_time * 1000)),
			pe_after = MIN(int, current->max_delay, q_rem);
		preempt_timer_count = (now + pe_after) / 1000;
		preempt_thread = cand;
		preempt_status |= PS_DELAYED;

		TRACE("%s: current=%lu:%lu preemption delayed for %dµs\n", __func__,
			TID_THREADNUM(current->id), TID_VERSION(current->id), pe_after);
	} else {
		/* async preemption of @current. */
		if(CHECK_FLAG(*ctl_p, 0x20)
			&& !CHECK_FLAG(old_preempt_status, PS_DELAYED)
			&& (next != current || current->quantum == 0))
		{
			TRACE("%s: set TF_PREEMPT on current=%lu:%lu\n", __func__,
				TID_THREADNUM(current->id), TID_VERSION(current->id));
			current->flags |= TF_PREEMPT;
		} else {
			TRACE("%s: silent async preempt (q=0 | tq=0) on current=%lu:%lu\n",
				__func__, TID_THREADNUM(current->id), TID_VERSION(current->id));
		}
		if(next == current) next = NULL;
		*ctl_p &= ~0x80;	/* clear "I" */
		preempt_status = 0;
	}

end:
	x86_irq_restore();
	return next;
}


static struct thread *preemptor_of(struct thread *self, uint64_t run_until)
{
	struct rb_node *pos = &self->sched_rb;
	pos = rb_next(pos);		/* skip self. */
	/* skip over threads with equal priority. */
	while(pos != NULL) {
		struct thread *other = container_of(pos, struct thread, sched_rb);
		if(other->pri > self->pri) break;
		pos = rb_next(pos);
	}
	/* out of the ones that preempt this thread, choose the one with earliest
	 * wakeup.
	 */
	struct thread *preemptor = NULL;
	while(pos != NULL) {
		struct thread *other = container_of(pos, struct thread, sched_rb);
		if(other->wakeup_time > run_until) break;
		if(other->pri > self->pri
			&& (preemptor == NULL
				|| other->wakeup_time < preemptor->wakeup_time)
			&& other->total_quantum > 0)
		{
			preemptor = other;
		}
		pos = rb_next(pos);
	}

	return preemptor;
}


/* set preemption parameters, current_thread. returns the thread that should
 * actually be returned into, which is different from @next iff a
 * higher-priority thread's wakeup happens before @switch_at.
 */
static struct thread *enter_next_thread(
	struct thread *next, uint64_t switch_at)
{
	assert(get_current_thread() == NULL);
	assert(hook_empty(&next->post_exn_call));
	assert(next->total_quantum > 0);
	assert(!CHECK_FLAG(next->flags, TF_PREEMPT));

	/* FIXME: a terrible hack that keeps everything working before timeslice
	 * borrowing comes about.
	 */
	if(next->quantum <= 0) next->quantum = 10000;	/* ewwwwwww */

	/* find the known descheduling point for @next. this can happen in one of
	 * three ways:
	 *   - its current quantum runs out, pending eventual refill via
	 *     schedule();
	 *   - its total_quantum runs out;
	 *   - a higher-priority thread encounters an IPC timeout within the
	 *     slice from the prior two.
	 */
	assert(next->quantum > 0);
	uint32_t runtime = next->quantum;	/* good for ~71.58 minutes */
	if(runtime > next->total_quantum) {
		runtime = next->total_quantum;
	}

	next->status = TS_RUNNING;
	struct thread *preemptor = preemptor_of(next, switch_at + runtime);
	/* modify the incoming @next in this rarest of cases. */
	if(preemptor != NULL && switch_at >= preemptor->wakeup_time) {
		TRACE("%s: bumping preemptor=%lu:%lu over next=%lu:%lu\n",
			__func__, TID_THREADNUM(preemptor->id), TID_VERSION(preemptor->id),
			TID_THREADNUM(next->id), TID_VERSION(next->id));
		next = preemptor;
		preemptor = NULL;
	}
	if(preemptor != NULL) {
		assert(switch_at < preemptor->wakeup_time);
		runtime = preemptor->wakeup_time - switch_at;

		TRACE("%s: next=%lu:%lu will be preempted by %lu:%lu in %uµs\n",
			__func__, TID_THREADNUM(next->id), TID_VERSION(next->id),
			TID_THREADNUM(preemptor->id), TID_VERSION(preemptor->id),
			(unsigned)runtime);
		TRACE("%s: ... next->pri=%d, preemptor->pri=%d\n",
			__func__, (int)next->pri, (int)preemptor->pri);
	}

	/* set up us the interrupt. */
	uint64_t preempt_at = (switch_at + runtime + 999) / 1000;
	TRACE("%s: running next=%lu:%lu for %uµs (preempt_at=%u, tq=%u)...\n",
		__func__, TID_THREADNUM(next->id), TID_VERSION(next->id), runtime,
		(unsigned)preempt_at, (unsigned)next->total_quantum);
	x86_irq_disable_push();
	current_thread = next;	/* move "zig" */
	preempt_timer_count = preempt_at;
	preempt_thread = preemptor;
	preempt_status = 0;
	task_switch_time = switch_at / 1000;
	x86_irq_restore();

	return next;
}


/* if *@next_p has TF_PREEMPT set, and resolving it causes its
 * exceptionhandler to pre-empt *@next_p, then *@next_p will be set to the
 * exceptionhandler, the thread-entering effect will take on that thread
 * instead of the earlier value, and the caller should exit to it.
 */
static void entering_thread(struct thread **next_p)
{
	uint64_t now = ksystemclock();
	struct thread *next = *next_p, *orig = next;
	if(CHECK_FLAG(next->flags, TF_PREEMPT)) {
		/* FIXME: get preemption time from somewhere! */
		TRACE("%s: sending preempt fault for %lu:%lu\n",
			__func__, TID_THREADNUM(next->id), TID_VERSION(next->id));
		struct thread *exh = send_preempt_fault(next,
				(L4_Clock_t){ .raw = now }),
			*oldnext = next;
		next = sched_resolve_next(NULL, NULL, next, exh);
		if(next == exh) {
			*next_p = next;
			TRACE("%s: exh trumping! status'=%s\n", __func__,
				sched_status_str(oldnext));
			TRACE("%s: ... replacing next with exh=%lu:%lu\n", __func__,
				TID_THREADNUM(exh->id), TID_VERSION(exh->id));
		}
		assert(next->status == TS_READY
			|| next->status == TS_RUNNING);
	}

	if(next->total_quantum == 1) {
		TRACE("%s: detected a delayed tq=0 condition\n", __func__);
		next = sched_resolve_next(NULL, NULL, next, enter_tq0_state(next));
		assert(next != orig);
		assert(next->status == TS_READY
			|| next->status == TS_RUNNING);
	}

	*next_p = enter_next_thread(next, now);
}


/* common tail between sched_ipc_handoff_*().
 * updates the proper scheduling of @src, and leaves @dst as it was.
 */
static void handoff_epilog(struct thread *src, struct thread *dst)
{
	/* FIXME: resolve TF_PREEMPT for @dst! */
	/* FIXME: modify caller's @dst! */
	struct thread *actual = enter_next_thread(dst, ksystemclock());
	assert(actual == dst);
	assert(dst->status == TS_RUNNING);
	if(src->u0.partner == dst) {
		/* break it up */
		src->u0.partner = NULL;
		/* TODO: maintain the cached timeout value */
	}
	assert(dst == get_current_thread());
	sq_update_thread(src);
}


void sched_ipc_handoff_quick(struct thread *src, struct thread *dst)
{
	TRACE("handoff[quick]: %lu:%lu -> %lu:%lu\n",
		TID_THREADNUM(src->id), TID_VERSION(src->id),
		TID_THREADNUM(dst->id), TID_VERSION(dst->id));

	assert(src->status == TS_RECV_WAIT || src->status == TS_R_RECV);
	assert(src == get_current_thread());
	assert(check_sched_module());

	src->wakeup_time = ~(uint64_t)0;
	leaving_thread(src);
	handoff_epilog(src, dst);
	assert(check_sched_module());
}


void sched_ipc_handoff_timeout(
	struct thread *src, struct thread *dst, L4_Time_t timeout)
{
	TRACE("handoff[timeout]: %lu:%lu -> %lu:%lu\n",
		TID_THREADNUM(src->id), TID_VERSION(src->id),
		TID_THREADNUM(dst->id), TID_VERSION(dst->id));
	assert(src->status == TS_RECV_WAIT || src->status == TS_R_RECV);
	assert(src == get_current_thread());
	assert(check_sched_module());

	/* this can be quite slow because of the ksystemclock() call within
	 * wakeup_at().
	 */
	src->wakeup_time = wakeup_at(src->recv_timeout);
	leaving_thread(src);
	handoff_epilog(src, dst);
	assert(check_sched_module());
}


/* note that @with_ltid is allowed to be nilthread, but with_gtid isn't. */
void cancel_pending_receive(
	struct space *sp,
	L4_GthreadId_t with_gtid, L4_LthreadId_t with_ltid,
	L4_Word_t errcode)
{
	assert(with_gtid.raw != L4_anythread.raw);
	assert(with_gtid.raw != L4_nilthread.raw);
	assert(with_ltid.raw != L4_anylocalthread.raw);
	assert(with_ltid.X.zeros == 0);
	assert((errcode & 1) == 1);		/* code indicates receive phase */

	for(struct rb_node *cur = rb_first(&sched_tree);
		cur != NULL;
		cur = rb_next(cur))
	{
		struct thread *t = rb_entry(cur, struct thread, sched_rb);
		if(runnable_at(t) > 0) break;

		if(t->status == TS_R_RECV
			&& (t->ipc_from.raw == with_gtid.raw
				|| (with_ltid.raw != L4_nilthread.raw
					&& t->ipc_from.raw == with_ltid.raw
					&& t->space == sp)))
		{
			if(!post_exn_fail(t)) {
				set_ipc_error_thread(t, errcode);
				thread_wake(t);
			}
		}
	}

#ifndef NDEBUG
	/* assert that there are no R_RECV threads remaining for the two TIDs. */
	for(struct rb_node *cur = rb_first(&sched_tree);
		cur != NULL;
		cur = rb_next(cur))
	{
		struct thread *t = rb_entry(cur, struct thread, sched_rb);
		assert(t->status != TS_R_RECV
			|| (t->ipc_from.raw != with_gtid.raw
				&& (with_ltid.raw == L4_nilthread.raw
					|| t->ipc_from.raw != with_ltid.raw
					|| t->space != sp)));
	}
#endif
}


/* when this function returns NULL and *saw_zero_p is set to true, the
 * scheduler should grant all ready threads another timeslice and redo from
 * start. (the flag indicates that a thread with a zero quantum was skipped
 * over.)
 */
static struct thread *schedule_next_thread(bool *saw_zero_p)
{
	const uint64_t now = ksystemclock();

	struct thread *pick = NULL;
	bool saw_zero = false;
	int last_pri = 255;
	for(struct rb_node *cur = rb_first(&sched_tree), *next;
		cur != NULL;
		cur = next)
	{
		next = rb_next(cur);

		struct thread *cand = rb_entry(cur, struct thread, sched_rb);
#if 0
		TRACE("%s: cand=%lu:%lu, pri=%d, status=%s\n", __func__,
			TID_THREADNUM(cand->id), TID_VERSION(cand->id), (int)cand->pri,
			sched_status_str(cand));
#endif

		assert(cand->status != TS_STOPPED);

		if(cand->status == TS_SEND_WAIT || cand->status == TS_RECV_WAIT) {
#if 0
			TRACE("%s: ... wakeup_time=%u ms\n", __func__,
				(unsigned)(cand->wakeup_time / 1000));
#endif
			if(cand->wakeup_time > now) {
				/* no need to look any further; no candidate was found. */
				break;
			}

			/* timed out. */
			if(cand->ipc != NULL) {
				ipc_xfer_timeout(cand->ipc);
				assert(cand->ipc == NULL);
				assert(!IS_IPC_WAIT(cand->status));
			} else {
				timeout_ipc(cand);
			}
			if(cand->status != TS_READY) continue;
		} else if(cand->status == TS_R_RECV
			&& cand->recv_timeout.raw != L4_ZeroTime.raw
			&& cand->wakeup_time < now)
		{
			/* apply timeout to a "ready to active receive" thread. the
			 * justification is that the timeout should occur before the
			 * active receive does even if the timeout is caused by scheduling
			 * delay.
			 */
			timeout_ipc(cand);
			if(cand->status != TS_READY) continue;
		}

		/* ignore lower-priority threads if a zero-quantum thread was seen
		 * with a higher priority.
		 */
		if(cand->pri < last_pri) {
			if(saw_zero) continue; else last_pri = cand->pri;
		}

		if(cand->quantum == 0) saw_zero = true;
		else if(pick == NULL || pick->pri < cand->pri) pick = cand;
	}

	*saw_zero_p = saw_zero;
	return pick;
}


NORETURN void exit_to_thread(struct thread *next)
{
	assert(!x86_irq_is_enabled());
	assert(get_current_thread() == next);

	space_switch(next->space);
	cop_switch(next);

	int gs_sel = next->utcb_ptr_seg << 3 | 3;
	if(USE_SYSENTER && CHECK_FLAG(next->flags, TF_SYSCALL)) {
		/* speedy gonzales */
		assert(ADDR_IN_FPAGE(next->space->kip_area, next->ctx.eip));
		next->flags &= ~TF_SYSCALL;
		TRACE("%s: before sysexit, next->ctx.eflags=%#lx\n",
			__func__, next->ctx.eflags);
		sysexit_to_ring3(&next->ctx, gs_sel);
	} else {
		assert(next->utcb_ptr_seg != 0);
		next->flags &= ~TF_SYSCALL;
		TRACE("%s: before iret, next->ctx.eflags=%#lx\n",
			__func__, next->ctx.eflags);
		iret_to_ring3(&next->ctx, gs_sel);
	}
}


static void maybe_exit_to_preempt(struct thread *current)
{
	assert(current != NULL);
	assert(kernel_preempt_to != NULL);

	struct thread *next = kernel_preempt_to;
	kernel_preempt_to = NULL;

	TRACE("%s: current=%lu:%lu, next=%lu:%lu\n", __func__,
		TID_THREADNUM(current->id), TID_VERSION(current->id),
		TID_THREADNUM(next->id), TID_VERSION(next->id));

	if(current == next) {
		/* the preemptor would've been switched to anyway (e.g. because of IPC
		 * scheduling). skip preemption and allow return to userspace either
		 * directly or through scheduling.
		 */
		return;
	}

	if(next->status == TS_R_RECV) {
		/* handle this case through the scheduler; it'd be completely
		 * unacceptable to run ipc_recv_half() from an interrupt-disabled
		 * section, e.g. after return_from_exn().
		 */
		if(current == get_current_thread()) leaving_thread(current);
		exit_to_scheduler(current);
	}

	/* TODO: check for delayed pre-emption and pre-empt signaling. */

	/* otherwise, enter the pre-emptor as usual. */
	assert(current->status == TS_RUNNING);
	current->status = TS_READY;

	if(current == get_current_thread()) {
		assert(current->wakeup_time == 0);
		leaving_thread(current);
	}

	if(next->quantum <= 0) {
		/* the idea here is that the preemptor is the only ready thread in its
		 * priority band, so schedule()'s timeslice-adding loop would do this
		 * exact same thing.
		 */
		if(next->ts_len.raw == L4_Never.raw) {
			next->quantum = ~(uint32_t)0;
		} else {
			uint64_t ts = time_in_us(next->ts_len);
			do {
				next->quantum += ts;
			} while(next->quantum <= 0);
		}
	}
	TRACE("%s: ... switching to pre-emptor\n", __func__);
	entering_thread(&next);
	exit_to_thread(next);
}


static struct thread *send_preempt_exception(struct thread *t)
{
	void *utcb = thread_get_utcb(t);
	struct thread *exh = get_tcr_thread(t, utcb, L4_TCR_EXCEPTIONHANDLER);
	if(unlikely(exh == NULL)) {
		thread_halt(t);
	} else {
		TRACE("%s: exh=%lu:%lu\n", __func__,
			TID_THREADNUM(exh->id), TID_VERSION(exh->id));
		struct x86_exregs fake_exregs = {
			.r = t->ctx.r,
			.eip = t->ctx.eip,
			.esp = t->ctx.r.esp,
			.reason = -4,
		};
		ipc_user_complete(t,
			send_exn_ipc(t, utcb, -4, &fake_exregs, &exh),
			&exh);
		assert(IS_IPC(t->status));
	}

	return exh;
}


/* used to send a preemption fault at thread resume. returns exh for preempt
 * checks.
 */
static struct thread *send_preempt_fault(struct thread *t, L4_Clock_t at)
{
	assert(t->status == TS_READY);

	void *t_utcb = thread_get_utcb(t);
	struct thread *exh = get_tcr_thread(t, t_utcb, L4_TCR_EXCEPTIONHANDLER);
	if(exh != NULL) {
		struct thread *dst = exh;
		void *msg_utcb = ipc_user((L4_MsgTag_t){ .X.label = 0xffd0, .X.u = 2 },
			t, t_utcb, &dst, 2);
		L4_VREG(msg_utcb, L4_TCR_MR(1)) = at.raw >> 32;
		L4_VREG(msg_utcb, L4_TCR_MR(2)) = at.raw & 0xffffffff;
		t->send_timeout = L4_ZeroTime;
		bool done = ipc_user_complete_oneway(t, msg_utcb, &dst);
		if(!done) exh = NULL;	/* can't itself preempt. */
		assert(hook_empty(&t->post_exn_call));
	}

	assert(t->status == TS_READY || t->status == TS_RUNNING);
	return exh;
}


static NORETURN void schedule(void *param_ptr)
{
	assert(get_current_thread() == NULL);
	struct thread *prev = param_ptr;
	uint64_t now;

again:
	assert(x86_irq_is_enabled());
	x86_irq_disable();
	now = ksystemclock();
	if(irq_defer_active) {
		/* the singing scheduler */
		irq_call_deferred(NULL);
	}
	x86_irq_enable();
	if(kernel_preempt_to != NULL) {
		TRACE("%s: kernel_preempt_to=%lu:%lu\n", __func__,
			TID_THREADNUM(kernel_preempt_to->id),
			TID_VERSION(kernel_preempt_to->id));
		if(likely(prev != NULL) && prev->status == TS_RUNNING) {
			maybe_exit_to_preempt(prev);
		} else {
			/* pre-emption at boot, pre-emption of a dead thread, or one
			 * that's sleeping.
			 */
			kernel_preempt_to = NULL;
		}
	}

	assert(kernel_preempt_to == NULL);
	assert(prev == NULL || prev->status != TS_RUNNING);

	/* find next ready thread. */
	bool new_slices;
	struct thread *next = schedule_next_thread(&new_slices);
#ifdef DEBUG_ME_HARDER
	if(next != NULL) {
		TRACE("%s: next=%p (%lu:%lu), status=%s\n", __func__, next,
			TID_THREADNUM(next->id), TID_VERSION(next->id),
			sched_status_str(next));
	}
#endif
	if(next == NULL && !new_slices) {
		/* system is idle. */
		assert(check_sched_module());
		kernel_irq_ok = true;
		asm volatile ("hlt" ::: "memory");
		kernel_irq_ok = false;
		goto again;
	} else if(next == NULL) {
		/* add timeslices to threads that're ready and have none. */
		TRACE("%s: refilling!\n", __func__);
		RB_FOREACH(node, &sched_tree) {
			struct thread *t = rb_entry(node, struct thread, sched_rb);
			if(IS_READY(t->status) && t->quantum == 0
				&& t->ts_len.raw != L4_ZeroTime.raw)
			{
				/* just take a slice, you, you, double thread */
				if(t->ts_len.raw == L4_Never.raw) {
					t->quantum = 60000000;	/* a minute */
				} else {
					t->quantum = time_in_us(t->ts_len);
					TRACE("refilled %lu:%lu with %u µs\n",
						TID_THREADNUM(t->id), TID_VERSION(t->id),
						t->quantum);
				}
			} else if(IS_IPC_WAIT(t->status) && t->wakeup_time > now) {
				/* no need to advance past this point. */
				break;
			}
		}
		goto again;
	}

	if(CHECK_FLAG(next->flags, TF_PREEMPT)) {
		/* signal preemption. */
		TRACE("%s: sending preempt fault for next=%lu:%lu\n", __func__,
			TID_THREADNUM(next->id), TID_VERSION(next->id));
		next->flags &= ~TF_PREEMPT;
		/* FIXME: get preempt time from somewhere */
		struct thread *exh = send_preempt_fault(next,
			(L4_Clock_t){ .raw = now });
		TRACE("%s: ... next->status'=%s\n", __func__, sched_status_str(next));
		/* (XFER and R_RECV threads don't preempt.) */
		assert(next->status == TS_READY || next->status == TS_RUNNING);
		next = sched_resolve_next(NULL, NULL, next, exh);
		if(next == exh) TRACE("%s: exh > next\n", __func__);
	}

	if(next->status == TS_XFER) {
		TRACE("%s: resume!\n", __func__);
		bool done = ipc_resume(next);
		if(!done || !IS_READY(next->status)) goto again;
	}
	/* not exclusive with previous, as ipc_resume() sets @next to TS_R_RECV
	 * when it was the sender of a call
	 */
	if(next->status == TS_R_RECV) {
		TRACE("%s: receive half in next=%lu:%lu!\n", __func__,
			TID_THREADNUM(next->id), TID_VERSION(next->id));
		assert(!L4_IsNilThread(next->ipc_from));
		ipc_recv_half(next, thread_get_utcb(next));
		if(next->status != TS_READY) {
			/* either entered passive receive (and not eligible to run
			 * anymore), or preempted by sender or sender's redirection.
			 * try again.
			 */
			TRACE("%s: starting over (status=%s)!\n", __func__,
				sched_status_str(next));
			goto again;
		}
	}

	TRACE("%s: %lu:%lu -> %lu:%lu\n", __func__,
		prev == NULL ? 0 : TID_THREADNUM(prev->id),
		prev == NULL ? 0 : TID_VERSION(prev->id),
		TID_THREADNUM(next->id), TID_VERSION(next->id));

	x86_irq_disable();
	assert(get_current_thread() == NULL);
	if(irq_defer_active) next = irq_call_deferred(next);
	struct thread *actual = enter_next_thread(next, now);
	assert(actual == next);	/* always true. */
	exit_to_thread(next);
}


NORETURN void exit_to_scheduler(struct thread *prev)
{
	assert(x86_irq_is_enabled());

	L4_Word_t *stk = syscall_stack + KERNEL_STACK_SIZE - 16;
	*(--stk) = (L4_Word_t)prev;
	call_on_stack(&schedule, stk);
	panic("schedule() returned [it should never]");
}


/* this function is a NORETURN due to non-local exit. it'll always either go
 * into the scheduler (syscall stack to kth) or do a preemption (in r_f_e()).
 */
NORETURN void return_to_scheduler(void)
{
	struct thread *self = get_current_thread();
	leaving_thread(self);
	exit_to_scheduler(self);
}


void return_from_irq(struct thread *next)
{
	assert(!x86_irq_is_enabled());
	if(irq_defer_active) next = irq_call_deferred(next);

	struct thread *current = get_current_thread();
	if(current != NULL && next != current) {
		/* preëmpted or descheduled. */
		void *cur_utcb = thread_get_utcb(current);
		L4_Word_t *ctl_p = &L4_VREG(cur_utcb, L4_TCR_COP_PREEMPT);
		if(CHECK_FLAG(*ctl_p, 0x20)) current->flags |= TF_PREEMPT;
		*ctl_p &= ~0x80;	/* clear "I" */

		if(current->status == TS_RUNNING) {
			current->status = TS_READY;
			current->wakeup_time = 0;
			sq_update_thread(current);
		}
		assert(current->status == TS_READY
			|| current->status == TS_R_RECV
			|| current->status == TS_SEND_WAIT);
		leaving_thread(current);

		TRACE("%s: preempt or deschedule of current=%lu:%lu\n", __func__,
			TID_THREADNUM(current->id), TID_VERSION(current->id));
	}

	if(next == NULL) {
		/* TODO: make scheduler depend on !intena */
		TRACE("%s: going to scheduler\n", __func__);
		assert(!kernel_irq_ok);
		x86_irq_enable();
		exit_to_scheduler(current);
	} else if(next != current) {
		/* switch into other thread. */
		assert(next != NULL);
		if(IS_IPC(next->status)) {
			assert(next != current);
			timeout_ipc(next);
			assert(next->status == TS_READY);
		}
		entering_thread(&next);
		assert(next != NULL);
		TRACE("%s: going to next=%lu:%lu\n", __func__,
			TID_THREADNUM(next->id), TID_VERSION(next->id));
		exit_to_thread(next);
	} else {
		/* exit via caller. */
	}
}


/* this must be the last line of an exception handler that doesn't call one of
 * the return_to_*() family of kernel exits. it tests for and handles latent
 * interrupts, and resolves in-kernel preëmption.
 */
void return_from_exn(void)
{
	x86_irq_disable();
	struct thread *current = get_current_thread();
	/* FIXME: check in-kernel preëmptions with sched_resolve_next()! */
	// if(kernel_preempt_to != NULL) maybe_exit_to_preempt(current);
	return_from_irq(current);
}


void return_to_other(struct thread *other)
{
	struct thread *current = get_current_thread();

	assert(other != current);
	/* TODO: resolve R_RECV first */
	if(other->status != TS_READY) return;

	TRACE("%s: %c-%c: %lu:%lu -> %lu:%lu\n", __func__,
		current->space == kernel_space ? 'K' : 'U',
		other->space == kernel_space ? 'K' : 'U',
		TID_THREADNUM(current->id), TID_VERSION(current->id),
		TID_THREADNUM(other->id), TID_VERSION(other->id));

	current->status = TS_READY;
	leaving_thread(current);

	/* donate remaining timeslice, but only to a thread of equal or lower
	 * priority. this prevents quantums from being leaked to higher-priority
	 * threads.
	 *
	 * note that this mechanism effectively overrides regular scheduling until
	 * the other thread is preempted by another process, or by its quantum
	 * running out.
	 */
	if(current->pri >= other->pri) {
		uint32_t new_ts = other->quantum + current->quantum;
		if(new_ts < other->quantum) new_ts = ~(uint32_t)0;
		current->quantum = 0;
		other->quantum = new_ts;
	}

	entering_thread(&other);
	return_from_exn();
	exit_to_thread(other);
}


NORETURN void return_to_partner(void)
{
	struct thread *current = get_current_thread(),
		*other = current->u0.partner;
	assert(current->u0.partner != NULL);

	leaving_thread(current);
	handoff_epilog(current, other);
	return_from_exn();
	exit_to_thread(other);
}


NORETURN void return_to_ipc(void *msg_utcb, struct thread *target)
{
	struct thread *cur = get_current_thread();
	assert(!hook_empty(&cur->post_exn_call));
	if(msg_utcb == thread_get_utcb(cur)) {
		/* POKE slow, 1 */
		ipc_user_complete(cur, msg_utcb, &target);
		return_to_scheduler();
	} else {
		/* faster, pussycat! */
		assert(IS_SCHED(cur));
		cur->status = TS_R_RECV;
		cur->wakeup_time = ~0ull;
		sq_update_thread(cur);
		assert(msg_utcb == thread_get_utcb(target));
		set_ipc_return_regs(&target->ctx.r, target, msg_utcb);
		thread_wake(target);
		bool was_partner = target->u0.partner == cur;
		sched_ipc_handoff_quick(cur, target);
		if(!was_partner) target->u0.partner = cur;
		return_from_exn();
		exit_to_thread(target);
	}
}


SYSCALL void sys_threadswitch(L4_ThreadId_t target)
{
	struct thread *current = get_current_thread();

	TRACE("%s: called in %lu:%lu; target is %lu:%lu\n",
		__func__, TID_THREADNUM(current->id), TID_VERSION(current->id),
		L4_ThreadNo(target), L4_Version(target));

	struct thread *other;
	if(!L4_IsNilThread(target)
		&& (other = resolve_tid_spec(current->space, target)) != NULL)
	{
		/* on failure (i.e. other not TS_READY), return to caller. also return
		 * if other isn't.
		 */
		if(other != current) return_to_other(other);
	} else {
		current->status = TS_READY;
		/* the thread gets a new quantum once other threads have run. */
		current->quantum = 0;
		return_to_scheduler();
	}
}


static bool sched_valid_time(L4_Time_t t) {
	return !(t.raw == L4_ZeroTime.raw || L4_IsTimePoint_NP(t));
}


static int ipc_xfer_schedstate(struct thread *t) {
	assert(t->ipc != NULL);
	return t->ipc->to == t ? L4_SCHEDRESULT_RECEIVING : L4_SCHEDRESULT_SENDING;
}


SYSCALL L4_Word_t sys_schedule(
	L4_ThreadId_t dest_tid,
	L4_Word_t prioctl,
	L4_Word_t *timectl_p,
	L4_Word_t procctl,
	L4_Word_t preemptctl)
{
	struct thread *current = get_current_thread();

	L4_Word_t result, ec = 0, timectl = *timectl_p;

	/* test for user TID range */
	if(L4_IsGlobalId(dest_tid)
		&& unlikely(L4_ThreadNo(dest_tid) < first_user_threadno()))
	{
		result = L4_SCHEDRESULT_ERROR;
		ec = L4_ERROR_INVALID_THREAD;
		goto end_noupdate;
	}

	struct thread *dest = NULL;
	if(L4_IsNilThread(dest_tid)
		|| (dest = resolve_tid_spec(current->space, dest_tid)) == NULL)
	{
		result = L4_SCHEDRESULT_DEAD;
		goto end_noupdate;
	}

	/* access check */
	struct thread *sched_thread = resolve_tid_spec(
		dest->space, dest->scheduler);
	if(unlikely(sched_thread == NULL)) goto inv_param;
	if(sched_thread->space != current->space) {
		result = L4_SCHEDRESULT_ERROR;
		ec = L4_ERROR_NO_PRIVILEGE;
		goto end_noupdate;
	}

	/* cooked inputs */
	L4_Time_t ts_len = { .raw = (timectl >> 16) & 0xffff },
		total_quantum = { .raw = timectl & 0xffff };
	if((ts_len.raw != 0xffff && !sched_valid_time(ts_len))
		|| (total_quantum.raw != 0xffff && !sched_valid_time(total_quantum)))
	{
		goto inv_param;
	}
	L4_Word_t pri = prioctl & 0xff;
	if(pri != 0xff && pri > current->pri) goto inv_param;
	L4_Word_t procnum = procctl & 0xffff,
		sens_pri = (preemptctl >> 16) & 0xff,
		max_delay = preemptctl & 0xffff;
	/* NOTE: this deviates from the L4.X2 spec, which doesn't specify that
	 * sens_pri must be equal or lower to the scheduler's priority.
	 *
	 * (also: this and @pri should be compared to the higher of the actual
	 * scheduler thread's and @current's priorities.)
	 */
	if(sens_pri != 0xff && sens_pri > current->pri) {
		goto inv_param;
	}

	L4_Word_t tq = dest->total_quantum == 1 ? 0 : dest->total_quantum;
	*timectl_p = (L4_Word_t)L4_TimePeriod(dest->quantum).raw << 16
		| L4_TimePeriod(tq).raw;

	if(IS_IPC(dest->status)
		/* TODO: check flag to see if current IPC is by kernel, instead */
		&& !hook_empty(&dest->post_exn_call))
	{
		result = L4_SCHEDRESULT_RUNNING;
	} else {
		static const uint8_t status_to_schedresult[] = {
			[TS_STOPPED] = L4_SCHEDRESULT_INACTIVE,
			[TS__UNUSED] = L4_SCHEDRESULT_DEAD,	/* undefined */
			[TS_RUNNING] = L4_SCHEDRESULT_RUNNING,
			[TS_READY] = L4_SCHEDRESULT_RUNNING,
			[TS_SEND_WAIT] = L4_SCHEDRESULT_PENDING_SEND,
			[TS_RECV_WAIT] = L4_SCHEDRESULT_WAITING,
			[TS_R_RECV] = L4_SCHEDRESULT_WAITING,
			[TS_XFER] = 0,		/* signals a thing */
		};
		int s = dest->status;
		if(s == TS_XFER) result = ipc_xfer_schedstate(dest);
		else {
			assert(s >= 0 && s < NUM_ELEMENTS(status_to_schedresult));
			result = status_to_schedresult[s];
			assert(result > 0);
		}
	}

	/* timectl */
	if(timectl != ~(L4_Word_t)0) {
		if(ts_len.raw != 0xffff) {
			dest->ts_len = ts_len;
			uint64_t slice = time_in_us(ts_len);
			if(slice > ~(uint32_t)0) {
				slice = ~(uint32_t)0;
				dest->ts_len = ts_len = L4_TimePeriod(slice);
			}
			dest->quantum = slice;
		}
		if(total_quantum.raw != 0xffff) {
			bool re_act = dest->status == TS_READY
				&& dest->total_quantum == 0;
			if(total_quantum.raw == L4_Never.raw) {
				dest->total_quantum = ~(uint64_t)0;
			} else {
				dest->total_quantum = time_in_us(total_quantum);
			}
			dest->quantum = time_in_us(dest->ts_len);

			if(re_act) sq_insert_thread(dest);
		}
	}

	/* prioctl */
	if(pri != 0xff) dest->pri = pri;

	/* procctl */
	/* TODO: check procnum against KIP's max_procs value */
	if(procnum > 0) {
		/* ignored. */
	}

	/* preemptctl */
	if(sens_pri != 0xff) dest->sens_pri = sens_pri;
	if(max_delay != 0xffff) dest->max_delay = max_delay;

end:
	if(dest != NULL && dest->status != TS_STOPPED) {
		sq_update_thread(dest);
	}

end_noupdate:
	if(unlikely(ec != 0)) {
		assert((result & 0xff) == L4_SCHEDRESULT_ERROR);
		L4_VREG(thread_get_utcb(current), L4_TCR_ERRORCODE) = ec;
	}
	return result;

inv_param:
	result = L4_SCHEDRESULT_ERROR;
	ec = L4_ERROR_INVALID_PARAM;
	goto end;
}
