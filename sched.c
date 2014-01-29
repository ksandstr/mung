
#include <stdio.h>
#include <stdlib.h>
#include <ccan/list/list.h>
#include <ccan/htable/htable.h>
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
#include <ukernel/thread.h>
#include <ukernel/trace.h>
#include <ukernel/hook.h>
#include <ukernel/bug.h>
#include <ukernel/interrupt.h>
#include <ukernel/cpu.h>
#include <ukernel/sched.h>


/* for "[KU]-[KU]: %d:%d -> %d:%d" prints */
#define TRACE(fmt, ...) TRACE_MSG(TRID_SCHED, fmt, ##__VA_ARGS__)


static struct thread *scheduler_thread = NULL;
static struct rb_root sched_tree = { };

struct thread *current_thread = NULL;

/* these control the timer interrupt. write with irqs disabled only. */
uint64_t preempt_timer_count = ~(uint64_t)0;
uint64_t task_switch_time = 0;
L4_Word_t *scheduler_mr1 = NULL;
int preempt_task_pri = 0;
bool preempt_delayed = false;


COLD void init_sched(struct thread *current)
{
	current_thread = current;
}


static inline uint64_t runnable_at(const struct thread *t) {
	if(t->status == TS_R_RECV) return 0; else return t->wakeup_time;
}


/* this establishes a strict ordering between threads in order of
 *   - wakeup time (per microsecond)
 *   - priority
 *   - thread number (unique)
 *
 * therefore collisions can only occur when a thread is being inserted into
 * the tree twice.
 */
static inline int sq_cmp(const struct thread *a, const struct thread *b)
{
	if(a == b) return 0;

	uint64_t wa = runnable_at(a), wb = runnable_at(b);
	if(wa < wb) return -1;
	else if(wa > wb) return 1;

	if(a->pri > b->pri) return -1;
	else if(a->pri < b->pri) return 1;

	uint32_t na = TID_THREADNUM(a->id), nb = TID_THREADNUM(b->id);
	if(na < nb) return -1;
	else if(na > nb) return 1;
	else return 0;
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
		[TS_DEAD] = "dead",
		[TS_RUNNING] = "running",
		[TS_READY] = "ready",
		[TS_R_RECV] = "r_recv",
		[TS_SEND_WAIT] = "send_wait",
		[TS_RECV_WAIT] = "recv_wait",
		[TS_XFER] = "xfer",
	};
	assert(status >= 0 && status < (sizeof(table) / sizeof(table[0])));
	return table[status];
}


void sq_insert_thread(struct thread *t)
{
	assert(t->status != TS_STOPPED && t->status != TS_DEAD);

	struct thread *dupe = sq_insert_thread_helper(&sched_tree, t);
	BUG_ON(dupe != NULL, "thread %lu:%lu already in tree",
		TID_THREADNUM(t->id), TID_VERSION(t->id));
	rb_insert_color(&t->sched_rb, &sched_tree);
}


void sq_remove_thread(struct thread *t)
{
#ifndef NDEBUG
	bool found = false;
	RB_FOREACH(node, &sched_tree) {
		if(node == &t->sched_rb) {
			found = true;
			break;
		}
	}
	assert(found);
#endif

	rb_erase(&t->sched_rb, &sched_tree);
}


/* simple IPC timeout. signaled to exactly one thread. */
static void timeout_ipc(struct thread *t)
{
	assert(hook_empty(&t->post_exn_call));

	bool is_send = t->status == TS_SEND_WAIT;
	thread_ipc_fail(t);
	set_ipc_error_thread(t, (1 << 1) | (is_send ? 0 : 1));
}


static uint64_t next_preempt_at(
	int *preempt_pri,
	struct thread *self,
	uint64_t switch_at_us)
{
	*preempt_pri = -1;		/* can always be loud (see isr_irq0_bottom()) */
	uint64_t at = ~(uint64_t)0;

	struct rb_node *pos = &self->sched_rb;
	pos = rb_next(pos);		/* skip self. */
	/* skip over threads with equal priority. */
	while(pos != NULL) {
		struct thread *other = container_of(pos, struct thread, sched_rb);
		if(other->pri > self->pri) break;
		pos = rb_next(pos);
	}
	/* out of the ones that preempt this thread, choose the closest. */
	bool got = false;
	uint64_t q_end = switch_at_us + self->quantum;
	while(pos != NULL) {
		struct thread *other = container_of(pos, struct thread, sched_rb);
		assert(other->wakeup_time > switch_at_us);
		if(preempted_by(self, switch_at_us, other)) {
			if(other->wakeup_time < at) {
				*preempt_pri = other->pri;
				at = other->wakeup_time;
			}
			got = true;
		} else if(other->wakeup_time > q_end) {
			/* early exit */
			break;
		}
		pos = rb_next(pos);
	}

	return got ? at : 0;
}


/* dock a thread's quantum. not called for context switch on self-deletion. */
static void leaving_thread(struct thread *self)
{
	assert(self->status != TS_RUNNING);

	uint32_t passed = (read_global_timer() - task_switch_time) * 1000;
	if(passed > self->quantum) self->quantum = 0;
	else self->quantum -= passed;
}


/* set preemption parameters, current_thread */
static void entering_thread(struct thread *next)
{
	assert(hook_empty(&next->post_exn_call));

	/* TODO: find the clock tick when this thread'll be pre-empted due to
	 * exhausted quantum; or when the total quantum exhausted message will be
	 * triggered (XXX: what is that?); or when a higher-priority thread's IPC
	 * wakeup time occurs before the quantum expires.
	 *
	 * it's a simple enough walk through the sched tree _before_
	 * sq_update_thread() is called on "next".
	 */
	assert(next->quantum > 0);
	next->status = TS_RUNNING;
	task_switch_time = read_global_timer();
	int preempt_pri;
	uint64_t preempt_at = next_preempt_at(&preempt_pri, next,
		task_switch_time * 1000);
	if(next->ts_len.raw != L4_Never.raw) {
		uint64_t q_end = task_switch_time * 1000 + next->quantum;
		if(preempt_at == 0 || preempt_at > q_end) preempt_at = q_end;
	}

	/* write parameters used by the irq0 handler */
	preempt_at = (preempt_at + 999) / 1000;
	assert(x86_irq_is_enabled());
	x86_irq_disable();
	current_thread = next;
	preempt_timer_count = preempt_at;
	preempt_task_pri = preempt_pri;
	preempt_delayed = false;
	x86_irq_enable();
}


/* when this function returns NULL and *saw_zero_p is set to true, the
 * scheduler should grant all ready threads another timeslice and redo from
 * start. (the flag indicates that a thread with a zero quantum was skipped
 * over.)
 */
static struct thread *schedule_next_thread(
	struct thread *current,
	bool *saw_zero_p)
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
		if(cand == current) continue;

		assert(cand->status != TS_DEAD);
		assert(cand->status != TS_STOPPED);

		if(cand->status == TS_SEND_WAIT || cand->status == TS_RECV_WAIT) {
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

	assert(pick != current);
	*saw_zero_p = saw_zero;
	return pick;
}


/* saves scheduler context into *prev. loads space and thread context for
 * *next.
 */
static void switch_thread(struct thread *prev, struct thread *next)
{
	assert(IS_KERNEL_THREAD(prev));

	space_switch(next->space);
	cop_switch(next);

	/* load the new context */
	int gs_sel = !IS_KERNEL_THREAD(next) ? next->utcb_ptr_seg << 3 | 3 : 0;
	if(IS_KERNEL_THREAD(next)) {
		/* zomg optimized */
		swap_context(&prev->ctx, &next->ctx);
	} else if(use_sysenter && CHECK_FLAG(next->flags, TF_SYSCALL)) {
		assert(ADDR_IN_FPAGE(next->space->kip_area, next->ctx.eip));
		next->flags &= ~TF_SYSCALL;
		sysexit_from_kth(&prev->ctx, &next->ctx, gs_sel);
	} else {
		/* go go goblin balls! */
		assert(next->utcb_ptr_seg != 0);
		next->flags &= ~TF_SYSCALL;
		swap_to_ring3(&prev->ctx, &next->ctx, gs_sel);
	}
}


NORETURN void switch_thread_u2u(struct thread *next)
{
	assert(!IS_KERNEL_THREAD(get_current_thread()));
	assert(!IS_KERNEL_THREAD(next));

	space_switch(next->space);
	cop_switch(next);

	assert(next->utcb_ptr_seg != 0);
	int utcb_sel = next->utcb_ptr_seg << 3 | 3;
	/* TODO: use a compiletime macro for use_sysenter */
	if(use_sysenter && CHECK_FLAG(next->flags, TF_SYSCALL)) {
		assert(ADDR_IN_FPAGE(next->space->kip_area, next->ctx.eip));
		next->flags &= ~TF_SYSCALL;
		sysexit_to_ring3(&next->ctx, utcb_sel);
	} else {
		next->flags &= ~TF_SYSCALL;
		struct x86_exregs dummy;
		swap_to_ring3(&dummy, &next->ctx, utcb_sel);
		panic("u2u swap_to_ring3() shouldn't return!");
	}

	assert(false);
}


bool schedule(void)
{
	/* "self" is the kthread that calls schedule(). */
	struct thread *self = get_current_thread();
	assert(IS_KERNEL_THREAD(self));
	assert(self->status != TS_RUNNING);

	/* find next ready thread. */
	bool new_slices;
	struct thread *next = schedule_next_thread(self, &new_slices);
	if(next == NULL && !new_slices) {
		assert(scheduler_thread == NULL
			|| scheduler_thread == self);
		return false;
	} else if(next == NULL) {
		/* add timeslices to threads that're ready and have none. */
		uint64_t now = ksystemclock();
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
		return schedule();
	}

	if(next->status == TS_XFER) {
		assert(!IS_KERNEL_THREAD(next));
		bool preempt = false, done = ipc_resume(next, &preempt);
		if(!done || preempt) return schedule();
	}
	/* not exclusive with previous, as ipc_resume() sets @next to TS_R_RECV
	 * when it was the sender of a call
	 */
	if(next->status == TS_R_RECV) {
		/* FIXME: handle halted threads properly; they should leave the
		 * scheduling queue after the receive phase.
		 */
		assert(!IS_KERNEL_THREAD(next));
		bool preempt = false, r_done;
		r_done = ipc_recv_half(next, thread_get_utcb(next), &preempt);
		if((!r_done && next->status == TS_RECV_WAIT) || (r_done && preempt)) {
			/* either entered passive receive (and not eligible to run
			 * anymore), or preempted by the sender. try again.
			 */
			return schedule();
		}
	}

	TRACE("%s: %c-%c: %lu:%lu -> %lu:%lu\n", __func__,
		self->space == kernel_space ? 'K' : 'U',
		next->space == kernel_space ? 'K' : 'U',
		TID_THREADNUM(self->id), TID_VERSION(self->id),
		TID_THREADNUM(next->id), TID_VERSION(next->id));

	entering_thread(next);
	switch_thread(self, next);

	TRACE("%s: returned to %lu:%lu from going to %lu:%lu; (current_thread is %lu:%lu)\n",
		__func__,
		TID_THREADNUM(self->id), TID_VERSION(self->id),
		TID_THREADNUM(next->id), TID_VERSION(next->id),
		TID_THREADNUM(current_thread->id), TID_VERSION(current_thread->id));

	assert(current_thread == self);
	assert(self->status == TS_RUNNING);

	return true;
}


NORETURN void scheduler_loop(struct thread *self)
{
	assert(scheduler_mr1 == NULL);
	scheduler_thread = self;
	scheduler_mr1 = &L4_VREG(thread_get_utcb(self), L4_TCR_MR(1));
	while(true) {
		*scheduler_mr1 = L4_nilthread.raw;
		self->status = TS_READY;
		if(kernel_irq_deferred) int_latent();
		if(!schedule()) {
			kernel_irq_ok = true;
			asm volatile ("hlt" ::: "memory");
			kernel_irq_ok = false;
		} else if(*scheduler_mr1 != L4_nilthread.raw) {
			L4_ThreadId_t prev_tid = { .raw = *scheduler_mr1 };
			assert(L4_IsGlobalId(prev_tid));
			struct thread *prev = thread_find(*scheduler_mr1);
			if(prev == NULL
				|| TID_VERSION(prev->id) != L4_Version(prev_tid))
			{
				continue;
			}

			TRACE("%s: thread %lu:%lu was preempted at %#llx\n",
				__func__, L4_ThreadNo(prev_tid), L4_Version(prev_tid),
				read_global_timer());
			void *utcb = thread_get_utcb(prev);
			L4_Word_t *preempt_p = &L4_VREG(utcb, L4_TCR_COP_PREEMPT),
				preempt = *preempt_p;
			bool signal_preempt = CHECK_FLAG(preempt, 0x20),
				delay = CHECK_FLAG(preempt, 0x40);
				// pending = CHECK_FLAG(preempt, 0x80);
			/* FIXME: this clause is untested */
			if(delay && preempt_delayed) {
				*preempt_p &= ~(0x40 | 0x80);
				delay = false;
			}
			if(signal_preempt) {
				/* FIXME: store information in the thread structure so that a
				 * preemption exception will be sent when the thread is
				 * entered next.
				 */
				/* send an immediate preemption exception. */
				struct thread *exh = thread_get_exnh(prev, utcb);
				if(likely(exh != NULL)) {
					build_exn_ipc(prev, utcb, -4, &prev->ctx);
					ipc_user(prev, exh, 0);
					/* halt the thread if its exception handler is AWOL. */
					if(!IS_IPC(prev->status)) {
						assert(L4_VREG(utcb, L4_TCR_ERRORCODE) != 0);
						thread_halt(prev);
					}
				}
				*preempt_p &= ~0x80;
			}
		}
	}
}


/* this function must always perform a nonlocal exit, because it'll always
 * represent an intra-privilege control transfer (user thread in kernel mode
 * to kernel thread). so it's a stack exiting thing.
 *
 * FIXME: the above is bullshit. iret_to_scheduler() isn't required: we're
 * already in kernel space. in truth it'd suffice just to jump to the
 * scheduler context sans the IRET.
 */
void return_to_scheduler(void)
{
	struct thread *self = get_current_thread(),
		*next = scheduler_thread;
	assert(scheduler_thread != NULL);
	assert(self != scheduler_thread);

	TRACE("%s: %c-%c: %lu:%lu -> %lu:%lu\n", __func__,
		self->space == kernel_space ? 'K' : 'U',
		next->space == kernel_space ? 'K' : 'U',
		TID_THREADNUM(self->id), TID_VERSION(self->id),
		TID_THREADNUM(next->id), TID_VERSION(next->id));

	leaving_thread(self);

	assert(self->status != TS_RUNNING);
	assert(next->status != TS_RUNNING);
	next->status = TS_RUNNING;
	current_thread = next;

	if(!x86_irq_is_enabled()) x86_irq_enable();		/* for r_f_e() */
	return_from_exn();
	assert((next->ctx.eflags & (1 << 14)) == 0);
	iret_to_scheduler(&next->ctx);
}


COLD void return_from_dead(void)
{
	struct thread *next = scheduler_thread;
	assert(scheduler_thread != NULL);

	TRACE("%s: going back to scheduler\n", __func__);

	assert(next->status != TS_RUNNING);
	next->status = TS_RUNNING;
	current_thread = next;

	if(!x86_irq_is_enabled()) x86_irq_enable();		/* for r_f_e() */
	return_from_exn();
	assert((next->ctx.eflags & (1 << 14)) == 0);
	iret_to_scheduler(&next->ctx);
}


/* returns on failure. caller should fall back to return_to_scheduler(). */
static void return_to_other(struct thread *current, struct thread *other)
{
	assert(other != current);
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

	entering_thread(other);
	return_from_exn();
	switch_thread_u2u(other);
}


void return_to_ipc(struct thread *target)
{
	struct thread *cur = get_current_thread();
	assert(!hook_empty(&cur->post_exn_call));
	ipc_user(cur, target, 0);

	/* TODO: schedule the target thread next */

	return_to_scheduler();
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
		if(other != current) return_to_other(current, other);
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

	L4_Word_t result = L4_SCHEDRESULT_ERROR, ec = 0, timectl = *timectl_p;

	/* test for user TID range */
	if(unlikely(L4_ThreadNo(dest_tid) < first_user_threadno())) {
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
	if(IS_KERNEL_THREAD(dest)) goto inv_param;

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
	if((sens_pri != 0xff && sens_pri > current->pri)
		|| (max_delay != 0xffff
			&& L4_IsTimePoint_NP((L4_Time_t){ .raw = max_delay })))
	{
		goto inv_param;
	}

	*timectl_p = (L4_Word_t)L4_TimePeriod(dest->quantum).raw << 16
		| L4_TimePeriod(dest->total_quantum).raw;

	if(IS_IPC(dest->status)
		/* TODO: check flag to see if current IPC is by kernel, instead */
		&& !hook_empty(&dest->post_exn_call))
	{
		result = L4_SCHEDRESULT_RUNNING;
	} else {
		static const uint8_t status_to_schedresult[] = {
			[TS_STOPPED] = L4_SCHEDRESULT_INACTIVE,
			[TS_DEAD] = L4_SCHEDRESULT_DEAD,	/* kth only */
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
		assert(IS_KERNEL_THREAD(dest) || result != L4_SCHEDRESULT_DEAD);
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
			dest->total_quantum = time_in_us(total_quantum);
			dest->quantum = time_in_us(dest->ts_len);
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
	dest->max_delay = max_delay;

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
	ec = L4_ERROR_INVALID_PARAM;
	goto end;
}
