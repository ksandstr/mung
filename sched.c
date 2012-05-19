
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
#include <ukernel/misc.h>
#include <ukernel/thread.h>


#define TRACE_VERBOSE 0		/* 1 for "[KU]-[KU]: %d:%d -> %d:%d" prints */


#if TRACE_VERBOSE
#define TRACE(fmt, ...) printf(fmt, __VA_ARGS__)
#else
#define TRACE(fmt, ...)
#endif


struct thread *current_thread = NULL;
struct thread *scheduler_thread = NULL;

static struct rb_root sched_tree = { };

extern struct list_head thread_list, dead_thread_list;
extern struct htable thread_hash;


struct thread *get_current_thread(void) {
	return current_thread;
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
	};
	assert(status >= 0 && status < (sizeof(table) / sizeof(table[0])));
	return table[status];
}


void sq_insert_thread(struct thread *t)
{
	assert(t->status != TS_STOPPED && t->status != TS_DEAD);

	struct thread *dupe = sq_insert_thread_helper(&sched_tree, t);
	if(unlikely(dupe != NULL)) {
		printf("%s: thread %d:%d already in tree\n", __func__,
			TID_THREADNUM(t->id), TID_VERSION(t->id));
		return;
	}
	rb_insert_color(&t->sched_rb, &sched_tree);

	TRACE("%s: inserted %d:%d (status %s, wakeup %#llx, pri %d)\n", __func__,
		TID_THREADNUM(t->id), TID_VERSION(t->id), sched_status_str(t),
		t->wakeup_time, (int)t->pri);
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


static struct thread *schedule_next_thread(struct thread *current)
{
	uint64_t now = read_global_timer() * 1000;

	struct thread *pick = NULL;
	TRACE("%s: called in %d:%d at %#llx\n", __func__,
		TID_THREADNUM(current->id), TID_VERSION(current->id), now);
	for(struct rb_node *cur = rb_first(&sched_tree), *next;
		cur != NULL;
		cur = next)
	{
		next = rb_next(cur);

		struct thread *cand = rb_entry(cur, struct thread, sched_rb);
		if(cand == current) continue;
		TRACE("%s: candidate %d:%d (status %s, wakeup_time %#llx, pri %d)\n",
			__func__, TID_THREADNUM(cand->id), TID_VERSION(cand->id),
			sched_status_str(cand), cand->wakeup_time, (int)cand->pri);

		assert(cand->status != TS_DEAD);
		assert(cand->status != TS_STOPPED);

		if(cand->status == TS_SEND_WAIT || cand->status == TS_RECV_WAIT) {
			if(cand->wakeup_time > now) break;

			/* timed out. */
			const bool send = cand->status == TS_SEND_WAIT;
			if(CHECK_FLAG(cand->flags, TF_HALT)) thread_stop(cand);
			else thread_wake(cand);
			set_ipc_error_thread(cand, (1 << 1) | (send ? 0 : 1));
		} else if(cand->status == TS_R_RECV
			&& cand->recv_timeout.raw != L4_ZeroTime.raw
			&& cand->wakeup_time < now)
		{
			/* apply timeout to a "ready to active receive" thread. the
			 * justification is that the timeout should occur before the
			 * active receive does even if the timeout is caused by scheduling
			 * delay.
			 */
			/* FIXME: move this into a timeout_ipc() function */
			if(CHECK_FLAG(cand->flags, TF_HALT)) thread_stop(cand);
			else thread_wake(cand);
			set_ipc_error_thread(cand, (1 << 1) | 1);
		}

		if(pick == NULL || pick->pri < cand->pri) pick = cand;
	}

#if TRACE_VERBOSE && 0
	if(pick == NULL) {
		static int blargh = 0;
		if(++blargh == 5) panic("foo!");
	} else {
		TRACE("%s: picked %d:%d\n",
			__func__, TID_THREADNUM(pick->id), TID_VERSION(pick->id));
	}
#endif

	return pick;
}


bool schedule(void)
{
	struct thread *self = get_current_thread();
	assert(self->space == kernel_space);
	assert(self->status != TS_RUNNING);

	/* find next ready thread. */
	struct thread *next = schedule_next_thread(self);
	assert(next != self);		/* by schedule_next_thread() def */
	if(unlikely(next == NULL)) {
		assert(scheduler_thread == NULL
			|| scheduler_thread == self);
		return false;
	}

	if(next->status == TS_R_RECV) {
		assert(!IS_KERNEL_THREAD(next));
		if(!ipc_recv_half(next) && next->status == TS_RECV_WAIT) {
			/* try again (passive receive) */
			return schedule();
		}
	}

	TRACE("%s: %c-%c: %d:%d -> %d:%d\n", __func__,
		self->space == kernel_space ? 'K' : 'U',
		next->space == kernel_space ? 'K' : 'U',
		TID_THREADNUM(self->id), TID_VERSION(self->id),
		TID_THREADNUM(next->id), TID_VERSION(next->id));

	assert(self->space != NULL);
	assert(next->space != NULL);

	next->status = TS_RUNNING;
	sq_update_thread(next);
	current_thread = next;
	if(self->space != next->space) {
		if(self->space->tss != next->space->tss) {
			int slot = next->space->tss_seg;
			if(slot == 0) slot = SEG_KERNEL_TSS;
			unbusy_tss(slot);
			set_current_tss(slot);
		}
		if(self->space == kernel_space) {
			/* switch from kernel initial space */
			asm volatile ("movl %0, %%cr3"
				:: "a" (next->space->pdirs->id << 12)
				: "memory");
			/* go go goblin balls! */
			assert(next->utcb_ptr_seg != 0);
			swap_to_ring3(&self->ctx, &next->ctx, next->utcb_ptr_seg << 3 | 3);
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

	TRACE("%s: returned to %d:%d from going to %d:%d; (current_thread is %d:%d)\n",
		__func__,
		TID_THREADNUM(self->id), TID_VERSION(self->id),
		TID_THREADNUM(next->id), TID_VERSION(next->id),
		TID_THREADNUM(current_thread->id), TID_VERSION(current_thread->id));

	assert(current_thread == self);
	assert(self->status == TS_RUNNING);

	return true;
}


NORETURN void end_kthread(void)
{
	struct thread *self = get_current_thread();
	printf("%s: kthread %d:%d terminating\n", __func__, TID_THREADNUM(self->id),
		TID_VERSION(self->id));

	assert(self->space == kernel_space);
	list_del_from(&self->space->threads, &self->space_link);
	htable_del(&thread_hash, int_hash(TID_THREADNUM(self->id)), self);
	list_add(&dead_thread_list, &self->dead_link);
	self->status = TS_DEAD;
	sq_remove_thread(self);
	schedule();

	panic("schedule() in end_kthread() returned???");
}


void return_to_scheduler(struct x86_exregs *regs)
{
	struct thread *self = get_current_thread(),
		*next = scheduler_thread;
	assert(scheduler_thread != NULL);
	assert(self != scheduler_thread);

	TRACE("%s: %c-%c: %d:%d -> %d:%d\n", __func__,
		self->space == kernel_space ? 'K' : 'U',
		next->space == kernel_space ? 'K' : 'U',
		TID_THREADNUM(self->id), TID_VERSION(self->id),
		TID_THREADNUM(next->id), TID_VERSION(next->id));

	assert(self->status != TS_RUNNING);
	assert(next->status != TS_RUNNING);
	next->status = TS_RUNNING;
	sq_update_thread(next);
	current_thread = next;

	assert((next->ctx.eflags & (1 << 14)) == 0);
	*regs = next->ctx;
}


void return_to_ipc(struct x86_exregs *regs, struct thread *target)
{
	ipc_simple(target);

	/* TODO: schedule the target thread next */

	return_to_scheduler(regs);
}


void sys_threadswitch(struct x86_exregs *regs)
{
	struct thread *current = get_current_thread();
	current->ctx = *regs;

	L4_ThreadId_t target = { .raw = regs->eax };
	struct thread *other = L4_IsNilThread(target) ? NULL : thread_find(target.raw);
	if(other != NULL && IS_READY(other->status)) {
		/* TODO: cause switch to "other" */
	}

	current->status = TS_READY;
	return_to_scheduler(regs);
}


static bool sched_valid_time(L4_Time_t t) {
	return t.raw == 0xffff || (t.period.a == 0 && t.raw != 0);
}


void sys_schedule(struct x86_exregs *regs)
{
	struct thread *current = get_current_thread();

	/* raw inputs */
	L4_ThreadId_t dest_tid = { .raw = regs->eax };
	L4_Word_t timectl = regs->edx, procctl = regs->esi,
		prioctl = regs->ecx, preemptctl = regs->edi;
	/* outputs */
	L4_Word_t old_timectl = 0, result = L4_SCHEDRESULT_ERROR, ec = 0;

	struct thread *dest = thread_find(dest_tid.raw);
	if(dest == NULL || IS_KERNEL_THREAD(dest)) goto inv_param;

	/* cooked inputs */
	L4_Time_t ts_len = { .raw = (timectl >> 16) & 0xffff },
		total_quantum = { .raw = timectl & 0xffff };
	if(!sched_valid_time(ts_len) || !sched_valid_time(total_quantum)) {
		goto inv_param;
	}
	L4_Word_t pri = prioctl & 0xff;
	if(pri != 0xff && pri > current->pri) goto inv_param;
	L4_Word_t procnum = procctl & 0xffff,
		sens_pri = (preemptctl >> 16) & 0xff,
		max_delay = preemptctl & 0xffff;

	old_timectl = (L4_Word_t)L4_TimePeriod(dest->quantum).raw << 16
		| L4_TimePeriod(dest->total_quantum).raw;

	if(IS_IPC(dest->status)
		/* TODO: check flag to see if current IPC is by kernel */
		&& (dest->post_exn_call != NULL
			|| dest->saved_mrs > 0 || dest->saved_brs > 0))
	{
		result = 3;		/* "running", as IPC not by usermode */
	} else {
		static const uint8_t status_to_schedresult[] = {
			[TS_STOPPED] = L4_SCHEDRESULT_INACTIVE,
			[TS_RUNNING] = L4_SCHEDRESULT_RUNNING,
			[TS_READY] = L4_SCHEDRESULT_RUNNING,
			[TS_SEND_WAIT] = L4_SCHEDRESULT_PENDING_SEND,
			[TS_RECV_WAIT] = L4_SCHEDRESULT_WAITING,
			[TS_R_RECV] = L4_SCHEDRESULT_WAITING,
			/* TODO: 5 "sending" (in the middle of string transfer, send)
			 *       7 "receiving" (same, receive)
			 */
		};
		int s = dest->status;
		if(s < 0 || s >= NUM_ELEMENTS(status_to_schedresult)) {
			printf("WARNING: %s: unknown state %d in thread %d:%d\n",
				__func__, (int)dest->status, TID_THREADNUM(dest->id),
				TID_VERSION(dest->id));
			ec = L4_ERROR_INVALID_THREAD;
			goto end;
		}

		result = status_to_schedresult[s];
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
	if(dest != NULL && dest->status != TS_STOPPED) sq_update_thread(dest);

	if(unlikely(ec != 0)) {
		assert((result & 0xff) == L4_SCHEDRESULT_ERROR);
		L4_VREG(thread_get_utcb(current), L4_TCR_ERRORCODE) = ec;
	}
	regs->eax = result;
	regs->edx = old_timectl;
	return;

inv_param:
	ec = L4_ERROR_INVALID_PARAM;
	goto end;
}
