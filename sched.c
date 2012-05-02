
#include <stdio.h>
#include <stdlib.h>
#include <ccan/list/list.h>
#include <ccan/htable/htable.h>
#include <ccan/likely/likely.h>

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
	uint64_t wa = a->wakeup_time, wb = b->wakeup_time;
	if(wa < wb) return -1;
	else if(wb > wa) return 1;

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


/* TODO: rejigger the clock to record ticks as 1024ths of a second to avoid
 * this base-10 division bullshit.
 *
 * returns current time in 1024ths of a second, i.e. a 54:10 fixed-point
 * format.
 */
static uint64_t get_sched_time(void)
{
	uint64_t raw = read_global_timer();
	return (raw * 1024) / 1000;
}


static struct thread *schedule_next_thread(struct thread *current)
{
	uint64_t now = get_sched_time();

	RB_FOREACH(node, &sched_tree) {
		struct thread *cand = rb_entry(node, struct thread, sched_rb);
		if(cand == current) continue;
#if 0
		TRACE("%s: candidate %d:%d (status %d, wakeup_time %#llx)\n",
			__func__, TID_THREADNUM(cand->id), TID_VERSION(cand->id),
			cand->status, cand->wakeup_time);
#endif
		if(!IS_READY(cand->status) && (cand->wakeup_time >> 10) <= now) {
			/* cancel IPCs, set errors, etc */
			TRACE("%s: would timeout IPC on thread %d:%d\n", __func__,
				TID_THREADNUM(cand->id), TID_VERSION(cand->id));
		} else if(IS_READY(cand->status)) {
			return cand;
		}
	}

#if 0
	static int blargh = 0;
	if(++blargh == 5) panic("foo!");
#endif

	return NULL;
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
		if(!ipc_recv_half(next)) {
			/* try again (passive receive) */
			assert(next->status == TS_RECV_WAIT);
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

	assert((next->ctx.regs[9] & (1 << 14)) == 0);
	iret_to_scheduler(&next->ctx);
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
	thread_save_exregs(current, regs);

	L4_ThreadId_t target = { .raw = regs->eax };
	struct thread *other = L4_IsNilThread(target) ? NULL : thread_find(target.raw);
	if(other != NULL && IS_READY(other->status)) {
		/* TODO: cause switch to "other" */
	}

	current->status = TS_READY;
	return_to_scheduler(regs);
}


void sys_schedule(struct x86_exregs *regs)
{
	L4_ThreadId_t dest_tid = { .raw = regs->eax };
	L4_Word_t timectl = regs->edx, procctl = regs->esi,
		prioctl = regs->ecx, preemptctl = regs->edi;

	printf("%s: called; dest %d:%d, procctl %#x, prioctl %#x\n", __func__,
		TID_THREADNUM(dest_tid.raw), TID_VERSION(dest_tid.raw),
		(unsigned)procctl, (unsigned)prioctl);
	printf("%s: ... timectl %#x, preemptctl %#x\n", __func__,
		(unsigned)timectl, (unsigned)preemptctl);
	L4_Word_t old_timectl = 0, result = 0;

	struct thread *dest = thread_find(dest_tid.raw);
	if(dest == NULL) result = 1;	/* "dead" */
	else if(IS_IPC(dest->status)
		/* TODO: check flag to see if current IPC is by kernel */
		&& (dest->post_exn_call != NULL
			|| dest->saved_mrs > 0 || dest->saved_brs > 0))
	{
		result = 3;		/* "running", as IPC not by usermode */
	} else {
		switch(dest->status) {
			case TS_STOPPED:
				result = 2;		/* "inactive" */
				break;
			case TS_RUNNING: case TS_READY:
				result = 3;		/* "running" */
				break;
			case TS_SEND_WAIT:
				result = 4;		/* "pending send" */
				break;
			case TS_RECV_WAIT:
			case TS_R_RECV:
				result = 6;		/* "waiting to receive" */
				break;
			/* TODO: 5 "sending" (in the middle of string transfer, send)
			 *       7 "receiving" (same, receive)
			 */
			default:
				printf("WARNING: %s: unknown state %d in thread %d:%d\n",
					__func__, (int)dest->status, TID_THREADNUM(dest->id),
					TID_VERSION(dest->id));
				result = 0;		/* "error" */
				L4_VREG(thread_get_utcb(get_current_thread()),
					L4_TCR_ERRORCODE) = 2;	/* "invalid thread ID" */
				break;
		}
	}

	/* TODO: apply procctl, prioctl, timectl, preemptctl */

	printf("%s: result %d, old_timectl %d\n", __func__, result, old_timectl);
	regs->eax = result;
	regs->edx = old_timectl;
}
