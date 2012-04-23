
#include <stdio.h>
#include <ccan/list/list.h>
#include <ccan/htable/htable.h>
#include <ccan/likely/likely.h>

#include <l4/vregs.h>

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

extern struct list_head thread_list, dead_thread_list;
extern struct htable thread_hash;


struct thread *get_current_thread(void) {
	return current_thread;
}


static struct thread *schedule_next_thread(struct thread *current)
{
	/* well, that's simple. */
	struct thread *next = NULL;
	list_for_each(&thread_list, next, link) {
		if(next != current && IS_READY(next->status)) return next;
	}

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
	current_thread = next;
	if(self->space != next->space) {
		if(unlikely(self->space == kernel_space)) {
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
	list_del_from(&thread_list, &self->link);
	htable_del(&thread_hash, int_hash(TID_THREADNUM(self->id)), self);
	list_add(&dead_thread_list, &self->link);
	self->status = TS_DEAD;
	schedule();

	/* schedule() won't return to this thread due to it having been moved to
	 * dead_thread_list.
	 */

	panic("swap_context() in end_kthread() returned???");
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
	current_thread = next;

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
			case TS_STOPPED: case TS_INACTIVE:
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
