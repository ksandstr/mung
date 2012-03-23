
#include <stdio.h>
#include <ccan/list/list.h>
#include <ccan/htable/htable.h>
#include <ccan/likely/likely.h>

#include <ukernel/ipc.h>
#include <ukernel/space.h>
#include <ukernel/thread.h>


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

	printf("%s: %c-%c: %d:%d -> %d:%d\n", __func__,
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
