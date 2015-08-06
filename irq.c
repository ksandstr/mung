
#include <stdio.h>
#include <assert.h>
#include <ccan/likely/likely.h>
#include <ccan/compiler/compiler.h>

#include <l4/vregs.h>

#include <ukernel/thread.h>
#include <ukernel/misc.h>
#include <ukernel/thread.h>
#include <ukernel/sched.h>
#include <ukernel/space.h>
#include <ukernel/x86.h>
#include <ukernel/interrupt.h>


/* temporary stack for when interrupts occur in kernel space -- i.e. during a
 * syscall. (or possibly NMIs during a regular interrupt, but that's a TODO.)
 *
 * this is preallocated and used for kernel-mode interrupts because the kernel
 * stack can be arbitrarily far along due to e.g. deep_call() call sites. an
 * interrupt cannot call malloc(), so deep_call() is out of the question.
 */
static void *irq_stack = NULL;


static inline bool irq_in_kernel(const struct x86_exregs *frame) {
	/* ring0 interrupts push a short stack frame. */
	return x86_frame_len(frame) < sizeof(*frame);
}


static void irq_bottom_real(void *parameter)
{
	struct x86_exregs *regs = parameter;
	int irq = regs->reason - 0x20;

	assert(irq >= 0 && irq <= 15);
	if(unlikely(!int_trigger(irq, irq_in_kernel(regs)))) {
		printf("got unexpected irq# %#x\n", (unsigned)irq);
		static int last = -1, repeat = 0;
		if(irq != last) {
			last = irq;
			repeat = 0;
		} else if(++repeat == 5) {
			/* TODO */
			panic("FIXME: should mask interrupt!");
		}
	}
}


void isr_irq_bottom(struct x86_exregs *regs)
{
	const int vecn = regs->reason, irq = vecn - 0x20;
	(*global_pic.send_eoi)(irq);
	if(!irq_in_kernel(regs)) {
		/* user thread was interrupted. stack is the interrupt handler and
		 * syscall stack, which can support isr_irq_bottom_soft().
		 */
		irq_bottom_real(regs);
	} else {
		/* interrupt occurred in kernel space. the frame is on the
		 * previously-installed kernel stack, which may be the syscall stack
		 * or the scheduler thread's stack, or whatever. call the handler on a
		 * separate IRQ stack.
		 */
		void *stk_top = irq_stack + PAGE_SIZE;
		stk_top -= sizeof(struct x86_exregs *);
		*(struct x86_exregs **)stk_top = regs;
		call_on_stack(&irq_bottom_real, stk_top);
	}
}


/* the timer interrupt. runs with interrupts disabled.
 *
 * TODO: the preemption bit is way, way too large. unify it with
 * thread-switching due to interrupt.
 */
void isr_irq0_bottom(struct x86_exregs *regs)
{
	uint64_t now = ++global_timer_count;
	(*systemclock_p) += 1000;
	(*global_pic.send_eoi)(0);

	if(now >= preempt_timer_count) {
		preempt_timer_count = ~(uint64_t)0;
		if(x86_frame_len(regs) < sizeof(*regs)) {
			/* no preemption in kernel code. (quite yet. kernel threads could
			 * be preempted as any other, but system calls with the exception
			 * of string transfers are uninterruptable.)
			 */
		} else {
			/* (this segment seems long for a timer interrupt. but then, a
			 * timer interrupt occurs at the start of each tick, which was
			 * just observed.)
			 */
			struct thread *current = get_current_thread();
			assert(current->status == TS_RUNNING);
			bool preempt = true;
			if(preempt_task_pri <= (int)current->sens_pri) {
				L4_Word_t *ctl;
				if(preempt_task_pri >= 0
					&& !preempt_delayed
					&& current->max_delay > 0
					&& (ctl = &L4_VREG(thread_get_utcb(current), L4_TCR_COP_PREEMPT),
						CHECK_FLAG(*ctl, 0x40)))
				{
					/* delay preemption. */
					*ctl |= 0x80;	/* set I-flag. */
					preempt = false;
					preempt_delayed = true;

					/* FIXME: this doesn't account for a silent preemption
					 * during the delay time. to determine whether that'll
					 * occur, the scheduling queue should be examined.
					 * (the long scheduler path could be avoided by storing
					 * whether the delay time is simple in a variable
					 * alongside preempt_task_pri. if it is, do as follows. if
					 * not, consult the scheduler.)
					 *
					 * to be fair, that case is not covered by unit tests
					 * right now either.
					 */
					uint32_t q_spent = (now - task_switch_time) * 1000, q_rem;
					if(current->quantum <= q_spent) q_rem = 0;
					else q_rem = current->quantum - q_spent;
					uint64_t pe_at = now * 1000
						+ MIN(uint32_t, q_rem, current->max_delay);
					preempt_timer_count = (pe_at + 999) / 1000;
				} else {
					*scheduler_mr1 = current->id;
				}
			} else {
				/* silent preemption. */
				*scheduler_mr1 = L4_nilthread.raw;
			}

			if(preempt) {
				x86_irq_enable();
				thread_save_ctx(current, regs);
				current->status = TS_READY;
				current->wakeup_time = 0;
				sq_update_thread(current);
				return_to_scheduler();
				assert(false);
			}
		}
	}
}


COLD void init_irq(void)
{
	struct page *p = get_kern_page(0);	/* TODO: record this somewhere */
	irq_stack = p->vm_addr;
}
