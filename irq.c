
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


static void *irq_stack[16];
static uint32_t irq_pending[16];


static void isr_irq_bottom_soft(int irq, struct x86_exregs *regs)
{
	assert(irq >= 0 && irq <= 15);
	/* FIXME: send interrupt IPC to the proper recipient */
	printf("got unexpected interrupt 0x%x\n", (unsigned)irq);
}


static void isr_irq_bottom_wrap(void *parameter)
{
	struct x86_exregs *regs = parameter;
	int irq = regs->reason - 0x20;
	x86_irq_enable();
	isr_irq_bottom_soft(irq, regs);
	x86_irq_disable();
	if(unlikely(irq_pending[irq] > 1)) {
		irq_pending[irq] = 1;
		isr_irq_bottom_wrap(parameter);
	}
}


static inline void call_on_stack(void (*fn)(void *), void *stack)
{
	/* this trick relies on ebx being a callee-saved register. */
	asm volatile (
		"\txchgl %%ebx, %%esp\n"
		"\tcall *%%edi\n"
		"\tmovl %%ebx, %%esp\n"
		: "=b" (stack)
		: "0" (stack), "D" (fn)
		: "memory", "cc", "edx", "ecx", "eax");
}


static bool irq_switched_stack(void)
{
	char foo = 0;
	intptr_t pos = (intptr_t)&foo;
	return (pos & ~PAGE_MASK) == (kernel_tss.esp0 & ~PAGE_MASK);
}


void isr_irq_bottom(struct x86_exregs *regs)
{
	const int vecn = regs->reason, irq = vecn - 0x20;

#if 0
	printf("%s: frame at 0x%x, syscall_stack at 0x%x\n", __func__,
		(uintptr_t)regs, (uintptr_t)&syscall_stack[0]);
#endif

	pic_send_eoi(irq);
	if(unlikely(irq_pending[irq]++ > 0)) return;
	if(likely(irq_switched_stack())) {
		/* stack was switched. install the IRQ stack in the TSS segment and
		 * proceed.
		 */
		intptr_t old_esp0 = kernel_tss.esp0;
		kernel_tss.esp0 = (intptr_t)irq_stack[irq];
		assert(kernel_tss.esp0 != old_esp0);
		isr_irq_bottom_wrap(regs);
		kernel_tss.esp0 = old_esp0;
	} else {
		/* was called on a kernel stack. call handler on IRQ stack. */
		void *stk_top = irq_stack[irq] + PAGE_SIZE;
		stk_top -= sizeof(struct x86_exregs *);
		*(struct x86_exregs **)stk_top = regs;
		call_on_stack(&isr_irq_bottom_wrap, stk_top);
	}

	assert(irq_pending[irq] == 1);
	irq_pending[irq] = 0;
}


/* the timer interrupt. runs with interrupts disabled by design. */
void isr_irq0_bottom(struct x86_exregs *regs)
{
	uint64_t now = ++global_timer_count;
	(*systemclock_p) += 1000;
	pic_send_eoi(0);

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
				thread_save_ctx(current, regs);
				current->status = TS_READY;
				return_to_scheduler();
				assert(false);
			}
		}
	}
}


COLD void init_irq(void)
{
	/* allocate IRQ stacks. (TODO: don't allocate for IRQs that are nulled out
	 * in the IDT.)
	 */
	for(int i=0; i < 16; i++) {
		irq_pending[i] = 0;

		struct page *p = get_kern_page(0);
		/* TODO: record it as reserved */
		irq_stack[i] = p->vm_addr;
	}
}
