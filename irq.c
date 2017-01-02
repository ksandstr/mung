
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <ccan/likely/likely.h>
#include <ccan/compiler/compiler.h>

#include <l4/vregs.h>

#include <ukernel/thread.h>
#include <ukernel/sched.h>
#include <ukernel/space.h>
#include <ukernel/bug.h>
#include <ukernel/x86.h>
#include <ukernel/trace.h>
#include <ukernel/kip.h>
#include <ukernel/misc.h>
#include <ukernel/trampoline.h>
#include <ukernel/interrupt.h>


#define TRACE(fmt, ...) TRACE_MSG(TRID_IRQ, fmt, ##__VA_ARGS__)


struct irq_handler {
	irq_handler_fn fn;
	unsigned flags;
};


DEFINE_TRAMPOLINE(send_eoi);
DEFINE_TRAMPOLINE(mask_irq);
DEFINE_TRAMPOLINE(unmask_irq);


/* interrupt handlers set by other parts of the microkernel. these override
 * userspace where present.
 *
 * NOTE: irq_handlers is indexed by interrupt number; add 0x20 to get IDT
 * offsets.
 */
static int max_irq_handler = -1;
static struct irq_handler *irq_handlers = NULL;

/* deferral masks. simple enough: a 1 bit means the interrupt occurred while
 * the CPU was in kernel and not idle, and was masked from happening again
 * until processed. it's assumed that the platform PIC buffers another
 * interrupt up while masked.
 */
static L4_Word_t *defer_set_masks;

bool irq_defer_active = false, kernel_irq_ok = false;


static inline bool irq_in_kernel(const struct x86_exregs *frame) {
	/* ring0 interrupts push a short stack frame. */
	return x86_frame_len(frame) < sizeof(*frame);
}


static COLD struct thread *int_spurious(int irqn)
{
	printf("%s: irqn=%d\n", __func__, irqn);
	BUG_ON(irqn >= first_user_threadno(),
		"irqn=%d >= first_user_threadno=%d !!",
		irqn, (int)first_user_threadno());
	return get_current_thread();
}


static irq_handler_fn choose_handler(bool *automask_p, int irqn)
{
	irq_handler_fn handler = NULL;
	if(irqn <= max_irq_handler) {
		handler = irq_handlers[irqn].fn;
		*automask_p = CHECK_FLAG(irq_handlers[irqn].flags, IHF_AUTOMASK);
	}
	if(handler == NULL) {
		handler = &int_spurious;
		*automask_p = true;
	}
	return handler;
}


static void irq_defer(int irqn)
{
	TRACE("%s: irqn=%d\n", __func__, irqn);

	assert(!x86_irq_is_enabled());
	assert(irqn <= max_irq_handler);	/* to unfuck, fix PIC init */

	irq_defer_active = true;
	L4_Word_t m = 1ul << (irqn % WORD_BITS),
		*limb = &defer_set_masks[irqn / WORD_BITS];

	if(!CHECK_FLAG(*limb, m)) {
		*limb |= m;
		mask_irq(irqn);
	}
}


/* call latent interrupt handlers, resolve preemptions, return the final
 * @next. caller must check for retval == current && I ∈ current.PreemptFlags,
 * and engage max_delay as appropriate.
 */
struct thread *irq_call_deferred(struct thread *next)
{
	assert(!x86_irq_is_enabled());
	assert(!kernel_irq_ok);

	TRACE("%s: next=%lu:%lu\n", __func__,
		next != NULL ? TID_THREADNUM(next->id) : 0,
		next != NULL ? TID_VERSION(next->id) : 0);

	/* initialize resolution state, feed it a primary event.
	 *
	 * XXX shouldn't we just keep @next the way it is? maybe the caller will
	 * have done that for us already, which seems reasonable since this
	 * function is always called from a kernel exit path.
	 */
	struct thread *current = get_current_thread();
	void *cur_utcb = current != NULL ? thread_get_utcb(current) : NULL;
	next = sched_resolve_next(current, cur_utcb, current, next);

	/* call all handlers, then unmask their interrupts on the PIC. since
	 * handlers are run with interrupts enabled, further interrupts may occur
	 * and be deferred before interrupts are disabled again. these interrupts
	 * will be handled iteratively before the unmask step.
	 *
	 * NOTE that this is not immune to concurrent modification of
	 * irq_handlers[]. consequently, when switching handlers, the incoming one
	 * may end up called for an interrupt posted before the set_irq_handler()
	 * call and so should be robust against spurious occurrences, or some
	 * procedure should be used to cause a legit interrupt for sure before
	 * interrupt reënable.
	 */
	int n_limbs = (max_irq_handler + WORD_BITS) / WORD_BITS,
		n_done, n_total = 0;
	L4_Word_t m_done[n_limbs], m_copied[n_limbs];
	for(int i=0; i < n_limbs; i++) m_done[i] = 0;
	do {
		if(!irq_defer_active) break;	/* none were deferred. */
		for(int i=0; i < n_limbs; i++) m_copied[i] = defer_set_masks[i];
		irq_defer_active = false;
		x86_irq_enable();
		n_done = 0;
		for(int i=0; i < n_limbs; i++) {
			L4_Word_t m = m_copied[i] & ~m_done[i];
			while(m != 0) {
				int b = ffsl(m) - 1;
				assert(b >= 0);
				m &= ~(1ul << b);
				m_done[i] |= 1ul << b;
				assert(m == (m_copied[i] & ~m_done[i]));
				int irqn = i * WORD_BITS + b;
				bool dummy;
				irq_handler_fn handler = choose_handler(&dummy, irqn);
				assert(handler != NULL);
				TRACE("%s: irqn=%d, handler=%p\n", __func__, irqn, handler);

				next = sched_resolve_next(current, cur_utcb, next,
					(*handler)(irqn));
				n_done++;
			}
		}
		n_total += n_done;
		x86_irq_disable();
	} while(n_done > 0);
	TRACE("%s: n_total=%d\n", __func__, n_total);

	/* unmask the processed IRQs. */
	for(int i=0; i < n_limbs && n_total > 0; i++) {
		assert(m_done[i] == defer_set_masks[i]);
		defer_set_masks[i] = 0;
		while(m_done[i] != 0) {
			int b = ffsl(m_done[i]) - 1;
			assert(b >= 0);
			m_done[i] &= ~(1ul << b);
			int irqn = i * WORD_BITS + b;
			unmask_irq(irqn, irq_handlers[irqn].flags);
			n_total--;
		}
	}
	irq_defer_active = false;

	TRACE("%s: next'=%lu:%lu\n", __func__,
		next != NULL ? TID_THREADNUM(next->id) : 0,
		next != NULL ? TID_VERSION(next->id) : 0);
	return next;
}


void isr_irq_bottom(struct x86_exregs *regs)
{
	assert(!x86_irq_is_enabled());

	int irqn = regs->reason - 0x20;	/* TODO: get offset from global_pic */
	assert(irqn >= 0);
	send_eoi(irqn);

	bool automask;
	irq_handler_fn handler = choose_handler(&automask, irqn);
	if(automask) {
		/* masking for interrupts that've been directed to userspace, and ones
		 * that occurred without handler.
		 */
		mask_irq(irqn);
	}

	if(irq_in_kernel(regs) && !kernel_irq_ok) {
		/* kernel code was interrupted. defer and return. */
		irq_defer(irqn);
	} else if(irq_in_kernel(regs)) {
		/* the idling kernel was interrupted. enable deferring, call the
		 * handler and schedule its result immediately.
		 *
		 * TODO: combine this with the next section. kernel_irq_ok should be
		 * set to false regardless of what it was before, and
		 * current_regs_saved should be set to irq_in_kernel() -- since saving
		 * the idling kernel's frame is nonsensical anyway.
		 */
		assert(kernel_irq_ok);
		kernel_irq_ok = false;
		TRACE("%s: idle call on irqn=%d, handler=%p\n",
			__func__, irqn, handler);
		x86_irq_enable();
		assert(handler != NULL);
		struct thread *next = (*handler)(irqn);
		x86_irq_disable();
		assert(get_current_thread() == NULL);
		return_from_irq(next);
		assert(false);	/* no current to return to, while idle. */
	} else {
		/* userspace was interrupted. */
		assert(!kernel_irq_ok);
		assert(irqn > max_irq_handler
			|| !CHECK_FLAG(defer_set_masks[irqn / WORD_BITS],
				1ul << (irqn % WORD_BITS)));
		x86_irq_enable();
		TRACE("%s: userspace call on irqn=%d, handler=%p\n",
			__func__, irqn, handler);
		assert(handler != NULL);
		struct thread *next = (*handler)(irqn);
		x86_irq_disable();
		if(next != get_current_thread() || irq_defer_active) {
			save_user_ex(regs);
		}
		return_from_irq(next);
		/* if return_from_irq() returned, current thread wasn't preempted. */
	}
}


/* the IAPC PIT 1000hz interrupt. */
void isr_irq0_bottom(struct x86_exregs *regs)
{
	uint64_t now = ++global_timer_count;
	(*systemclock_p) += 1000;
	send_eoi(0);

	if(now < preempt_timer_count) return;
	TRACE("%s: preempt hit at now=%u\n", __func__, (unsigned)now);

	preempt_timer_count = ~0ull;
	assert(preempt_thread == NULL
		|| thread_is_valid(preempt_thread));

	if(irq_in_kernel(regs) && !kernel_irq_ok) {
		/* defer a magical call to on_preempt() as though it were any old
		 * interrupt.
		 */
		irq_defer(0);
		TRACE("%s: preempt deferred\n", __func__);
		return;
	}

	struct thread *current = get_current_thread();
	bool ctx_saved = false;
	if(CHECK_FLAG(preempt_status, PS_DELAYED)
		|| (current != NULL && preempt_thread != NULL
			&& preempt_thread->pri <= current->sens_pri
			&& current->max_delay == 0
			&& CHECK_FLAG_ALL(L4_VREG(thread_get_utcb(current),
				L4_TCR_COP_PREEMPT), 0x60)))
	{
		assert(!CHECK_FLAG(current->flags, TF_SYSCALL));
		save_user_ex(regs);
		ctx_saved = true;
	}
	struct thread *next = on_preempt(0);
	if(current == NULL) {
		/* scheduled activation from idle. */
		if(next != NULL) {
			TRACE("%s: scheduled activation of next=%lu:%lu\n", __func__,
				TID_THREADNUM(next->id), TID_VERSION(next->id));
		} else {
			TRACE("%s: idle -> idle\n", __func__);
		}
		kernel_irq_ok = false;
	} else if(next != current) {
		/* async preëmption of @current. */
		assert(!CHECK_FLAG(current->flags, TF_SYSCALL));
		TRACE("%s: async preëmpt of %lu:%lu\n", __func__,
			TID_THREADNUM(current->id), TID_VERSION(current->id));
		if(!ctx_saved) save_user_ex(regs);
		if(!IS_IPC(current->status)) {
			current->status = TS_READY;
			current->wakeup_time = 0;
			sq_update_thread(current);
		}
	}

	return_from_irq(next);
	TRACE("%s: returning to userspace (preempt_timer_count'=%u)\n",
		__func__, (unsigned)preempt_timer_count);
	BUG_ON(next != current, "shouldn't get here!");
}


COLD void set_irq_handler(
	int irq, irq_handler_fn handler, unsigned flags)
{
	assert(irq >= 0);
	assert(!CHECK_FLAG_ANY(flags,
		~(IHF_ACT_LOW | IHF_LEVEL_TRIG | IHF_AUTOMASK)));

	x86_irq_disable_push();
	if(max_irq_handler < irq) {
		irq_handlers = realloc(irq_handlers,
			(irq + 1) * sizeof(*irq_handlers));
		/* (this rounds up correctly. don't try to fix it.) */
		int n_masks_old = (max_irq_handler + WORD_BITS) / WORD_BITS,
			n_masks_new = (irq + WORD_BITS) / WORD_BITS;
		defer_set_masks = realloc(defer_set_masks,
			n_masks_new * sizeof(L4_Word_t));
		if(irq_handlers == NULL || defer_set_masks == NULL) {
			panic("happy happy joy joy");	/* will you shut up */
		}
		for(int i = max_irq_handler + 1; i <= irq; i++) {
			irq_handlers[i].fn = NULL;
		}
		for(int i = n_masks_old; i < n_masks_new; i++) {
			defer_set_masks[i] = 0;
		}

		max_irq_handler = irq;
	}

	irq_handlers[irq] = (struct irq_handler){
		.fn = handler, .flags = flags,
	};
	defer_set_masks[irq / WORD_BITS] &= ~(1ul << (irq % WORD_BITS));
	x86_irq_restore();
}
