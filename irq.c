
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <ccan/likely/likely.h>
#include <ccan/compiler/compiler.h>

#include <l4/vregs.h>

#include <ukernel/thread.h>
#include <ukernel/thread.h>
#include <ukernel/sched.h>
#include <ukernel/space.h>
#include <ukernel/misc.h>
#include <ukernel/bug.h>
#include <ukernel/x86.h>
#include <ukernel/trace.h>
#include <ukernel/interrupt.h>


#define TRACE(fmt, ...) TRACE_MSG(TRID_IRQ, fmt, ##__VA_ARGS__)


/* interrupt handlers set by other parts of the microkernel. these override
 * userspace where present. note that irq_handlers is indexed by vector
 * number, i.e. add 0x20 for external interrupts.
 */
static int max_irq_handler = -1;
static irq_handler_fn *irq_handlers = NULL;

/* deferral array. simple enough. length as that of @irq_handlers. contents
 * per vector number are 0 for no deferral, 1 for simple deferral, and 2 for
 * masked deferral.
 */
static uint8_t *deferred_vecs;
static L4_Word_t *defer_set_masks;

bool irq_defer_active = false, kernel_irq_ok = false;


static inline bool irq_in_kernel(const struct x86_exregs *frame) {
	/* ring0 interrupts push a short stack frame. */
	return x86_frame_len(frame) < sizeof(*frame);
}


static irq_handler_fn choose_handler(int vecn)
{
	assert(!x86_irq_is_enabled());
	irq_handler_fn handler = NULL;
	if(vecn == 0x20) handler = &on_preempt;
	else if(vecn <= max_irq_handler) handler = irq_handlers[vecn];
	if(handler == NULL) handler = &int_trigger;
	return handler;
}


static void irq_defer(int vec_num)
{
	assert(!x86_irq_is_enabled());
	assert(abs(vec_num) <= max_irq_handler);	/* to solve, fix PIC init */

	vec_num = abs(vec_num);
	irq_defer_active = true;
	defer_set_masks[vec_num / WORD_BITS] |= 1ul << (vec_num % WORD_BITS);

	/* handle unsigned wraparound. */
	if(unlikely(deferred_vecs[vec_num] == 0xff)) {
		deferred_vecs[vec_num] = 1;
	}

	if(++deferred_vecs[vec_num] > 1) {
		/* TODO: translate bare vector numbers to IRQ, or redefine mask_irq to
		 * map vector numbers.
		 */
		if(vec_num >= 0x20 && vec_num <= 0x2f) {
			(*global_pic.mask_irq)(vec_num - 0x20);
		}
	}
}


/* call latent interrupt handlers, resolve preemptions, return the winner.
 * caller must check for retval == current && I ∈ current.PreemptFlags, and
 * engate max_delay as appropriate.
 */
struct thread *irq_call_deferred(struct thread *next)
{
	assert(!x86_irq_is_enabled());
	assert(!kernel_irq_ok);

	/* initialize resolution state, feed it a primary event. */
	struct thread *current = get_current_thread();
	void *cur_utcb = current != NULL ? thread_get_utcb(current) : NULL;
	next = sched_resolve_next(current, cur_utcb, current, next);

	int n_defer_masks = (max_irq_handler + WORD_BITS) / WORD_BITS, n_done;
	do {
		if(!irq_defer_active) break;
		n_done = 0;
		for(int i=0; i < n_defer_masks; i++) {
			L4_Word_t m = defer_set_masks[i];
			while(m != 0) {
				int b = ffsl(m) - 1;
				assert(b >= 0);
				m &= ~(1ul << b);
				int vecn = i * WORD_BITS + b;
				int n_defers = deferred_vecs[vecn];
				assert(n_defers > 0);
				irq_handler_fn handler = choose_handler(vecn);
				deferred_vecs[vecn] = 0;
				defer_set_masks[i] &= ~(1ul << b);

				x86_irq_enable();
				next = sched_resolve_next(current, cur_utcb, next,
					(*handler)(n_defers > 1 ? -vecn : vecn));
				x86_irq_disable();
				n_done++;
			}
		}
	} while(n_done > 0);
	irq_defer_active = false;

	return next;
}


void isr_irq_bottom(struct x86_exregs *regs)
{
	assert(!x86_irq_is_enabled());

	int vecn = regs->reason;
	(*global_pic.send_eoi)(vecn - 0x20);

	irq_handler_fn handler = choose_handler(vecn);
	if(handler == &int_trigger) {
		(*global_pic.mask_irq)(vecn - 0x20);
		vecn = -vecn;
	}

	if(irq_in_kernel(regs) && !kernel_irq_ok) {
		/* kernel code was interrupted. mark and proceed. */
		irq_defer(vecn);
	} else if(irq_in_kernel(regs)) {
		/* the idling kernel was interrupted. */
		assert(kernel_irq_ok);
		kernel_irq_ok = false;
		x86_irq_enable();
		struct thread *next = (*handler)(vecn);
		x86_irq_disable();
		if(irq_defer_active) save_user_ex(regs);
		return_from_irq(next);
		/* going back to schedule(). */
		kernel_irq_ok = true;
	} else {
		/* userspace was interrupted. */
		assert(!kernel_irq_ok);
		assert(vecn > max_irq_handler
			|| deferred_vecs[vecn] == 0);
		x86_irq_enable();
		struct thread *next = (*handler)(vecn);
		x86_irq_disable();
		if(irq_defer_active) save_user_ex(regs);
		return_from_irq(next);
		/* if return_from_irq() returned, current thread wasn't preempted. */
	}
}


/* the IAPC PIT 1000hz interrupt. */
void isr_irq0_bottom(struct x86_exregs *regs)
{
	uint64_t now = ++global_timer_count;
	(*systemclock_p) += 1000;
	(*global_pic.send_eoi)(0);

	if(now < preempt_timer_count) return;
	TRACE("%s: preempt hit at now=%u\n", __func__, (unsigned)now);

	preempt_timer_count = ~0ull;
	assert(preempt_thread == NULL
		|| thread_is_valid(preempt_thread));

	if(irq_in_kernel(regs) && !kernel_irq_ok) {
		/* defer a magical call to on_preempt() as though it were any old
		 * interrupt.
		 */
		irq_defer(0x20);
		TRACE("%s: preempt deferred\n", __func__);
		return;
	}

	struct thread *current = get_current_thread();
	bool ctx_saved = false;
	if(CHECK_FLAG(preempt_status, PS_DELAYED)) {
		assert(!CHECK_FLAG(current->flags, TF_SYSCALL));
		save_user_ex(regs);
		ctx_saved = true;
	}
	struct thread *next = on_preempt(0x20);
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


COLD void set_irq_handler(int vector, irq_handler_fn handler)
{
	assert(vector >= 0);

	x86_irq_disable_push();
	if(max_irq_handler < vector) {
		irq_handlers = realloc(irq_handlers,
			(vector + 1) * sizeof(*irq_handlers));
		deferred_vecs = realloc(deferred_vecs, vector + 1);
		/* (this rounds up correctly. don't try to fix it.) */
		int n_masks_old = (max_irq_handler + WORD_BITS) / WORD_BITS,
			n_masks_new = (vector + WORD_BITS) / WORD_BITS;
		defer_set_masks = realloc(defer_set_masks,
			n_masks_new * sizeof(L4_Word_t));
		if(irq_handlers == NULL || deferred_vecs == NULL
			|| defer_set_masks == NULL)
		{
			panic("happy happy joy joy");	/* will you shut up */
		}
		for(int i = max_irq_handler + 1; i <= vector; i++) {
			irq_handlers[i] = NULL;
			deferred_vecs[i] = 0;
		}
		for(int i = n_masks_old; i < n_masks_new; i++) {
			defer_set_masks[i] = 0;
		}

		max_irq_handler = vector;
	}

	irq_handlers[vector] = handler;
	deferred_vecs[vector] = 0;
	defer_set_masks[vector / WORD_BITS] &= ~(1ul << (vector % WORD_BITS));
	x86_irq_restore();
}
