
#ifndef SEEN_UKERNEL_INTERRUPT_H
#define SEEN_UKERNEL_INTERRUPT_H

#include <stdint.h>
#include <stdbool.h>

#include <l4/types.h>


struct thread;


/* PIC/APIC operations. @irq is a global interrupt number in the range
 * [0..SystemBase).
 *
 * (yes, indirect calls suck -- this would be a real good spot for some
 * boot-time patching sometime after SMP and that sort of things...)
 */
struct pic_ops
{
	void (*send_eoi)(int irq);
	void (*mask_irq)(int irq);
	void (*unmask_irq)(int irq, bool act_high, bool level_trig);
};


/* from kmain.c */

extern struct pic_ops global_pic;


/* from pic.c. support for x86 without APIC, or where APIC is disabled. */

/* returns highest interrupt number, or negative on error. */
extern int xtpic_init(struct pic_ops *ops);
extern void xtpic_disable(void);	/* on APIC enable */


/* from apic.c. support for the I/O APIC. */

extern int ioapic_init(struct pic_ops *ops);

/* from irq.c */

extern void init_irq(void);


/* from thread.c */

/* returns false for spurious (disabled) interrupt. @in_kernel is true when
 * the interrupt occurred while executing in supervisor mode.
 */
extern bool int_trigger(int intnum, bool in_kernel);

/* 0 on ok, ipc error number if not (without send/receive phase bit) */
extern int int_clear(int intnum, struct thread *sender);

extern int int_poll(struct thread *t, int intnum);

/* clears kernel_irq_deferred, may set kernel_preempt_pending. this function
 * must be called with interrupts enabled.
 *
 * also, it's tempting to rework it into an int_latent_noirq() or some such.
 * this is a bad idea: because the function re-enables interrupts during its
 * execution, the caller really must put itself in an interrupt-safe state
 * before calling int_latent(). asserting for the interrupt flag enforces this
 * fully. if it turns out that an extra cli/sti is expensive to the point of
 * measurable significance (and int_latent() is only called at kernel exits
 * where deferred interrupts were observed), then an optimized path might be
 * worth more than not.
 */
extern void int_latent(void);

extern volatile bool kernel_irq_ok, kernel_irq_deferred;


#endif
