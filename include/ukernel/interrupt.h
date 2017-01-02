
#ifndef SEEN_UKERNEL_INTERRUPT_H
#define SEEN_UKERNEL_INTERRUPT_H

#include <stdint.h>
#include <stdbool.h>

#include <l4/types.h>

#include <ukernel/x86.h>


struct thread;


/* flags for set_irq_handler().
 *
 * IHF_ACT_LOW and IHF_LEVEL_TRIG are bits 13 and 15 resp. of the x86 IOAPIC
 * IOREDTBL format, imitated for efficiency.
 */
#define IHF_ACT_LOW 1
#define IHF_AUTOMASK 2		/* mask PIC vector before handler call */
#define IHF_LEVEL_TRIG 4


/* PIC/APIC operations. @irq is a global interrupt number in the range
 * [0..SystemBase).
 *
 * rather than called directly, these will be used to set up the
 * similarly-named trampolines when there's exactly 1 PIC in the system; and
 * used for slower multiplexing if there are more.
 */
struct pic_ops
{
	void (*send_eoi)(int irq);
	void (*mask_irq)(int irq);
	void (*unmask_irq)(int irq, unsigned flags);
};


/* from pic.c. support for x86 without APIC, or where APIC is disabled. */

/* returns highest interrupt number, or negative on error. */
extern int xtpic_init(struct pic_ops *ops);
extern void xtpic_disable(void);	/* on APIC enable */


/* from apic.c. support for the I/O APIC. */

extern int ioapic_init(struct pic_ops *ops);


/* from irq.c */

/* operations on the global PIC set. call with interrupts disabled only. */
extern void send_eoi(int irq);
extern void mask_irq(int irq);
extern void unmask_irq(int irq, unsigned flags);	/* @flags is IHF_* */

/* interrupt control variables. access with interrupts disabled only.
 *
 * @kernel_irq_ok enables full interrupt processing during idle-halt, causes
 * deferral when in kernel code otherwise.
 */
extern bool irq_defer_active, kernel_irq_ok;

/* vec_num is the number of the interrupt vector affected (i.e. exregs->reason
 * on x86). since vectors 0..0x20 are mapped to CPU-internal things (in
 * particular 0 is the divide exception), @vec_num != 0. external (platform
 * PIC) interrupts start wherever.
 *
 * IRQs will be enabled during the handler's execution and the generic IRQ
 * bottom half will defer execution of any other handlers until userspace or
 * idle. @vec_num is always positive; deferral is not indicated to the handler
 * in any way.
 *
 * the handler may return three distinct values to control where the CPU goes
 * afterward:
 *   - [schedule] NULL: the current thread should not be resumed. kernel exits
 *     to scheduler and possibly goes idle.
 *   - [continue] the value of get_current_thread(): the current thread should
 *     keep running.
 *   - [preempt] any other <struct thread *>: the indicated thread should
 *     preempt the current thread, potentially taking precedence over other
 *     preemptors.
 */
typedef struct thread *(*irq_handler_fn)(int vec_num);

/* set in-kernel handling of external interrupt @irq.
 *
 * @flags is a mask of IHF_*, indicating polarity, trigger mode, and whether
 * the interrupt should always be masked in the tophalf routine. when
 * IHF_AUTOMASK ∈ @flags, the interrupt will be masked before @handler is
 * called, and stay masked until something unmasks it explicitly.
 *
 * while @irq is unmasked, @handler may run fewer times than the interrupt is
 * raised, but a condition that the interrupt signals will never go unobserved
 * by @handler.
 *
 * by default all interrupts are directed to a spurious-interrupt handler
 * which reports the interrupt and masks it until unmasked somewhere else.
 * setting @handler to NULL restores default behaviour.
 *
 * FIXME: the incoming @handler may end up being called for a deferred
 * interrupt that was signaled before @handler was set.
 */
extern void set_irq_handler(int irq, irq_handler_fn handler, unsigned flags);

/* clears irq_defer_active. must be called with interrupts disabled.
 * return_from_exn() and return_from_irq() call this; nowhere else should.
 */
extern struct thread *irq_call_deferred(struct thread *next);


/* from thread.c */

/* interface for ipc.c. 0 on ok, ipc error number if not (without send/receive
 * phase bit)
 */
extern int int_clear(int intnum, struct thread *sender);
extern int int_poll(struct thread *t, int intnum);


#endif
