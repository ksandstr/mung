
#ifndef SEEN_UKERNEL_INTERRUPT_H
#define SEEN_UKERNEL_INTERRUPT_H

#include <stdint.h>
#include <stdbool.h>

#include <l4/types.h>

#include <ukernel/x86.h>


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

/* interrupt control variables. access with interrupts disabled only.
 *
 * @kernel_irq_ok enables full interrupt processing during idle-halt, causes
 * deferral when in kernel code otherwise.
 */
extern bool irq_defer_active, kernel_irq_ok;

/* vec_num is the number of the interrupt vector affected (i.e. exregs->reason
 * on x86). since vectors 0..0x20 are mapped to CPU-internal things (in
 * particular 0 is the divide exception), @vec_num != 0. if latent interrupt
 * handling masked the vector in the applicable interrupt controller, @vec_num
 * is instead the negative vector number. a handler should unmask the relevant
 * interrupt where applicable; this may cause the handler to run again before
 * kernel exit.
 *
 * during the handler's execution, IRQs will be enabled but the kernel tophalf
 * will defer execution of any other handlers until exit.
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

/* a NULL value for @handler disables the previous handler, causing the
 * interrupt tophalf to call int_trigger() (aka routing to userspace) for
 * vectors 0x20..0x2f inclusive.
 */
extern void set_irq_handler(int vector, irq_handler_fn handler);

/* clears irq_defer_active. must be called with interrupts disabled.
 * return_from_exn() and return_from_irq() call this; nowhere else should.
 */
extern struct thread *irq_call_deferred(struct thread *next);


/* from thread.c */

/* IRQ bottomhalf handler */
extern struct thread *int_trigger(int vecnum);

/* interface for ipc.c. 0 on ok, ipc error number if not (without send/receive
 * phase bit)
 */
extern int int_clear(int intnum, struct thread *sender);
extern int int_poll(struct thread *t, int intnum);


#endif
