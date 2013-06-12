
#ifndef SEEN_UKERNEL_INTERRUPT_H
#define SEEN_UKERNEL_INTERRUPT_H

#include <stdint.h>
#include <stdbool.h>

#include <l4/types.h>


struct thread;


/* from pic.c */

/* x86 architecture without APIC */
extern void initialize_pics(int off_pic1, int off_pic2);

/* send end-of-interrupt via PIC */
extern void pic_send_eoi(int irq);

extern void pic_set_mask(uint8_t pic1, uint8_t pic2);
extern void pic_clear_mask(uint8_t pic1, uint8_t pic2);


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

/* returns true on preempt, clears kernel_irq_deferred */
extern bool int_latent(void);

extern volatile bool kernel_irq_ok, kernel_irq_deferred;


#endif
