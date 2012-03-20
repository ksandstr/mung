
#ifndef SEEN_UKERNEL_INTERRUPT_H
#define SEEN_UKERNEL_INTERRUPT_H

#include <stdint.h>


/* from pic.c */

/* x86 architecture without APIC */
extern void initialize_pics(int off_pic1, int off_pic2);

/* send end-of-interrupt via PIC */
extern void pic_send_eoi(int irq);

extern void pic_set_mask(uint8_t pic1, uint8_t pic2);
extern void pic_clear_mask(uint8_t pic1, uint8_t pic2);


/* from irq.c */

extern void init_irq(void);


#endif
