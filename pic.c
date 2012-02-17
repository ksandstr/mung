
/* routines for working with the 8259 PIC */

#include <stdio.h>
#include <stdint.h>
#include <assert.h>
#include <ukernel/ioport.h>
#include <ukernel/interrupt.h>


#define IO_PIC1_CMD 0x20
#define IO_PIC1_DATA 0x21
#define IO_PIC2_CMD 0xa0
#define IO_PIC2_DATA 0xa1

#define ICW1_INIT 0x10
#define ICW1_ICW4 0x01
#define ICW4_8086 0x01		/* 8086 mode */

#define PIC_EOI 0x20		/* end-of-interrupt */


/* template courtesy of the osdev wiki */
void initialize_pics(int off_pic1, int off_pic2)
{
	assert(off_pic1 >= 0 && off_pic1 <= 255 && (off_pic1 & 0x7) == 0);
	assert(off_pic2 >= 0 && off_pic2 <= 255 && (off_pic2 & 0x7) == 0);

	uint8_t mask1 = inb(IO_PIC1_DATA), mask2 = inb(IO_PIC2_DATA);

	outb(IO_PIC1_CMD, ICW1_INIT | ICW1_ICW4);
	outb(IO_PIC2_CMD, ICW1_INIT | ICW1_ICW4);
	outb(IO_PIC1_DATA, off_pic1);
	outb(IO_PIC2_DATA, off_pic2);
	outb(IO_PIC1_DATA, 4);		/* inform PIC1 (master) of PIC2 (slave) */
	outb(IO_PIC2_DATA, 2);		/* advise PIC2 of cascade identity (?) */

	outb(IO_PIC1_DATA, ICW4_8086);
	outb(IO_PIC2_DATA, ICW4_8086);

	/* restore saved masks. */
	outb(IO_PIC1_DATA, mask1);
	outb(IO_PIC2_DATA, mask2);

	printf("PIC1 mask is 0x%x; PIC2 mask is 0x%x\n", mask1, mask2);
}


void pic_send_eoi(int irq)
{
	if(irq >= 8) outb(IO_PIC2_CMD, PIC_EOI);
	outb(IO_PIC1_CMD, PIC_EOI);
}


void pic_clear_mask(uint8_t pic1, uint8_t pic2)
{
	if(pic1 != 0) outb(IO_PIC1_DATA, inb(IO_PIC1_DATA) & ~pic1);
	if(pic2 != 0) outb(IO_PIC2_DATA, inb(IO_PIC2_DATA) & ~pic2);
}


void pic_set_mask(uint8_t pic1, uint8_t pic2)
{
	if(pic1 != 0) outb(IO_PIC1_DATA, inb(IO_PIC1_DATA) | pic1);
	if(pic2 != 0) outb(IO_PIC2_DATA, inb(IO_PIC2_DATA) | pic2);
}
