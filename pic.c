
/* routines for working with the 8259 PIC */

#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <assert.h>
#include <ccan/compiler/compiler.h>

#include <ukernel/ioport.h>
#include <ukernel/interrupt.h>
#include <ukernel/x86.h>


#define IO_PIC1_CMD 0x20
#define IO_PIC1_DATA 0x21
#define IO_PIC2_CMD 0xa0
#define IO_PIC2_DATA 0xa1

#define ICW1_INIT 0x10
#define ICW1_ICW4 0x01
#define ICW4_8086 0x01		/* 8086 mode */

#define PIC_READ_IRR 0x0a	/* OCW3 irq ready next CMD read */
#define PIC_READ_ISR 0x0b	/* OCW3 irq service next CMD read */
#define PIC_EOI 0x20		/* end-of-interrupt */


/* template courtesy of the osdev wiki */
static void initialize_pics(int off_pic1, int off_pic2)
{
	assert(!x86_irq_is_enabled());

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

#if 0
	printf("PIC1 mask is 0x%x; PIC2 mask is 0x%x\n", mask1, mask2);
#endif
}


void pic_send_eoi(int irq)
{
	assert(!x86_irq_is_enabled());

	if(irq >= 8) outb(IO_PIC2_CMD, PIC_EOI);
	outb(IO_PIC1_CMD, PIC_EOI);
}


static void pic_mask_all(void)
{
	assert(!x86_irq_is_enabled());

	outb(IO_PIC1_DATA, 0xff);
	outb(IO_PIC2_DATA, 0xff);
}


void pic_mask_irq(int irq)
{
	if(irq >= 8) {
		irq -= 8;
		outb(IO_PIC2_DATA, inb(IO_PIC2_DATA) | (1 << irq));
	} else {
		outb(IO_PIC1_DATA, inb(IO_PIC1_DATA) | (1 << irq));
	}
}


/* NOTE: this completely ignores polarity. */
void pic_unmask_irq(int irq, unsigned flags)
{
	if(!CHECK_FLAG(flags, IHF_LEVEL_TRIG)) {
		printf("%s: ignoring request for edge-triggered irq%d\n",
			__func__, irq);
		return;
	}

	if(irq >= 8) {
		irq -= 8;
		outb(IO_PIC2_DATA, inb(IO_PIC2_DATA) & ~(1 << irq));
	} else {
		outb(IO_PIC1_DATA, inb(IO_PIC1_DATA) & ~(1 << irq));
	}
}


/* routines courtesy of the osdev wiki (slightly modified) */

static uint16_t __pic_get_irq_reg(int ocw3)
{
	/* OCW3 to PIC CMD to get the register values. PIC2 is chained, and
	 * represents IRQs 8-15. PIC1 is IRQs 0-7, with 2 being the chain
	 */
	outb(IO_PIC1_CMD, ocw3);
	outb(IO_PIC2_CMD, ocw3);
	return (inb(IO_PIC2_CMD) << 8) | inb(IO_PIC1_CMD);
}


/* read interrupt request register (i.e. pending mask) */
static inline uint16_t pic_get_irr(void) {
	return __pic_get_irq_reg(PIC_READ_IRR);
}


/* read interrupt service register (i.e. the current interrupt) */
static inline uint16_t pic_get_isr(void) {
	return __pic_get_irq_reg(PIC_READ_ISR);
}


/* called from ISR tophalf context. interrupts are disabled. */
void isr_xtpic_bottom(struct x86_exregs *regs)
{
	uint16_t serv_mask = pic_get_isr();
	if(serv_mask == 0) {
		/* spurious. */
		pic_send_eoi(15);	/* EOIs both chips */
		return;
	}
	regs->reason = 0x20 + ffsl(serv_mask) - 1;
	isr_irq_bottom(regs);
}


COLD int xtpic_init(struct pic_ops *ops)
{
	/* olde-timey interrupts appear in 0x20..0x2f, i.e. irq# + 0x20 */
	initialize_pics(0x20, 0x28);
	pic_mask_all();

	*ops = (struct pic_ops){
		.send_eoi = &pic_send_eoi,
		.mask_irq = &pic_mask_irq,
		.unmask_irq = &pic_unmask_irq,
	};
	/* preallocate all vectors, permit defer, and direct to userspace. */
	set_irq_handler(0xf, NULL, 0);

	return 15;		/* highest interrupt number */
}


COLD void xtpic_disable(void)
{
	/* TODO: Linux uses a more fancy method to disconnect the XT-PIC. imitate
	 * that, or see whether FreeBSD or Hurd do the same thing too.
	 */
	pic_mask_all();
}
