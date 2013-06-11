
#include <stdint.h>
#include <stdbool.h>

#include <ukernel/gdt.h>
#include <ukernel/idt.h>


static void set_int_gate(
	struct idt_entry *ints,
	int vector,
	void (*tophalf)(void),
	int selector)
{
	ints[vector] = (struct idt_entry){
		.offset_1 = (uintptr_t)tophalf & 0xffff,
		.offset_2 = (uintptr_t)tophalf >> 16,
		.selector = selector,
		.type_attr = IDT_PRESENT | 0xe,		/* 32-bit interrupt gate */
	};
}


#define IRQ_GATE(ints, sel, num) \
	do { \
		extern void isr_irq##num##_top(void); \
		set_int_gate((ints), 0x20 + (num), &isr_irq##num##_top, (sel)); \
	} while(false)

#define EXN_GATE(ints, sel, num, name) \
	do { \
		extern void isr_exn_##name##_top(void); \
		set_int_gate((ints), (num), &isr_exn_##name##_top, (sel)); \
	} while(false)


void setup_idt(int code_seg)
{
	static struct idt_entry ints[256];
	for(int i=0; i < 256; i++) {
		ints[i] = (struct idt_entry){ /* all zero */ };
	}

	/* TODO: null out exceptions that aren't in use, also interrupts that are
	 * likewise
	 */
	int code_sel = code_seg << 3;
	EXN_GATE(ints, code_sel, 0, de);	/* divide error */
	EXN_GATE(ints, code_sel, 3, int3);	/* int3 (KDB) */
	EXN_GATE(ints, code_sel, 6, ud);	/* invalid opcode */
	EXN_GATE(ints, code_sel, 7, nm);	/* FPU/MMX/etc. reloads */
	EXN_GATE(ints, code_sel, 13, gp);	/* general protection */
	EXN_GATE(ints, code_sel, 14, pf);	/* pagefault */
	EXN_GATE(ints, code_sel, 16, mf);	/* x87 floating-point exception */
	EXN_GATE(ints, code_sel, 0x8d, exregs_sc);	/* ExchangeRegisters */
	EXN_GATE(ints, code_sel, 0x8e, memctl_sc);	/* MemoryControl */
	EXN_GATE(ints, code_sel, 0x8f, basic_sc);	/* basic syscall */
	/* (permit access to syscall interrupts to ring 3.) */
	for(int i=0x8d; i <= 0x8f; i++) ints[i].type_attr |= 3 << 5;

	IRQ_GATE(ints, code_sel, 0);	/* IRQ0 (timer) */
	IRQ_GATE(ints, code_sel, 1);
	/* cascade is ignored */
	IRQ_GATE(ints, code_sel, 3);
	IRQ_GATE(ints, code_sel, 4);
	IRQ_GATE(ints, code_sel, 5);
	IRQ_GATE(ints, code_sel, 6);
	IRQ_GATE(ints, code_sel, 7);
	IRQ_GATE(ints, code_sel, 8);
	IRQ_GATE(ints, code_sel, 9);
	IRQ_GATE(ints, code_sel, 10);
	IRQ_GATE(ints, code_sel, 11);
	IRQ_GATE(ints, code_sel, 12);
	IRQ_GATE(ints, code_sel, 13);	/* IRQ13 is doubtful */
	IRQ_GATE(ints, code_sel, 14);
	IRQ_GATE(ints, code_sel, 15);

	struct {
		uint16_t limit;
		uint32_t base;
	} __attribute__((packed)) idt_desc = {
		.limit = sizeof(ints),
		.base = KERNEL_TO_LINEAR((uintptr_t)&ints[0]),
	};
	__asm__ __volatile__("\tlidt %0\n":: "m" (idt_desc));
}
