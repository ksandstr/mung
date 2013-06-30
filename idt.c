
#include <stdint.h>
#include <stdbool.h>

#include <ukernel/gdt.h>
#include <ukernel/idt.h>
#include <ukernel/x86.h>


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


static void set_trap_gate(
	struct idt_entry *ints,
	int vector,
	void (*tophalf)(void),
	int selector)
{
	ints[vector] = (struct idt_entry){
		.offset_1 = (uintptr_t)tophalf & 0xffff,
		.offset_2 = (uintptr_t)tophalf >> 16,
		.selector = selector,
		.type_attr = IDT_PRESENT | 0xf,		/* 32-bit trap gate */
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
		set_trap_gate((ints), (num), &isr_exn_##name##_top, (sel)); \
	} while(false)


void setup_idt(int code_seg, int max_irq)
{
	static struct idt_entry ints[256];
	for(int i=0; i < 256; i++) {
		ints[i] = (struct idt_entry){ /* all zero */ };
	}

	int code_sel = code_seg << 3;
	EXN_GATE(ints, code_sel, 0, de);	/* divide error */
	EXN_GATE(ints, code_sel, 3, int3);	/* int3 (KDB) */
	EXN_GATE(ints, code_sel, 6, ud);	/* invalid opcode */
	EXN_GATE(ints, code_sel, 7, nm);	/* FPU/MMX/etc. reloads */
	EXN_GATE(ints, code_sel, 13, gp);	/* general protection */
	EXN_GATE(ints, code_sel, 14, pf);	/* pagefault */
	EXN_GATE(ints, code_sel, 16, mf);	/* x87 floating-point exception */
	/* FIXME: SSE math fault is missing! */

	IRQ_GATE(ints, code_sel, 0);	/* IRQ0 (timer) */

	if(apic_enabled) {
		/* LAPIC fixed interrupt vectors, 0x21 .. (0x20 + max_irq) */
		for(int i=0x21; i <= 0x20 + max_irq; i++) {
			extern void isr_apic_top(void);
			set_int_gate(ints, i, &isr_apic_top, code_sel);
		}
	} else {
		/* hours and hours of footage of two giraffes fucking */
		IRQ_GATE(ints, code_sel, 1);
		/* FIXME: add the rest */
	}

	/* syscall ISRs; 0x80 .. 0x8f */
	EXN_GATE(ints, code_sel, 0x8d, exregs_sc);	/* ExchangeRegisters */
	EXN_GATE(ints, code_sel, 0x8e, memctl_sc);	/* MemoryControl */
	EXN_GATE(ints, code_sel, 0x8f, basic_sc);	/* basic syscall */
	/* (permit access to defined syscall interrupts for user code.) */
	for(int i=0x8d; i <= 0x8f; i++) ints[i].type_attr |= 3 << 5;

	struct {
		uint16_t limit;
		uint32_t base;
	} __attribute__((packed)) idt_desc = {
		.limit = sizeof(ints),
		.base = KERNEL_TO_LINEAR((uintptr_t)&ints[0]),
	};
	__asm__ __volatile__("\tlidt %0\n":: "m" (idt_desc));
}
