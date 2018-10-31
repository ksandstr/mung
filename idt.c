
#include <stdint.h>
#include <stdbool.h>
#include <ccan/compiler/compiler.h>

#include <ukernel/gdt.h>
#include <ukernel/idt.h>
#include <ukernel/x86.h>
#include <ukernel/config.h>


static inline void set_idt_gate(
	struct idt_entry *ints,
	int vector,
	void (*tophalf)(void),
	int selector,
	int type_attr)
{
	ints[vector] = (struct idt_entry){
		.offset_1 = (uintptr_t)tophalf & 0xffff,
		.offset_2 = (uintptr_t)tophalf >> 16,
		.selector = selector,
		.type_attr = IDT_PRESENT | type_attr,
	};
}


/* 0xe = 32-bit interrupt gate */
#define STATIC_IRQ_GATE(ints, sel, num) \
	do { \
		extern void isr_irq##num##_top(void); \
		set_idt_gate((ints), 0x20 + (num), &isr_irq##num##_top, (sel), 0xe); \
	} while(false)

/* 0xf = 32-bit trap gate */
#define EXN_GATE(ints, sel, num, name) \
	do { \
		extern void isr_exn_##name##_top(void); \
		set_idt_gate((ints), (num), &isr_exn_##name##_top, (sel), 0xf); \
	} while(false)


COLD void setup_idt(int max_irq)
{
	static struct idt_entry ints[256];
	for(int i=0; i < 256; i++) {
		ints[i] = (struct idt_entry){ /* all zero */ };
	}

	const int code_sel = SEG_KERNEL_CODE << 3;
	EXN_GATE(ints, code_sel, 0, de);	/* divide error */
	EXN_GATE(ints, code_sel, 3, int3);	/* int3 (KDB) */
	EXN_GATE(ints, code_sel, 6, ud);	/* invalid opcode */
	EXN_GATE(ints, code_sel, 7, nm);	/* FPU/MMX/etc. reloads */
	EXN_GATE(ints, code_sel, 13, gp);	/* general protection */
	EXN_GATE(ints, code_sel, 14, pf);	/* pagefault */
	EXN_GATE(ints, code_sel, 16, mf);	/* x87 floating-point exception */
	EXN_GATE(ints, code_sel, 19, xm);	/* SIMD floating-point exception */

	STATIC_IRQ_GATE(ints, code_sel, 0);	/* IRQ0 (timer) */

	void (*irq_top_fn)(void);
	int int_low, int_high;
	if(apic_enabled) {
		/* LAPIC fixed interrupt vectors appear in 0x21 .. (0x20 + max_irq) */
		extern void isr_apic_top(void);
		irq_top_fn = &isr_apic_top;
		int_low = 0x21; int_high = 0x20 + max_irq;
	} else {
		/* per pic.c, olde-timey irqs appear in 0x20..0x2f. */
		extern void isr_xtpic_top(void);
		irq_top_fn = &isr_xtpic_top;
		int_low = 0x21; int_high = 0x2f;
	}
	for(int i=int_low; i <= int_high; i++) {
		set_idt_gate(ints, i, irq_top_fn, code_sel, 0xe);
	}

	/* softint vectors. disabled when unnecessary; the kernel interface is a
	 * function call, not a bare mechanism.
	 */
	if(!USE_SYSENTER) {
		/* syscall ISRs; 0x80 .. 0x8f */
		EXN_GATE(ints, code_sel, 0x8c, lipc_sc);	/* Lipc */
		EXN_GATE(ints, code_sel, 0x8d, exregs_sc);	/* ExchangeRegisters */
		EXN_GATE(ints, code_sel, 0x8e, memctl_sc);	/* MemoryControl */
		EXN_GATE(ints, code_sel, 0x8f, basic_sc);	/* basic syscall */
		/* (permit access to defined syscall interrupts for user code.) */
		for(int i=0x8c; i <= 0x8f; i++) ints[i].type_attr |= 3 << 5;
	}

	struct {
		uint16_t limit;
		uint32_t base;
	} __attribute__((packed)) idt_desc = {
		.limit = sizeof(ints),
		.base = KERNEL_TO_LINEAR((uintptr_t)&ints[0]),
	};
	__asm__ __volatile__("\tlidt %0\n":: "m" (idt_desc));
}
