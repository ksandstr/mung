
/* x86 features. most of these should only be called with interrupts
 * disabled.
 */

#ifndef SEEN_UKERNEL_X86_H
#define SEEN_UKERNEL_X86_H

#include <stdint.h>
#include <stdlib.h>
#include <stdbool.h>

#include <ukernel/tss.h>
#include <ukernel/ioport.h>
#include <ukernel/gdt.h>
#include <ukernel/idt.h>


/* courtesy of L4Ka::pistachio */
struct x86_exregs {
	uint32_t reason;
	/* actual process state */
	uint32_t es;		/* 4 */
	uint32_t ds;		/* 8 */
	uint32_t edi;		/* 12 */
	uint32_t esi;		/* 16 */
	uint32_t ebp;		/* 20 */
	uint32_t __esp;		/* 24: trapgate ESP on exn entry */
	uint32_t ebx;		/* 28 */
	uint32_t edx;		/* 32 */
	uint32_t ecx;		/* 36 */
	uint32_t eax;		/* 40 */
	/* trapgate frame */
	uint32_t error;		/* 44 */
	uint32_t eip;		/* 48 */
	uint32_t cs;		/* 52 */
	uint32_t eflags;	/* 56 */
	uint32_t esp;		/* 60: process ESP (r/w by swap_context()) */
	uint32_t ss;		/* 64 */
} __attribute__((packed));


static inline size_t x86_frame_len(const struct x86_exregs *frame)
{
	/* examine the code segment selector's privilege bits */
	bool is_short = (frame->cs & 0x3) == 0;
	if(is_short) return sizeof(*frame) - sizeof(uint32_t) * 2;
	else return sizeof(*frame);
}


static inline void x86_flush_tlbs(void) {
	__asm__ __volatile__ (
		"\tmovl %%cr3, %%eax\n"
		"\tmovl %%eax, %%cr3\n"
		::: "memory", "eax");
}


/* invalidates a page in the LINEAR ADDRESS SPACE.
 *
 * valid since the 80486, and this microkernel won't support the 386, so no
 * specialcasing.
 */
static inline void x86_invalidate_page(uintptr_t address) {
	__asm__ __volatile__ ("invlpg %0" :: "m" (*(char *)address): "memory");
}


static inline void x86_set_eflags(uint32_t mask) {
	__asm__ __volatile__ (
		"\tpushf\n"
		"\torl (%%esp), %0\n"
		"\tmovl %0, (%%esp)\n"
		"\tpopf\n"
		:: "r" (mask));
}


static inline void x86_clear_eflags(uint32_t mask) {
	__asm__ __volatile__ (
		"\tpushf\n"
		"\tandl (%%esp), %0\n"
		"\tmovl %0, (%%esp)\n"
		"\tpopf\n"
		:: "r" (~mask));
}


static inline uint32_t x86_get_eflags(void) {
	uint32_t output;
	__asm__ __volatile__ (
		"\tpushf\n"
		"\tmovl (%%esp), %0\n"
		"\tpopf\n"
		: "=r" (output));
	return output;
}


static inline void x86_irq_disable(void) {
	asm volatile ("cli" ::: "memory");
}


static inline void x86_irq_enable(void) {
	asm volatile ("sti" ::: "memory");
}


#endif
