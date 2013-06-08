
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


/* CR0 flags (incomplete) */
#define X86_CR0_MP (1 << 1)
#define X86_CR0_EM (1 << 2)
#define X86_CR0_TS (1 << 3)
#define X86_CR0_NE (1 << 5)



struct thread;

/* courtesy of L4Ka::pistachio */
struct x86_exregs {
	L4_Word_t reason;
	/* actual process state */
	L4_Word_t es;		/* 4 */
	L4_Word_t ds;		/* 8 */
	L4_Word_t edi;		/* 12 */
	L4_Word_t esi;		/* 16 */
	L4_Word_t ebp;		/* 20 */
	L4_Word_t __esp;	/* 24: trapgate ESP on exn entry */
	L4_Word_t ebx;		/* 28 */
	L4_Word_t edx;		/* 32 */
	L4_Word_t ecx;		/* 36 */
	L4_Word_t eax;		/* 40 */
	/* trapgate frame */
	L4_Word_t error;	/* 44 */
	L4_Word_t eip;		/* 48 */
	L4_Word_t cs;		/* 52 */
	L4_Word_t eflags;	/* 56 */
	L4_Word_t esp;		/* 60: process ESP (r/w by swap_context()) */
	L4_Word_t ss;		/* 64 */
} __attribute__((packed));


/* feature info from CPUID page 01h (edx, ecx) */
struct x86_features {
	uint32_t edx, ecx;
};


/* from exception.c */

/* keeps track of coprocessor contexts etc. */
extern void cop_init(void);
extern void cop_switch(struct thread *next);
extern void cop_killa(struct thread *dead);


/* constructs a per-architecture exception message using the given context
 * while saving prior message registers. doesn't perform IPC or set the
 * message tag's label field. sets up a reply handler that stores the message
 * in "t"'s context.
 */
extern void build_exn_ipc(
	struct thread *t,
	void *utcb,
	int label,
	const struct x86_exregs *regs);

/* (TODO: this should be moved into an architecture-independent header, like
 * build_exn_ipc(); and the x86_exregs structure should be made opaque to
 * portable code.)
 */
extern struct thread *get_thread_exh(struct thread *t, void *utcb);


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
static inline void x86_invalidate_page(uintptr_t address)
{
	/* FIXME: somehow this doesn't work at all; a page table modified right
	 * before doesn't even budge. so instead we'll flush the entire TLB like
	 * it's 1987.
	 */
#if 0
	__asm__ __volatile__ ("invlpg (%0)" :: "r" (address): "memory");
#else
	x86_flush_tlbs();
#endif
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


static inline void x86_alter_cr0(uint32_t and, uint32_t or)
{
	asm volatile (
		"movl %%cr0, %%edx\n"
		"andl %0, %%edx\n"
		"orl %1, %%edx\n"
		"movl %%edx, %%cr0\n"
		:: "g" (and), "g" (or)
		: "edx");
}


struct cpuid_out {
	uint32_t eax, ebx, ecx, edx;
};


static inline void x86_cpuid(
	struct cpuid_out *out,
	uint32_t eax,
	uint32_t ebx,
	uint32_t ecx,
	uint32_t edx)
{
	asm volatile (
		"cpuid\n"
		"movl %%eax, %0\n"
		"movl %%ebx, %1\n"
		"movl %%ecx, %2\n"
		"movl %%edx, %3\n"
		: "=m" (out->eax), "=m" (out->ebx), "=m" (out->ecx), "=m" (out->edx)
		: "a" (eax), "b" (ebx), "c" (ecx), "d" (edx));
}


/* FPU shit */

static inline void x86_init_fpu(void) {
	asm volatile ("fwait; finit");
}


static inline void x86_fsave(void *ptr) {
	asm volatile ("fwait; fsave (%0)" :: "r" (ptr));
}


static inline void x86_frstor(void *ptr) {
	asm volatile ("frstor (%0)" :: "r" (ptr));
}


/* from cpu.c (should be in x86.c or some such, or in an ia32 directory
 * alongside gdt, idt, etc. bits?)
 */
extern void scan_cpuid(void);


#endif
