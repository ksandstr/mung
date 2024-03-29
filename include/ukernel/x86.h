
/* x86 features. some of these should only be called with interrupts disabled.
 * caveat lector.
 */

#ifndef SEEN_UKERNEL_X86_H
#define SEEN_UKERNEL_X86_H

#include <stdint.h>
#include <stdlib.h>
#include <stdbool.h>
#include <stdnoreturn.h>
#include <assert.h>

#include <ccan/likely/likely.h>

#include <ukernel/tss.h>
#include <ukernel/ioport.h>
#include <ukernel/gdt.h>
#include <ukernel/idt.h>
#include <ukernel/util.h>


/* interrupts enabled, reserved bit set, all other bits clear. */
#define X86_DEFAULT_USER_EFLAGS ((1u << 9) | (1u << 1))


/* CR0 flags (incomplete) */
#define X86_CR0_MP (1 << 1)
#define X86_CR0_EM (1 << 2)
#define X86_CR0_TS (1 << 3)
#define X86_CR0_NE (1 << 5)

/* CR4 flags (incomplete) */
#define X86_CR4_TSD 0x4
#define X86_CR4_PGE 0x80
#define X86_CR4_OSFXSR 0x200
#define X86_CR4_OSXMMEXCPT 0x400

/* CPUID page 01h EDX flag masks */
#define X86_FEATURE_D_APIC (1 << 9)
#define X86_FEATURE_D_SEP (1 << 11)
#define X86_FEATURE_D_FXSR (1 << 24)
#define X86_FEATURE_D_SSE (1 << 25)
#define X86_FEATURE_D_SSE2 (1 << 26)

/* same for ECX */
#define X86_FEATURE_C_SSE3 (1 << 0)
#define X86_FEATURE_C_SSSE3 (1 << 9)
#define X86_FEATURE_C_SSE41 (1 << 19)
#define X86_FEATURE_C_SSE42 (1 << 20)

/* TODO: set these at configuration time to remove a runtime check in the #NM
 * handler.
 */
#define CPU_HAS_FXSR() CHECK_FLAG(get_features()->edx, X86_FEATURE_D_FXSR)
#define CPU_HAS_SSE() CHECK_FLAG(get_features()->edx, X86_FEATURE_D_SSE)


/* MSRs */
#define IA32_APIC_BASE	0x1b
#define IA32_SYSENTER_CS	0x174
#define IA32_SYSENTER_ESP	0x175
#define IA32_SYSENTER_EIP	0x176


struct thread;
struct space;
struct page;


/* basic thread context. part of x86_exregs so that it can be written very
 * quickly with a PUSHAD instruction. doesn't carry %eip or %eflags.
 */
struct x86_regs {
	L4_Word_t edi;
	L4_Word_t esi;	/* 4 */
	L4_Word_t ebp;	/* 8 */
	L4_Word_t esp;	/* 12 */
	L4_Word_t ebx;	/* 16 */
	L4_Word_t edx;	/* 20 */
	L4_Word_t ecx;	/* 24 */
	L4_Word_t eax;	/* 28 */
} __attribute__((packed));


/* full thread context. */
struct x86_ctx {
	struct x86_regs r;
	L4_Word_t eip;		/* 32 */
	L4_Word_t eflags;	/* 36 */
} __attribute__((packed));


/* courtesy of L4Ka::pistachio */
struct x86_exregs {
	L4_Word_t reason;
	/* TODO: remove es, ds from premises */
	L4_Word_t es;		/* 4 */
	L4_Word_t ds;		/* 8 */
	struct x86_regs r;	/* 12 (NOTE: r.esp is trapgate ESP on exn entry) */
	/* trapgate frame */
	L4_Word_t error;	/* 44 */
	L4_Word_t eip;		/* 48 */
	L4_Word_t cs;		/* 52 */
	L4_Word_t eflags;	/* 56 */
	L4_Word_t esp;		/* 60: process ESP (r/w by swap_context()) */
	L4_Word_t ss;		/* 64 */
} __attribute__((packed));


/* feature info from CPUID page 01h (edx, ecx) */
struct x86_features
{
	uint8_t type, family, model, stepping;
	uint32_t edx, ecx;
};


/* from exception.c */

/* keeps track of coprocessor contexts etc. */
extern void cop_init(void);
extern void cop_switch(struct thread *next);
extern void cop_killa(struct thread *dead);

/* set in sys_*() to indicate that userspace return should happen via the
 * scheduler; e.g. in the self-modifying case of Schedule.
 */
extern bool kernel_exit_via_sched;


/* calls ipc_user() and constructs a per-architecture exception message to
 * *@handler_p using the given context. sets up a reply handler that stores
 * the message in "t"'s context. returns ipc_user()'s return value.
 *
 * (TODO: this should be moved into an architecture-independent header, and
 * the exregs pointer removed in favour of @t's context [saved at kernel
 * enter].)
 */
extern void *send_exn_ipc(
	struct thread *t, void *utcb, int label,
	const struct x86_exregs *regs,
	struct thread **handler_p);


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


static inline void x86_set_eflags(uint32_t mask) {
	__asm__ __volatile__ (
		"\tpushf\n"
		"\torl (%%esp), %0\n"
		"\tmovl %0, (%%esp)\n"
		"\tpopf\n"
		:: "r" (mask));
}


static inline uint32_t x86_get_eflags(void) {
	uint32_t output;
	__asm__ __volatile__ (
		"\tpushf\n"
		"\tpopl %0\n"
		: "=g" (output));
	return output;
}


static inline bool x86_irq_is_enabled(void) {
	uint16_t flags = x86_get_eflags();
	return CHECK_FLAG(flags, 1 << 9);	/* interrupt enable */
}


static inline void x86_irq_disable(void) {
	assert(x86_irq_is_enabled());
	asm volatile ("cli" ::: "memory");
}


static inline void x86_irq_enable(void) {
	assert(!x86_irq_is_enabled());
	asm volatile ("sti" ::: "memory");
}


/* must appear in a proper block, though the variable declaration already
 * errors if it doesn't.
 */
#define x86_irq_disable_push() \
	bool __saved_i = x86_irq_is_enabled(); \
	if(__saved_i) x86_irq_disable();

#define x86_irq_restore() \
	do { if(__saved_i) x86_irq_enable(); } while(0)


static inline void x86_set_cr0(uint32_t or_mask) {
	asm volatile (
		"movl %%cr0, %%edx\n"
		"orl %0, %%edx\n"
		"movl %%edx, %%cr0\n"
		:: "g" (or_mask)
		: "edx");
}


static inline void x86_clear_cr0(uint32_t clear_mask) {
	asm volatile (
		"movl %%cr0, %%edx\n"
		"andl %0, %%edx\n"
		"movl %%edx, %%cr0\n"
		:: "g" (~clear_mask)
		: "edx");
}


static inline void x86_alter_cr0(uint32_t and, uint32_t or)
{
	if(and != ~0u && or != 0) {
		asm volatile (
			"movl %%cr0, %%edx\n"
			"andl %0, %%edx\n"
			"orl %1, %%edx\n"
			"movl %%edx, %%cr0\n"
			:: "g" (and), "g" (or)
			: "edx");
	} else {
		if(and != ~0u) x86_clear_cr0(~and);
		if(or != 0) x86_set_cr0(or);
	}
}


/* retain privileged EFLAGS bits from thread context (passed in @t_eflags),
 * except for the trap flag, which userspace may set.
 */
static inline uint32_t x86_clean_eflags(uint32_t t_eflags, uint32_t in)
{
	uint32_t r = (t_eflags & 0xfffffe00) | (in & 0x1ff);
	/* set reserved bits correctly */
	r &= 0xffffffd7;
	r |= 0x00000002;
	return r;
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


/* FPU/MMX/SSE shit */

static inline void x86_init_fpu(void) {
	asm volatile ("fwait; finit");
}


static inline void x86_fnsave(void *ptr) {
	asm volatile ("fnsave (%0)" :: "r" (ptr));
}


static inline void x86_frstor(void *ptr) {
	asm volatile ("frstor (%0)" :: "r" (ptr));
}


static inline void x86_fxsave(void *ptr) {
	/* NOTE: fxsave, like fnsave, ignores pending exceptions. */
	asm volatile ("fxsave (%0)" :: "r" (ptr));
}


static inline void x86_fxrstor(void *ptr) {
	asm volatile ("fxrstor (%0)" :: "r" (ptr));
}


/* MSR access */

static inline uint64_t x86_rdmsr(uint32_t msrid) {
	uint64_t val;
	asm volatile ("rdmsr": "=A" (val): "c" (msrid));
	return val;
}


static inline void x86_wrmsr(uint32_t msrid, uint64_t value) {
	asm volatile ("wrmsr":: "A" (value), "c" (msrid));
}


/* memory-mapped I/O, modelled after the x86 I/O port crap.
 * (point being to access the registers exactly once, which volatile pointers
 * just don't do as neatly.)
 *
 * these don't have an "x86_" prefix because they're supposed to be portable
 * between architectures, and most everything has MMIO. so an "arm64.h" would
 * likewise define mm_inl() and the rest.
 *
 * NOTE: the mm_outX() family could all have a __builtin_constant_p() branch.
 */
static inline uint32_t mm_inl(uintptr_t address) {
	uint32_t value;
	asm volatile ("movl %1, %0"
		: "=r" (value)
		: "m" (*(uint32_t *)address));
	return value;
}


static inline void mm_outl(uintptr_t address, uint32_t value) {
	if(__builtin_constant_p(value)) {
		asm volatile ("movl %0, %1"
			:: "i" (value), "m" (*(uint32_t *)address));
	} else {
		asm volatile ("movl %0, %1"
			:: "r" (value), "m" (*(uint32_t *)address));
	}
}


static inline void mm_outb(uintptr_t address, uint8_t value) {
	if(__builtin_constant_p(value)) {
		asm volatile ("movb %0, %1"
			:: "i" (value), "m" (*(uint32_t *)address));
	} else {
		asm volatile ("movb %0, %1"
			:: "q" (value), "m" (*(uint32_t *)address));
	}
}


static inline void mm_andl(uintptr_t address, uint32_t mask) {
	asm volatile ("andl %0, %1"
		:: "r" (mask), "m" (*(uint32_t *)address));
}


static inline void mm_orl(uintptr_t address, uint32_t mask) {
	asm volatile ("orl %0, %1"
		:: "r" (mask), "m" (*(uint32_t *)address));
}


/* from arch_x86.c (being the True and Proper module for this header to
 * describe)
 */

/* allocate @g->ptab_page and map it. */
struct map_group;
extern int x86_alloc_ptab(struct map_group *g);

/* free @g->ptab_page and disconnect it from @g->space->pdirs. */
extern void x86_free_ptab(struct map_group *g);

/* one for interrupts, another for syscalls. the difference is storing of
 * %eip, which is synthesized on return from syscall and retained for
 * interrupts.
 */
extern void save_user_ex(struct x86_exregs *regs);
extern void save_user_regs(struct x86_regs *ctx);


/* from cpu.c (should be in x86.c or some such, or in an ia32 directory
 * alongside gdt, idt, etc. bits?)
 */
extern void scan_cpuid(void);


/* from apic.c */

extern bool apic_enabled, apic_disable_opt;
extern int apic_probe(void);	/* depends on CPUID; ret < 0 when no APIC */

/* NOTE: this unmasks the IRQ! */
extern int ioapic_route_legacy_irq(int irqnum, int ioa_vector);


/* from irq.c */

/* general interrupt-handling function called from the controller-specific ISR
 * bottom-half. @regs->reason should indicate the interrupt vector number plus
 * 0x20, so 0x20..0x2f for the XT-PIC and 0x20 .. 0x20 + n_apics*32 - 1 for
 * APICs.
 *
 * this must be called with interrupts disabled. it returns the same way.
 */
extern void isr_irq_bottom(struct x86_exregs *regs);


/* low-level context switching from context-32.S . */

/* goes back to userspace while restoring all the registers, as when
 * pre-empted by interrupt.
 */
extern noreturn void iret_to_ring3(const struct x86_ctx *load, int gs_sel);

/* NOTE: this doesn't fill in TCR_SYSEXIT_E[CD]X! */
extern noreturn void sysexit_to_ring3(const struct x86_ctx *userctx, int gs_sel);

#endif
