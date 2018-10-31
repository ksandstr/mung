
/* stuff for handling segmentation on the x86 */

#ifndef SEEN_UKERNEL_GDT_H
#define SEEN_UKERNEL_GDT_H

/* descriptor indexes loaded by setup_gdt(). also used by isr-32.S . */
#define SEG_KERNEL_CODE 1		/* 4 GiB, linear, code, ring0, SYSENTER CS */
#define SEG_KERNEL_DATA 2		/* 4 GiB, linear, data, ring0, -''- SS */
#define SEG_USER_CODE 3			/* SYSEXIT CS */
#define SEG_USER_DATA 4			/* -''- SS */
#define SEG_KERNEL_TSS 5		/* TSS, refers to "kernel_tss" */

#define N_KERNEL_SEGS 6		/* last segment index + 1 */


#ifndef IN_ASM_SOURCE

#include <stdbool.h>
#include <l4/types.h>
#include <ukernel/mm.h>

#define KERNEL_TO_LINEAR(addr) (is_kernel_high ? (addr) + KERNEL_SEG_START : (addr))


/* type fields for code/data */
#define DESC_A_ACCESSED (1 << 0)
#define DESC_A_RW (1 << 1)		/* readable / read-write */
#define DESC_A_DC (1 << 2)		/* direction/conforming bit */
#define DESC_A_EX (1 << 3)		/* executable bit */

/* type values for system descriptors (well, just the one) */
#define DESC_A_TSS_32BIT 0x09

#define DESC_A_SYSTEM (1 << 4)	/* clear for system, set for code/data */
#define DESC_A_PRIV_MASK ((1 << 5) | (1 << 6))
#define DESC_A_PRESENT (1 << 7)

#define DESC_F_SZ (1 << 2)		/* 0 = 16 bit, 1 = 32 bit */
#define DESC_F_GR (1 << 3)		/* 0 = bytes, 1 = 4k pages */


/* false at boot, set to true by go_high() */
extern bool is_kernel_high;

/* must be called with interrupts OFF. */
extern void setup_gdt(void);

/* called after setup_paging() */
extern void go_high(void);

/* the GDT slot reservation mechanism. initialized in kmain(). reserves
 * segment descriptors for the %gs:0 access to UTCBs, and shares them between
 * address spaces where possible.
 */
extern void init_gdt_resv(void);

/* sharing of pointer segments, i.e. the base thing for values in
 * %fs and %gs .
 */
extern int reserve_gdt_ptr_seg(uintptr_t l_addr);
extern void release_gdt_ptr_seg(uintptr_t l_addr, int slot);

/* set_gdt_slot() reserves a free GDT slot exclusively. used by space.c to
 * allocate TSS segment descriptors.
 */
extern int set_gdt_slot(L4_Word_t base, L4_Word_t limit, int access, int flags);
extern void free_gdt_slot(int slot);

/* TSS mangling */
extern void unbusy_tss(int slot);
extern void set_current_tss(int slot);


#endif

#endif
