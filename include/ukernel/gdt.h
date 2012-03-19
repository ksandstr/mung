
/* stuff for handling segmentation on the x86 */

#ifndef SEEN_UKERNEL_GDT_H
#define SEEN_UKERNEL_GDT_H

/* descriptor indexes loaded by setup_gdt(). also used by isr-32.S . */
#define SEG_USER_CODE 1
#define SEG_USER_DATA 2
#define SEG_KERNEL_CODE 3		/* 4 GiB, linear, code, ring0 */
#define SEG_KERNEL_DATA 4		/* 4 GiB, linear, data, ring0 */
#define SEG_KERNEL_TSS 5		/* TSS, refers to "kernel_tss" */
#define SEG_KERNEL_CODE_HIGH 6
#define SEG_KERNEL_DATA_HIGH 7

#define N_KERNEL_SEGS 8		/* last segment index + 1 */


#ifndef IN_ASM_SOURCE

#include <stdbool.h>
#include <ukernel/mm.h>

#define KERNEL_TO_LINEAR(addr) (is_kernel_high ? (addr) + KERNEL_SEG_START : (addr))

/* false at boot, set to true by go_high() */
extern bool is_kernel_high;

/* must be called with interrupts OFF. */
extern void setup_gdt(void);

/* called after setup_paging() */
extern void go_high(void);

#endif

#endif
