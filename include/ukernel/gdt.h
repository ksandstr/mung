
/* stuff for handling segmentation on the x86 */

#ifndef SEEN_UKERNEL_GDT_H
#define SEEN_UKERNEL_GDT_H

#include <stdbool.h>

#include <ukernel/mm.h>


#define KERNEL_TO_LINEAR(addr) (is_kernel_high ? (addr) + KERNEL_SEG_START : (addr))

/* false at boot, set to true by go_high() */
extern bool is_kernel_high;


/* descriptor indexes loaded by setup_gdt(). */

enum kernel_seg_ix {
	/* hardcoded for context-32.S's benefit */
	SEG_USER_CODE = 1,
	SEG_USER_DATA = 2,

	SEG_KERNEL_CODE,	/* 4 GiB, linear, code, ring0 */
	SEG_KERNEL_DATA,	/* 4 GiB, linear, data, ring0 */
	SEG_KERNEL_TSS,		/* TSS, refers to "kernel_tss" */

	SEG_KERNEL_CODE_HIGH,
	SEG_KERNEL_DATA_HIGH,

	N_KERNEL_SEGS
};


/* must be called with interrupts OFF. */
extern void setup_gdt(void);

/* called after setup_paging() */
extern void go_high(void);

#endif
