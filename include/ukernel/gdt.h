
#ifndef SEEN_UKERNEL_GDT_H
#define SEEN_UKERNEL_GDT_H


/* descriptor indexes loaded by setup_gdt(). */

enum kernel_seg_ix {
	SEG_KERNEL_CODE = 1,	/* 4 GiB, linear, code, ring0 */
	SEG_KERNEL_DATA = 2,	/* 4 GiB, linear, data, ring0 */
	SEG_KERNEL_TSS = 3,		/* TSS, refers to "kernel_tss" */

	N_KERNEL_SEGS
};


/* must be called with interrupts OFF. */
extern void setup_gdt(void);


#endif
