
#ifndef SEEN_UKERNEL_GDT_H
#define SEEN_UKERNEL_GDT_H


/* descriptor indexes loaded by setup_gdt(). */

enum kernel_seg_ix {
	SEG_KERNEL_CODE = 1,	/* 4 GiB, linear, code, ring0 */
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
