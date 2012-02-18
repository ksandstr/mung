
#ifndef SEEN_UKERNEL_TSS_H
#define SEEN_UKERNEL_TSS_H

#include <stdint.h>


/* x86 task state segment. */
struct tss
{
	uint16_t link;
	uint16_t resv_0;
	uint32_t esp0;
	uint16_t ss0;
	uint16_t resv_1;
	uint32_t esp1;
	uint16_t ss1;
	uint16_t resv_2;
	uint32_t esp2;
	uint16_t ss2;
	uint16_t resv_3;
	uint32_t cr3;
	uint32_t eip;
	uint32_t eflags;
	uint32_t eax;
	uint32_t ecx;
	uint32_t edx;
	uint32_t ebx;
	uint32_t esp;
	uint32_t ebp;
	uint32_t esi;
	uint32_t edi;
	uint16_t es;
	uint16_t resv_4;
	uint16_t cs;
	uint16_t resv_5;
	uint16_t ss;
	uint16_t resv_6;
	uint16_t ds;
	uint16_t resv_7;
	uint16_t fs;
	uint16_t resv_8;
	uint16_t gs;
	uint16_t resv_9;
	uint16_t ldtr;
	uint16_t resv_10;
	uint16_t debug_trap;		/* low bit = T, 1-15 = reserved */
	uint16_t iopb_offset;
} __attribute__((packed));


extern struct tss kernel_tss;		/* from kmain.c */


#endif
