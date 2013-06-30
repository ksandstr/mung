
#ifndef SEEN_UKERNEL_IDT_H
#define SEEN_UKERNEL_IDT_H

#include <stdint.h>


/* IDT type attr bits */
#define IDT_PRESENT (1 << 7)
#define IDT_PRIVILEGE ((1 << 6) | (1 << 5))
#define IDT_STORAGE (1 << 4)
#define IDT_GATE_TYPE 0x0f


struct idt_entry {
	uint16_t offset_1;
	uint16_t selector;
	uint8_t zero_0;
	uint8_t type_attr;
	uint16_t offset_2;
} __attribute__((packed));


extern void setup_idt(int code_seg_id, int max_irq);


#endif
