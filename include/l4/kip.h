
#ifndef __L4__KERNEL_IFACE_PAGE_H__
#define __L4__KERNEL_IFACE_PAGE_H__

#include <l4/types.h>


/* MemoryDesc, adapted from L4Ka::Pistachio <l4/kip.h> */

typedef union {
	L4_Word_t raw[2];
	struct {
		L4_Word_t type:4;
		L4_Word_t t:4;
		L4_Word_t __padding1:1;
		L4_Word_t v:1;
		L4_Word_t low:22;

		L4_Word_t __padding2:10;
		L4_Word_t high:22;
	} __attribute__((__packed__)) x;
} L4_MemoryDesc_t;


#define L4_UndefinedMemoryType 0
#define L4_ConventionalMemoryType 1
#define L4_ReservedMemoryType 2
#define L4_DedicatedMemoryType 3
#define L4_SharedMemoryType 4
#define L4_BootLoaderSpecificMemoryType 14
#define L4_ArchitectureSpecificMemoryType 15


static inline L4_MemoryDesc_t *L4_MemoryDesc(void *kip, L4_Word_t num)
{
	/* FIXME: use a KIP struct */
	L4_Word_t meminfo = *(L4_Word_t *)(kip + 0x54);
	if(num >= (meminfo & 0xffff)) return NULL;
	else return (L4_MemoryDesc_t *)(kip + (meminfo >> 16)) + num;
}

static inline int L4_IsMemoryDescVirtual(L4_MemoryDesc_t *m) {
	return m->x.v;
}

static inline L4_Word_t L4_MemoryDescType(L4_MemoryDesc_t *m) {
	return (m->x.type >= 0x0E) ? (m->x.type + (m->x.t << 4)) : (m->x.type);
}

static inline L4_Word_t L4_MemoryDescLow(L4_MemoryDesc_t *m) {
	return m->x.low << 10;
}

static inline L4_Word_t L4_MemoryDescHigh(L4_MemoryDesc_t *m) {
	return (m->x.high << 10) | 0x3ff;
}


#endif
