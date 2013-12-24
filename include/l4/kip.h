
#ifndef __L4__KERNEL_IFACE_PAGE_H__
#define __L4__KERNEL_IFACE_PAGE_H__

#include <l4/types.h>
#include <l4/syscall.h>


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


typedef union {
	L4_Word_t raw;
	struct {
		L4_Word_t __padding:16;
		L4_Word_t subid:8;
		L4_Word_t id:8;
	} X;
} L4_KernelId_t;


typedef union {
	L4_Word_t raw;
	struct {
		L4_Word_t __padding:16;
		L4_Word_t subversion:8;
		L4_Word_t version:8;
	} X;
} L4_ApiVersion_t;


typedef union {
	L4_Word_t raw;
	struct {
		L4_Word_t ee:2;
		L4_Word_t ww:2;
		L4_Word_t __padding:28;
	} X;
} L4_ApiFlags_t;


typedef struct {
	/* (half a word each.) */
	L4_Word_t n:16;
	L4_Word_t MemDescPtr:16;
} L4_MemoryInfo_t;


typedef struct {
	L4_KernelId_t KernelId;
	union {
		L4_Word_t raw;
		struct {
			L4_Word_t day:5;
			L4_Word_t month:4;
			L4_Word_t year:7;
			L4_Word_t __padding:16;
		} X;
	} KernelGenDate;

	union {
		L4_Word_t raw;
		struct {
			L4_Word_t subsubver:16;
			L4_Word_t subver:8;
			L4_Word_t ver:8;
		} X;
	} KernelVer;

	L4_Word_t Supplier;
	char VersionString[0];
} L4_KernelDesc_t;


typedef struct {
	L4_Word_t magic;
	L4_ApiVersion_t ApiVersion;
	L4_ApiFlags_t ApiFlags;
	L4_Word_t KernelVerPtr;		/* seen as KernDescPtr in the spec */

	/* 0x10 */
	L4_Word_t __padding10[17];
	L4_MemoryInfo_t MemoryInfo;
	L4_Word_t __padding58[2];

	/* 0x60 */
	L4_Word_t __padding60[16];

	/* 0xA0 */
	L4_Word_t __paddingA0[2];
	union {
		L4_Word_t raw;
		struct {
			L4_Word_t m:10;
			L4_Word_t a:6;
			L4_Word_t s:6;
			L4_Word_t __padding:10;
		} X;
	} UtcbAreaInfo;			/* seen as UtcbInfo in the spec */
	union {
		L4_Word_t raw;
		struct {
			L4_Word_t s:6;
			L4_Word_t __padding:26;
		} X;
	} KipAreaInfo;

	/* 0xB0 */
	L4_Word_t __paddingB0[2];
	L4_Word_t BootInfo;
	L4_Word_t ProcDescPtr;

	/* 0xC0 */
	union {
		L4_Word_t raw;
		struct {
			L4_Word_t ReadPrecision:16;
			L4_Word_t SchedulePrecision:16;
		} X;
	} ClockInfo;
	union {
		L4_Word_t raw;
		struct {
			L4_Word_t t:8;
			L4_Word_t SystemBase:12;
			L4_Word_t UserBase:12;
		} X;
	} ThreadInfo;
	union {
		L4_Word_t raw;
		struct {
			L4_Word_t rwx:3;
			L4_Word_t __padding:7;
			L4_Word_t page_size_mask:22;
		} X;
	} PageInfo;
	union {
		L4_Word_t raw;
		struct {
			L4_Word_t processors:16;
			L4_Word_t __padding:12;
			L4_Word_t s:4;
		} X;
	} ProcessorInfo;

	/* 0xD0 */
	L4_Word_t SpaceControl;
	L4_Word_t ThreadControl;
	L4_Word_t ProcessorControl;
	L4_Word_t MemoryControl;

	/* 0xE0 */
	L4_Word_t Ipc;
	L4_Word_t Lipc;
	L4_Word_t Unmap;
	L4_Word_t ExchangeRegisters;

	/* 0xF0 */
	L4_Word_t SystemClock;
	L4_Word_t ThreadSwitch;
	L4_Word_t Schedule;
	L4_Word_t __paddingF0;

	/* 0x100 */
	L4_Word_t __padding100[4];

	/* 0x110 */
	L4_Word_t ArchSyscall0;
	L4_Word_t ArchSyscall1;
	L4_Word_t ArchSyscall2;
	L4_Word_t ArchSyscall3;
} __attribute__((__packed__)) L4_KernelInterfacePage_t;


static inline void *L4_GetKernelInterface(void)
{
	L4_Word_t dummy;
	return L4_KernelInterface(&dummy, &dummy, &dummy);
}


static inline L4_Word_t L4_BootInfo(void *kip_ptr)
{
	L4_KernelInterfacePage_t *kip = kip_ptr;
	return kip->BootInfo;
}


static inline L4_MemoryDesc_t *L4_MemoryDesc(void *kip, L4_Word_t num)
{
	/* FIXME: use a KIP struct */
	L4_Word_t meminfo = *(L4_Word_t *)(kip + 0x54);
	if(num >= (meminfo & 0xffff)) return NULL;
	else return (L4_MemoryDesc_t *)(kip + (meminfo >> 16)) + num;
}

static inline int L4_IsMemoryDescVirtual(const L4_MemoryDesc_t *m) {
	return m->x.v;
}

static inline L4_Word_t L4_MemoryDescType(const L4_MemoryDesc_t *m) {
	return (m->x.type >= 0x0E) ? (m->x.type + (m->x.t << 4)) : (m->x.type);
}

static inline L4_Word_t L4_MemoryDescLow(const L4_MemoryDesc_t *m) {
	return m->x.low << 10;
}

static inline L4_Word_t L4_MemoryDescHigh(const L4_MemoryDesc_t *m) {
	return (m->x.high << 10) | 0x3ff;
}


static inline L4_Word_t L4_UtcbAreaSizeLog2(void *kip_ptr) {
	L4_KernelInterfacePage_t *kip = kip_ptr;
	return kip->UtcbAreaInfo.X.s;
}

static inline L4_Word_t L4_UtcbAlignmentLog2(void *kip_ptr) {
	L4_KernelInterfacePage_t *kip = kip_ptr;
	return kip->UtcbAreaInfo.X.a;
}

static inline L4_Word_t L4_UtcbSize(void *kip_ptr) {
	L4_KernelInterfacePage_t *kip = kip_ptr;
	return (1 << kip->UtcbAreaInfo.X.a) * kip->UtcbAreaInfo.X.m;
}


#endif
