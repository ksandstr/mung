
#include <stdio.h>
#include <stdint.h>
#include <string.h>

#include <l4/types.h>
#include <l4/kcp.h>
#include <l4/kip.h>

#include <ukernel/mm.h>
#include <ukernel/syscall.h>
#include <ukernel/thread.h>
#include <ukernel/kip.h>


void *kip_mem = NULL;


/* let's use interrupt 0x8f, for now.
 *
 * TODO: use whatever L4Ka::Pistachio uses when SYSENTER/SYSCALL
 * aren't present.
 */
static void make_syscall_stub(void *start, int *len_p, int sc_num)
{
	/* ThreadSwitch must retain %ebx. */
	bool keep_ebx = (sc_num == SC_THREADSWITCH);
	int p = 0;
	uint8_t *mem = start;
	if(keep_ebx) mem[p++] = 0x53;	/* PUSH EBX */
	mem[p++] = 0xbb;	/* MOV EBX, imm32 */
	*(uint32_t *)&mem[p] = sc_num; p += 4;
	mem[p++] = 0xcd;	/* INT */
	mem[p++] = 0x8f;
	if(keep_ebx) mem[p++] = 0x5b;	/* POP EBX */
	mem[p++] = 0xc3;	/* RET */
	*len_p = p;
}


static void make_int_sc_stub(uint8_t *mem, int *len_p, int interrupt)
{
	int p = 0;
	mem[p++] = 0xcd;	/* INT imm8 */
	mem[p++] = (unsigned)interrupt;
	mem[p++] = 0xc3;	/* RET */
	*len_p = p;
}


/* extra clever thing where the syscall stub never enters the kernel.
 * instead, the clock tick is stored in the last 8 bytes of the KIP.
 */
static void make_systemclock_stub(void *start, int *len_p)
{
	int p = 0;
	uint8_t *mem = start;
	mem[p++] = 0xe8;			/* CALL rel32 */
	*(uint32_t *)&mem[p] = 0xb;	/* past the next instructions */
	p += 4;
	mem[p++] = 0x8b;			/* MOV edx, [ecx + 4] */
	mem[p++] = 0x51;
	mem[p++] = 0x04;
	mem[p++] = 0x8b;			/* MOV eax, [ecx] */
	mem[p++] = 0x01;
	mem[p++] = 0x39;			/* CMP [ecx + 4], edx */
	mem[p++] = 0x51;
	mem[p++] = 0x04;
	mem[p++] = 0x75;			/* JNE to first mov */
	mem[p++] = 0xf6;
	mem[p++] = 0xc3;			/* RET */

	/* the "read & massage EIP" sequence */
	mem[p++] = 0x8b;			/* MOV ecx, [esp] */
	mem[p++] = 0x0c;
	mem[p++] = 0x24;
	mem[p++] = 0x81;			/* OR ecx, 0xfff */
	mem[p++] = 0xc9;
	*(uint32_t *)&mem[p] = 0xfffU;
	p += 4;
	mem[p++] = 0x83;			/* AND ecx, ~7 */
	mem[p++] = 0xe1;
	mem[p++] = 0xf8;
	mem[p++] = 0xc3;			/* RET */

	*len_p = p;
}


/* compose a 32-bit little-endian kernel interface page. */
void make_kip(
	void *mem,
	L4_Word_t kern_start,
	L4_Word_t kern_end,
	int max_irq,
	L4_Time_t sched_prec)
{
	/* preserve memorydescs from the KCP, while adding two entries at the
	 * front: the virtual address space, and the kernel's reserved slice
	 * therein.
	 */
	L4_KernelConfigurationPage_t *kcp = mem;
	int num_mds = kcp->MemoryInfo.n;
	L4_MemoryDesc_t mds[num_mds + 3];
	mds[0] = (L4_MemoryDesc_t){
		.x.v = 1, .x.type = L4_ConventionalMemoryType,
		.x.low = 0, .x.high = ~(L4_Word_t)0 >> 10,
	};
	mds[1] = (L4_MemoryDesc_t){
		.x.v = 1, .x.type = L4_ReservedMemoryType,
		.x.low = KERNEL_SEG_START >> 10, .x.high = ~(L4_Word_t)0 >> 10,
	};
	memcpy(&mds[2], mem + kcp->MemoryInfo.MemDescPtr,
		sizeof(L4_Word_t) * 2 * num_mds);
	num_mds += 2;

	/* also, add a reserved region for the kernel's initial heap. this may be
	 * further restricted by the bootloader's dedicated regions; that's fine.
	 */
	int first_ded = 0;
	while(first_ded < num_mds
		&& L4_MemoryDescType(&mds[first_ded]) != L4_DedicatedMemoryType)
	{
		first_ded++;
	}
	if(first_ded < num_mds) {
		/* move other descriptors up. */
		int count = num_mds - first_ded;
		L4_MemoryDesc_t tmp[count];
		memcpy(tmp, &mds[first_ded], count * sizeof(L4_MemoryDesc_t));
		memcpy(&mds[first_ded + 1], tmp, count * sizeof(L4_MemoryDesc_t));
	}
	mds[first_ded] = (L4_MemoryDesc_t){
		.x.v = 0, .x.type = L4_ReservedMemoryType,
		.x.low = kern_start >> 10, .x.high = kern_end >> 10,
	};
	num_mds++;

	/* fill in KernelInterfacePage */
	L4_KernelInterfacePage_t *kip = mem;
	memcpy(&kip->magic, "L4\346K", 4);
	/* L4.X2, rev 5 (XXX: what the heck is revision 5?) */
	kip->ApiVersion = (L4_ApiVersion_t){
		.X.version = 0x84, .X.subversion = 5,
	};
	kip->ApiFlags = (L4_ApiFlags_t){ };	/* 32-bit, little-endian */
	/* UtcbInfo: minimum area 4k (one page); size 512B; alignment 512B */
	kip->UtcbAreaInfo.raw = 0;
	kip->UtcbAreaInfo.X.s = PAGE_BITS;	/* minimum area 4k */
	kip->UtcbAreaInfo.X.a = 9;			/* alignment 512B */
	kip->UtcbAreaInfo.X.m = 1;			/* size multiplier (size = m * a) */
	kip->KipAreaInfo.raw = 0;
	kip->KipAreaInfo.X.s = PAGE_BITS;	/* KIP size (4k) */
	/* BootInfo (0xb8) is left unaltered, per spec */

	/* ClockInfo: 1 ms clock resolution, scheduler resolution as given by
	 * caller
	 */
	kip->ClockInfo.raw = 0;
	kip->ClockInfo.X.ReadPrecision = L4_TimePeriod(1000).raw;
	kip->ClockInfo.X.SchedulePrecision = sched_prec.raw;
	/* ThreadInfo: SystemBase max_irq + 1, UserBase = SystemBase + kth_max,
	 * full bits in threadno
	 */
	kip->ThreadInfo.raw = 0;
	kip->ThreadInfo.X.t = 18;
	kip->ThreadInfo.X.SystemBase = max_irq + 1;
	kip->ThreadInfo.X.UserBase = max_irq + 1 + NUM_KERNEL_THREADS;
	/* PageInfo: page-size mask for only 4k pages (for now);
	 *           rw independent (execute implied by read).
	 */
	kip->PageInfo.raw = 0;
	kip->PageInfo.X.rwx = L4_Readable | L4_Writable;
	kip->PageInfo.X.page_size_mask = 1 << 2;
	/* ProcessorInfo */
	kip->ProcessorInfo.raw = 0;
	kip->ProcessorInfo.X.processors = 0;	/* one CPU */
	kip->ProcessorInfo.X.s = 0;				/* procdesc struct size (TODO) */

	/* syscall stubs. */
	static const struct {
		uint16_t sc_num;
		uint16_t offset;
	} syscalls[] = {
		/* not included:
		 * SC_SYSTEMCLOCK, implemented as a soft syscall.
		 * SC_EXCHANGEREGISTERS, SC_MEMORYCONTROL; have interrupts of their
		 * very own.
		 */
		{ SC_IPC, 0xe0 },
		{ SC_LIPC, 0xe4 },
		{ SC_UNMAP, 0xe8 },
		{ SC_THREADSWITCH, 0xf4 },
		{ SC_SCHEDULE, 0xf8 },
		{ SC_SPACECONTROL, 0xd0 },
		{ SC_THREADCONTROL, 0xd4 },
		{ SC_PROCESSORCONTROL, 0xd8 },
	};
	const int num_syscalls = sizeof(syscalls) / sizeof(syscalls[0]);
	int kip_pos = 0x100;
	for(int i=0; i < num_syscalls; i++) {
		int len = 0;
		make_syscall_stub(mem + kip_pos, &len, syscalls[i].sc_num);
		*(L4_Word_t *)(mem + syscalls[i].offset) = kip_pos;
		kip_pos += (len + 63) & ~63;
	}
	/* slightly more special ones. */
	int len = 0;
	/* SystemClock (pure vsyscall) */
	make_systemclock_stub(mem + kip_pos, &len);
	*(L4_Word_t *)(mem + 0xf0) = kip_pos;
	kip_pos += (len + 63) & ~63;
	/* MemoryControl */
	make_int_sc_stub(mem + kip_pos, &len, 0x8e);
	*(L4_Word_t *)(mem + 0xdc) = kip_pos;
	kip_pos += (len + 63) & ~63;
	/* ExchangeRegisters */
	make_int_sc_stub(mem + kip_pos, &len, 0x8d);
	*(L4_Word_t *)(mem + 0xec) = kip_pos;
	kip_pos += (len + 63) & ~63;

	void *memdesc_base = mem + kip_pos;
	memcpy(memdesc_base, mds, sizeof(L4_Word_t) * 2 * num_mds);
	kip_pos += sizeof(L4_Word_t) * 2 * num_mds;
	kip->MemoryInfo.n = num_mds;
	kip->MemoryInfo.MemDescPtr = (L4_Word_t)(memdesc_base - mem);

	/* KernelDesc */
	L4_KernelDesc_t *kdesc = mem + kip_pos;
	*kdesc = (L4_KernelDesc_t){
		.KernelId = {
			.X.id = 23, .X.subid = 0,
		},
		.KernelGenDate = {
			/* FIXME: actually generate this at build time, somehow */
			.X.year = 13, .X.month = 4, .X.day = 20,
		},
		.KernelVer = {
			/* current version is 0.0.1 (Prototype Pidgeon) */
			.X.ver = 0, .X.subver = 0, .X.subsubver = 1,
		},
	};
	memcpy(&kdesc->Supplier, "aGUY", 4);	/* he's our supplier. */
	int ver_len = strlcpy(kdesc->VersionString,
		"mung 0.0.1 (Prototype Pidgeon); x86; UP;",
		PAGE_SIZE - (kip_pos + sizeof(*kdesc)));
	kip->KernelVerPtr = (L4_Word_t)kdesc & PAGE_MASK;
	kip_pos += ver_len + 1 + sizeof(*kdesc);

	/* feature strings, immediately following kdesc->VersionString */
#ifndef NDEBUG
	kip_pos += strlcpy(mem + kip_pos, "zomgwallhax", PAGE_SIZE - kip_pos) + 1;
#endif
	memcpy(mem + kip_pos++, "", 1);		/* terminator */

	/* TODO: fill this in */
	kip->ProcDescPtr = 0;
}
