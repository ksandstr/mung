
#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>

#include <l4/types.h>
#include <l4/bootinfo.h>
#include <l4/kcp.h>
#include <l4/kip.h>

#include <ukernel/mm.h>
#include <ukernel/syscall.h>
#include <ukernel/thread.h>
#include <ukernel/cpu.h>
#include <ukernel/util.h>
#include <ukernel/memdesc.h>
#include <ukernel/kip.h>
#include <ukernel/config.h>


void *kip_mem = NULL;
size_t lipc_epilog_offset;

#ifdef CONFIG_X86_SYSENTER
struct sysexit_offs sysexit_epilogs;
#endif


/* let's use interrupt 0x8f, for now.
 *
 * TODO: use whatever L4Ka::Pistachio uses when SYSENTER/SYSCALL
 * aren't present.
 *
 * TODO: rename this function to indicate that it uses software interrupts and
 * a syscall ID in EBX.
 */
static int make_syscall_stub(void *start, int sc_num)
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

	return p;
}


static int make_int_sc_stub(uint8_t *mem, int interrupt)
{
	int p = 0;
	mem[p++] = 0xcd;	/* INT imm8 */
	mem[p++] = (unsigned)interrupt;
	mem[p++] = 0xc3;	/* RET */

	return p;
}


static int make_sysenter_stub(void *start, int sc_num, bool save_bees)
{
	int p = 0;

#ifdef CONFIG_X86_SYSENTER
	uint8_t *mem = start;
	if(save_bees) {
		static const uint8_t code[] = {
			0x55,				/* pushl %ebp */
			0x65, 0x8b, 0x2d, 0x00, 0x00, 0x00, 0x00,	/* movl %gs:0, %ebp */
			0x89, 0x5d, 0xf8,	/* movl %ebx, -0x8(%ebp) */
			0x8f, 0x45, 0xfc,	/* popl -0x4(%ebp) */
		};
		memcpy(&mem[p], code, NUM_ELEMENTS(code));
		p += NUM_ELEMENTS(code);
	}
	mem[p++] = 0xb3; mem[p++] = sc_num;	/* movb $sc_num, %bl */
	mem[p++] = 0x89; mem[p++] = 0xe5;	/* movl %esp, %ebp */
	mem[p++] = 0x0f; mem[p++] = 0x34;	/* sysenter */
#endif

	return p;
}


static int make_sysexit_epilog(void *start, bool get_ecx, bool get_edx)
{
	int p = 0;
#ifdef CONFIG_X86_SYSENTER
	uint8_t *mem = start;
	if(get_ecx) {
		/* movl %gs:0, %ecx */
		mem[p++] = 0x65; mem[p++] = 0x8b; mem[p++] = 0x0d;
		for(int i=0; i < 4; i++) mem[p++] = 0x00;
		if(get_edx) {
			/* movl -8(%ecx), %edx */
			mem[p++] = 0x8b; mem[p++] = 0x51; mem[p++] = 0xf8;
		}
		/* movl -4(%ecx), %ecx */
		mem[p++] = 0x8b; mem[p++] = 0x49; mem[p++] = 0xfc;
	} else if(get_edx) {
		/* used by Schedule (timectl in edx) */
		/* movl %gs:0, %edx */
		mem[p++] = 0x65;
		mem[p++] = 0x8b;
		mem[p++] = 0x15;
		for(int i=0; i < 4; i++) mem[p++] = 0x00;
		/* movl -8(%edx), %edx */
		mem[p++] = 0x8b; mem[p++] = 0x52; mem[p++] = 0xf8;
	}
	mem[p++] = 0xc3;		/* ret */
#endif

	return p;
}


/* extra clever thing where the syscall stub never enters the kernel.
 * instead, the clock tick is stored in the last 8 bytes of the KIP.
 */
static int make_systemclock_stub(uint8_t *mem)
{
	static const uint8_t code[] = {
		0xe8, 0x0b, 0x00, 0x00, 0x00,	/* call {rel32} [past the ret] */
		0x8b, 0x51, 0x04,	/* movl 4(%ecx), %edx */
		0x8b, 0x01,			/* movl (%ecx), %eax */
		0x39, 0x51, 0x04,	/* cmpl %edx, 4(%ecx) */
		0x75, 0xf6,			/* jne [back to first mov] */
		0xc3,				/* ret */
		/* the "read & massage EIP" sequence */
		0x8b, 0x0c, 0x24,	/* movl (%esp), %ecx */
		0x81, 0xc9, 0xff, 0x0f, 0x00, 0x00,	/* orl $0xfff, %ecx */
		0x83, 0xe1, 0xf8,	/* andl $~7, %ecx */
		0xc3,				/* ret */
	};
	memcpy(mem, code, NUM_ELEMENTS(code));
	return NUM_ELEMENTS(code);
}


static void make_lipc_stub(
	void *start, int *len_p, int ipc_offset, bool use_sysenter)
{
	int p = 0;

	void *kip_start = (void *)((L4_Word_t)start & ~0xffful);
	uint8_t *mem = start;
	/* prelude: combine masks into %ebp to check if Lipc is applicable. */
	static const uint8_t prelude[] = {
		0x89, 0xc5,			/* movl %eax, %ebp */
		0x83, 0xe5, 0x3f,	/* andl $0x3f, %ebp */
		0x31, 0xdb,			/* xorl %ebx, %ebx */
		0x85, 0xd2,			/* test %edx, %edx */
		0x0f, 0x94, 0xc3,	/* sete %bl */
		0x09, 0xdd,			/* orl %ebx, %ebp */
		0x89, 0xf3,			/* movl %esi, %ebx */
		0x81, 0xe3, 0xc0, 0xff, 0x00, 0x00,		/* andl $0xffc0, %ebx */
		0x09, 0xdd,			/* orl %ebx, %ebp */
		0x85, 0xed,			/* test %ebp, %ebp */
	};
	for(int i=0; i < NUM_ELEMENTS(prelude); i++) mem[p++] = prelude[i];

	int pos = (void *)&mem[p] - kip_start;
	mem[p++] = 0x0f;		/* jne rel32 (the Ipc stub) */
	mem[p++] = 0x85;
	/* as for me, I hate the Intel manuals' explanation of what the offset is
	 * relative to. apparently the "current value of EIP" is "address of jump
	 * insn + its length".
	 */
	*(int32_t *)&mem[p] = ipc_offset - pos - 6;
	p += 4;
	mem[p++] = 0x8b;		/* movl -0x38(%eax), %ebx */
	mem[p++] = 0x58;
	mem[p++] = 0xc8;
	mem[p++] = 0x3b;		/* cmpl -0x38(%edi), %ebx */
	mem[p++] = 0x5f;
	mem[p++] = 0xc8;
	pos = (void *)&mem[p] - kip_start;
	mem[p++] = 0x0f;		/* jne rel32 (the Ipc stub) */
	mem[p++] = 0x85;
	*(int32_t *)&mem[p] = ipc_offset - pos - 6;
	p += 4;

	if(USE_SYSENTER && use_sysenter) {
		/* the SYSENTER path. the kernel synthesizes an %eip for us (into one
		 * of the sysexit epilogs for Ipc/Lipc), but %esp must be passed in a
		 * register; this'll be %ebp. further, %ebx carries the syscall
		 * number. so the simpler things the INT option does aren't applicable
		 * verbatim.
		 *
		 * instead we'll push MR1 and MR2 into the stack. _sysenter_top will
		 * grab them using kernel segment games, and something will pop them
		 * before kernel exit.
		 */
		static const uint8_t sysenter_lipc_tail[] = {
			0xb3, 0x02,			/* movb $0x2, %bl */
			0xff, 0x77, 0x08,	/* pushl 0x8(%edi) */
			0xff, 0x77, 0x04,	/* pushl 0x4(%edi) */
			0x89, 0xe5,			/* movl %esp, %ebp */
			0x0f, 0x34,			/* sysenter */
		};
		memcpy(mem + p, sysenter_lipc_tail, NUM_ELEMENTS(sysenter_lipc_tail));
		p += NUM_ELEMENTS(sysenter_lipc_tail);
	} else {
		/* the int $0x8c path. pass MR1, MR2 in %ebx, %ebp resp. */
		static const uint8_t rest[] = {
			0x8b, 0x5f, 0x04,	/* movl 0x4(%edi), %ebx */
			0x8b, 0x6f, 0x08,	/* movl 0x8(%edi), %ebp */
			0xcd, 0x8c,			/* int $0x8c	[Lipc] */
		};
		memcpy(mem + p, rest, NUM_ELEMENTS(rest));
		p += NUM_ELEMENTS(rest);
	}

	*len_p = p;
}


static int make_lipc_epilog(uint8_t *mem)
{
	static const uint8_t code[] = {
		0x66, 0x83, 0xfe, 0x02,	/* cmp $0x2, %esi */
		0x7e, 0x12,				/* jle [ret position] */
		0x56,					/* pushl %esi */
		0x57,					/* pushl %edi */
		0x83, 0xe6, 0x3f,		/* andl $0x3f, %esi */
		0x8d, 0x4e, 0xfe,		/* leal -2(%esi), %ecx */
		0x8d, 0x70, 0x0c,		/* leal 12(%eax), %esi */
		0x8d, 0x7f, 0x0c,		/* leal 12(%edi), %edi */
		0xf3, 0xa5,				/* rep movsl */
		0x5f,					/* popl %edi */
		0x5e,					/* popl %esi */
		0xc3,					/* ret */
	};
	memcpy(mem, code, NUM_ELEMENTS(code));
	return NUM_ELEMENTS(code);
}


struct rip_param {
	struct memdescbuf *mdb;
	bool ok_acc;
};


static COLD void reserve_init_page(struct page *page, void *priv)
{
	struct rip_param *p = priv;
	L4_Word_t addr = page->id << PAGE_BITS;
	p->ok_acc &= mdb_set(p->mdb, addr, addr + PAGE_SIZE - 1, false,
		L4_ReservedMemoryType, 0);
}


/* this un-dedicates all simple boot modules. it panics if any lie within the
 * kernel reserved ranges.
 *
 * it does absolutely nothing about SimpleExecs, the EFI header, or the MBI
 * pointer.
 */
static COLD bool export_bootmodule(struct memdescbuf *mdb, L4_BootRec_t *rec)
{
	if(rec->type != L4_BootInfo_Module) return true;

	L4_Boot_Module_t *mod = (void *)rec;
	L4_Word_t start = mod->start,
		end = (mod->start + mod->size - 1) | PAGE_MASK;

	if(!L4_IsNilFpage(mdb_query(mdb, start, end, false, false,
		L4_ReservedMemoryType)))
	{
		panic("boot module overlaps kernel reserved space");
	}

	return mdb_set(mdb, start, end, false, L4_ConventionalMemoryType, 0);
}


static COLD void modify_kernel_memdescs(
	struct memdescbuf *mdb,
	L4_KernelConfigurationPage_t *kcp,
	L4_Word_t kern_start, L4_Word_t kern_end)
{
	/* un-dedicate, un-reserve roottask memory. */
	bool ok = mdb_set(mdb, kcp->root_server.low, kcp->root_server.high,
		false, L4_ConventionalMemoryType, 0);

	/* same for the user boot modules. we'll assume that mbiloader puts
	 * BootInfo on the KCP, so that we can translate the physical address into
	 * kmain.c's copy.
	 */
	if(kcp->BootInfo != 0) {
		L4_BootInfo_t *binf = (void *)kcp + (kcp->BootInfo & PAGE_MASK);
		printf("BootInfo=%#lx, kcp=%p, binf=%p\n", kcp->BootInfo, kcp, binf);
		printf("  magic=%#lx, version=%#lx\n", binf->magic, binf->version);
		if(L4_BootInfo_Valid(binf) && binf->first_entry != 0) {
			L4_BootRec_t *rec = (void *)binf + binf->first_entry;
			for(int i=0; i < binf->num_entries; i++) {
				ok &= export_bootmodule(mdb, rec);
				if(rec->offset_next == 0) break;
				rec = L4_BootRec_Next(rec);
			}
		}
	}

	/* the kernel high VM segment. */
	ok &= mdb_set(mdb, KERNEL_SEG_START, ~0ul,
		true, L4_ReservedMemoryType, 0);

	/* set the kernel's load range to reserved instead of dedicated. */
	ok &= mdb_set(mdb, kern_start, kern_end, false, L4_ReservedMemoryType, 0);

	/* kernel reserved pages. */
	struct rip_param p = { .mdb = mdb, .ok_acc = ok };
	heap_for_each_init_page(&reserve_init_page, &p);
	ok = p.ok_acc;

	if(!ok) panic("mdb_set() failed");
}


/* compose a 32-bit little-endian kernel interface page. */
void make_kip(
	void *mem,
	L4_Word_t kern_start, L4_Word_t kern_end,
	int max_irq,
	L4_Time_t sched_prec)
{
	/* copy the KCP's memorydescs into a buffer so that we can modify them to
	 * indicate e.g. the ia32 kernel high segment, kernel memory reservations,
	 * and so forth.
	 */
	L4_KernelConfigurationPage_t *kcp = mem;
	struct memdescbuf mdb = { .size = kcp->MemoryInfo.n + 15 };
	mdb.ptr = malloc(sizeof(L4_MemoryDesc_t) * mdb.size);
	mdb.len = kcp->MemoryInfo.n;
	memcpy(mdb.ptr, mem + kcp->MemoryInfo.MemDescPtr,
		sizeof(L4_MemoryDesc_t) * kcp->MemoryInfo.n);
	modify_kernel_memdescs(&mdb, kcp, kern_start, kern_end);

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
		bool save_bees;	/* %ebx, %ebp are preserved or parameters */
	} syscalls[] = {
		/* not included:
		 * SC_SYSTEMCLOCK, implemented as a soft syscall.
		 * SC_LIPC, gets a special stub.
		 *
		 * also, in non-sysenter mode SC_EXREGS and SC_MEMCTL have dedicated
		 * interrupt vectors.
		 */
		{ SC_IPC, 0xe0 },
		{ SC_UNMAP, 0xe8 },
		{ SC_THREADSWITCH, 0xf4, true },
		{ SC_EXREGS, 0xec, true },
		{ SC_MEMCTL, 0xdc, true },
		{ SC_SCHEDULE, 0xf8 },
		{ SC_SPACECONTROL, 0xd0 },
		{ SC_THREADCONTROL, 0xd4 },
		{ SC_PROCESSORCONTROL, 0xd8 },
	};
	const int num_syscalls = sizeof(syscalls) / sizeof(syscalls[0]);
	int kip_pos = 0x100, ipc_pos = kip_pos;
	if(USE_SYSENTER && cpu_has_sysenter()) {
		printf("using SYSENTER/SYSEXIT for syscalls\n");
		/* FIXME: move this into cpu.c or somewhere; it should run for each
		 * CPU, possibly per space if smallspaces are involved
		 */
		extern void _sysenter_top();
		x86_wrmsr(IA32_SYSENTER_CS, SYSENTER_CS_SEG << 3);
		x86_wrmsr(IA32_SYSENTER_EIP,
			(L4_Word_t)&_sysenter_top + KERNEL_SEG_START);
		x86_wrmsr(IA32_SYSENTER_ESP, kernel_tss.esp0 + KERNEL_SEG_START);

		for(int i=0; i < num_syscalls; i++) {
			int len = make_sysenter_stub(mem + kip_pos, syscalls[i].sc_num,
				syscalls[i].save_bees);
			*(L4_Word_t *)(mem + syscalls[i].offset) = kip_pos;

			/* sprinkle certain epilogs in between for cache efficiency. */
			switch(syscalls[i].sc_num) {
				case SC_IPC:
					sysexit_epilogs.fast = kip_pos + len;
					len += make_sysexit_epilog(
						mem + kip_pos + len, false, false);
					break;
				case SC_SCHEDULE:
					sysexit_epilogs.edx = kip_pos + len;
					len += make_sysexit_epilog(
						mem + kip_pos + len, false, true);
					break;
				case SC_SPACECONTROL:
					sysexit_epilogs.ecx = kip_pos + len;
					len += make_sysexit_epilog(
						mem + kip_pos + len, true, false);
					break;
				case SC_EXREGS:
					sysexit_epilogs.ecdx = kip_pos + len;
					len += make_sysexit_epilog(
						mem + kip_pos + len, true, true);
					break;
			}

			kip_pos = (kip_pos + len + 15) & ~15;
		}
		/* Lipc */
		kip_pos = (kip_pos + 63) & ~63;
		int len = 0;
		make_lipc_stub(mem + kip_pos, &len, ipc_pos, true);
		*(L4_Word_t *)(mem + 0xe4) = kip_pos;
		kip_pos += len;
	} else {
		/* syscalls via software interrupt, like it's still 1994 */
		for(int i=0; i < num_syscalls; i++) {
			int sc = syscalls[i].sc_num;
			if(sc == SC_EXREGS || sc == SC_MEMCTL) continue;
			int len = make_syscall_stub(mem + kip_pos, sc);
			*(L4_Word_t *)(mem + syscalls[i].offset) = kip_pos;
			kip_pos += (len + 15) & ~15;
		}
		/* ExchangeRegisters */
		int len = make_int_sc_stub(mem + kip_pos, 0x8d);
		*(L4_Word_t *)(mem + 0xec) = kip_pos;
		kip_pos += (len + 15) & ~15;
		/* MemoryControl */
		len = make_int_sc_stub(mem + kip_pos, 0x8e);
		*(L4_Word_t *)(mem + 0xdc) = kip_pos;
		kip_pos = (kip_pos + len + 63) & ~63;
		/* Lipc */
		make_lipc_stub(mem + kip_pos, &len, ipc_pos, false);
		*(L4_Word_t *)(mem + 0xe4) = kip_pos;
		kip_pos += len;
	}
	/* Lipc epilogue (same regardless of SYSEXIT usage; no bump for
	 * cacheline!)
	 */
	int len = make_lipc_epilog(mem + kip_pos);
	lipc_epilog_offset = kip_pos;
	kip_pos = (kip_pos + len + 63) & ~63;
	/* SystemClock (pure vsyscall) */
	len = make_systemclock_stub(mem + kip_pos);
	*(L4_Word_t *)(mem + 0xf0) = kip_pos;
	kip_pos += (len + 63) & ~63;

	/* MemoryInfo */
	mdb_sort(&mdb);			/* sort to combine shrapnel in normalize */
	mdb_normalize(&mdb);
	mdb_sort(&mdb);
	void *memdesc_base = mem + kip_pos;
	memcpy(memdesc_base, mdb.ptr, sizeof(L4_MemoryDesc_t) * mdb.len);
	kip_pos += sizeof(L4_MemoryDesc_t) * mdb.len;
	kip->MemoryInfo.n = mdb.len;
	kip->MemoryInfo.MemDescPtr = memdesc_base - mem;

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
