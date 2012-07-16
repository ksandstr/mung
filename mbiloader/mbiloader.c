
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include <l4/types.h>
#include <l4/kcp.h>
#include <l4/kip.h>
#include <l4/bootinfo.h>

#include <ukernel/x86.h>
#include <ukernel/16550.h>
#include <ukernel/mm.h>
#include <ukernel/util.h>

#include "multiboot.h"
#include "elf.h"


/* TODO: add dynamic memory allocation? */
#define MAX_BOOT_MODS 32
#define MAX_MMAP_ENTS 32


struct boot_module
{
	L4_Word_t start, end;
	L4_Word_t load_start, load_end;
	L4_Word_t sp, ip;
	char cmdline[128];
} __attribute__((packed));


static int num_boot_mods = 0;
static struct boot_module boot_mods[MAX_BOOT_MODS];


/* rudimentary serial port output from µiX (via kmain.c) */
#define COM_PORT 0x3f8

void computchar(unsigned char ch)
{
//	unsigned int iter = 1;

	/* we'll poll the LSR until the transmit register is empty. */
	while((inb(COM_PORT + UART_LSR) & UART_LSR_ETHR) == 0) {
#if 0
		/* yield every 128k iterations. that's far more than the time required
		 * to transmit one byte even over a 2400bps line.
		 */
		if((++iter & (128*1024-1)) == 0) L4_Yield();
#endif
	}
	outb(COM_PORT + UART_RDWR, ch);
	/* and then poll again until the holding register is empty, i.e. until
	 * the character has really been transmitted.
	 *
	 * (yeah, a proper serial driver would use the FIFO. no, this is quite
	 * enough for now thank you.)
	 */
//	iter = 1;
	while((inb(COM_PORT + UART_LSR) & UART_LSR_EDHR) == 0) {
//		if((++iter & (128*1024-1)) == 0) L4_Yield();
	}
	if(ch == '\n') computchar('\r');
}


void con_putstr(const char *str)
{
	while(*str != '\0') computchar(*(str++));
}


void NORETURN panic(const char *message)
{
	printf("PANIC: %s\n", message);
	while(true) {
		asm("cli; hlt");
	}
}


#ifndef NDEBUG
void __assert_failure(
	const char *condition,
	const char *file,
	unsigned int line,
	const char *function)
{
	printf("assert(%s) failed in `%s' (%s:%u)\n", condition, function,
		file, line);
	panic("*** assertion failure");
}
#endif


void *malloc(size_t size) {
	panic("malloc(3) not implemented in mbiloader");
}


/* construct a 32-bit kernel configuration page.
 * TODO: handle kdebug also.
 */
static void fill_kcp(
	uint8_t *kcp_base,
	struct boot_module *kernel_mod,
	struct boot_module *s0_mod,
	struct boot_module *s1_mod,
	struct boot_module *roottask_mod,
	int mmap_count,
	const struct multiboot_mmap_entry *mm)
{
	memset(kcp_base, 0, PAGE_SIZE);

	/* set the bytes one at a time to avoid confusing the KCP-finding
	 * loop.
	 */
	kcp_base[0] = 'L';
	kcp_base[1] = '4';
	kcp_base[2] = 230;		// 'µ' in iso-8859-1 (?)
	kcp_base[3] = 'K';
	*(L4_Word_t *)&kcp_base[0x04] = 66642;	/* API version */
	*(L4_Word_t *)&kcp_base[0x10] = 0;		/* kdebug.init */
	*(L4_Word_t *)&kcp_base[0x14] = 0;		/* kdebug.entry */
	*(L4_Word_t *)&kcp_base[0x18] = 0;		/* kdebug.low */
	*(L4_Word_t *)&kcp_base[0x1c] = 0;		/* kdebug.high */
	*(L4_Word_t *)&kcp_base[0x20] = s0_mod->sp;
	*(L4_Word_t *)&kcp_base[0x24] = s0_mod->ip;
	*(L4_Word_t *)&kcp_base[0x28] = s0_mod->load_start;
	*(L4_Word_t *)&kcp_base[0x2c] = s0_mod->load_end | PAGE_MASK;
	struct boot_module null_mod = { };
	if(s1_mod == NULL) s1_mod = &null_mod;
	*(L4_Word_t *)&kcp_base[0x30] = s1_mod->sp;
	*(L4_Word_t *)&kcp_base[0x34] = s1_mod->ip;
	*(L4_Word_t *)&kcp_base[0x38] = s1_mod->load_start;
	*(L4_Word_t *)&kcp_base[0x3c] = s1_mod != &null_mod
		? (s1_mod->load_end | PAGE_MASK) : 0;
	if(roottask_mod == NULL) roottask_mod = &null_mod;
	*(L4_Word_t *)&kcp_base[0x40] = roottask_mod->sp;
	*(L4_Word_t *)&kcp_base[0x44] = roottask_mod->ip;
	*(L4_Word_t *)&kcp_base[0x48] = roottask_mod->load_start;
	*(L4_Word_t *)&kcp_base[0x4c] = roottask_mod != &null_mod
		? (roottask_mod->load_end | PAGE_MASK) : 0;

	*(L4_Word_t *)&kcp_base[0x58] = 0;		/* kdebug.config0 */
	*(L4_Word_t *)&kcp_base[0x5c] = 0;		/* kdebug.config1 */

	int resv_pos = 0x100;	/* reserved after the defined KCP bits. */
	L4_BootInfo_t *binf = (L4_BootInfo_t *)&kcp_base[resv_pos];
	*(L4_Word_t *)&kcp_base[0xb8] = (L4_Word_t)binf;	/* BootInfo */
	resv_pos += sizeof(L4_BootInfo_t);
	*binf = (L4_BootInfo_t){
		.magic = L4_BOOTINFO_MAGIC,
		.version = L4_BOOTINFO_VERSION,
		.size = sizeof(L4_BootInfo_t),
		.first_entry = 0, .num_entries = 0,
	};

	/* memory descriptors. the bootloader passes just conventional and
	 * reserved memory.
	 */
	const int md_pos = resv_pos;
	L4_MemoryDesc_t *mdbuf = (L4_MemoryDesc_t *)&kcp_base[resv_pos];
	int p = 0;
	/* the x86 virtual address space. */
	mdbuf[p++] = (L4_MemoryDesc_t){
		.x.type = L4_ConventionalMemoryType, .x.v = 1,
		.x.low = 0, .x.high = ~0ul >> 10,
	};
	for(int i=0; i < mmap_count; i++) {
		if(mm[i].type != MULTIBOOT_MEMORY_AVAILABLE) {
			/* bootloader specific, pass type as seen */
			mdbuf[p++] = (L4_MemoryDesc_t){
				.x.type = L4_BootLoaderSpecificMemoryType,
				.x.t = mm[i].type, .x.v = 0,
				.x.low = mm[i].addr >> 10,
				.x.high = (mm[i].addr + mm[i].len - 1) >> 10,
			};
		} else {
			/* available memory. pass as conventional. */
			mdbuf[p++] = (L4_MemoryDesc_t){
				.x.type = L4_ConventionalMemoryType, .x.v = 0,
				.x.low = mm[i].addr >> 10,
				.x.high = (mm[i].addr + mm[i].len - 1) >> 10,
			};
		}
	}
	/* dedicate memory for the idempotently mapped kernel/root servers */
	struct boot_module *bms[] = {
		s0_mod, s1_mod, roottask_mod, kernel_mod
	};
	for(int i=0; i < sizeof(bms) / sizeof(bms[0]); i++) {
		struct boot_module *m = bms[i];
		if(m != NULL && m->end > 0) {
			printf("dedicating %#lx .. %#lx for boot module %d\n",
				m->load_start, m->load_end, i);
			mdbuf[p++] = (L4_MemoryDesc_t){
				.x.type = L4_DedicatedMemoryType, .x.v = 0,
				.x.low = m->load_start >> 10, .x.high = m->load_end >> 10,
			};
		}
	}
	/* and for the other boot modules. (also collects those boot modules that
	 * should be passed to the root server.)
	 */
	const struct boot_module *list_mods[num_boot_mods];
	int num_list = 0;
	for(int i=0; i < num_boot_mods; i++) {
		const struct boot_module *m = &boot_mods[i];
		bool found = false;
		for(int j=0; j < sizeof(bms) / sizeof(bms[0]); j++) {
			if(m == bms[j]) {
				found = true;
				break;
			}
		}
		if(!found) {
			printf("dedicating %#lx .. %#lx for module `%s'\n",
				m->start & ~PAGE_MASK, m->end | PAGE_MASK, m->cmdline);
			mdbuf[p++] = (L4_MemoryDesc_t){
				.x.type = L4_DedicatedMemoryType, .x.v = 0,
				.x.low = m->start >> 10, .x.high = m->end >> 10,
			};
			list_mods[num_list++] = m;
		}
	}
	resv_pos += sizeof(L4_MemoryDesc_t) * p;

	/* MemoryInfo */
	assert(offsetof(L4_KernelConfigurationPage_t, MemoryInfo) == 0x54);
	*(L4_Word_t *)&kcp_base[0x54] = md_pos << 16 | p;

	/* BootRecs */
	binf->num_entries = num_list;
	L4_BootRec_t *prev = NULL;
	for(int i=0; i < num_list; i++) {
		const struct boot_module *m = list_mods[i];
		L4_Boot_Module_t *mod = (L4_Boot_Module_t *)&kcp_base[resv_pos];
		resv_pos += sizeof(*mod);
		/* the command line, too. */
		int cllen = strlen(m->cmdline), cl_resv = (cllen + 4) & ~3;
		char *cmdline_buf = (char *)&kcp_base[resv_pos];
		memcpy(cmdline_buf, m->cmdline, cllen);
		memset(&cmdline_buf[cllen], 0, cl_resv - cllen);
		resv_pos += cl_resv;

		*mod = (L4_Boot_Module_t){
			.type = L4_BootInfo_Module, .version = 1,
			.start = m->start,
			.size = m->end - m->start + 1,
			.cmdline_offset = (uint8_t *)cmdline_buf - (uint8_t *)mod,
		};

		/* linkage */
		if(prev == NULL) {
			binf->first_entry = (uint8_t *)mod - (uint8_t *)binf;
		} else {
			prev->offset_next = (uint8_t *)mod - (uint8_t *)prev;
		}
		assert(offsetof(L4_Boot_Module_t, offset_next)
			== offsetof(L4_BootRec_t, offset_next));
		prev = (L4_BootRec_t *)mod;
	}
	assert(binf->num_entries == 0 || binf->first_entry != 0);
}


static void parse_elf_range(struct boot_module *mod)
{
	const void *elf = (const void *)mod->start;
	const Elf32_Ehdr *eh = elf;
	uintptr_t phoff = eh->e_phoff;
	uintptr_t m_start = ~0ul, m_end = 0;
	for(int i=0; i < eh->e_phnum; i++, phoff += eh->e_phentsize) {
		const Elf32_Phdr *ph = elf + phoff;
		if(ph->p_type != PT_LOAD) continue;

		m_start = MIN(uintptr_t, m_start, ph->p_vaddr & ~PAGE_MASK);
		uintptr_t past = (ph->p_vaddr + ph->p_memsz + PAGE_SIZE - 1) & ~PAGE_MASK;
		m_end = MAX(uintptr_t, m_end, past - 1);
	}
	mod->load_start = m_start;
	mod->load_end = m_end;
}


static void load_elf_module(struct boot_module *mod)
{
	const void *elf = (const void *)mod->start;
	const Elf32_Ehdr *eh = elf;
	uintptr_t phoff = eh->e_phoff;
	for(int i=0; i < eh->e_phnum; i++, phoff += eh->e_phentsize) {
		const Elf32_Phdr *ph = elf + phoff;
		if(ph->p_type != PT_LOAD) continue;

		size_t copysize = ph->p_filesz;
		memcpy((void *)ph->p_vaddr, (void *)mod->start + ph->p_offset,
			copysize);
		if(copysize < ph->p_memsz) {
			memset((void *)ph->p_vaddr + copysize, 0,
				ph->p_memsz - ph->p_filesz);
		}
	}

	mod->sp = 0;
	mod->ip = eh->e_entry;

	/* blank the memory just to be safe. */
	memset((void *)mod->start, 0, mod->end - mod->start + 1);
}


/* verify that the load ranges of the boot modules don't overlap with one
 * another, or with a later module's MBI load address, or with mbiloader's own
 * range.
 *
 * in the second case affected modules are moved to after reloc_addr.
 */
static void check_boot_modules(L4_Word_t *reloc_addr)
{
	extern char _start, _end;
	const L4_Word_t this_start = (L4_Word_t)&_start,
		this_end = (L4_Word_t)&_end;
	for(int i=0; i < num_boot_mods; i++) {
		struct boot_module *m = &boot_mods[i];

		/* check for overlap with mbiloader. */
		if(RANGE_OVERLAP(m->load_start, m->load_end, this_start, this_end)) {
			printf("mbiloader: module %d's load range [%#lx..%#lx] overlaps mbiloader!\n"
				"  (mbiloader is at [%#lx..%#lx])\n",
				i, m->load_start, m->load_end, this_start, this_end);
			panic("mbiloader: boot module overlaps with loader");
		}

		/* verify that the _load addresses_ don't overlap. */
		for(int j=0; j < i; j++) {
			struct boot_module *o = &boot_mods[j];
			if(RANGE_OVERLAP(m->load_start, m->load_end,
				o->load_start, o->load_end))
			{
				printf("mbiloader: modules %d and %d overlap per ELF headers:\n",
					i, j);
				printf("  load ranges [0x%lx .. 0x%lx] and [0x%lx .. 0x%lx] respectively\n",
					m->load_start, m->load_end, o->load_start, o->load_end);
				panic("boot module ranges overlap!");
			}
		}

		/* now check if load addresses overlap with the modules' data, and
		 * move those modules if so.
		 */
		for(int j=i + 1; j < num_boot_mods; j++) {
			if(RANGE_OVERLAP(m->load_start, m->load_end,
				boot_mods[j].start, boot_mods[j].end - 1))
			{
				printf("moving module %d (`%s') to %#lx\n",
					j, boot_mods[j].cmdline, *reloc_addr);
				size_t length = boot_mods[j].end - boot_mods[j].start + 1;
				memcpy((void *)*reloc_addr, (void *)boot_mods[j].start, length);
				boot_mods[j].start = *reloc_addr;
				boot_mods[j].end = *reloc_addr + length - 1;
				*reloc_addr += length;
				*reloc_addr = (*reloc_addr + PAGE_SIZE - 1) & ~PAGE_MASK;
			}
		}
	}
}


int bootmain(multiboot_info_t *mbi, uint32_t magic)
{
	printf("mbiloader says hello!\n");

	/* find top of physical memory. */
	uintptr_t ram_top = 0;
	size_t mem_before_640k = 0, mem_after_1m = 0;
	int mmap_count = 0;
	struct multiboot_mmap_entry mmap_ents[MAX_MMAP_ENTS];
	if(CHECK_FLAG(mbi->flags, MULTIBOOT_INFO_MEM_MAP)) {
		printf("mmap_length %#x, mmap_addr %#x\n", mbi->mmap_length,
			mbi->mmap_addr);
		mmap_count = mbi->mmap_length / sizeof(struct multiboot_mmap_entry);
		/* copy them off. */
		memcpy(mmap_ents, (void *)mbi->mmap_addr,
			mmap_count * sizeof(struct multiboot_mmap_entry));
		struct multiboot_mmap_entry *mme = (void *)mbi->mmap_addr;
		for(int i=0; i < mmap_count; i++) {
			printf("  %s: addr %#x, len %#x (%d MiB)\n",
				mme[i].type == MULTIBOOT_MEMORY_AVAILABLE
					? "available" : "reserved",
				(unsigned)mme[i].addr, (unsigned)mme[i].len,
				(int)(mme[i].len / (1024 * 1024)));
			if(mme[i].type != MULTIBOOT_MEMORY_AVAILABLE) continue;

			if(mme[i].addr >= 0x100000) mem_after_1m += mme[i].len / 1024;
			else if(mme[i].addr < 640 * 1024) {
				uintptr_t end = MIN(uintptr_t, mme[i].addr + mme[i].len,
					640 * 1024);
				mem_before_640k += (end - mme[i].addr) / 1024;
			}

			if(mme[i].addr + mme[i].len > 0xffffffffull) {
				ram_top = ~0ul;
			} else {
				ram_top = MAX(uintptr_t, ram_top, mme[i].addr + mme[i].len);
			}
		}
		printf("MBI ram_top is %#lx\n", (unsigned long)ram_top);
	} else {
		printf("no multiboot memory-map info!");
	}

	/* get parameters for "*P". fall back to the conservative sum from
	 * before.
	 */
	if(CHECK_FLAG(mbi->flags, MULTIBOOT_INFO_MEMORY)) {
		mem_before_640k = mbi->mem_lower;
		mem_after_1m = mbi->mem_upper;
	}

	/* scan boot modules, noting their load-ranges. */
	uintptr_t r_start = ~0ul, r_end = 0;
	if(CHECK_FLAG(mbi->flags, MULTIBOOT_INFO_MODS)
		&& mbi->mods_count > 0)
	{
		multiboot_module_t *m_base = (multiboot_module_t *)mbi->mods_addr;
		num_boot_mods = MIN(int, mbi->mods_count, MAX_BOOT_MODS);
		for(int i=0; i < num_boot_mods; i++) {
			struct boot_module *bm = &boot_mods[i];
			bm->start = m_base[i].mod_start;
			bm->end = m_base[i].mod_end;
			strncpy(bm->cmdline, (const char *)m_base[i].cmdline,
				sizeof(bm->cmdline));
			/* TODO: allow for non-ELF modules (initrd, etc) */
			parse_elf_range(bm);
			printf("  mod %d = [%#lx .. %#lx] -> [%#lx .. %#lx] `%s'\n", i,
				bm->start, bm->end, bm->load_start, bm->load_end,
				bm->cmdline);
			r_start = MIN(uintptr_t, bm->start, r_start);
			/* TODO: is the "- 1" correct? */
			r_end = MAX(uintptr_t, bm->end - 1, r_end);
		}
	} else {
		panic("no multiboot modules found!");
	}
	printf("multiboot modules are between %#x and %#x inclusive.\n",
		r_start, r_end);

	L4_Word_t heap_start = r_end;
	for(int i=0; i < num_boot_mods; i++) {
		heap_start = MAX(uintptr_t, heap_start, boot_mods[i].load_end + 1);
	}
	heap_start = (heap_start + PAGE_SIZE - 1) & ~PAGE_MASK;
	printf("relocation heap starts at %#lx\n", heap_start);
	check_boot_modules(&heap_start);

	/* locate kernel, sigma0, sigma1, roottask modules.
	 *
	 * the kernel is identified by being the first module. sigma0 and sigma1
	 * are identified by name. roottask follows after sigma1, or after sigma0
	 * if sigma1 isn't present.
	 */
	struct boot_module *kernel_mod = &boot_mods[0],
		*s0_mod = NULL, *s1_mod = NULL, *roottask_mod;
	for(int i=1; i < num_boot_mods; i++) {
		char *slash = strrchr(boot_mods[i].cmdline, '/');
		if(slash == NULL) slash = boot_mods[i].cmdline; else slash++;
		if(s0_mod == NULL && strcmp(slash, "sigma0") == 0) {
			s0_mod = &boot_mods[i];
		} else if(s1_mod == NULL && strcmp(slash, "sigma1") == 0) {
			s1_mod = &boot_mods[i];
		}
	}
	if(s0_mod == NULL) panic("cannot find module for sigma0!");
	roottask_mod = (s1_mod != NULL ? s1_mod : s0_mod) + 1;
	if(roottask_mod >= &boot_mods[num_boot_mods]) {
		printf("no roottask found, leaving KCP slot empty\n");
		roottask_mod = NULL;
	}

	/* load kernel, s0, s1, roottask */
	load_elf_module(kernel_mod);
	load_elf_module(s0_mod);
	if(s1_mod != NULL) load_elf_module(s1_mod);
	if(roottask_mod != NULL) load_elf_module(roottask_mod);

	void *kcp_base = (void *)heap_start;
	heap_start += PAGE_SIZE;
	fill_kcp(kcp_base, kernel_mod, s0_mod, s1_mod, roottask_mod,
		mmap_count, mmap_ents);

	/* the x86 boot parameter "*P"; we'll recycle some unspecified KCP fields
	 * for this.
	 */
	void *bigp = kcp_base + 0x60;
	*(L4_Word_t *)(bigp + 0x00) = 1;
	*(L4_Word_t *)(bigp + 0x04) = mem_before_640k;
	*(L4_Word_t *)(bigp + 0x08) = mem_after_1m;
	printf("entering L4.X2 kernel...\n");
	asm volatile (
		"\tjmp *%%ecx\n"
		:: "a" (0x2BADB002), "b" (bigp), "c" (kernel_mod->ip));

	printf("mbiloader: shouldn't get here! halting.\n");

	return 0;
}
