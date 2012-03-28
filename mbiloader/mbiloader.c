
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include <ukernel/x86.h>
#include <ukernel/16550.h>
#include <ukernel/mm.h>
#include <ukernel/misc.h>

#include "multiboot.h"
#include "elf.h"


#define MAX_BOOT_MODS 32


struct boot_module
{
	uintptr_t start, end;
	uintptr_t load_start, load_end;
	uintptr_t sp, ip;
	char cmdline[128];
} __attribute__((packed));


#define BETWEEN(low, high, x) ((low) <= (x) && (x) <= (high))
#define RANGE_OVERLAP(l0, h0, l1, h1) \
	(BETWEEN((l0), (h0), (l1)) || BETWEEN((l0), (h0), (h1)) \
		|| BETWEEN((l1), (h1), (l0)) || BETWEEN((l1), (h1), (h0)))


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


void putstr(const char *str)
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


/* construct a 32-bit kernel configuration page. */
static void fill_kcp(
	uint8_t *kcp_base,
	struct boot_module *kernel_mod,
	struct boot_module *s0_mod,
	struct boot_module *s1_mod,
	struct boot_module *roottask_mod)
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

	*(L4_Word_t *)&kcp_base[0x54] = 0;		/* MemoryInfo */
	*(L4_Word_t *)&kcp_base[0x58] = 0;		/* kdebug.config0 */
	*(L4_Word_t *)&kcp_base[0x5c] = 0;		/* kdebug.config1 */

	*(L4_Word_t *)&kcp_base[0xb8] = 0;		/* BootInfo */

	/* TODO: memory descriptors */
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
 * another, or with a later module's MBI load address. (TODO: or with the
 * mbiloader's code range!)
 *
 * in the latter case the modules are relocated to after reloc_addr.
 */
static void check_boot_modules(uintptr_t *reloc_addr)
{
	for(int i=0; i < num_boot_mods; i++) {
		struct boot_module *m = &boot_mods[i];
		for(int j=0; j < i; j++) {
			struct boot_module *o = &boot_mods[j];
			if(RANGE_OVERLAP(m->load_start, m->load_end,
				o->load_start, o->load_end))
			{
				printf("mbiloader: modules %d and %d overlap per ELF headers:\n",
					i, j);
				printf("  load ranges [0x%x .. 0x%x] and [0x%x .. 0x%x] respectively\n",
					m->load_start, m->load_end, o->load_start, o->load_end);
				panic("boot module ranges overlap!");
			}
		}

		for(int j=i + 1; j < num_boot_mods; j++) {
			if(RANGE_OVERLAP(m->load_start, m->load_end,
				boot_mods[j].start, boot_mods[j].end - 1))
			{
				printf("moving module %d (`%s') to 0x%x\n",
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
	if(CHECK_FLAG(mbi->flags, MULTIBOOT_INFO_MEM_MAP)) {
		printf("mmap_length %#x, mmap_addr %#x\n", mbi->mmap_length,
			mbi->mmap_addr);
		struct multiboot_mmap_entry *mme = (void *)mbi->mmap_addr;
		for(int i=0; i < mbi->mmap_length / sizeof(*mme); i++) {
			if(mme[i].type == MULTIBOOT_MEMORY_RESERVED) continue;

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
		panic("no multiboot memory-map info!");
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
			printf("  mod %d = [%#x .. %#x] -> [%#x .. %#x] `%s'\n", i,
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

	uintptr_t heap_start = r_end;
	for(int i=0; i < num_boot_mods; i++) {
		heap_start = MAX(uintptr_t, heap_start, boot_mods[i].load_end + 1);
	}
	heap_start = (heap_start + PAGE_SIZE - 1) & ~PAGE_MASK;
	printf("relocation heap starts at 0x%x\n", heap_start);
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
	fill_kcp(kcp_base, kernel_mod, s0_mod, s1_mod, roottask_mod);

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
