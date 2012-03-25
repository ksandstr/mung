
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


int bootmain(multiboot_info_t *mbi, uint32_t magic)
{
	printf("mbiloader says hello!\n");

	/* find top of physical memory. */
	uintptr_t ram_top = 0;
	if(CHECK_FLAG(mbi->flags, MULTIBOOT_INFO_MEM_MAP)) {
		printf("mmap_length %#x, mmap_addr %#x\n", mbi->mmap_length,
			mbi->mmap_addr);
		struct multiboot_mmap_entry *mme = (void *)mbi->mmap_addr;
		for(int i=0; i < mbi->mmap_length / sizeof(*mme); i++) {
			if(mme[i].type == MULTIBOOT_MEMORY_RESERVED) continue;
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
			parse_elf_range(bm);
			printf("  mod %d = [%#x .. %#x] -> [%#x .. %#x] `%s'\n", i,
				bm->start, bm->end, bm->load_start, bm->load_end,
				bm->cmdline);
			r_start = MIN(uintptr_t, bm->start, r_start);
			r_end = MAX(uintptr_t, bm->end - 1, r_end);
		}
	} else {
		panic("no multiboot modules found!");
	}
	printf("multiboot modules are between %#x and %#x inclusive.\n",
		r_start, r_end);

	return 0;
}
