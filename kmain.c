
#include <stdio.h>
#include <stdbool.h>
#include <ukernel/types.h>
#include <ukernel/ioport.h>
#include <ukernel/16550.h>
#include <ccan/likely/likely.h>

#include "multiboot.h"
#include "mm.h"


typedef uint32_t pdir_t;
typedef uint32_t page_t;

struct idt_entry {
	unsigned short offset_1;
	unsigned short selector;
	unsigned char zero_0;
	unsigned char type_attr;
	unsigned short offset_2;
} __attribute__((packed));


/* courtesy of L4Ka::pistachio */
struct x86_exregs {
	uint32_t reason;
	uint32_t es;
	uint32_t ds;
	uint32_t edi;
	uint32_t esi;
	uint32_t ebp;
	uint32_t __esp;
	uint32_t ebx;
	uint32_t edx;
	uint32_t ecx;
	uint32_t eax;
	/* trapgate frame */
	uint32_t error;
	uint32_t eip;
	uint32_t cs;
	uint32_t eflags;
	uint32_t esp;
	uint32_t ss;
};


#define CHECK_FLAG(mask, bit) (((mask) & (bit)) != 0)

#define PAGE_ALIGN __attribute__((aligned(4096)))

/* x86 page directory flags */
#define PDIR_PRESENT (1 << 0)
#define PDIR_RW (1 << 1)
#define PDIR_USER (1 << 2)
#define PDIR_WRITETHROUGH (1 << 3)
#define PDIR_CACHEDISABLE (1 << 4)
#define PDIR_ACCESSED (1 << 5)
#define PDIR_LARGE (1 << 6)
#define PDIR_IGNORED (1 << 7)

/* x86 page table flags */
#define PT_PRESENT (1 << 0)
#define PT_RW (1 << 1)
#define PT_USER (1 << 2)
#define PT_WRITETHROUGH (1 << 3)
#define PT_CACHEDISABLE (1 << 4)
#define PT_ACCESSED (1 << 5)
#define PT_DIRTY (1 << 6)
#define PT_GLOBAL (1 << 7)

/* IDT type attr bits */
#define IDT_PRESENT (1 << 7)
#define IDT_PRIVILEGE ((1 << 6) | (1 << 5))
#define IDT_STORAGE (1 << 4)
#define IDT_GATE_TYPE 0x0f



static pdir_t kernel_pdirs[1024] PAGE_ALIGN;


/* rudimentary serial port output from µiX */
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


static void put_supervisor_page(intptr_t addr, uint32_t page_id)
{
	pdir_t *dir = &kernel_pdirs[addr >> 22];
	page_t *pages;
	if(unlikely(!CHECK_FLAG(*dir, PDIR_PRESENT))) {
		struct page *pg = get_kern_page();
		pages = pg->vm_addr;
		*dir = (pdir_t)(pg->id << 12) | PDIR_PRESENT | PDIR_RW;
	} else {
		/* FIXME: access the page directory via a id-to-vm mapping, even
		 * though these are guaranteed idempotent right now.
		 */
		pages = (void *)(*dir & ~PAGE_MASK);
	}

	int poffs = (addr >> 12) & 0xfff;
	pages[poffs] = (page_id << PAGE_BITS) | PT_PRESENT | PT_RW;

	/* valid since the 80486. */
	__asm__ volatile("invlpg %0"::"m" (*(char *)addr): "memory");
}


static void setup_paging(intptr_t id_start, intptr_t id_end)
{
	/* all present bits are turned off. */
	for(int i=0; i < 1024; i++) kernel_pdirs[i] = 0;

	/* identitymap between id_start and id_end inclusive */
	id_start &= ~PAGE_MASK;
	id_end = (id_end + PAGE_SIZE - 1) & ~PAGE_MASK;
	for(intptr_t addr = id_start; addr < id_end; addr += PAGE_SIZE) {
		put_supervisor_page(addr, addr >> PAGE_BITS);
	}

	/* load the supervisor page table (kernel_pdirs into CR3), then enable
	 * paging.
	 */
	__asm__ __volatile__ (
		"\tmovl %0, %%cr3\n"
		"\tmovl %%cr0, %%eax\n"
		"\torl $0x80000000, %%eax\n"
		"\tmovl %%eax, %%cr0\n"
		:
		: "a" (kernel_pdirs)
		: "memory");
}


static void panic(const char *message)
{
	printf("PANIC: %s\n", message);
	asm("cli; hlt");
}


static void setup_idt(void)
{
	extern void isr_top(void);
	extern void isr14_top(void);

	static struct idt_entry ints[256];
	for(int i=0; i < 256; i++) {
		ints[i] = (struct idt_entry){ /* all zero */ };
	}

	unsigned short sel;
	__asm__ __volatile__ ("\tmovl %%cs, %%eax\n": "=a" (sel));
	printf("using selector %d\n", (int)sel);

	/* division by zero */
	ints[0] = (struct idt_entry){
		.offset_1 = (unsigned long)&isr_top & 0xffff,
		.offset_2 = (unsigned long)&isr_top >> 16,
		.selector = sel,
		.type_attr = IDT_PRESENT | 0xe,		/* 32-bit interrupt gate */
	};

	/* pagefaults */
	ints[14] = (struct idt_entry){
		.offset_1 = (unsigned long)&isr14_top & 0xffff,
		.offset_2 = (unsigned long)&isr14_top >> 16,
		.selector = sel,
		.type_attr = IDT_PRESENT | 0xe,		/* 32-bit interrupt gate */
	};

	struct {
		unsigned short limit;
		unsigned long base;
	} __attribute__((packed)) idt_desc = {
		.limit = sizeof(ints),
		.base = (unsigned long)&ints[0],
	};
	__asm__ __volatile__("\tlidt %0\n":: "m" (idt_desc));
}


void isr_bottom()
{
	unsigned char *videoram = (unsigned char *)0xb8000;
	videoram[0] = 'A';
	videoram[1] = 0x07;		/* light grey (7) on black (0). */

	asm("cli; hlt");
}


void isr14_bottom(struct x86_exregs *regs)
{
	uint32_t fault_addr;
	__asm__ __volatile__("\tmovl %%cr2, %0\n" : "=r" (fault_addr)
		:: "memory");

#if 0
	unsigned char *videoram = (unsigned char *)0xb8000;
	videoram[0] = 'Z';
	videoram[1] = 0x07;		/* light grey (7) on black (0). */
#endif

	/* set up an identity mapping if it's in the first 4 MiB. */
	int dir = fault_addr >> 22, p = (fault_addr >> 12) & 0xfff;
	page_t *pages = (page_t *)(kernel_pdirs[dir] & ~0xfff);
	if(pages == NULL) {
		/* TODO: track this page: it is used for kernel-space page tables. */
		struct page *pg = get_kern_page();
		pages = pg->vm_addr;
		for(int i=0; i < 1024; i++) pages[i] = 0;
		kernel_pdirs[dir] = (pdir_t)(pg->id << PAGE_BITS) | PDIR_PRESENT | PDIR_RW;
	}
	if(fault_addr < 4 * 1024 * 1024) {
		/* idempotent mappings of low physical RAM (ugly) */
		pages[p] = (p << 12) | PT_PRESENT | PT_RW;
	} else if((fault_addr & 0xf0000000) == 0xf0000000) {
		if(!CHECK_FLAG(pages[p], PT_PRESENT)) {
			/* allocate. */
			struct page *pg = get_kern_page();
			pages[p] = (pg->id << PAGE_BITS) | PT_PRESENT | PT_RW;
			printf("allocated physical page 0x%x for fault in kernel memory\n",
				(unsigned)pg->id << PAGE_BITS);
		}
	} else {
		printf("pf (%s, %s, %s) @ 0x%x (eip 0x%x)\n",
			CHECK_FLAG(regs->error, 4) ? "user" : "supervisor",
			CHECK_FLAG(regs->error, 2) ? "write" : "read",
			CHECK_FLAG(regs->error, 1) ? "protection" : "notpresent",
			fault_addr, regs->eip);

		printf("unable to handle. halting.\n");
		asm("cli; hlt");
	}
#if 1
	/* valid since the 80486. */
	__asm__ volatile("invlpg %0"::"m" (*(char *)fault_addr): "memory");
#else
	__asm__ __volatile__(
		"\tmovl %%cr3, %%eax\n"
		"\tmovl %%eax, %%cr3\n"
		::: "eax", "memory");
#endif
}


static void add_mbi_memory(
	struct multiboot_info *mbi,
	intptr_t excl_start,
	intptr_t excl_end)
{
	printf("%s: excl_start 0x%x, excl_end 0x%x\n", __func__,
		(unsigned)excl_start, (unsigned)excl_end);
	for(struct multiboot_mmap_entry *mm = (void *)mbi->mmap_addr;
		(unsigned long)mm < mbi->mmap_addr + mbi->mmap_length;
		mm = (void *)mm + mm->size + sizeof(mm->size))
	{
		if(mm->type != MULTIBOOT_MEMORY_AVAILABLE
			|| mm->len < PAGE_SIZE
			|| mm->addr > (uint64_t)~0u)
		{
			continue;
		}

		intptr_t start = mm->addr, end = (mm->addr + mm->len) & ~PAGE_MASK;
		if(start > excl_end || end < excl_start) {
			add_boot_pages(start, end);
		} else if(start >= excl_start && end <= excl_end) {
			/* skip entirely */
		} else if(start >= excl_start) {
			add_boot_pages(excl_end + 1, end);
		} else if(end <= excl_end) {
			add_boot_pages(start, excl_start - 1);
		} else {
			/* brute force. */
			for(intptr_t p = start; p < end; p += PAGE_SIZE) {
				if(p < excl_start || p > excl_end) {
					add_boot_pages(p, p + PAGE_SIZE - 1);
				}
			}
		}
	}
}


void malloc_panic(void) {
	panic("malloc failure!");
}


void kmain(void *mbd, unsigned int magic)
{
	if(magic != 0x2BADB002) {
		/* hang! */
		return;
	}

	/* also, output some stuff to the serial port. */
	printf("hello, world! mbd is at 0x%x\n", (unsigned)mbd);

	struct multiboot_info *mbi = mbd;
	printf("flags 0x%x\n", mbi->flags);
	if(CHECK_FLAG(mbi->flags, MULTIBOOT_INFO_MEMORY)) {
		printf("mem_lower 0x%x, mem_upper 0x%x\n", mbi->mem_lower,
			mbi->mem_upper);
	}
	if(CHECK_FLAG(mbi->flags, MULTIBOOT_INFO_BOOTDEV)) {
		printf("bootdev 0x%x\n", mbi->boot_device);
	}
	if(CHECK_FLAG(mbi->flags, MULTIBOOT_INFO_MODS)) {
		printf("mods_count %u, mods_addr 0x%x\n", mbi->mods_count,
			mbi->mods_addr);
	}

	bool found_mem = false;
	intptr_t resv_start = 0, resv_end = 0;
	if(CHECK_FLAG(mbi->flags, MULTIBOOT_INFO_MEM_MAP)) {
		printf("multiboot memory map (0x%x, length 0x%x):\n",
			mbi->mmap_addr, mbi->mmap_length);
		for(struct multiboot_mmap_entry *mm = (void *)mbi->mmap_addr;
			(unsigned long)mm < mbi->mmap_addr + mbi->mmap_length;
			mm = (void *)mm + mm->size + sizeof(mm->size))
		{
			printf("  %s: addr 0x%x, size 0x%x, len 0x%x (%d MiB)\n",
				mm->type == MULTIBOOT_MEMORY_AVAILABLE
					? "available" : "reserved",
				(unsigned)mm->addr, (unsigned)mm->size,
				(unsigned)mm->len, (int)(mm->len / (1024 * 1024)));

			if(mm->type == MULTIBOOT_MEMORY_AVAILABLE
				&& mm->len >= 8 * 1024 * 1024
				&& mm->addr <= ~0u)
			{
				found_mem = true;
				init_kernel_heap(mm, &resv_start, &resv_end);
			}
		}
	}
	if(!found_mem) {
		panic("didn't find any memory in multiboot spec!");
	}

	printf("setting up interrupts...\n");
	setup_idt();

	printf("setting up paging (id maps between 0x%x and 0x%x)...\n",
		(unsigned)resv_start, (unsigned)resv_end);
	setup_paging(resv_start, resv_end);
	printf("adding identity maps for MBI memory...\n");
	/* NOTE: this is a big hack: generally the MBI info fits in less than 4k
	 * of memory.
	 */
	put_supervisor_page((intptr_t)mbi & ~PAGE_MASK, (intptr_t)mbi >> PAGE_BITS);

	if(CHECK_FLAG(mbi->flags, MULTIBOOT_INFO_MEM_MAP)) {
		printf("adding MultiBoot memory...\n");
		add_mbi_memory(mbi, resv_start & ~PAGE_MASK, resv_end | PAGE_MASK);
	}

#if 0
	static int zero;
	printf("divide by zero: %d\n", 777 / zero);
#endif

	/* handleable fault. */
	volatile char *memory = (char *)0x210000;
	memory[0] = 1;
	memory[1] = 2;

	/* malloc test. */
	char *foo = malloc(64);
	foo[0] = 'q';
	foo[1] = 'w';
	foo[2] = 'e';

	/* somewhat less so. */
	*(char *)0xdeadbeef = 1;

	printf("slamming teh brakes now.\n");
}
