
#include <stdio.h>
#include <ukernel/types.h>
#include <ukernel/ioport.h>
#include <ukernel/16550.h>
#include <multiboot.h>


typedef unsigned long pdir_t;
typedef unsigned long page_t;

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


static void setup_paging(void)
{
	/* all present bits are turned off. */
	for(int i=0; i < 1024; i++) kernel_pdirs[i] = 0;

	/* identitymap the first 4 megabytes from 0 */
	static page_t pages[1024] PAGE_ALIGN;
	kernel_pdirs[0] = (pdir_t)&pages[0];
	kernel_pdirs[0] |= PDIR_PRESENT | PDIR_RW | PDIR_USER;

	for(int i=0; i < 1024; i++) {
		if(i < 512) {
			/* first two megs are mapped right away. */
			pages[i] = (i << 12) | PT_PRESENT | PT_RW;
		} else {
			/* the other two, test out paging. */
			pages[i] = 0;
		}
	}

	/* load the page table, then. */
	__asm__ __volatile__ (
		"\tmovl %0, %%cr3\n"
		"\tmovl %%cr0, %%eax\n"
		"\torl $0x80000000, %%eax\n"
		"\tmovl %%eax, %%cr0\n"
		:
		: "a" (kernel_pdirs)
		: "memory");
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

	printf("pf (%s, %s, %s) @ 0x%x (eip 0x%x)\n",
		CHECK_FLAG(regs->error, 4) ? "user" : "supervisor",
		CHECK_FLAG(regs->error, 2) ? "write" : "read",
		CHECK_FLAG(regs->error, 1) ? "protection" : "notpresent",
		fault_addr, regs->eip);

	/* set up an identity mapping if it's in the first 4 MiB. */
	if(fault_addr < 1024 * 4096) {
		int dir = fault_addr >> 22, p = (fault_addr >> 12) & 0xfff;
		page_t *pages = (page_t *)(kernel_pdirs[dir] & ~0xfff);
		pages[p] = (p << 12) | PT_PRESENT | PT_RW;
#if 1
		/* valid since the 80486. */
		__asm__ volatile("invlpg %0"::"m" (*(char *)fault_addr): "memory");
#else
		__asm__ __volatile__(
			"\tmovl %%cr3, %%eax\n"
			"\tmovl %%eax, %%cr3\n"
			::: "eax", "memory");
#endif
	} else {
		printf("unable to handle. halting.\n");
		asm("cli; hlt");
	}
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
		}
	}

	printf("setting up interrupts...\n");
	setup_idt();

	printf("setting up paging...\n");
	setup_paging();

#if 0
	static int zero;
	printf("divide by zero: %d\n", 777 / zero);
#endif

	/* handleable fault. */
	volatile char *memory = (char *)0x210000;
	memory[0] = 1;
	memory[1] = 2;

	/* somewhat less so. */
	*(char *)0xdeadbeef = 1;

	printf("slamming teh brakes now.\n");
}
