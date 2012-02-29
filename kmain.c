
#include <stdio.h>
#include <stdbool.h>
#include <assert.h>
#include <ccan/likely/likely.h>
#include <ccan/compiler/compiler.h>
#include <ccan/list/list.h>

#include <ukernel/mm.h>
#include <ukernel/space.h>
#include <ukernel/x86.h>
#include <ukernel/interrupt.h>
#include <ukernel/16550.h>
#include <ukernel/timer.h>
#include <ukernel/thread.h>
#include <ukernel/misc.h>

#include "multiboot.h"


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


/* keyboard variables. these are just for testing the PIC setup, and will be
 * replaced with the timer chip's variables soon enough.
 *
 * what does a microkernel do with a keyboard, anyhow???
 */
#define KBD_STATUS_REG	0x64	/* read */
#define KBD_CTL_REG		0x64	/* write */
#define KBD_DATA_REG	0x60	/* read */
#define KBD_CMD_REG		0x60	/* write */

#define KBD_STAT_OBF	0x01	/* output buffer full */

#define KBD_CMD_WRITECMD	0x60	/* write keyboard command byte */
#define KBD_CMD_MOU_ENABLE	0xa7	/* enable mouse */
#define KBD_CMD_MOU_DISABLE	0xa8	/* disable mouse */
#define KBD_CMD_MOU_TEST	0xa9	/* test mouse. results come back on pt 0x60 */

#define KBD_KBF_KEYINTR		0x01	/* 1 = enable, 0 = disable */
#define KBD_KBF_MOUINTR		0x02	/* 1 = enable, 0 = disable */
#define KBD_KBF_SYSFLG		0x04	/* system flag (1 = selftest ok, 0 = fail) */
#define KBD_KBF_INHIB_OVER	0x08	/* PC/AT inhibit override (must be 0) */
#define KBD_KBF_KEY_DISABLE	0x10	/* 1 = disable, 0 = no change */
#define KBD_KBF_KEY_ENABLE	0x20	/* 1 = enable, 0 = no change */
#define KBD_KBF_MOU_ENABLE	0x40	/* 1 = enable, 0 = no change */
#define KBD_KBF_PCCOMPAT	0x80	/* PC compat crap, must be 0 */


struct tss kernel_tss;

static struct list_head resv_page_list = LIST_HEAD_INIT(resv_page_list);
static struct page *next_dir_page = NULL;

static uint8_t syscall_stack[4096] PAGE_ALIGN;
static uint32_t irq_pending[16];
static void *irq_stack[16];

/* should only be read with interrupts disabled! */
static uint64_t timer_count = 0;


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


void NORETURN panic(const char *message)
{
	printf("PANIC: %s\n", message);
	while(true) {
		asm("cli; hlt");
	}
}


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


static int __attribute__((pure)) list_length(struct list_head *list)
{
	int count = 0;
	for(struct list_node *n = resv_page_list.n.next;
		n != &resv_page_list.n;
		n = n->next)
	{
		count++;
	}
	return count;
}


static struct page *find_page_by_id(uint32_t id)
{
	/* TODO: use a more efficient mapping than a search over a linked
	 * list...
	 */
	struct page *page;
	list_for_each(&resv_page_list, page, link) {
		if(page->id == id) return page;
	}
	return NULL;
}


void put_supervisor_page(intptr_t addr, uint32_t page_id)
{
	assert(kernel_space != NULL);

	intptr_t l_addr = is_kernel_high ? addr + KERNEL_SEG_START : addr;

	pdir_t *kernel_pdirs = kernel_space->pdirs->vm_addr;
	pdir_t *dir = &kernel_pdirs[l_addr >> 22];
	page_t *pages;
	bool alloc_next = false;
	if(unlikely(!CHECK_FLAG(*dir, PDIR_PRESENT))) {
		if(next_dir_page == NULL) {
			panic("terrible recursion in put_supervisor_page()!");
		}
		struct page *pg = next_dir_page;
		next_dir_page = NULL;
		alloc_next = true;
		pages = pg->vm_addr;
		for(int i = 0; i < 1024; i++) pages[i] = 0;
		*dir = (pdir_t)(pg->id << 12) | PDIR_PRESENT | PDIR_RW;

		/* reflect directory allocation in the kernel's high segment */
		if(unlikely(!is_kernel_high) && addr < KERNEL_SEG_SIZE) {
			printf("reflecting directory for 0x%x to 0x%x\n",
				(unsigned)addr, (unsigned)addr + KERNEL_SEG_START);
			pdir_t *high = &kernel_pdirs[(addr + KERNEL_SEG_START) >> 22];
			*high = *dir;
		}
	} else {
		struct page *dir_page = find_page_by_id(*dir >> PAGE_BITS);
		if(dir_page == NULL) panic("directory page not found!");
		pages = dir_page->vm_addr;
	}

	int poffs = (l_addr >> 12) & 0x3ff;
	pages[poffs] = (page_id << PAGE_BITS) | PT_PRESENT | PT_RW;

	x86_invalidate_page(l_addr);
	if(unlikely(!is_kernel_high) && addr < KERNEL_SEG_SIZE) {
		/* invalidate the reflected page as well. */
		x86_invalidate_page(addr + KERNEL_SEG_START);
	}

	if(alloc_next) {
		next_dir_page = get_kern_page();
		list_add(&resv_page_list, &next_dir_page->link);
	}
}


static void setup_paging(intptr_t id_start, intptr_t id_end)
{
	assert(kernel_space != NULL);

	next_dir_page = get_kern_page();
	list_add(&resv_page_list, &next_dir_page->link);

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
		: "a" (kernel_space->pdirs->vm_addr)
		: "memory");
}


static void init_kernel_tss(struct tss *t)
{
	assert(sizeof(struct tss) == 104);

	intptr_t stk = (intptr_t)&syscall_stack[0];
	stk += sizeof(syscall_stack) - 16;
	stk &= ~15ul;

	*t = (struct tss){
		.ss0 = SEG_KERNEL_DATA << 3,
		.esp0 = stk,
		.iopb_offset = sizeof(struct tss),
	};
}


static inline void irq_disable(void) {
	asm volatile ("cli" ::: "memory");
}


static inline void irq_enable(void) {
	asm volatile ("sti" ::: "memory");
}


void isr_exn_de_bottom(struct x86_exregs *regs)
{
	printf("#DE(0x%x) at eip 0x%x, esp 0x%x\n", regs->error,
		regs->eip, regs->esp);
	panic("#DE");
}


void isr_exn_gp_bottom(struct x86_exregs *regs)
{
	printf("#GP(0x%x) at eip 0x%x, esp 0x%x\n", regs->error,
		regs->eip, regs->esp);
	asm("cli; hlt");
}


void isr_exn_pf_bottom(struct x86_exregs *regs)
{
	uint32_t fault_addr;
	__asm__ __volatile__("\tmovl %%cr2, %0\n" : "=r" (fault_addr)
		:: "memory");

	printf("pf (%s, %s, %s) @ 0x%x (eip 0x%x)\n",
		CHECK_FLAG(regs->error, 4) ? "user" : "supervisor",
		CHECK_FLAG(regs->error, 2) ? "write" : "read",
		CHECK_FLAG(regs->error, 1) ? "protection" : "notpresent",
		fault_addr, regs->eip);

#if 0
	unsigned char *videoram = (unsigned char *)0xb8000;
	videoram[0] = 'Z';
	videoram[1] = 0x07;		/* light grey (7) on black (0). */
#endif

	/* set up an identity mapping if it's in the first 4 MiB. */
	int dir = fault_addr >> 22, p = (fault_addr >> 12) & 0x3ff;
	pdir_t *kernel_pdirs = kernel_space->pdirs->vm_addr;
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
			printf("allocated physical page 0x%x for fault in kernel memory at 0x%x\n",
				(unsigned)pg->id << PAGE_BITS, fault_addr);
		}
	} else {
		printf("unable to handle. halting.\n");
		asm("cli; hlt");
	}

	x86_invalidate_page(fault_addr);
}


static void pump_keyboard(void)
{
	for(;;) {
		uint8_t status = inb(KBD_STATUS_REG);
		if(!CHECK_FLAG(status, KBD_STAT_OBF)) break;

		uint8_t byte = inb(KBD_DATA_REG);
		if(byte == 0x39) {
			printf("spacebar was pressed. timer count %d\n",
				(int)timer_count);
		}
	}
}


static void isr_irq_bottom_soft(int irq)
{
	assert(irq >= 0 && irq <= 15);
	if(irq == 1) {
		pump_keyboard();
	} else {
		printf("got unexpected interrupt 0x%x\n", (unsigned)irq);
	}
}


static void isr_irq_bottom_wrap(void *parameter)
{
	volatile struct x86_exregs *regs = parameter;
	const int irq = regs->reason - 0x20;
	irq_enable();
	isr_irq_bottom_soft(irq);
	irq_disable();
	if(unlikely(irq_pending[irq] > 1)) {
		irq_pending[irq] = 1;
		isr_irq_bottom_wrap(parameter);
	}
}


static inline void call_on_stack(void (*fn)(void *), void *stack)
{
	/* this trick relies on ebx being a callee-saved register. */
	asm volatile (
		"\txchgl %%ebx, %%esp\n"
		"\tcall *%%edi\n"
		"\tmovl %%ebx, %%esp\n"
		: "=b" (stack)
		: "0" (stack), "D" (fn)
		: "memory", "cc", "edx", "ecx", "eax");
}


static bool irq_switched_stack(void)
{
	char foo = 0;
	intptr_t pos = (intptr_t)&foo;
	return (pos & ~PAGE_MASK) == (kernel_tss.esp0 & ~PAGE_MASK);
}


void isr_irq_bottom(struct x86_exregs *regs)
{
	const int vecn = regs->reason, irq = vecn - 0x20;

#if 0
	printf("%s: frame at 0x%x, syscall_stack at 0x%x\n", __func__,
		(uintptr_t)regs, (uintptr_t)&syscall_stack[0]);
#endif

	pic_send_eoi(irq);
	if(unlikely(irq_pending[irq]++ > 0)) return;
	if(likely(irq_switched_stack())) {
		/* stack was switched. install the IRQ stack in the TSS segment and
		 * proceed.
		 */
		intptr_t old_esp0 = kernel_tss.esp0;
		kernel_tss.esp0 = (intptr_t)irq_stack[irq];
		assert(kernel_tss.esp0 != old_esp0);
		isr_irq_bottom_wrap(regs);
		kernel_tss.esp0 = old_esp0;
	} else {
		/* was called on a kernel stack. call handler on IRQ stack. */
		void *stk_top = irq_stack[irq] + PAGE_SIZE;
		stk_top -= sizeof(struct x86_exregs *);
		*(struct x86_exregs **)stk_top = regs;
		call_on_stack(&isr_irq_bottom_wrap, stk_top);
	}

	assert(irq_pending[irq] == 1);
	irq_pending[irq] = 0;
}


/* the timer interrupt. runs with interrupts disabled by design. */
void isr_irq0_bottom(void)
{
	timer_count++;
	pic_send_eoi(0);
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


static void test_thread(void *ptr)
{
	int x = 666;
	printf("test thread active! local variable at 0x%x.\n", (unsigned)&x);

	yield(NULL);

	printf("test thread was returned into. ptr is `%s'\n",
		(const char *)ptr);
}


void thread_test(void)
{
	struct thread *other = create_kthread(THREAD_ID(18, 1),
		&test_thread, "hello, thread!");
	space_add_thread(kernel_space, other);
	printf("other thread created. switching...\n");
	yield(other);
	printf("back in old thread. boinking yield...\n");
	for(int i=0; i < 8; i++) yield(NULL);
	printf("thread_test() ends.\n");
}


void space_test(void)
{
	struct thread *t = thread_new(THREAD_ID(128, 1));
	struct space *sp = space_new();
	thread_set_space(t, sp);
	thread_set_spip(t, 0xcafeb00b, 0xc0def000);
	thread_start(t);
}


void malloc_panic(void) {
	panic("malloc failure!");
}


static void crawl_multiboot_info(
	struct multiboot_info *mbi,
	intptr_t *resv_start_p,
	intptr_t *resv_end_p)
{
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
				init_kernel_heap(mm, resv_start_p, resv_end_p);
			}
		}
	}
	if(!found_mem) {
		panic("didn't find any memory in multiboot spec!");
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
	intptr_t resv_start = 0, resv_end = 0;
	crawl_multiboot_info(mbd, &resv_start, &resv_end);

	/* initialize interrupt-related data structures with the I bit cleared. */
	irq_disable();

	asm volatile ("lldt %%ax" :: "a" (0));
	init_kernel_tss(&kernel_tss);

#if 0
	printf("dumping gdt...\n");
	struct gdt_desc gd;
	asm volatile ("sgdt %0" : "=m" (gd));
	dump_gdt(&gd);
#endif

	setup_gdt();
	setup_idt(SEG_KERNEL_CODE);

	/* map olde-timey PC interrupts 0-15 to 0x20 .. 0x2f inclusive */
	printf("initializing PIC...\n");
	initialize_pics(0x20, 0x28);
	pic_set_mask(0xff, 0xff);	/* mask them all off for now. */

	/* allocate IRQ stacks. (TODO: don't allocate for IRQs that are nulled out
	 * in the IDT.)
	 */
	for(int i=0; i < 16; i++) {
		irq_pending[i] = 0;

		struct page *p = get_kern_page();
		/* TODO: record it as reserved */
		irq_stack[i] = p->vm_addr;
	}

	irq_enable();

	printf("setting up paging (id maps between 0x%x and 0x%x)...\n",
		(unsigned)resv_start, (unsigned)resv_end);
	struct list_head ksp_resv = LIST_HEAD_INIT(ksp_resv);
	init_spaces(&ksp_resv);
	setup_paging(resv_start, resv_end);
	printf("adding identity maps for MBI memory...\n");
	/* NOTE: this is a big hack: generally the MBI info fits in less than 4k
	 * of memory. FIXME: it should also take the MBI page out of circulation
	 * until it's no longer required.
	 */
	put_supervisor_page((intptr_t)mbi & ~PAGE_MASK, (intptr_t)mbi >> PAGE_BITS);

	if(CHECK_FLAG(mbi->flags, MULTIBOOT_INFO_MEM_MAP)) {
		printf("adding MultiBoot memory...\n");
		add_mbi_memory(mbi, resv_start & ~PAGE_MASK, resv_end | PAGE_MASK);
	}

	space_add_resv_pages(kernel_space, &ksp_resv);
	list_head_init(&ksp_resv);

	printf("so far, the kernel reserves %d pages (%u KiB) of memory.\n",
		list_length(&resv_page_list), list_length(&resv_page_list) * PAGE_SIZE / 1024);

	/* move the kernel to a high linear address, and change its segments so
	 * that it sees itself at a low address.
	 */
	irq_disable();

	go_high();
	setup_gdt();
	setup_idt(SEG_KERNEL_CODE_HIGH);

	/* then unmap the low space. */
	int last_resv_dir = (resv_end + (1 << 22) - 1) >> 22;
	pdir_t *kernel_pdirs = kernel_space->pdirs->vm_addr;
	assert(kernel_pdirs[last_resv_dir + 1] == 0);
	for(int i=0; i <= last_resv_dir; i++) {
		kernel_pdirs[i] = 0;
	}
	x86_flush_tlbs();

	irq_enable();

#if 0
	static int zero;
	printf("divide by zero: %d\n", 777 / zero);
#endif

	printf("performing malloc test...\n");
	char *foo = malloc(64);
	foo[0] = 'q';
	foo[1] = 'w';
	foo[2] = 'e';
	foo[3] = '\0';
	printf("string at 0x%x should say `qwe': `%s'\n", (unsigned)foo, foo);
	free(foo);

	printf("enabling keyboard & keyboard interrupt\n");
	outb(KBD_CMD_REG, KBD_CMD_WRITECMD);
	outb(KBD_CMD_REG, KBD_KBF_KEYINTR | KBD_KBF_KEY_ENABLE);
	pump_keyboard();
	pic_clear_mask(0x02, 0x00);

	printf("enabling timer interrupt\n");
	setup_timer_ch0();
	pic_clear_mask(0x01, 0x00);

	struct thread *first_thread = init_threading(THREAD_ID(17, 1));
	space_add_thread(kernel_space, first_thread);
	thread_test();

	space_test();

	printf("kmain() entering halt-schedule loop.\n");
	while(true) {
		if(!schedule()) {
			asm volatile ("hlt");
		}
	}
}
