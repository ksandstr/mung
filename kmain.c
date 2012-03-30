
#include <stdio.h>
#include <stdbool.h>
#include <assert.h>
#include <ccan/likely/likely.h>
#include <ccan/compiler/compiler.h>
#include <ccan/list/list.h>

#include <l4/types.h>
#include <l4/ipc.h>
#include <l4/message.h>
#include <l4/vregs.h>

#include <ukernel/mm.h>
#include <ukernel/space.h>
#include <ukernel/x86.h>
#include <ukernel/interrupt.h>
#include <ukernel/16550.h>
#include <ukernel/timer.h>
#include <ukernel/ipc.h>
#include <ukernel/thread.h>
#include <ukernel/mapdb.h>
#include <ukernel/misc.h>


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
struct space *sigma0_space = NULL;
struct page *kip_page = NULL;

static struct list_head resv_page_list = LIST_HEAD_INIT(resv_page_list);
static struct page *next_dir_page = NULL;

uint8_t syscall_stack[4096] PAGE_ALIGN;

uint64_t *global_timer_count = NULL;


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


void abort(void)
{
	/* NOTE: this may crap out if the stack is fucked. the net effect is the
	 * same.
	 */
	printf("abort(3) called from 0x%x via 0x%x\n",
		(unsigned)__builtin_return_address(0),
		(unsigned)__builtin_return_address(1));
	panic("aborted");
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
	for(struct list_node *n = list->n.next; n != &list->n; n = n->next) {
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


uint64_t read_global_timer(void)
{
	x86_irq_disable();
	uint64_t value = *global_timer_count;
	x86_irq_enable();
	return value;
}


void put_supervisor_page(uintptr_t addr, uint32_t page_id)
{
	assert(kernel_space != NULL);

	uintptr_t l_addr = is_kernel_high ? addr + KERNEL_SEG_START : addr;
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
	if(page_id == 0) {
		pages[poffs] = 0;
	} else {
		pages[poffs] = (page_id << PAGE_BITS) | PT_PRESENT | PT_RW;
	}

	x86_invalidate_page(l_addr);
	if(unlikely(!is_kernel_high) && addr < KERNEL_SEG_SIZE) {
		/* invalidate the reflected page as well. */
		x86_invalidate_page(addr + KERNEL_SEG_START);
	}

	if(alloc_next) {
		next_dir_page = get_kern_page(0);
		list_add(&resv_page_list, &next_dir_page->link);
	}
}


static void setup_paging(uintptr_t id_start, uintptr_t id_end)
{
	assert(kernel_space != NULL);

	next_dir_page = get_kern_page(0);
	list_add(&resv_page_list, &next_dir_page->link);

	/* identitymap between id_start and id_end inclusive */
	id_start &= ~PAGE_MASK;
	id_end = (id_end + PAGE_SIZE - 1) & ~PAGE_MASK;
	for(uintptr_t addr = id_start; addr < id_end; addr += PAGE_SIZE) {
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


void pump_keyboard(void)
{
	for(;;) {
		uint8_t status = inb(KBD_STATUS_REG);
		if(!CHECK_FLAG(status, KBD_STAT_OBF)) break;

		uint8_t byte = inb(KBD_DATA_REG);
		if(byte == 0x39) {
			printf("spacebar was pressed. timer count %d\n",
				(int)read_global_timer());
		}
	}
}


static void add_kcp_memory(
	void *kcp_base,
	intptr_t excl_start,
	intptr_t excl_end)
{
	printf("%s: excl_start 0x%x, excl_end 0x%x\n", __func__,
		(unsigned)excl_start, (unsigned)excl_end);
	/* FIXME: write accessors for these. */
	L4_Word_t *md_base = kcp_base + (*(L4_Word_t *)(kcp_base + 0x54) >> 16);
	int md_count = *(L4_Word_t *)(kcp_base + 0x54) & 0xffff;
	for(int i=0; i < md_count; i++) {
		L4_Word_t low = md_base[i*2 + 0], high = md_base[i * 2 + 1];
		int type = low & 0xf; //, t = (low >> 4) & 0xf;
		bool virtual = CHECK_FLAG(low, 0x200);
		low &= ~0x3ff;
		high &= ~0x3ff;
		size_t size = (high - low + 1) / 1024;

		if(type != 1 || virtual || size < 4 || low < 0x100000) continue;

		const uintptr_t start = low, end = high;
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


static void pager_thread(void *parameter)
{
	printf("pager thread started.\n");

	for(;;) {
		L4_ThreadId_t from = L4_anythread;
		L4_MsgTag_t tag = kipc(L4_nilthread, &from,
			L4_Timeouts(L4_Never, L4_Never));

		for(;;) {
			if(L4_IpcFailed(tag)) {
				printf("pager ipc failed (no ec yet)\n");
				break;
			}

			L4_ThreadId_t sender = from;
			void *utcb = thread_get_utcb(get_current_thread());
			if((tag.X.label & 0xfff0) == 0xffe0) {
				L4_Word_t fault_addr = L4_VREG(utcb, L4_TCR_MR(1)),
					fault_ip = L4_VREG(utcb, L4_TCR_MR(2));
				printf("%s: pagefault from %d:%d at 0x%x (ip 0x%x)\n",
					__func__, TID_THREADNUM(from.raw), TID_VERSION(from.raw),
					fault_addr, fault_ip);

				/* ... sigma0 should never produce pagefaults, so we'll just
				 * leave it hanging.
				 */
				break;
			} else if(tag.X.label == 0x5370) {
				/* sigma0's con_putstr() protocol. */
				char buf[257];
				for(int i=0; i < tag.X.u; i++) {
					L4_Word_t val = L4_VREG(utcb, L4_TCR_MR(i + 1));
					memcpy(&buf[i * 4], &val, sizeof(L4_Word_t));
				}
				buf[tag.X.u * 4] = '\0';
				int len = strlen(buf);
				while(len > 0 && buf[len - 1] == '\n') buf[--len] = '\0';
				printf("[sigma0]: %s\n", buf);
			} else if(tag.X.label == 0x2369) {
				/* respond, to test out ReplyWait. */
				printf("%s: got test message at %llu, mr1 is %#x\n", __func__,
					read_global_timer(), L4_VREG(utcb, L4_TCR_MR(1)));
				L4_VREG(utcb, L4_TCR_MR(0)) = ((L4_MsgTag_t){ .X.u = 1 }).raw;
				L4_VREG(utcb, L4_TCR_MR(1)) = 0xc0def00d;
			} else {
				printf("%s: unknown IPC label 0x%x from %d:%d\n",
					__func__, tag.X.label, TID_THREADNUM(from.raw),
					TID_VERSION(from.raw));
				break;
			}

			/* ReplyWait */
			from = L4_anythread;
			tag = kipc(sender, &from, L4_Timeouts(L4_Never, L4_Never));
		}
	}
}


static void spawn_sigma0(
	L4_Word_t s0_start,
	L4_Word_t s0_end,
	L4_Word_t s0_sp,
	L4_Word_t s0_ip)
{
	/* sigma0 shouldn't really have a pager, but this avoids a special case in
	 * the fault handler and thus is quite OK.
	 */
	struct thread *pager = create_kthread(&pager_thread, NULL),
		*t = thread_new(THREAD_ID(128, 1));
	struct space *sp = space_new();
	sigma0_space = sp;
	space_set_utcb_area(sp, L4_FpageLog2(0x50000, 13));
	space_set_kip_area(sp, L4_FpageLog2(0x10000, 12));
	thread_set_space(t, sp);
	thread_set_utcb(t, L4_Address(sp->utcb_area));
	void *u_base = thread_get_utcb(t);
	L4_VREG(u_base, L4_TCR_PAGER) = pager->id;

	/* map sigma0's own pages. these will be idempotent, as all memory that
	 * sigma0 can map idempotently.
	 */
	assert((s0_start & PAGE_MASK) == 0);
	assert((s0_end & PAGE_MASK) == 0xfff);
	int num_pages = (s0_end - s0_start + 1) >> PAGE_BITS;
	uint32_t *ids = malloc(sizeof(uint32_t) * num_pages);
	for(int i=0; i < num_pages; i++) {
		ids[i] = (s0_start + i * PAGE_SIZE) >> PAGE_BITS;
	}
	mapdb_init_range(&sigma0_space->mapdb, s0_start, ids, num_pages,
		L4_FullyAccessible);
	free(ids);

	thread_set_spip(t, s0_sp, s0_ip);
	thread_start(t);
}


void malloc_panic(void) {
	panic("malloc failure!");
}


static void *find_kcp(size_t mem_after_1m)
{
	void *ptr = (void *)0x100000;
	for(size_t i=0; i < mem_after_1m; i++, ptr += PAGE_SIZE) {
		if(memcmp(ptr, "L4\346K", 4) == 0) return ptr;
	}

	return NULL;
}


/* entry point called from loader-32.S. first parameter is the "*P" pointer
 * per L4.X2 ia32 booting, second is 0x2BADB002.
 */
void kmain(void *bigp, unsigned int magic)
{
	if(magic != 0x2BADB002) {
		/* hang! */
		return;
	}

	/* also, output some stuff to the serial port. */
	printf("hello, world! P is at %p\n", bigp);

	size_t mem_before_640k = *(L4_Word_t *)(bigp + 0x04),
		mem_after_1m = *(L4_Word_t *)(bigp + 0x08);
	printf("%u KiB of memory below 640K; %u KiB after 1M\n",
		mem_before_640k, mem_after_1m);
	void *kcp_base = find_kcp(mem_after_1m);
	if(kcp_base == NULL) panic("cannot find kernel configuration page!");
	printf("KCP found at %p\n", kcp_base);

	/* TODO: instead of simply copying the KCP, it should be relocated to page
	 * 0 (or some such) and reused as the KIP.
	 */
	static uint8_t kcp_copy[PAGE_SIZE] PAGE_ALIGN;
	memcpy(kcp_copy, kcp_base, PAGE_SIZE);
	kcp_base = &kcp_copy[0];

	uintptr_t resv_start = ~0ul, resv_end = 0;
	init_kernel_heap(kcp_base, &resv_start, &resv_end);

	/* initialize interrupt-related data structures with the I bit cleared. */
	x86_irq_disable();

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

	init_irq();

	x86_irq_enable();

	printf("setting up paging (id maps between 0x%x and 0x%x)...\n",
		(unsigned)resv_start, (unsigned)resv_end);
	struct list_head ksp_resv = LIST_HEAD_INIT(ksp_resv);
	init_spaces(&ksp_resv);
	setup_paging(resv_start, resv_end);

	/* (see comment for init_spaces().) */
	mapdb_init(&kernel_space->mapdb, kernel_space);
	add_kcp_memory(kcp_base, resv_start & ~PAGE_MASK, resv_end | PAGE_MASK);

	space_add_resv_pages(kernel_space, &ksp_resv);
	list_head_init(&ksp_resv);

	kip_page = get_kern_page(0);
	list_add(&resv_page_list, &kip_page->link);
	make_kip(kip_page->vm_addr);
	global_timer_count = kip_page->vm_addr + PAGE_SIZE - sizeof(uint64_t);
	*global_timer_count = 0;
	printf("KIP on page id %d\n", kip_page->id);

	printf("so far, the kernel reserves %d pages (%u KiB) of memory.\n",
		list_length(&resv_page_list), list_length(&resv_page_list) * PAGE_SIZE / 1024);

	/* move the kernel to a high linear address, and change its segments so
	 * that it sees itself at a low address.
	 */
	x86_irq_disable();

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

	x86_irq_enable();

	/* per-module inits & init-time testing */
	init_gdt_resv();
	init_mapdb();
	init_ipc();

	struct thread *first_thread = init_threading(THREAD_ID(17, 1));
	space_add_thread(kernel_space, first_thread);

	spawn_sigma0(*(L4_Word_t *)(kcp_base + 0x28),
		*(L4_Word_t *)(kcp_base + 0x2c), *(L4_Word_t *)(kcp_base + 0x20),
		*(L4_Word_t *)(kcp_base + 0x24));

	printf("enabling keyboard & keyboard interrupt\n");
	outb(KBD_CMD_REG, KBD_CMD_WRITECMD);
	outb(KBD_CMD_REG, KBD_KBF_KEYINTR | KBD_KBF_KEY_ENABLE);
	pump_keyboard();
	pic_clear_mask(0x02, 0x00);

	printf("enabling timer interrupt\n");
	setup_timer_ch0();
	pic_clear_mask(0x01, 0x00);

	printf("kmain() entering halt-schedule loop.\n");
	while(true) {
		first_thread->status = TS_READY;
		if(!schedule()) {
			asm volatile ("hlt");
		}
	}
}
