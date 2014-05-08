
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
#include <l4/kip.h>
#include <l4/kcp.h>

#include <ukernel/mm.h>
#include <ukernel/space.h>
#include <ukernel/cpu.h>
#include <ukernel/x86.h>
#include <ukernel/acpi.h>
#include <ukernel/interrupt.h>
#include <ukernel/timer.h>
#include <ukernel/ipc.h>
#include <ukernel/thread.h>
#include <ukernel/kth.h>
#include <ukernel/sched.h>
#include <ukernel/mapdb.h>
#include <ukernel/kip.h>
#include <ukernel/bug.h>
#include <ukernel/ktest.h>
#include <ukernel/misc.h>


struct tss kernel_tss;
struct space *sigma0_space = NULL;
struct thread *s0_pager = NULL;

struct pic_ops global_pic;

static struct list_head resv_page_list = LIST_HEAD_INIT(resv_page_list);
static struct page *next_dir_page = NULL;

/* system call & interrupt stack (x86 specific). */
uint8_t syscall_stack[PAGE_SIZE] PAGE_ALIGN;
uint8_t kcp_copy[PAGE_SIZE] PAGE_ALIGN;

uint64_t global_timer_count = 0;
uint64_t *systemclock_p = NULL;


void NORETURN panic(const char *message)
{
	printf("PANIC: %s\n", message);
	while(true) {
		asm("cli; hlt");
	}
}


void NORETURN __not_reached(const char *file, int line, const char *func)
{
	printf("NOT_REACHED: %s:%d: function `%s'\n", file, line, func);
	panic("NOT_REACHED");
}


void abort(void)
{
	/* NOTE: this may crap out if the stack is fucked. the net effect is the
	 * same.
	 */
	printf("abort(3) called from %p via %p\n",
		__builtin_return_address(0), __builtin_return_address(1));
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


static inline uint64_t read_64bit_timer(void *ptr)
{
	volatile uint32_t *half = ptr;
	uint32_t high = half[1], low = half[0];
	if(high != half[1]) return read_64bit_timer(ptr);
	return (uint64_t)high << 32 | low;
}


uint64_t read_global_timer(void) {
	return read_64bit_timer(&global_timer_count);
}


uint64_t ksystemclock(void) {
	return read_64bit_timer(systemclock_p);
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
			printf("reflecting directory for %#x to %#x\n",
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
		"movl %%eax, %%cr3\n"
		"movl %%cr0, %%eax\n"
		"orl $0x80000000, %%eax\n"
		"movl %%eax, %%cr0\n"
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


static void add_s0_pages(L4_Word_t start, L4_Word_t end)
{
	assert(sigma0_space != NULL);

	uint32_t ids[128];
	int count = (end - start + 1) >> PAGE_BITS, done = 0;
	printf("adding %#lx .. %#lx to sigma0 (%d pages)\n", start, end, count);
	while(done < count) {
		int seg = MIN(int, 128, count - done);
		for(int i=0; i < seg; i++) ids[i] = (start >> PAGE_BITS) + done + i;
		mapdb_init_range(&sigma0_space->mapdb, start + (done << PAGE_BITS),
			ids, seg, L4_FullyAccessible);
		done += seg;
	}
}


/* the algorithm is intentionally simple. there'll only be a single range
 * that's reserved by the kernel, and that is covered entirely by
 * [excl_start .. excl_end].
 */
static void add_mem_to_sigma0(
	const L4_KernelConfigurationPage_t *kcp,
	L4_Word_t excl_start,
	L4_Word_t excl_end)
{
	printf("%s: excl_start %#lx, excl_end %#lx\n", __func__,
		excl_start, excl_end);
	const L4_MemoryDesc_t *mds = (void *)kcp + kcp->MemoryInfo.MemDescPtr;
	int md_count = kcp->MemoryInfo.n;
	for(int i=0; i < md_count; i++) {
		L4_Word_t start = L4_MemoryDescLow(&mds[i]),
			end = L4_MemoryDescHigh(&mds[i]);
		if(L4_MemoryDescType(&mds[i]) != L4_ConventionalMemoryType
			|| L4_IsMemoryDescVirtual(&mds[i])
			|| end - start + 1 < PAGE_SIZE)
		{
			continue;
		}

		if(start > excl_end || end < excl_start) {
			add_s0_pages(start, end);
		} else if(start >= excl_start && end <= excl_end) {
			/* skip entirely */
		} else if(start >= excl_start) {
			add_s0_pages(excl_end + 1, end);
		} else if(end <= excl_end) {
			add_s0_pages(start, excl_start - 1);
		} else {
			/* brute force. */
			for(L4_Word_t p = start; p < end; p += PAGE_SIZE) {
				if(p < excl_start || p > excl_end) {
					add_s0_pages(p, p + PAGE_SIZE - 1);
				}
			}
		}
	}
}


static bool try_selftest(L4_ThreadId_t from, L4_MsgTag_t tag)
{
#ifdef ENABLE_SELFTEST
	if(tag.X.label == 0x5374) {		/* "St" */
		struct thread *t = thread_find(from.raw);
		assert(t != NULL);
		if(CHECK_FLAG(t->space->flags, SF_PRIVILEGE)) {
			if(L4_UntypedWords(tag) > 0) describe_all_tests();
			run_all_tests();
			return true;
		}
	}
#endif
	return false;
}


static void pager_thread(void *parameter)
{
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
				printf("%s: pagefault from %lu:%lu at %#lx (ip %#lx)\n",
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
				L4_VREG(utcb, L4_TCR_MR(0)) = 0;
			} else if(!try_selftest(from, tag)) {
				printf("%s: unknown IPC label %#lx (u %lu, t %lu) from %lu:%lu\n",
					__func__, (L4_Word_t)tag.X.label, (L4_Word_t)tag.X.u,
					(L4_Word_t)tag.X.t, TID_THREADNUM(from.raw), TID_VERSION(from.raw));
				break;
			}

			/* ReplyWait */
			from = L4_anythread;
			tag = kipc(sender, &from, L4_Timeouts(L4_ZeroTime, L4_Never));
		}
	}
}


/* NOTE: the resulting first thread is not started. */
static struct thread *spawn_kernel_server(
	L4_Word_t thread_id,
	const L4_KernelRootServer_t *s0,
	struct thread *pager,
	int utcb_size_log2,
	bool premap)	/* may only be true while sigma0_space == NULL */
{
	assert(utcb_size_log2 >= PAGE_BITS);

	struct thread *t = thread_new(thread_id);
	struct space *sp = space_new();
	/* position the UTCB area to fall within the kernel's reservation. */
	extern char _start;
	L4_Word_t align = (1 << utcb_size_log2) - 1,
		first_kernel_page = ((L4_Word_t)&_start + align) & ~align;
	space_set_utcb_area(sp, L4_FpageLog2(first_kernel_page, utcb_size_log2));
	/* set the KIP where it is physically to avoid overlap with idempotent
	 * mappings.
	 */
	space_set_kip_area(sp, L4_FpageLog2((L4_Word_t)kip_mem, PAGE_BITS));
	t->space = sp;
	assert(t->utcb_pos < 0 && t->utcb_page == NULL);
	bool ok = thread_set_utcb(t, L4_Address(sp->utcb_area));
	BUG_ON(!ok, "thread_set_utcb() failed");
	t->ts_len = L4_Never;
	void *u_base = thread_get_utcb(t);
	L4_VREG(u_base, L4_TCR_PAGER) = pager->id;

	if(premap) {
		/* create idempotent mappings of kernel server memory. */
		assert((s0->low & PAGE_MASK) == 0);
		assert((s0->high & PAGE_MASK) == 0xfff);
		int num_pages = (s0->high - s0->low + 1) >> PAGE_BITS;
		uint32_t *ids = malloc(sizeof(uint32_t) * num_pages);
		for(int i=0; i < num_pages; i++) {
			ids[i] = (s0->low + i * PAGE_SIZE) >> PAGE_BITS;
		}
		mapdb_init_range(&t->space->mapdb, s0->low, ids, num_pages,
			L4_FullyAccessible);
		free(ids);
	}

	thread_set_spip(t, s0->sp, s0->ip);

	return t;
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
	printf("KCP found at %p (copy at %p)\n", kcp_base, &kcp_copy[0]);
	memcpy(kcp_copy, kcp_base, PAGE_SIZE);
	kcp_base = &kcp_copy[0];

	uintptr_t resv_start = ~0ul, resv_end = 0;
	init_kernel_heap(kcp_base, &resv_start, &resv_end);

	scan_cpuid();
	if(!CHECK_FLAG(get_features()->edx, 1)) panic("math is hard!");

	/* initialize CPU fundamentals and interrupt controllers etc. with the I
	 * bit cleared.
	 */
	if(x86_irq_is_enabled()) x86_irq_disable();

	asm volatile ("lldt %%ax" :: "a" (0));
	init_kernel_tss(&kernel_tss);

	setup_gdt();
	setup_idt(SEG_KERNEL_CODE, 15);		/* XT-PIC, for now */
	init_irq();

	/* set MP. clear EMulation, NoExceptions, TaskSwitch. */
	x86_alter_cr0(~(X86_CR0_EM | X86_CR0_NE | X86_CR0_TS), X86_CR0_MP);
	x86_init_fpu();

	printf("setting up paging (kernel ram [%#lx..%#lx])\n",
		(L4_Word_t)resv_start, (L4_Word_t)resv_end);
	struct list_head ksp_resv = LIST_HEAD_INIT(ksp_resv);
	init_spaces(&ksp_resv);
	setup_paging(resv_start, resv_end);

	/* (see comment for init_spaces().) */
	int n = mapdb_init(&kernel_space->mapdb);
	if(n < 0) panic("mapdb_init() for the kernel space failed");

	space_finalize_kernel(kernel_space, &ksp_resv);
	list_head_init(&ksp_resv);

	/* NOTE: malloc(), free(), etc. are only available from this line down. */

	const L4_KernelConfigurationPage_t *kcp = kcp_base;
	L4_KernelRootServer_t s0_mod = kcp->sigma0,
		roottask_mod = kcp->root_server;

	n = acpi_init();
	if(n < 0) {
		printf("ACPI initialization failed!\n");
		panic("mung doesn't work without ACPI anymore. bawwww");
	}

#ifdef DISABLE_APIC
	apic_disable_opt = true;
#endif

	printf("enabling interrupt controllers...\n");
	int max_irq = -1;
	if(apic_probe() < 0 || (max_irq = ioapic_init(&global_pic)) < 0) {
		printf("IOAPIC not found, or disabled; falling back to XT-PIC\n");
		if((max_irq = xtpic_init(&global_pic)) < 0) {
			panic("could not initialize IOAPIC or XT-PIC!");
		}
	} else {
		/* the bright new world of interrupt routing hardware! yaaaay */
		assert(apic_enabled);
		xtpic_disable();
	}
	assert(max_irq >= 0);

	printf("re-enabling interrupt processing...\n");
	x86_irq_enable();

	/* initialize KIP & vaguely related bits */
	kip_mem = kcp_base;
	assert(kcp_base == (void *)&kcp_copy[0]);
	make_kip(kip_mem, resv_start & ~PAGE_MASK, resv_end | PAGE_MASK, max_irq,
		L4_TimePeriod(1000));
	systemclock_p = kip_mem + PAGE_SIZE - sizeof(uint64_t);
	*systemclock_p = 0;
	global_timer_count = 0;
	kcp_base = (void *)0xdeadbeef;

	/* move the kernel to a high linear address, and change its segments so
	 * that it sees itself at a low address.
	 */
	x86_irq_disable();

	go_high();
	setup_gdt();
	setup_idt(SEG_KERNEL_CODE_HIGH, max_irq);

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

	cop_init();
	init_threading();
	struct thread *first_thread = kth_init(
		L4_GlobalId(last_int_threadno() + 1, 1));
	init_sched(first_thread);

	/* this creates a pager for the s0, s1 processes. it isn't necessary, but
	 * in development it functions as a printf() output path and for catching
	 * segfaults, so that's OK.
	 */
	int next_user_tno = first_user_threadno();
	s0_pager = kth_start(&pager_thread, NULL);
	struct thread *s0_thread = spawn_kernel_server(
		THREAD_ID(next_user_tno++, 1), &s0_mod, s0_pager,
		PAGE_BITS, true);
	sigma0_space = s0_thread->space;
	/* (burn one TID for sigma1, which is missing from the spec.) */
	next_user_tno++;
	struct thread *roottask = NULL;
	if(roottask_mod.high >= PAGE_SIZE) {
		roottask = spawn_kernel_server(THREAD_ID(next_user_tno++, 1),
			&roottask_mod, s0_thread, 16, false);
		roottask->space->flags |= SF_PRIVILEGE;
		printf("roottask [%#lx .. %#lx] created as %lu:%lu.\n",
			roottask_mod.low, roottask_mod.high,
			TID_THREADNUM(roottask->id), TID_VERSION(roottask->id));
	}

	/* FIXME: pass KernelInterfacePage_t pointer instead. for now the KIP
	 * passes for the KCP.
	 */
	add_mem_to_sigma0(kip_mem, resv_start & ~PAGE_MASK, resv_end | PAGE_MASK);
	if(!space_add_ioperm(s0_thread->space, 0, 65536)) {
		panic("can't create sigma0 I/O bitmap");
	}
	thread_start(s0_thread);
	if(roottask != NULL) thread_start(roottask);

	first_thread->pri = 0;
	first_thread->sens_pri = 0;
	first_thread->total_quantum = 0;
	first_thread->quantum = 10000;
	first_thread->ts_len = L4_TimePeriod(10000);
	sq_update_thread(first_thread);

	printf("enabling timer interrupt\n");
	x86_irq_disable();
	setup_timer_ch0();
	if(apic_enabled) ioapic_route_legacy_irq(0, 0x20);
	else (*global_pic.unmask_irq)(0, false, false);	/* act-low, edge */
	x86_irq_enable();

	printf("entering kernel scheduler\n");
	scheduler_loop(first_thread);

	panic("kmain() returned from halt-schedule loop? what.");
}
