
#include <stdio.h>
#include <stdbool.h>
#include <stdnoreturn.h>
#include <string.h>
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
#include <ukernel/sched.h>
#include <ukernel/mapdb.h>
#include <ukernel/memdesc.h>
#include <ukernel/kip.h>
#include <ukernel/bug.h>
#include <ukernel/trampoline.h>
#include <ukernel/ktest.h>
#include <ukernel/misc.h>


struct tss kernel_tss;
struct space *sigma0_space = NULL;

static struct list_head resv_page_list = LIST_HEAD_INIT(resv_page_list);
static struct page *next_dir_page = NULL;

/* system call & interrupt stack (x86 specific). */
void *syscall_stack = NULL;

uint8_t kcp_copy[PAGE_SIZE] PAGE_ALIGN;

uint64_t global_timer_count = 0;
uint64_t *systemclock_p = NULL;


void noreturn panic(const char *message)
{
	printf("PANIC: %s\n", message);
	while(true) {
		asm("cli; hlt");
	}
}


void noreturn __not_reached(const char *file, int line, const char *func)
{
	printf("NOT_REACHED: %s:%d: function `%s'\n", file, line, func);
	panic("a `NOT_REACHED' was reached");		/* owie */
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


noreturn void __assert_failure(
	const char *condition,
	const char *file, int line, const char *func)
{
	printf("assert(%s) failed in `%s' (%s:%u)\n", condition, func,
		file, line);
	panic("*** assertion failure");
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


/* TODO: unify with invlpg in ptab_32bit.c, one day */
static inline void reload_ptab(uintptr_t address)
{
	if(likely(is_kernel_high)) {
		__asm__ __volatile__ ("invlpg (%0)"
			:: "r" (address - KERNEL_SEG_START)
			: "memory");
	} else {
		x86_flush_tlbs();
	}
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
		/* NB. this code path is only here for callers from before
		 * init_spaces(), since pdirs are always present after that.
		 *
		 * really this function should serve pre-init modifications only and
		 * the others should use the pt_*() family. (TODO?)
		 */
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
		/* linear search over the reserved-pages list. this is inefficient and
		 * bad; we should instead use htable_get() into
		 * kernel_space->ptab_pages, but that's unavailable before
		 * space_finalize_kernel().
		 */
		bool found = false;
		struct page *p;
		list_for_each(&resv_page_list, p, link) {
			if(p->id == *dir >> PAGE_BITS) {
				found = true;
				break;
			}
		}
		if(unlikely(!found)) panic("directory page not found!");
		pages = p->vm_addr;
	}

	int poffs = (l_addr >> 12) & 0x3ff;
	if(page_id == 0) {
		pages[poffs] = 0;
	} else {
		pages[poffs] = (page_id << PAGE_BITS)
			| PT_PRESENT | PT_RW | PT_GLOBAL;
	}

	reload_ptab(l_addr);
	if(unlikely(!is_kernel_high) && addr < KERNEL_SEG_SIZE) {
		/* invalidate the reflected page as well. */
		reload_ptab(addr + KERNEL_SEG_START);
	}

	if(alloc_next) {
		next_dir_page = get_kern_page(0);
		list_add(&resv_page_list, &next_dir_page->link);
	}
}


static COLD void idmap_supervisor_page(struct page *p, void *priv) {
	put_supervisor_page(p->id << PAGE_BITS, p->id);
}


static COLD void setup_paging(uintptr_t id_start, uintptr_t id_end)
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

	/* also map the kernel initial pages. */
	heap_for_each_init_page(&idmap_supervisor_page, NULL);
	struct page *p;
	list_for_each(&resv_page_list, p, link) {
		idmap_supervisor_page(p, NULL);
	}

	/* load the supervisor page table (kernel_pdirs into CR3), then enable
	 * paging.
	 */
	asm volatile (
		"movl %%eax, %%cr3\n"
		"movl %%cr0, %%eax\n"
		"orl $0x80000000, %%eax\n"
		"movl %%eax, %%cr0\n"
		:
		: "a" (kernel_space->pdirs->vm_addr)
		: "memory");
}


static void setup_cr4(void)
{
	/* clear TSD (enable RDTSC in ring 3), set PGE (apply global bit to share
	 * TLBs for lowest-level pagetable entries)
	 */
	asm volatile (
		"movl %%cr4, %%eax\n"
		"andl $~0x4, %%eax\n"
		"orl $0x80, %%eax\n"
		"movl %%eax, %%cr4\n"
		::: "eax", "memory");
}


static void init_kernel_tss(struct tss *t)
{
	assert(sizeof(struct tss) == 104);

	int n = posix_memalign(&syscall_stack,
		KERNEL_STACK_SIZE, KERNEL_STACK_SIZE);
	if(n != 0) {
		printf("n=%d\n", n);
		panic("can't allocate syscall stack");
	}

	*t = (struct tss){
		.ss0 = SEG_KERNEL_DATA << 3,
		.esp0 = ((uintptr_t)syscall_stack + KERNEL_STACK_SIZE - 16) & ~0xful,
		.iopb_offset = sizeof(struct tss),
	};
}


static void add_id_maps(struct space *sp, L4_Word_t start, L4_Word_t end)
{
	assert(sp != NULL);
	end |= PAGE_SIZE - 1;

	L4_Word_t addr;
	int s;
	for_page_range(start, end + 1, addr, s) {
		int l = 0;
		do {
			L4_Word_t sub_addr = addr + l * GROUP_SIZE;
			L4_Fpage_t p = L4_FpageLog2(sub_addr,
				MIN(int, s, size_to_shift(GROUP_SIZE)));
			L4_Set_Rights(&p, L4_FullyAccessible);
			int n = mapdb_put(sp, p, sub_addr >> PAGE_BITS, false);
			if(n < 0) {
				printf("%s: n=%d!!!\n", __func__, n);
				panic("argh");
			}
		} while(++l < (1u << s) / GROUP_SIZE);
	}
}


/* adds all contiguous PAGE_SIZE chunks of conventional, shared,
 * bootloader-specified, and architecture-specific memory to sigma0.
 */
static void add_mem_to_sigma0(const L4_KernelInterfacePage_t *kip)
{
	static const L4_Word_t types[] = {
		L4_ConventionalMemoryType,
		L4_SharedMemoryType,
		L4_BootLoaderSpecificMemoryType,
		L4_ArchitectureSpecificMemoryType,
	};
	struct memdescbuf mdb = {
		.ptr = (void *)kip + kip->MemoryInfo.MemDescPtr,
		.len = kip->MemoryInfo.n, .size = kip->MemoryInfo.n,
	};

#ifdef DEBUG_ME_HARDER
	printf("KIP MemoryDesc dump:\n");
#endif
	int b_subs = 0, arch_subs = 0;
	for(int i=0; i < mdb.len; i++) {
#ifdef DEBUG_ME_HARDER
		size_t sz = L4_MemoryDescHigh(&mdb.ptr[i]) - L4_MemoryDescLow(&mdb.ptr[i]) + 1;
		char sizestr[20], suffix = 'B';
		if(sz > 1024 * 1024 * 1024 && (sz & 0xc0000000) == 0) {
			sz >>= 30; suffix = 'G';
		} else if(sz > 1024 * 1024) {
			sz >>= 20; suffix = 'M';
		} else if(sz > 1024) {
			sz >>= 10; suffix = 'K';
		}
		snprintf(sizestr, sizeof sizestr, "%lu%c", (unsigned long)sz, suffix);
		printf("i=%02d\t%s range=[%#lx, %#lx], type=%#lx, size=%s\n",
			i, L4_IsMemoryDescVirtual(&mdb.ptr[i]) ? "virt" : "phys",
			L4_MemoryDescLow(&mdb.ptr[i]), L4_MemoryDescHigh(&mdb.ptr[i]),
			L4_MemoryDescType(&mdb.ptr[i]), sizestr);
#endif
		L4_Word_t t = L4_MemoryDescType(&mdb.ptr[i]);
		if((t & 0xf) == L4_BootLoaderSpecificMemoryType) {
			b_subs |= 1 << (t >> 4);
		} else if((t & 0xf) == L4_ArchitectureSpecificMemoryType) {
			arch_subs |= 1 << (t >> 4);
		}
	}
	b_subs &= ~4;		/* don't assign MBI reserved ranges. */

	for(int i=0; i < NUM_ELEMENTS(types); i++) {
		L4_Word_t type = types[i], subs;
		switch(type) {
			case L4_BootLoaderSpecificMemoryType: subs = b_subs; break;
			case L4_ArchitectureSpecificMemoryType: subs = arch_subs; break;
			default: subs = 1;
		}
		while(subs != 0) {
			int subtype = ffsl(subs) - 1;
			assert(CHECK_FLAG(subs, 1 << subtype));
			subs &= ~(1 << subtype);
			L4_Word_t q_start = 0, q_end = ~(L4_Word_t)0;
			for(;;) {
				L4_Fpage_t part = mdb_query(&mdb, q_start, q_end,
					false, false, (type & 0xf) | (subtype << 4));
				if(L4_IsNilFpage(part)) break;
				q_start = FPAGE_HIGH(part) + 1;
				if(L4_SizeLog2(part) < 12) continue;
				/* NOTE: this form of rounding drops those 4k pages that have
				 * a sub-4k boundary in them.
				 */
				add_id_maps(sigma0_space,
					(FPAGE_LOW(part) + PAGE_SIZE - 1) & ~PAGE_MASK,
					((FPAGE_HIGH(part) + 1) & ~PAGE_MASK) - 1);
			}
		}
	}
}


/* NOTE: the resulting first thread is not started. */
static COLD struct thread *spawn_kernel_server(
	L4_Word_t thread_id,
	const L4_KernelRootServer_t *mod,
	struct thread *pager,
	int utcb_size_log2,
	bool premap)	/* may only be true while sigma0_space == NULL */
{
	assert(utcb_size_log2 >= PAGE_BITS);

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
	struct thread *t = thread_new(thread_id);
	t->space = sp;
	t->scheduler = L4_GlobalId(TID_THREADNUM(t->id), TID_VERSION(t->id));
	assert(t->utcb_pos < 0 && t->utcb_page == NULL);
	int n = thread_set_utcb(t, L4_Address(sp->utcb_area));
	BUG_ON(n != 0, "thread_set_utcb() failed, n=%d", n);
	t->ts_len = L4_Never;
	void *u_base = thread_get_utcb(t);
	if(pager != NULL) L4_VREG(u_base, L4_TCR_PAGER) = pager->id;

	if(premap) {
		/* create idempotent mappings of kernel server memory. */
		assert((mod->low & PAGE_MASK) == 0);
		assert((mod->high & PAGE_MASK) == 0xfff);
		add_id_maps(t->space, mod->low, mod->high);
	}

	thread_set_spip(t, mod->sp, mod->ip);

	return t;
}


void malloc_panic(void) {
	panic("malloc failure!");
}


static COLD void *find_kcp(size_t mem_after_1m)
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

	extern char _start, _end;
	L4_Word_t kern_start = (L4_Word_t)&_start & ~PAGE_MASK,
		kern_end = (L4_Word_t)&_end | PAGE_MASK;
	init_kernel_heap(kcp_base, kern_end + 1, mem_after_1m * 1024);

	scan_cpuid();
	if(!CHECK_FLAG(get_features()->edx, 1)) panic("math is hard!");

	/* initialize CPU fundamentals and interrupt controllers etc. with the I
	 * bit cleared.
	 */
	if(x86_irq_is_enabled()) x86_irq_disable();

	printf("setting up paging (kernel loaded into [%#lx..%#lx])\n",
		kern_start, kern_end);
	struct list_head ksp_resv = LIST_HEAD_INIT(ksp_resv);
	init_spaces(&ksp_resv);
	list_append_list(&resv_page_list, &ksp_resv);
	setup_paging(kern_start, kern_end);

	/* (see comment for init_spaces().) */
	init_mapdb();
	mapdb_init(kernel_space);

	/* NOTE: malloc(), free(), etc. are only available from this line down. */

	asm volatile ("lldt %%ax" :: "a" (0));
	init_kernel_tss(&kernel_tss);

	setup_gdt();
	setup_idt(15);		/* XT-PIC, for now */

	/* set MP. clear EMulation, NoExceptions, TaskSwitch. */
	x86_alter_cr0(~(X86_CR0_EM | X86_CR0_NE | X86_CR0_TS), X86_CR0_MP);
	x86_init_fpu();

	const L4_KernelConfigurationPage_t *kcp = kcp_base;
	L4_KernelRootServer_t s0_mod = kcp->sigma0,
		roottask_mod = kcp->root_server;

	int n = acpi_init();
	if(n < 0) {
		printf("ACPI initialization failed!\n");
		panic("mung doesn't work without ACPI anymore. bawwww");
	}

#ifdef DISABLE_APIC
	apic_disable_opt = true;
#endif

	printf("enabling interrupt controllers...\n");
	int max_irq = -1;
	struct pic_ops global_pic;
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
	SET_TRAMPOLINE(send_eoi, global_pic.send_eoi);
	SET_TRAMPOLINE(mask_irq, global_pic.mask_irq);
	SET_TRAMPOLINE(unmask_irq, global_pic.unmask_irq);
	assert(max_irq >= 0);

	printf("re-enabling interrupt processing...\n");
	x86_irq_enable();

	/* initialize KIP & vaguely related bits */
	kip_mem = kcp_base;
	assert(kcp_base == (void *)&kcp_copy[0]);
	make_kip(kip_mem, kern_start, kern_end, max_irq, L4_TimePeriod(1000));
	systemclock_p = kip_mem + PAGE_SIZE - sizeof(uint64_t);
	*systemclock_p = 0;
	global_timer_count = 0;
	kcp_base = (void *)0xdeadbeef;

	/* move the kernel to a high linear address, and change its segments so
	 * that it sees itself at a low address.
	 */
	x86_irq_disable();

	go_high();
	setup_idt(max_irq);
	setup_cr4();

	/* then unmap the low space by chucking its directories. */
	pdir_t *kernel_pdirs = kernel_space->pdirs->vm_addr;
	for(int i=0; i < KERNEL_SEG_START >> 22; i++) {
		kernel_pdirs[i] = 0;
	}
	x86_flush_tlbs();

	x86_irq_enable();

	/* per-module inits & init-time testing */
	init_gdt_resv();
	cop_init();
	init_threading();

	int next_user_tno = first_user_threadno();
	/* sigma0 (and later sigma1), being effectively the root pager, starts out
	 * without one for itself. its putstr() style output will be handled by a
	 * KDB entry bottom half.
	 */
	struct thread *s0_thread = spawn_kernel_server(
		THREAD_ID(next_user_tno++, 1), &s0_mod, NULL,
		PAGE_BITS, true);
	s0_thread->pri = 254;
	sigma0_space = s0_thread->space;
	/* (burn one TID for sigma1, which is missing from the spec.) */
	next_user_tno++;
	struct thread *roottask = NULL;
	if(roottask_mod.high >= PAGE_SIZE) {
		roottask = spawn_kernel_server(THREAD_ID(next_user_tno++, 1),
			&roottask_mod, s0_thread, size_to_shift(UTCB_SIZE * 1024), false);
		roottask->pri = 253;
		roottask->space->flags |= SF_PRIVILEGE;
		printf("roottask [%#lx .. %#lx] created as %lu:%lu.\n",
			roottask_mod.low, roottask_mod.high,
			TID_THREADNUM(roottask->id), TID_VERSION(roottask->id));
	}

	/* FIXME: pass KernelInterfacePage_t pointer instead. for now the KIP
	 * passes for the KCP.
	 */
	add_mem_to_sigma0(kip_mem);
	if(!space_add_ioperm(s0_thread->space, 0, 65536)) {
		panic("can't create sigma0 I/O bitmap");
	}
	thread_start(s0_thread);
	if(roottask != NULL) thread_start(roottask);

	printf("enabling timer interrupt\n");
	x86_irq_disable();
	setup_timer_ch0();
	if(apic_enabled) ioapic_route_legacy_irq(0, 0x20);
	else unmask_irq(0, IHF_ACT_LOW);
	x86_irq_enable();

	printf("entering scheduler\n");
	exit_to_scheduler(NULL);
}
