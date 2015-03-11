
#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include <assert.h>
#include <ccan/list/list.h>
#include <ccan/container_of/container_of.h>
#include <ccan/alignof/alignof.h>

#include <l4/types.h>
#include <l4/kcp.h>
#include <l4/kip.h>

#include <ukernel/slab.h>
#include <ukernel/thread.h>
#include <ukernel/space.h>
#include <ukernel/misc.h>
#include <ukernel/mm.h>


#define N_FIRST_PAGES (2 * 1024 * 1024 / PAGE_SIZE)


/* a page of free address space in kernel memory. */
struct as_free {
	struct list_node link;
	L4_Word_t address;
};


static struct list_head k_free_pages = LIST_HEAD_INIT(k_free_pages),
	k_slab_pages = LIST_HEAD_INIT(k_slab_pages),
	k_heap_pages = LIST_HEAD_INIT(k_heap_pages),
	free_as_list = LIST_HEAD_INIT(free_as_list);

static struct kmem_cache *mm_page_cache = NULL,	/* <struct page> */
	*free_as_cache = NULL;		/* <struct as_free> */

static uintptr_t heap_pos = KERNEL_HEAP_TOP;


/* TODO: make the kernel heap also contiguous in address space -- and then
 * enable MORECORE_CONTIGUOUS in dlmalloc.c & leave DEFAULT_GRANULARITY at
 * default to minimize unused kernel RAM.
 *
 * to do this, reserve_heap_page() must allocate from the other end of the
 * kernel address range.
 */
void *sbrk(intptr_t increment)
{
	if(increment > 0) {
		uintptr_t n_pages = ((uintptr_t)increment + PAGE_SIZE - 1) >> PAGE_BITS;
		heap_pos -= n_pages << PAGE_BITS;
		const uintptr_t start_pos = heap_pos;
		for(uintptr_t i=0; i < n_pages; i++) {
			struct page *pg = get_kern_page(start_pos + i * PAGE_SIZE);
			assert((uintptr_t)pg->vm_addr == start_pos + i * PAGE_SIZE);
			list_add(&k_heap_pages, &pg->link);
		}
	}
	return (void *)heap_pos;
}


/* reserves address space in the sbrk()-style heap, and from the list of
 * pages released with free_heap_page(), preferring the latter.
 *
 * NB: disabling recycling of old heap addresses makes for an interesting
 * one-bit way to debug kernel-space page table weirdness.
 */
uintptr_t reserve_heap_page(void)
{
	struct as_free *f = list_pop(&free_as_list, struct as_free, link);
	if(f != NULL) {
		uintptr_t addr = f->address;
		kmem_cache_free(free_as_cache, f);
		assert((addr & PAGE_MASK) == 0);
		return addr;
	} else {
		heap_pos -= PAGE_SIZE;
		assert((heap_pos & PAGE_MASK) == 0);
		return heap_pos;
	}
}


void free_heap_page(uintptr_t addr)
{
	assert((addr & PAGE_MASK) == 0);

	put_supervisor_page(addr, 0);

	struct as_free *f = kmem_cache_alloc(free_as_cache);
	f->address = addr;
	list_add(&free_as_list, &f->link);
}


uintptr_t reserve_heap_range(size_t size)
{
	assert(size == (1 << size_to_shift(size)));

	uintptr_t first = (heap_pos - size) & ~(size - 1),
		last = first + size - 1;
	/* recycle the useless part one page at a time.
	 *
	 * this is inefficient: we could instead 1) add a fpage-based subrange
	 * type [and define free_heap_page() in terms of it], 1b) collapse
	 * neighbour pages, 2) make it use a rb-tree for constant-time merge
	 * checks, and 3) do this in larger chunks than PAGE_SIZE at a time.
	 */
	for(uintptr_t a = last + 1; a < heap_pos; a += PAGE_SIZE) {
		free_heap_page(a);
	}

	heap_pos = first;
	return first;
}


/* maps @pg into a reserve_heap_page() area. */
void *map_vm_page(struct page *pg, int duration)
{
	if(CHECK_FLAG(pg->flags, PAGEF_VMREF)) {
		if(duration == VM_REF) pg->refcount++;
		return pg->vm_addr;
	}

	if(pg->vm_addr == NULL) {
		pg->vm_addr = (void *)reserve_heap_page();
		if(duration == VM_REF) {
			pg->refcount = 1;
			pg->flags |= PAGEF_VMREF;
		} else {
			assert(duration == VM_SYSCALL);
			pg->refcount = 0;
			pg->flags &= ~PAGEF_VMREF;
			/* TODO: set VM_SYSCALL epoch */
		}
		put_supervisor_page((uintptr_t)pg->vm_addr, pg->id);
	}

	return pg->vm_addr;
}


void unref_vm_page(struct page *pg)
{
	if(CHECK_FLAG(pg->flags, PAGEF_VMREF)) {
		assert(pg->refcount > 0);
		if(--pg->refcount == 0) {
			/* downgrade to VM_SYSCALL scope. */
			pg->flags &= ~PAGEF_VMREF;
			/* TODO: set VM_SYSCALL epoch */
		}
	}
}


static COLD bool page_is_available(
	const L4_KernelConfigurationPage_t *kcp,
	L4_Word_t addr)
{
	assert(offsetof(L4_KernelConfigurationPage_t, MemoryInfo) == 0x54);
	L4_MemoryDesc_t *mds = (void *)kcp + kcp->MemoryInfo.MemDescPtr;
	int md_count = kcp->MemoryInfo.n;
	bool virt_ok = false, conv_ok = false, reserved = false;
//	printf("%s: address %#x (%d memorydescs)...\n", __func__, addr, md_count);
	for(int i=0; i < md_count; i++) {
		L4_Word_t low = L4_MemoryDescLow(&mds[i]),
			high = L4_MemoryDescHigh(&mds[i]);
		int type = L4_MemoryDescType(&mds[i]);
		bool virtual = L4_IsMemoryDescVirtual(&mds[i]);

		if(addr < low || addr > high) continue;

#if 0
		size_t size = (high - low + 1) / 1024;
		printf("memdesc %d: %#x .. %#x, size %u KiB, type %x, %s\n",
			i, low, high, (unsigned)size, type,
			virtual ? "virtual" : "physical");
#endif

		if(!virt_ok && virtual) {
			virt_ok = true;
		} else if(!virtual && type == L4_ConventionalMemoryType
			&& virt_ok && !conv_ok)
		{
			conv_ok = true;
		} else if(!virtual && conv_ok && type != L4_ConventionalMemoryType) {
			reserved = true;
			break;
		}
	}

	return virt_ok && conv_ok && !reserved;
}


/* reserves initial memory for the kernel. this is subsequently used to
 * allocate spaces, threads, UTCB pages, page tracking structures, copies of
 * ACPI tables, etc...
 */
void init_kernel_heap(
	void *kcp_base,
	uintptr_t *resv_start,
	uintptr_t *resv_end)
{
	const L4_KernelConfigurationPage_t *kcp = kcp_base;

	/* grab early pages from conventional memory that isn't reserved by a
	 * bootloader-defined object.
	 */
	extern char _start, _end;
	L4_Word_t next_addr = (L4_Word_t)&_end;	/* ... or the kernel binary. */
	next_addr = (next_addr + PAGE_SIZE - 1) & ~PAGE_MASK;
	printf("kernel early memory low address is %#x\n", (unsigned)next_addr);
	int got = 0;
	static struct page first_pages[N_FIRST_PAGES];
	while(got < N_FIRST_PAGES) {
		if(next_addr > (64 * 1024 * 1024)) {
			/* stop at the 64 MiB mark. */
			panic("init_kernel_heap limit reached");
		}
		if(page_is_available(kcp, next_addr)) {
			struct page *pg = &first_pages[got++];
			pg->id = next_addr >> PAGE_BITS;
			pg->vm_addr = (void *)next_addr;
			list_add(&k_free_pages, &pg->link);
		}
		next_addr += PAGE_SIZE;
	}
	printf("uppermost reserved byte is at %#x\n", (unsigned)next_addr - 1);

	/* initialize page slab & return. */
	mm_page_cache = kmem_cache_create("mm_page_cache", sizeof(struct page),
		ALIGNOF(struct page), 0, NULL, NULL);
	free_as_cache = kmem_cache_create("free_as_cache", sizeof(struct as_free),
		ALIGNOF(struct as_free), 0, NULL, NULL);
	*resv_start = MIN(uintptr_t, (uintptr_t)&_start, *resv_start);
	*resv_end = MAX(uintptr_t, next_addr - 1, *resv_end);	/* (inclusive.) */
}


struct page *get_kern_page(uintptr_t vm_addr)
{
	assert((vm_addr & PAGE_MASK) == 0);
	assert(!list_empty(&k_free_pages));
	/* (get from tail of list, as that's where the idempotent heap is during
	 * early boot. otherwise there is a chance of endless recursion through
	 * put_supervisor_page()'s not finding the page directory for the vm heap.
	 * [... more recently though, .next seems to work just as well.])
	 */
	struct page *p;
	do {
		p = container_of(k_free_pages.n.prev, struct page, link);
		list_del(&p->link);
		/* don't return pages with a physical address below 0x100000, as these
		 * are special on the x86 (video memory, etc)
		 */
		if(p->id < (0x100000 >> PAGE_BITS)) {
			/* TODO: stick these in a reserved list. maybe. */
			printf("%s: NOTE: skipping page id %u (physaddr 0x%x)\n",
				__func__, p->id, (uintptr_t)p->id << PAGE_BITS);
			p = NULL;
		}
	} while(p == NULL);

	if(vm_addr == 0) {
		if(p->vm_addr == NULL) {
			/* map it in at some address. */
			vm_addr = reserve_heap_page();
			put_supervisor_page(vm_addr, p->id);
			p->vm_addr = (void *)vm_addr;
		}
	} else {
		if(p->vm_addr != NULL) {
			/* remove the heap reservation. */
			uintptr_t addr = (uintptr_t)p->vm_addr;
			put_supervisor_page(addr, 0);
			free_heap_page(addr);
			p->vm_addr = NULL;
		}

		put_supervisor_page(vm_addr, p->id);
		p->vm_addr = (void *)vm_addr;
	}

	return p;
}


void free_kern_page(struct page *page)
{
	/* better here than at the call sites. */
	if(page == NULL) return;

	if(page->vm_addr != NULL) {
		free_heap_page((uintptr_t)page->vm_addr);
		page->vm_addr = NULL;
	}
	/* grow the freelist at the end for t_slab.c's sake */
	list_add_tail(&k_free_pages, &page->link);
}


/* interface for slab.c */
void *kmem_alloc_new_page(void)
{
	struct page *pg = get_kern_page(0);
	if(pg != NULL) {
		list_add(&k_slab_pages, &pg->link);
		assert(pg->vm_addr != NULL);
		return pg->vm_addr;
	} else {
		return NULL;
	}
}


void kmem_free_page(void *ptr)
{
	/* TODO: proper data structures, man. hash tables won't work because
	 * kmem_alloc_new_page() will be called before the sbrk heap is ready;
	 * so a custom solution seems to be required. (perhaps a chain of pages,
	 * each containing 1000 or 500 pointers hashed by address. would require a
	 * bigger test of this mechanism in t_slab.c, and also an bigger slice of
	 * kernel-reserved memory.)
	 */
	struct page *pg;
	list_for_each(&k_slab_pages, pg, link) {
		if(pg->vm_addr == ptr) {
			list_del_from(&k_slab_pages, &pg->link);
			free_kern_page(pg);
			return;
		}
	}

	printf("warning: %s(%p) refers to an unknown page\n", __func__, ptr);
}
