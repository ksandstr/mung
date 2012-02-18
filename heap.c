
#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include <assert.h>
#include <ccan/list/list.h>
#include <ccan/container_of/container_of.h>
#include <ccan/alignof/alignof.h>

#include <ukernel/mm.h>
#include <ukernel/slab.h>

#include "multiboot.h"

/* leave uppermost 4k unused for esoteric reasons. */
#define HEAP_TOP (KERNEL_SEG_SIZE - 4096)

#define N_FIRST_PAGES (2 * 1024 * 1024 / PAGE_SIZE)


static struct list_head k_free_pages = LIST_HEAD_INIT(k_free_pages),
	k_heap_pages = LIST_HEAD_INIT(k_heap_pages);
static struct kmem_cache *mm_page_cache = NULL;

static intptr_t heap_pos = HEAP_TOP;


void *sbrk(intptr_t increment)
{
	if(increment > 0) {
		int n_pages = (increment + PAGE_SIZE - 1) >> PAGE_BITS;
		heap_pos -= n_pages << PAGE_BITS;
		for(int i=0; i < n_pages; i++) {
			struct page *pg = get_kern_page();
			list_add(&k_heap_pages, &pg->link);
			put_supervisor_page(heap_pos + i * PAGE_SIZE, pg->id);
		}
	}
	return (void *)heap_pos;
}


/* reserves address space in the sbrk()-style heap. */
intptr_t reserve_heap_page(void)
{
	heap_pos -= PAGE_SIZE;
	return heap_pos;
}


void add_boot_pages(intptr_t start, intptr_t end)
{
	printf("%s: start 0x%x, end 0x%x\n", __func__, (unsigned)start,
		(unsigned)end);
	int npages = (end - start + PAGE_SIZE - 1) >> PAGE_BITS;
	for(int i=0; i < npages; i++) {
		struct page *p = kmem_cache_alloc(mm_page_cache);
		p->id = (start >> PAGE_BITS) + i;
		p->vm_addr = NULL;
		list_add(&k_free_pages, &p->link);
	}
	printf("%s: added %d pages.\n", __func__, npages);
}


/* reserves enough identity pages to create <struct page> for each physical
 * page. indicates which range to identity map by *resv_start and *resv_end.
 */
void init_kernel_heap(
	const struct multiboot_mmap_entry *mm,
	intptr_t *resv_start,
	intptr_t *resv_end)
{
	extern intptr_t _start, _end;
	intptr_t next_addr = ((intptr_t)&_end + PAGE_SIZE - 1) & ~PAGE_MASK;

	/* first, take a static 2 MiB for early kernel memory that's allocated per
	 * page.
	 */
	static struct page first_pages[N_FIRST_PAGES];
	for(int i=0; i < N_FIRST_PAGES; i++) {
		first_pages[i].id = next_addr >> PAGE_BITS;
		first_pages[i].vm_addr = (void *)next_addr;
		next_addr += PAGE_SIZE;
		list_add(&k_free_pages, &first_pages[i].link);
	}

	/* initialize page slab & return. */
	mm_page_cache = kmem_cache_create("mm_page_cache", sizeof(struct page),
		ALIGNOF(struct page), 0, NULL, NULL);
	*resv_start = (intptr_t)&_start;
	*resv_end = next_addr - 1;	/* (inclusive.) */
}


struct page *get_kern_page(void)
{
	assert(!list_empty(&k_free_pages));
	/* (get from tail of list, as that's where the idempotent heap is during
	 * early boot. otherwise there is a chance of endless recursion through
	 * put_supervisor_page()'s not finding the page directory for the vm heap.
	 * [... more recently though, .next seems to work just as well.])
	 */
	struct page *p = container_of(k_free_pages.n.prev, struct page, link);
	list_del(&p->link);
	if(p->vm_addr == NULL) {
		/* map it in. */
		intptr_t addr = reserve_heap_page();
		put_supervisor_page(addr, p->id);
		p->vm_addr = (void *)addr;
	}
	return p;
}


void free_kern_page(struct page *page)
{
	list_add(&k_free_pages, &page->link);
}
