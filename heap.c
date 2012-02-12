
#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include <ukernel/types.h>
#include <ccan/list/list.h>
#include <ccan/container_of/container_of.h>
#include <ccan/alignof/alignof.h>

#include "multiboot.h"
#include "mm.h"

/* leave uppermost 128k unused */
#define HEAP_TOP 0xfffe0000

#define N_FIRST_PAGES (2 * 1024 * 1024 / PAGE_SIZE)


static struct list_head k_free_pages = LIST_HEAD_INIT(k_free_pages);
static struct kmem_cache *mm_page_cache = NULL;

static intptr_t heap_pos = HEAP_TOP;


void *sbrk(intptr_t increment)
{
#if 0
	if(increment > 0) {
		int n_pages = (increment + 4095) >> 12;
		heap_pos -= n_pages << 12;
		add_supervisor_pages(heap_pos, n_pages);
	}
#endif
	return (void *)heap_pos;
}


/* reserves address space in the sbrk()-style heap. */
intptr_t reserve_heap_page(void)
{
#if 0
	heap_pos -= PAGE_SIZE;
#endif
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
//	assert(!list_empty(&k_free_pages));
	struct page *p = container_of(k_free_pages.n.next, struct page, link);
	list_del(&p->link);
	if(p->vm_addr == NULL) {
		/* FIXME: map the page in */
		printf("FOOOOO!\n");
	}
	return p;
}


void free_kern_page(struct page *page)
{
	list_add(&k_free_pages, &page->link);
}
