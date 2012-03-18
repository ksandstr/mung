
#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include <assert.h>
#include <ccan/list/list.h>
#include <ccan/container_of/container_of.h>
#include <ccan/alignof/alignof.h>

#include <ukernel/slab.h>
#include <ukernel/thread.h>
#include <ukernel/space.h>
#include <ukernel/mm.h>

#include "multiboot.h"



#define N_FIRST_PAGES (2 * 1024 * 1024 / PAGE_SIZE)


static struct list_head k_free_pages = LIST_HEAD_INIT(k_free_pages),
	k_heap_pages = LIST_HEAD_INIT(k_heap_pages);
static struct kmem_cache *mm_page_cache = NULL;

static uintptr_t heap_pos = KERNEL_HEAP_TOP;


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


/* reserves address space in the sbrk()-style heap. */
uintptr_t reserve_heap_page(void)
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

	uint32_t id_chunk[256];
	int pos = 0;
	while(pos < npages) {
		int seg = MIN(int, 256, npages - pos);
		for(int i=0; i < seg; i++) {
			id_chunk[i] = (start >> PAGE_BITS) + pos + i;
		}
		mapdb_init_range(&kernel_space->mapdb,
			start + pos * PAGE_SIZE, id_chunk, seg);
		pos += seg;
	}

	printf("%s: added %d pages.\n", __func__, npages);
}


/* reserves enough identity pages to create <struct page> for each physical
 * page. indicates which range to identity map by *resv_start and *resv_end.
 */
void init_kernel_heap(
	const struct multiboot_mmap_entry *mm,
	uintptr_t *resv_start,
	uintptr_t *resv_end)
{
	extern char _start, _end;
	uintptr_t next_addr = ((uintptr_t)&_end + PAGE_SIZE - 1) & ~PAGE_MASK;

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
			/* FIXME: stick these in a reserved list. maybe. */
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
		} else {
			vm_addr = (uintptr_t)p->vm_addr;
		}
	} else {
		if(p->vm_addr != NULL) {
			/* remove the heap reservation. */
			put_supervisor_page((uintptr_t)p->vm_addr, 0);
			/* TODO: call a release_heap_page() or some such */
			p->vm_addr = NULL;
		}

		/* FIXME: check that there isn't already a page at vm_addr */
		put_supervisor_page(vm_addr, p->id);
		p->vm_addr = (void *)vm_addr;
	}

	return p;
}


void free_kern_page(struct page *page)
{
	list_add(&k_free_pages, &page->link);
}
