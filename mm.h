
#ifndef SEEN_MM_H
#define SEEN_MM_H

#include <stdlib.h>
#include <stdint.h>
#include <ccan/list/list.h>

#include "multiboot.h"


#define PAGE_SIZE 4096
#define PAGE_BITS 12
#define PAGE_MASK 0xfff


/* represents a hardware page. */
struct page
{
	struct list_node link;
	void *vm_addr;		/* address in kernel space */

	/* page IDs are physical address >> PAGE_BITS, which allows referencing of
	 * 44 bits of physical memory (i.e. 16 TiB).
	 */
	uint32_t id;
};


extern void add_supervisor_pages(intptr_t heap_pos, int num_pages);

extern intptr_t reserve_heap_page(void);


/* kernel heap initialization. init_kernel_heap() is called with the
 * lowest-address multiboot memory segment that covers the kernel program
 * binary; the caller then identity-maps all memory between resv_start and
 * resv_end inclusive, enables paging, and adds the rest of the available
 * memory using add_boot_pages() .
 */
extern void init_kernel_heap(
	const struct multiboot_mmap_entry *mm,
	intptr_t *resv_start,
	intptr_t *resv_end);

extern void add_boot_pages(intptr_t start, intptr_t end);


/* supervisor page table access from kmain.c */

extern void put_supervisor_page(intptr_t addr, uint32_t page_id);


/* from heap.c, a page-grain allocator.
 * works during early boot already, backs the slab allocator.
 */

extern struct page *get_kern_page(void);
extern void free_kern_page(struct page *p);


/* from slab.c, in imitation of Linux */

struct kmem_cache;
typedef void (*kmem_cache_ctor)(void *, struct kmem_cache *, unsigned long);

extern struct kmem_cache *kmem_cache_create(
	const char *name,
	size_t size,
	size_t align,
	unsigned long flags,
	kmem_cache_ctor ctor,
	kmem_cache_ctor dtor);

extern void kmem_cache_destroy(struct kmem_cache *cache);

extern void *kmem_cache_alloc(struct kmem_cache *cache);
extern void *kmem_cache_zalloc(struct kmem_cache *cache);
extern void kmem_cache_free(struct kmem_cache *cache, void *ptr);

/* accessors */
extern size_t kmem_cache_size(struct kmem_cache *cache);
extern const char *kmem_cache_name(struct kmem_cache *cache);
extern int kmem_cache_shrink(struct kmem_cache *cache);

#endif
