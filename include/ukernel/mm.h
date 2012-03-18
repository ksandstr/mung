
#ifndef SEEN_MM_H
#define SEEN_MM_H

#include <stdlib.h>
#include <stdint.h>
#include <ccan/list/list.h>

#include "multiboot.h"


#define PAGE_SIZE 4096
#define PAGE_BITS 12
#define PAGE_MASK 0xfff

#define PAGE_ALIGN __attribute__((aligned(4096)))

#define KERNEL_SEG_SIZE (256u * 1024u * 1024u)
#define KERNEL_SEG_START (~KERNEL_SEG_SIZE + 1)

#define RESV_UTCB_SIZE ((NUM_KERNEL_THREADS * UTCB_SIZE + PAGE_SIZE - 1) & ~PAGE_MASK)
/* leaving uppermost 4k unused for esoteric reasons. */
#define KERNEL_HEAP_TOP (KERNEL_SEG_SIZE - 4096 - RESV_UTCB_SIZE)


typedef uint32_t pdir_t;
typedef uint32_t page_t;


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


/* represents a hardware page. */
struct page
{
	struct list_node link;
	void *vm_addr;		/* address in kernel space, NULL if not mapped */

	/* page IDs are physical address >> PAGE_BITS, which allows referencing of
	 * 44 bits of physical memory (i.e. 16 TiB).
	 */
	uint32_t id;
};


extern void add_supervisor_pages(intptr_t heap_pos, int num_pages);

extern uintptr_t reserve_heap_page(void);


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

extern void put_supervisor_page(uintptr_t addr, uint32_t page_id);


/* from heap.c, a page-grain allocator.
 * works during early boot already, backs the slab allocator.
 */

extern struct page *get_kern_page(void);
extern void free_kern_page(struct page *p);

#endif
