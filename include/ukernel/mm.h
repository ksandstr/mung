
#ifndef SEEN_MM_H
#define SEEN_MM_H

#include <stdlib.h>
#include <stdint.h>
#include <ccan/list/list.h>


#define PAGE_BITS 12
#define PAGE_SIZE (L4_Word_t)(1 << PAGE_BITS)
#define PAGE_MASK (PAGE_SIZE - 1)

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


/* parameter of map_vm_page() */
#define VM_REF 1		/* reference-counted kernel address space */
#define VM_SYSCALL 2	/* guaranteed valid until kernel exit */


/* values of <struct page>.flags */
#define PAGEF_VMREF 0x01	/* page's VM map is reference counted. */


/* tracks a physical page reserved by the kernel. */
struct page
{
	struct list_node link;
	void *vm_addr;		/* address in kernel space, NULL if not mapped */

	/* page IDs are physical address >> PAGE_BITS, which allows referencing of
	 * 44 bits of physical memory (i.e. 16 TiB).
	 */
	uint32_t id;

	uint16_t refcount;	/* # of map_vm_page() refs (wrt ->vm_addr) */
	uint16_t flags;		/* of PAGEF_* */
};


extern void add_supervisor_pages(intptr_t heap_pos, int num_pages);

/* TODO: change reserve_heap_range() to accept a sizelog2 instead */
extern uintptr_t reserve_heap_range(size_t size);	/* (aligned to 2**n) */
#define reserve_heap_page() reserve_heap_range(PAGE_SIZE)
extern void free_heap_page(uintptr_t address);

/* ensures @pg->vm_addr validity. duration is one of VM_*.
 * unref_vm_page() releases references from VM_REF.
 */
extern void *map_vm_page(struct page *pg, int duration);
extern void unref_vm_page(struct page *p);


/* kernel heap initialization. the caller must identitymap between *resv_start
 * and *resv_end when it enables paging.
 */
extern void init_kernel_heap(
	void *kcp_base,
	uintptr_t *resv_start,
	uintptr_t *resv_end);


/* supervisor page table access from kmain.c */

extern void put_supervisor_page(uintptr_t addr, uint32_t page_id);


/* from heap.c, a page-grain allocator.
 * works during early boot already, backs the slab allocator and sbrk()
 * mechanisms.
 *
 * when vm_addr is 0, get_kern_page() uses reserve_heap_page() to select an
 * address.
 */
extern struct page *get_kern_page(uintptr_t vm_addr);
extern void free_kern_page(struct page *p);

#endif
