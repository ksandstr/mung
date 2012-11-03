
#ifndef SEEN_UKERNEL_SPACE_H
#define SEEN_UKERNEL_SPACE_H

#include <stdint.h>
#include <ccan/list/list.h>
#include <ccan/htable/htable.h>
#include <l4/types.h>

#include <ukernel/thread.h>
#include <ukernel/mm.h>
#include <ukernel/mapdb.h>


/* space->flags */
#define SF_PRIVILEGE 0x1	/* *Control syscall access */


struct thread;

struct space
{
	struct list_node link;		/* in the global space list */
	struct list_head threads;	/* <struct thread> via space_link */

	uint16_t flags;				/* SF_* */

	/* pages for the UTCB area, allocated as threads are assigned UTCB
	 * segments in the address space
	 */
	struct page **utcb_pages;
	L4_Fpage_t utcb_area;
	L4_Fpage_t kip_area;

	thread_id redirector;

	struct htable ptab_pages;	/* <struct page *>, by page.id */
	struct map_db mapdb;

	/* x86 specific bits */
	struct page *pdirs;

	unsigned tss_seg;
	size_t tss_len;				/* total amount of RAM under "tss" */
	struct tss *tss;			/* immediately followed by I/O bitmap memory */
};


#define UTCB_SIZE 512
#define UTCB_PER_PAGE (PAGE_SIZE / UTCB_SIZE)
#define NUM_UTCB_PAGES(area) (L4_Size((area)) / UTCB_SIZE / UTCB_PER_PAGE)


/* kernel_space->mapdb contains mappings for all non-reserved memory seen by
 * the kernel. these mappings are subsequently granted to sigma0 during the
 * boot process as initial memory.
 */
extern struct space *kernel_space;

/* sigma0, however, is completely normal except that its pager provides
 * idempotent grants from the special kernel space.
 */
extern struct space *sigma0_space;


extern struct space *space_new(void);
extern void space_free(struct space *sp);
extern void space_add_thread(struct space *sp, struct thread *t);
extern void space_remove_thread(struct space *sp, struct thread *t);
extern void space_switch(struct space *sp);
/* (this only finds spaces that've got at least one thread associated with
 * them.)
 */
extern struct space *space_find(thread_id tid);
/* (these both return SpaceControl error, or 0 on success.) */
extern int space_set_utcb_area(struct space *sp, L4_Fpage_t area);
extern int space_set_kip_area(struct space *sp, L4_Fpage_t area);
extern struct thread *space_find_local_thread(
	struct space *sp,
	L4_LthreadId_t ltid);

/* use 0 for @page_id to erase a page. */
extern void space_put_page(
	struct space *sp,
	uintptr_t addr,
	uint32_t page_id,
	int access);

/* probe and reset the access bits of a page. if the probed address is in a
 * hole, the function returns -ENOENT; also, if next_addr_p != NULL, then
 * *next_addr_p will be set to a following address that might not be in the
 * same hole.
 */
extern int space_probe_pt_access(
	L4_Word_t *next_addr_p,
	struct space *sp,
	L4_Word_t addr);

extern size_t space_memcpy_from(
	struct space *sp,
	void *dest,
	L4_Word_t address,
	size_t size);

/* returns false on OOM */
extern bool space_add_ioperm(
	struct space *sp,
	L4_Word_t base_port,
	int size);


/* stubbed out interface for architectures (non-x86, non-amd64) that don't
 * have an INVLPG equivalent. a sequence of calls to space_put_page(), or
 * others that alter the page tables, should be terminated with
 * space_commit(). it may also end up doing something in a multiprocessor
 * environment.
 */
static inline void space_commit(struct space *sp) {
	/* emptiness */
}

extern void sys_unmap(L4_Word_t control);
extern L4_Word_t sys_spacecontrol(
	L4_ThreadId_t spacespec,
	L4_Word_t control,
	L4_Fpage_t kip_area,
	L4_Fpage_t utcb_area,
	L4_ThreadId_t redirector,
	L4_Word_t *old_control);


/* pages reserved before htable_add() can be used, are added to the list.
 * later kmain() calls space_add_resv_pages().
 */
extern void init_spaces(struct list_head *resv_list);
extern void space_add_resv_pages(
	struct space *sp,
	struct list_head *resv_list);


#endif
