
#ifndef SEEN_UKERNEL_SPACE_H
#define SEEN_UKERNEL_SPACE_H

#include <stdint.h>
#include <ccan/list/list.h>
#include <ccan/htable/htable.h>
#include <ccan/likely/likely.h>
#include <l4/types.h>

#include <ukernel/thread.h>
#include <ukernel/mm.h>
#include <ukernel/mapdb.h>
#include <ukernel/setjmp.h>
#include <ukernel/misc.h>


/* space->flags */
#define SF_PRIVILEGE 0x1	/* *Control syscall access */

#define UTCB_SIZE 512
#define UTCB_PER_PAGE (PAGE_SIZE / UTCB_SIZE)
#define NUM_UTCB_PAGES(area) (L4_Size((area)) / UTCB_SIZE / UTCB_PER_PAGE)


struct thread;


/* allocated in space.c's utcb_page_slab, except for kernel_space, where
 * they're allocated statically and only inserted into the hash table after
 * the malloc heap has been initialized.
 */
struct utcb_page
{
	/* page position in UTCB area.
	 *
	 * this limits the number of thread slots to 64k * 8 = 512k on 32-bit
	 * platforms, and half that on 64-bit ones. since each thread weighs over
	 * 200 bytes, and the current kernel allocation is at most 256 MiB, the
	 * limit won't be hit.
	 */
	uint16_t pos;
	uint16_t occmap;		/* little-endian occupancy map */
	struct page *pg;
	struct thread *slots[UTCB_PER_PAGE];
};


struct space
{
	struct list_node link;		/* in the global space list */

	uint16_t flags;				/* SF_* */

	L4_Fpage_t utcb_area;
	L4_Fpage_t kip_area;

	L4_ThreadId_t redirector;

	struct htable utcb_pages;	/* <struct utcb_page *>, by ->pos */
	struct htable ptab_pages;	/* <struct page *>, by page.id */
	struct map_db mapdb;

	/* x86 specific bits */
	struct page *pdirs;			/* toplevel page directory table */

	unsigned tss_seg;
	size_t tss_len;				/* total amount of RAM under "tss" */
	struct tss *tss;			/* immediately followed by I/O bitmap memory */
};


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
extern L4_ThreadId_t space_name(struct space *sp);
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

/* adds page to UTCB area iff utcb_pos & utcb_page are set */
extern void space_add_thread(struct space *sp, struct thread *t);
/* kicks the thread's UTCB page if it becomes empty. */
extern void space_remove_thread(struct space *sp, struct thread *t);
/* looks an utcb page up, or allocates one. returns NULL when ppos is outside
 * sp->utcb_area.
 */
extern struct utcb_page *space_get_utcb_page(struct space *sp, uint16_t ppos);

/* use 0 for @page_id to erase a page. */
extern void space_put_page(
	struct space *sp,
	uintptr_t addr,
	uint32_t page_id,
	int access);

/* removes page table entries within @fp. ignores L4_Rights(@fp). */
extern void space_clear_range(struct space *sp, L4_Fpage_t fp);

/* sets access for a range. if @fp.rights doesn't include Readable, the entry
 * is removed altogether to enforce non-readability.
 */
extern void space_set_range_access(struct space *sp, L4_Fpage_t fp);

extern void space_set_range(
	struct space *sp,
	L4_Fpage_t range,
	uint32_t first_pgid);

/* generate and prefill a second-level page table. if one is already present,
 * no-op and return false.
 */
extern bool space_prefill_upper(struct space *sp, L4_Word_t addr);

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

/* UTCB is accessed by the wrapper to sync ESI and MR0. */
extern SYSCALL void sys_unmap(L4_Word_t control, void *current_utcb);

extern SYSCALL L4_Word_t sys_spacecontrol(
	L4_ThreadId_t spacespec,
	L4_Word_t control,
	L4_Fpage_t kip_area,
	L4_Fpage_t utcb_area,
	L4_ThreadId_t redirector,
	L4_Word_t *old_control);


/* pages reserved before htable_add() can be used, are added to the list.
 * later kmain() calls space_finalize_kernel() to add those to kernel_space's
 * ptab_pages hash.
 */
extern void init_spaces(struct list_head *resv_list);

extern void space_finalize_kernel(
	struct space *sp,		/* must be kernel_space */
	struct list_head *resv_list);


/* in-kernel pf catching. variables defined in exception.c */

extern jmp_buf catch_pf_env;
extern volatile bool catch_pf_ok;

/* setjmp-like, fault address on 2nd return. */
#define catch_pf() ({ \
		assert(!catch_pf_ok); \
		uint32_t _faddr; \
		likely((_faddr = setjmp(catch_pf_env)) == 0) \
			? (catch_pf_ok = true, 0) : _faddr; \
	})

/* disables the mechanism before frame exit */
static inline void uncatch_pf(void) {
	catch_pf_ok = false;
}


#endif
