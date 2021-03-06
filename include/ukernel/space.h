
#ifndef SEEN_UKERNEL_SPACE_H
#define SEEN_UKERNEL_SPACE_H

#include <stdint.h>
#include <setjmp.h>
#include <ccan/list/list.h>
#include <ccan/htable/htable.h>
#include <ccan/likely/likely.h>
#include <ccan/container_of/container_of.h>
#include <l4/types.h>

#include <ukernel/rbtree.h>
#include <ukernel/thread.h>
#include <ukernel/mm.h>
#include <ukernel/misc.h>


/* space->flags */
#define SF_PRIVILEGE 0x1	/* *Control syscall access */
#define SF_REDIRECT  0x2	/* ->redirector is obeyed in ipc_send_half() */

#define UTCB_SIZE 512
#define UTCB_PER_PAGE (PAGE_SIZE / UTCB_SIZE)
#define NUM_UTCB_PAGES(area) (L4_Size((area)) / UTCB_SIZE / UTCB_PER_PAGE)


struct thread;
union pt_iter_u;


/* allocated in space.c's utcb_page_slab, except for kernel_space, where
 * they're allocated statically and only inserted into the hash table after
 * the malloc heap has been initialized.
 */
struct utcb_page
{
	struct rb_node rb;	/* in <struct space>.utcb_pages, by pos */

	/* page position in UTCB area.
	 *
	 * this limits the number of thread slots to 64k * 8 = 512k on 32-bit
	 * platforms, and half that on 64-bit ones. since each thread weighs over
	 * 200 bytes, and the current kernel allocation is at most 256 MiB, the
	 * limit won't be hit.
	 */
	uint16_t pos;
	uint16_t occmap;		/* little-endian occupancy map, 1 for present */
	struct page *pg;
	struct thread *slots[UTCB_PER_PAGE];
};


struct space
{
	/* cached value for UTCB validation. see space_set_utcb_area() */
	L4_Word_t utcb_top;
	L4_Fpage_t utcb_area;
	L4_Fpage_t kip_area;

	uint16_t flags;				/* SF_* */

	/* when flags & SF_REDIRECT, NULL for invalid (removed and not set) and
	 * non-NULL when set to a valid TID;
	 * otherwise no redirector was set (redir=anythread).
	 */
	struct thread *redirector;

	struct htable ptab_groups;	/* <struct map_group *>, by MG_START(grp) */
	struct rb_root utcb_pages;	/* of <struct utcb_page> */
	struct list_node link;		/* in the global space list */

	/* x86 specific bits */
	struct page *pdirs;			/* toplevel page directory table */

	unsigned tss_seg;
	size_t tss_len;				/* total amount of RAM under "tss" */
	struct tss *tss;			/* immediately followed by I/O bitmap memory */
};


extern struct space *kernel_space;

/* sigma0, however, is completely normal except that its pager provides
 * idempotent grants from the special kernel space.
 */
extern struct space *sigma0_space;

/* (v2: per-cpu when SMP happens, natch.) */
extern struct space *current_space;


extern struct space *space_new(void);
extern void space_free(struct space *sp);
extern L4_ThreadId_t space_name(struct space *sp);
/* returns the previous space */
extern struct space *space_switch(struct space *sp);
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

/* invalidates the redirector field in all spaces where it references @t. used
 * in deleting and version-altering ThreadControl cases. REDIR_WAIT[t] threads
 * in those spaces will be put into REDIR_WAIT[nil] to remove references to
 * @t; active receive handles other cases.
 */
extern void space_remove_redirector(struct thread *t);

/* @sp_iter designates the source space. */
extern size_t space_memcpy_from(
	void *dest,
	union pt_iter_u *sp_iter, L4_Word_t address, size_t size);

/* returns false on OOM */
extern bool space_add_ioperm(struct space *sp, L4_Word_t base_port, int size);


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
 *
 * XXX: space_finalize_kernel() might be removed. update this bit to match
 * once it's been decided.
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
