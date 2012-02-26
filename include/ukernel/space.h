
#ifndef SEEN_UKERNEL_SPACE_H
#define SEEN_UKERNEL_SPACE_H

#include <stdint.h>
#include <ccan/list/list.h>
#include <ccan/htable/htable.h>

#include <ukernel/thread.h>
#include <ukernel/mm.h>


struct space
{
	struct list_node link;		/* in the global space list */
	struct list_head threads;

	struct htable ptab_pages;	/* <struct page *>, by page.id */

	/* x86 specific bits */
	struct page *pdirs;
};


extern struct space *kernel_space;


extern struct space *space_new(void);
extern void space_free(struct space *sp);

/* pages reserved before htable_add() can be used, are added to the list.
 * later kmain() calls space_add_resv_pages().
 */
extern void init_spaces(struct list_head *resv_list);
extern void space_add_resv_pages(
	struct space *sp,
	struct list_head *resv_list);


#endif
