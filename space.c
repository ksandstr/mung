
#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include <assert.h>
#include <ccan/likely/likely.h>
#include <ccan/list/list.h>
#include <ccan/htable/htable.h>
#include <ccan/alignof/alignof.h>
#include <ccan/compiler/compiler.h>

#include <ukernel/mm.h>
#include <ukernel/slab.h>
#include <ukernel/misc.h>
#include <ukernel/space.h>


struct space *kernel_space = NULL;
static struct space kernel_space_mem;

static struct kmem_cache *space_slab = NULL;
static struct list_head space_list = LIST_HEAD_INIT(space_list);


static size_t hash_page_by_id(const void *page_ptr, void *priv)
{
	const struct page *p = page_ptr;
	return int_hash(p->id);
}


static void space_init(struct space *sp, struct list_head *resv_list)
{
	list_head_init(&sp->threads);
	htable_init(&sp->ptab_pages, &hash_page_by_id, NULL);

	sp->pdirs = get_kern_page();
	if(unlikely(resv_list != NULL)) list_add(resv_list, &sp->pdirs->link);
	else htable_add(&sp->ptab_pages, int_hash(sp->pdirs->id), sp->pdirs);
	pdir_t *dirs = sp->pdirs->vm_addr;
	if(unlikely(resv_list != NULL)) {
		/* creation of the kernel's <struct space>. */
		memset(dirs, '\0', PAGE_SIZE);
	} else {
		const pdir_t *kernel_pdirs = kernel_space->pdirs->vm_addr;
		for(int i=0; i < 1024; i++) {
			if(i < KERNEL_SEG_START >> 22) dirs[i] = 0;
			else dirs[i] = kernel_pdirs[i];
		}
	}

	list_add(&space_list, &sp->link);
}


struct space *space_new(void)
{
	assert(space_slab != NULL);

	struct space *sp = kmem_cache_alloc(space_slab);
	space_init(sp, NULL);

	return sp;
}


void space_free(struct space *sp)
{
	assert(list_empty(&sp->threads));

	struct htable_iter it;
	for(struct page *p = htable_first(&sp->ptab_pages, &it);
		p != NULL;
		p = htable_next(&sp->ptab_pages, &it))
	{
		htable_delval(&sp->ptab_pages, &it);
		free_kern_page(p);
	}
	assert(sp->ptab_pages.elems == 0);
	htable_clear(&sp->ptab_pages);

	list_del_from(&space_list, &sp->link);

	kmem_cache_free(space_slab, sp);
}


/* NOTE: this runs in the pre-heap environment. so htable ops aren't
 * available; instead the pages end up in the list given as parameter.
 */
COLD void init_spaces(struct list_head *resv_list)
{
	memset(&kernel_space_mem, 0, sizeof(kernel_space_mem));
	kernel_space = &kernel_space_mem;
	space_init(kernel_space, resv_list);

	/* preallocate page table pages for the kernel segment, so that it makes
	 * sense to copy the page table pointers.
	 */
	pdir_t *pdirs = kernel_space->pdirs->vm_addr;
	for(int i = KERNEL_SEG_START >> 22; i < 1024; i++) {
		struct page *pg = get_kern_page();
		pdirs[i] = pg->id << 12 | PDIR_PRESENT | PDIR_RW;
		list_add(resv_list, &pg->link);
		memset(pg->vm_addr, 0, PAGE_SIZE);
	}

	/* module inits */
	space_slab = kmem_cache_create("space_slab", sizeof(struct space),
		ALIGNOF(struct space), 0, NULL, NULL);
}


COLD void space_add_resv_pages(
	struct space *sp,
	struct list_head *resv_list)
{
	struct page *p;
	list_for_each(resv_list, p, link) {
		htable_add(&sp->ptab_pages, int_hash(p->id), p);
	}
}
