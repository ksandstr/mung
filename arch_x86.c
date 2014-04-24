
/* x86 (i386, ia32) architecture support. */

#include <stdint.h>
#include <stdbool.h>
#include <assert.h>
#include <ccan/htable/htable.h>

#include <ukernel/mm.h>
#include <ukernel/space.h>
#include <ukernel/util.h>
#include <ukernel/x86.h>


/* get-cmp function for struct space's ptab_pages
 *
 * NOTE: copypasta'd from space.c! it should be changed to use the
 * <ukernel/ptab.h> functions as well, and this one canonicalized.
 */
static bool cmp_page_id_to_key(const void *cand, void *key) {
	const struct page *pg = cand;
	return pg->id == *(uint32_t *)key;
}


/* TODO: rename these and #if them out when the two-level page table isn't
 * used.
 */
struct page *x86_get_ptab(struct space *sp, uintptr_t ptab_addr)
{
	uint32_t *pdir_mem = sp->pdirs->vm_addr;
	assert(pdir_mem != NULL);
	uint32_t pde = pdir_mem[ptab_addr >> 22];
	if(!CHECK_FLAG(pde, PDIR_PRESENT)) {
		return NULL;
	} else {
		uint32_t pgid = pde >> 12;
		return htable_get(&sp->ptab_pages, int_hash(pgid),
			&cmp_page_id_to_key, &pgid);
	}
}


struct page *x86_alloc_ptab(struct space *sp, uintptr_t ptab_addr)
{
	assert(x86_get_ptab(sp, ptab_addr) == NULL);

	uint32_t *pdir_mem = sp->pdirs->vm_addr;
	assert(pdir_mem != NULL);
	uint32_t *pde = &pdir_mem[ptab_addr >> 22];
	assert(!CHECK_FLAG(*pde, PDIR_PRESENT));
	struct page *pg = get_kern_page(0);
	if(pg == NULL) return NULL;
	if(!htable_add(&sp->ptab_pages, int_hash(pg->id), pg)) {
		free_kern_page(pg);
		return NULL;
	}

	assert(pg->vm_addr != NULL);
	memset(pg->vm_addr, 0, PAGE_SIZE);
	*pde = pg->id << 12 | PDIR_PRESENT | PDIR_USER | PDIR_RW;

	return pg;
}
