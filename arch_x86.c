/* x86 (i386, ia32) architecture support. */

#include <stdint.h>
#include <stdbool.h>
#include <assert.h>
#include <ccan/htable/htable.h>
#include <ccan/likely/likely.h>

#include <ukernel/mm.h>
#include <ukernel/space.h>
#include <ukernel/util.h>
#include <ukernel/x86.h>


/* get-cmp function for struct space's ptab_pages. */
static bool cmp_page_id_to_key(const void *cand, void *key) {
	const struct page *pg = cand;
	return pg->id == *(uint32_t *)key;
}


uint32_t *x86_get_ptab(struct space *sp, uint32_t page_addr)
{
	uint32_t *pdir_mem = sp->pdirs->vm_addr;
	if(unlikely(pdir_mem == NULL)) {
		pdir_mem = map_vm_page(sp->pdirs, VM_SYSCALL);
	}
	uint32_t pde = pdir_mem[page_addr >> 22];
	if(unlikely(!CHECK_FLAG(pde, PDIR_PRESENT))) return NULL;

	uint32_t pgid = pde >> 12;
	struct page *pg = htable_get(&sp->ptab_pages, int_hash(pgid),
		&cmp_page_id_to_key, &pgid);
	if(unlikely(pg == NULL)) return NULL;	/* doesn't even exist. */

	return map_vm_page(pg, VM_SYSCALL);
}


uint32_t *x86_alloc_ptab(struct space *sp, uintptr_t ptab_addr)
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

	return map_vm_page(pg, VM_SYSCALL);
}
