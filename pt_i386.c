
/* pagetable ops for the classic i386 two-level structure. */

#if !IN_PTAB_IMPL
#error "shouldn't compile this file outside <ukernel/ptab.h> inclusion"
#endif

#include <string.h>
#include <stdint.h>
#include <stdbool.h>
#include <l4/types.h>

#include <ukernel/mm.h>


#define PT_UPPER_WIDTH 10
#define PT_UPPER_BITS 22
#define PT_UPPER_SIZE (1 << PT_UPPER_BITS)
#define PT_UPPER_MASK (PT_UPPER_SIZE - 1)

#define _PTAB_MASK ((1ul << 22) - 1)


struct pt_iter
{
	struct space *sp;
	uintptr_t ptab_addr;	/* 4M area covered by cur_ptab, or 0 if invalid */
	struct page *cur_ptab;	/* from space->ptab_pages; NULL if absent */
};


static inline void pt_iter_init(struct pt_iter *it, struct space *sp)
{
	it->sp = sp;
	it->ptab_addr = 123;	/* ensures replacement */
	assert((it->ptab_addr & _PTAB_MASK) != 0);
	it->cur_ptab = NULL;
}


static inline void pt_iter_destroy(struct pt_iter *it)
{
#ifndef NDEBUG
	memset(it, 0, sizeof(*it));
#endif
}


static inline uint32_t pt_get_pgid(
	struct pt_iter *it,
	bool *upper_present_p,
	uintptr_t addr)
{
	if((addr & ~_PTAB_MASK) != it->ptab_addr) {
		it->ptab_addr = addr & ~_PTAB_MASK;
		it->cur_ptab = x86_get_ptab(it->sp, addr);
	}
	if(upper_present_p != NULL) {
		*upper_present_p = it->cur_ptab != NULL;
		assert(*upper_present_p == pt_upper_present(it, it->ptab_addr));
	}
	if(it->cur_ptab == NULL) return 0;

	uint32_t *ptab_mem = it->cur_ptab->vm_addr;
	assert(ptab_mem != NULL);
	uint32_t pte = ptab_mem[(addr >> 12) & 0x3ff];

	return CHECK_FLAG(pte, PT_PRESENT) ? pte >> 12 : 0;
}


static inline bool pt_upper_present(const struct pt_iter *it, uintptr_t addr)
{
	const uint32_t *pdir_mem = it->sp->pdirs->vm_addr;
	assert(pdir_mem != NULL);
	bool ret = CHECK_FLAG(pdir_mem[addr >> 22], PDIR_PRESENT);
	assert(!ret || x86_get_ptab(it->sp, addr) != NULL);
	assert(ret  || x86_get_ptab(it->sp, addr) == NULL);
	return ret;
}
