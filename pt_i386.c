
/* pagetable ops for the classic i386 two-level structure. */

#if !IN_PTAB_IMPL
#error "shouldn't compile this file outside <ukernel/ptab.h> inclusion"
#endif

#include <string.h>
#include <stdint.h>
#include <stdbool.h>
#include <l4/types.h>

#include <ukernel/mm.h>
#include <ukernel/space.h>
#include <ukernel/mapdb.h>


#define PT_UPPER_WIDTH 10
#define PT_UPPER_BITS 22
#define PT_UPPER_SIZE (1 << PT_UPPER_BITS)
#define PT_UPPER_MASK (PT_UPPER_SIZE - 1)

#define _PTAB_MASK ((1ul << 22) - 1)


#define MAX_INVLS 32		/* 128k of ops, then flush (TODO: not tuned!) */


struct pt_iter
{
	struct space *sp;
	uintptr_t ptab_addr;	/* 4M area covered by cur_ptab, or 0 if invalid */
	uint32_t *cur_ptab;		/* kernel-mapped page table */
	bool is_current;
	uint8_t n_invls;
};


static inline void pt_iter_init(struct pt_iter *it, struct space *sp)
{
	it->sp = sp;
	it->ptab_addr = 123;	/* ensures replacement */
	assert((it->ptab_addr & _PTAB_MASK) != 0);
	it->cur_ptab = NULL;
	it->n_invls = 0;
	it->is_current = (sp == current_space);
}


static inline void pt_iter_init_group(
	struct pt_iter *it, struct map_group *grp)
{
	assert(grp->ptab_page != NULL);
	*it = (struct pt_iter){
		.sp = grp->space, .n_invls = 0,
		.is_current = (grp->space == current_space),
		.ptab_addr = MG_START(grp) & ~_PTAB_MASK,
		.cur_ptab = map_vm_page(grp->ptab_page, VM_SYSCALL),
	};
}


static inline void pt_iter_destroy(struct pt_iter *it)
{
	if(it->n_invls >= MAX_INVLS) x86_flush_tlbs();
#ifndef NDEBUG
	memset(it, 0xbd, sizeof(*it));
#endif
}


static inline uint32_t *_pt_ptab(struct pt_iter *it, uintptr_t addr)
{
	if((addr & ~_PTAB_MASK) != it->ptab_addr) {
		uint32_t *tmp = x86_get_ptab(it->sp, addr);
		if(tmp == NULL) return NULL;
		it->ptab_addr = addr & ~_PTAB_MASK;
		it->cur_ptab = tmp;
	}

	return it->cur_ptab;
}


static inline void _pt_changed(struct pt_iter *it, uintptr_t addr)
{
	if(!it->is_current) return;
	if(it->n_invls < MAX_INVLS) {
		it->n_invls++;
		x86_invalidate_page(addr);
	}
}


static inline uint32_t pt_probe(
	struct pt_iter *it,
	int *access_p, int *side_access_p,
	uintptr_t addr, bool is_up, int below)
{
	uint32_t *ptab_mem = _pt_ptab(it, addr);
	if(unlikely(ptab_mem == NULL)) return 0;

	int ptab_ix = (addr >> 12) & 0x3ff;
	uint32_t pte = ptab_mem[ptab_ix];
	if(!CHECK_FLAG(pte, PT_PRESENT)) return 0;
	assert(pte != 0);

	if(access_p != NULL) {
		int rx = (pte & PT_ACCESSED) >> 3;
		rx |= rx >> 2;
		assert(!CHECK_FLAG(pte, PT_ACCESSED)
			|| rx == (L4_Readable | L4_eXecutable));
		assert(CHECK_FLAG(pte, PT_ACCESSED) || rx == 0);
		int w = (pte & PT_DIRTY) >> 5;
		assert(!CHECK_FLAG(pte, PT_DIRTY) || w == L4_Writable);
		assert(CHECK_FLAG(pte, PT_DIRTY) || w == 0);

		if(is_up) {
			/* clear access, accumulate it w/ @below into side. */
			ptab_mem[ptab_ix] = (pte & ~(PT_ACCESSED | PT_DIRTY))
				| (rx | w | below) << 9;
			*access_p = rx | w;
		} else {
			/* clear access, deliver and clear side. */
			ptab_mem[ptab_ix] = pte & ~(PT_ACCESSED | PT_DIRTY | 0xe00);
			*access_p = rx | w;
			*side_access_p = (pte & 0xe00) >> 9;
		}

		/* change of access or ignored bits always requires TLB reload. */
		_pt_changed(it, addr);
	}

	return pte >> 12;
}


static inline bool pt_set_page(
	struct pt_iter *it,
	uintptr_t addr, uint32_t pgid, int rights)
{
	uint32_t *ptab_mem = _pt_ptab(it, addr);
	assert(ptab_mem != NULL);
	uint32_t *pte = &ptab_mem[(addr >> 12) & 0x3ff];
	assert(L4_Writable == PT_RW);
	*pte = pgid << 12 | PT_USER | PT_PRESENT | (rights & L4_Writable);
	_pt_changed(it, addr);
	return true;
}


static inline void pt_set_rights(
	struct pt_iter *it, uintptr_t addr, int rights)
{
	uint32_t *ptab_mem = _pt_ptab(it, addr);
	assert(ptab_mem != NULL);
	uint32_t *pte = &ptab_mem[(addr >> 12) & 0x3ff];
	*pte = (*pte & ~(PT_PRESENT | PT_RW))
		| (rights & L4_Writable)
		| (CHECK_FLAG_ANY(rights, L4_FullyAccessible) ? PT_PRESENT : 0);
	_pt_changed(it, addr);
}


static inline void pt_clear_page(struct pt_iter *it, uintptr_t addr)
{
	uint32_t *ptab_mem = _pt_ptab(it, addr);
	assert(ptab_mem != NULL);
	ptab_mem[(addr >> 12) & 0x3ff] = 0;
	_pt_changed(it, addr);
}


static inline bool pt_page_present(struct pt_iter *it, uintptr_t addr)
{
	const uint32_t *ptab_mem = _pt_ptab(it, addr);
	return likely(ptab_mem != NULL)
		&& CHECK_FLAG_ANY(ptab_mem[(addr >> 12) & 0x3ff], PT_PRESENT | PT_RW);
}
