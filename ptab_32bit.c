
/* pagetable ops for the classic i386 two-level structure. */

#include <string.h>
#include <stdint.h>
#include <stdbool.h>
#include <assert.h>
#include <l4/types.h>

#include <ccan/likely/likely.h>

#include <ukernel/mm.h>
#include <ukernel/space.h>
#include <ukernel/mapdb.h>
#include <ukernel/rangealloc.h>
#include <ukernel/ptab.h>


void pt_iter_init(pt_iter_t *it, struct space *sp)
{
	assert(!ra_has_ptr(map_group_ra, sp));
	it->sp = sp;
}


void pt_iter_init_group(pt_iter_t *it, struct map_group *grp)
{
	assert(ra_has_ptr(map_group_ra, grp));
	assert(grp->ptab_page != NULL);
	it->grp = grp;
}


struct space *pti_space(const pt_iter_t *it) {
	return ra_has_ptr(map_group_ra, it->grp) ? it->grp->space : it->sp;
}


static uint32_t *_pt_ptab(pt_iter_t *it, uintptr_t addr)
{
	if(likely(!ra_has_ptr(map_group_ra, it->grp))
		&& (addr & ~GROUP_MASK) == it->grp->addr)
	{
		if(unlikely(it->grp->ptab_page->vm_addr == NULL)) {
			return map_vm_page(it->grp->ptab_page, VM_SYSCALL);
		} else {
			return it->grp->ptab_page->vm_addr;
		}
	} else {
		struct space *sp = ra_has_ptr(map_group_ra, it->grp)
			? it->grp->space : it->sp;
		struct map_group *grp = find_group(sp, addr);
#ifndef NDEBUG
		/* verify link in @sp->pdirs for @grp. */
		uint32_t *pdir_mem = sp->pdirs->vm_addr;
		if(pdir_mem == NULL) pdir_mem = map_vm_page(sp->pdirs, VM_SYSCALL);
		uint32_t pde = pdir_mem[addr >> 22];
		assert((pde >> 12) == grp->ptab_page->id);
#endif
		it->grp = grp;
		assert(ra_has_ptr(map_group_ra, it->grp));
		return map_vm_page(grp->ptab_page, VM_SYSCALL);
	}
}


static void touch(pt_iter_t *it, uintptr_t addr)
{
	assert(ra_has_ptr(map_group_ra, it->grp));
	if(it->grp->space == current_space) {
		/* (INVLPG is valid since the 80486, and this microkernel won't
		 * support the 386, so no specialcasing.)
		 */
		asm volatile ("invlpg (%0)"
			:: "r" (addr - KERNEL_SEG_START)
			: "memory");
	}
}


uint32_t pt_probe(
	pt_iter_t *it,
	int *access_p, int *side_access_p,
	uintptr_t addr, bool is_up, int below)
{
	uint32_t *ptab_mem = _pt_ptab(it, addr);
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

		/* change of access or ignored bits always requires TLB reload lest
		 * access be not updated or the ignored bits stomped.
		 */
		touch(it, addr);
	}

	return pte >> 12;
}


bool pt_set_page(pt_iter_t *it, uintptr_t addr, uint32_t pgid, int rights)
{
	uint32_t *ptab_mem = _pt_ptab(it, addr);
	uint32_t *pte = &ptab_mem[(addr >> 12) & 0x3ff];
	assert(L4_Writable == PT_RW);
	*pte = pgid << 12 | PT_USER | PT_PRESENT | (rights & L4_Writable);
	touch(it, addr);
	return true;
}


void pt_set_rights(pt_iter_t *it, uintptr_t addr, int rights)
{
	uint32_t *ptab_mem = _pt_ptab(it, addr);
	uint32_t *pte = &ptab_mem[(addr >> 12) & 0x3ff];
	*pte = (*pte & ~(PT_PRESENT | PT_RW))
		| (rights & L4_Writable)
		| (CHECK_FLAG_ANY(rights, L4_FullyAccessible) ? PT_PRESENT : 0);
	touch(it, addr);
}


void pt_clear_page(pt_iter_t *it, uintptr_t addr)
{
	uint32_t *ptab_mem = _pt_ptab(it, addr);
	ptab_mem[(addr >> 12) & 0x3ff] = 0;
	touch(it, addr);
}


bool pt_page_present(pt_iter_t *it, uintptr_t addr)
{
	const uint32_t *ptab_mem = _pt_ptab(it, addr);
	return CHECK_FLAG(ptab_mem[(addr >> 12) & 0x3ff], PT_PRESENT);
}
