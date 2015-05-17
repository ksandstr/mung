/* x86 (i386, ia32) architecture support. */

#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include <assert.h>
#include <errno.h>

#include <ccan/htable/htable.h>
#include <ccan/likely/likely.h>

#include <ukernel/space.h>
#include <ukernel/mapdb.h>
#include <ukernel/x86.h>


uint32_t *x86_get_ptab(struct space *sp, uint32_t page_addr)
{
	uint32_t addr = page_addr & ~(PAGE_SIZE * MAX_ENTRIES_PER_GROUP - 1);
	struct map_group *g = htable_get(&sp->ptab_groups, int_hash(addr),
		&cmp_group_addr, &addr);
	if(unlikely(g == NULL) || g->ptab_page == NULL) return NULL;

#ifndef NDEBUG
	uint32_t *pdir_mem = sp->pdirs->vm_addr;
	if(pdir_mem == NULL) pdir_mem = map_vm_page(sp->pdirs, VM_SYSCALL);
	uint32_t pde = pdir_mem[page_addr >> 22];
	assert((pde >> 12) == g->ptab_page->id);
#endif

	return map_vm_page(g->ptab_page, VM_SYSCALL);
}


int x86_alloc_ptab(struct map_group *g)
{
	struct space *sp = g->space;

	pdir_t *dirs = sp->pdirs->vm_addr;
	assert(dirs != NULL);
	uint32_t *pde = &dirs[(MG_START(g) >> 22) & 0x3ff];
	assert(g->ptab_page == NULL);
	assert(!CHECK_FLAG(*pde, PDIR_PRESENT));

	g->ptab_page = get_kern_page(0);
	if(unlikely(g->ptab_page == NULL)) return -ENOMEM;
	uint32_t *pt = map_vm_page(g->ptab_page, VM_SYSCALL);
	for(int i=0; i < 1024; i++) pt[i] = 0;

	*pde = g->ptab_page->id << 12 | PDIR_PRESENT | PDIR_USER | PDIR_RW;

	return 0;
}


void x86_free_ptab(struct map_group *g)
{
	if(g->ptab_page == NULL) return;

	pdir_t *dirs = map_vm_page(g->space->pdirs, VM_SYSCALL);
	assert(dirs != NULL);
	uint32_t *pde = &dirs[(MG_START(g) >> 22) & 0x3ff];
	assert(CHECK_FLAG(*pde, PDIR_PRESENT));
	assert(g->ptab_page->id == *pde >> 12);
	*pde = 0;

	free_kern_page(g->ptab_page);
	g->ptab_page = NULL;
}


/* FIXME: duplicated from irq.c! move this off somewhere they have in
 * common.
 */
static inline void call_on_stack(void (*fn)(void *), void *stack)
{
	/* this trick relies on ebx being a callee-saved register. */
	asm volatile (
		"\txchgl %%ebx, %%esp\n"
		"\tcall *%%edi\n"
		"\tmovl %%ebx, %%esp\n"
		: "=b" (stack)
		: "0" (stack), "D" (fn)
		: "memory", "cc", "edx", "ecx", "eax");
}


/* this low-level stuff doesn't have any better place to be in; and anyway,
 * thread stacks are per-architecture.
 */
void deep_call(void (*fn)(void *), void *paramptr)
{
	static int cur_depth = 0;

	if(cur_depth > 50) {
		printf("%s: limit reached\n", __func__);
		return;
	}

	void *stkbase = valloc(PAGE_SIZE);
	if(stkbase == NULL) panic("deep_call: can't allocate next stack");

	uintptr_t top = ((uintptr_t)stkbase + PAGE_SIZE - 16) & ~0xful;
	/* FIXME: there may be some SSE fail in here. it should get fixed before
	 * something deep-called starts using SSE, such as memcpy() or some such.
	 */
	L4_Word_t *stk = (L4_Word_t *)top;
	*(--stk) = (L4_Word_t)paramptr;

	// printf("%s: new stkbase=%p, stk=%p\n", __func__, stkbase, stk);
	cur_depth++;
	call_on_stack(fn, (void *)stk);
	cur_depth--;

	free(stkbase);
}


bool is_stack_safe(size_t margin)
{
	uintptr_t e = (uintptr_t)&e,
		low = e & ~((uintptr_t)PAGE_SIZE - 1);
	return e >= low + margin;
}
