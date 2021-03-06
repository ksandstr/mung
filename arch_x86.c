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
#include <ukernel/sched.h>
#include <ukernel/x86.h>


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
	if(g->space == current_space) x86_flush_tlbs();

	free_kern_page(g->ptab_page);
	g->ptab_page = NULL;
}


void call_on_stack(void (*fn)(void *), void *stack)
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
int _deep_call(void (*fn)(void *), void *paramptr)
{
	void *stkbase = aligned_alloc(KERNEL_STACK_SIZE, KERNEL_STACK_SIZE);
	if(unlikely(stkbase == NULL)) return -ENOMEM;

	uintptr_t top = ((uintptr_t)stkbase + PAGE_SIZE - 16) & ~0xful;
	/* FIXME: there may be some SSE fail in here. it should get fixed before
	 * something deep-called starts using SSE, such as memcpy() or some such.
	 */
	L4_Word_t *stk = (L4_Word_t *)top;
	*(--stk) = (L4_Word_t)paramptr;
	call_on_stack(fn, (void *)stk);

	free(stkbase);
	return 0;
}


bool is_stack_safe(size_t margin)
{
	uintptr_t e = (uintptr_t)&e,
		low = e & ~((uintptr_t)KERNEL_STACK_SIZE - 1);
	return e >= low + margin;
}


void save_user_ex(struct x86_exregs *regs)
{
	struct thread *c = get_current_thread();
	c->ctx.r = regs->r;
	c->ctx.r.esp = regs->esp;
	c->ctx.eip = regs->eip;
	c->ctx.eflags = regs->eflags;
	assert(!CHECK_FLAG(regs->eflags, 1 << 14));	/* NT */
	assert(CHECK_FLAG(regs->eflags, 1 << 9));	/* IF */
}


void save_user_regs(struct x86_regs *regs)
{
	struct thread *c = get_current_thread();
	c->ctx.r.ebx = regs->ebx;
	c->ctx.r.esi = regs->esi;
	c->ctx.r.edi = regs->edi;
	c->ctx.r.ebp = regs->ebp;
	c->ctx.r.esp = regs->esp;
	assert(!CHECK_FLAG(c->ctx.eflags, 1 << 14));	/* NT */
	assert(CHECK_FLAG(c->ctx.eflags, 1 << 9));		/* IF */
}
