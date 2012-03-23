
#include <stdio.h>
#include <stdint.h>
#include <ccan/likely/likely.h>

#include <l4/types.h>
#include <l4/vregs.h>

#include <ukernel/x86.h>
#include <ukernel/thread.h>
#include <ukernel/space.h>
#include <ukernel/ipc.h>
#include <ukernel/syscall.h>
#include <ukernel/misc.h>


void isr_exn_de_bottom(struct x86_exregs *regs)
{
	printf("#DE(0x%x) at eip 0x%x, esp 0x%x\n", regs->error,
		regs->eip, regs->esp);
	panic("#DE");
}


void isr_exn_ud_bottom(struct x86_exregs *regs)
{
	/* see if it's a LOCK NOP. (this is why the "syscall" is so slow.) */

	/* NOTE: could extend the kernel data segment to the full address space,
	 * and wrap the user-space pointer. that'd remove the farting around with
	 * the mapping database, page tables, and supervisor space.
	 */
	struct thread *current = get_current_thread();
	struct space *sp = current->space;
	const struct map_entry *e = mapdb_probe(&sp->mapdb,
		regs->eip & ~PAGE_MASK);
	if(e == NULL) panic("no map_entry for #UD eip? what.");
	uintptr_t heap_addr = reserve_heap_page();
	put_supervisor_page(heap_addr, e->page_id);
	int offset = regs->eip & PAGE_MASK;
	const uint8_t *mem = (void *)heap_addr;
	if(mem[offset] == 0xf0 && mem[offset + 1] == 0x90) {
		/* it is L4_KernelInterface().
		 * FIXME: proper values
		 */
		regs->eip += 2;
		regs->eax = L4_Address(current->space->kip_area);
		/* TODO: replace these with proper KIP accessors */
		const void *kip_mem = kip_page->vm_addr;
		regs->ecx = *(L4_Word_t *)(kip_mem + 0x04);		/* API VERSION */
		regs->edx = *(L4_Word_t *)(kip_mem + 0x08);		/* API FLAGS */
		/* id = 23 (because 2 + 3 = 5); subid = 17
		 * FIXME: get proper values at some point.
		 */
		regs->esi = (23 << 24) | (17 << 16);	/* KERNEL ID */
	} else {
		printf("#UD at eip 0x%x, esp 0x%x\n", regs->eip, regs->esp);
		/* TODO: pop an "invalid opcode" exception. */
		current->status = TS_STOPPED;
		return_to_scheduler(regs);
	}

	put_supervisor_page(heap_addr, 0);
	/* TODO: release heap_addr */
}


/* basically the slowest possible syscall wrappers. */
void isr_exn_basic_sc_bottom(struct x86_exregs *regs)
{
	switch(regs->ebx) {
		case SC_IPC:
		case SC_LIPC:		/* TODO: special implementation of LIPC */
			ipc_syscall(regs);
			break;

		default: {
			struct thread *current = get_current_thread();
			printf("unimplemented basic syscall %d (caller stopped)\n",
				regs->eax);
			current->status = TS_STOPPED;
			return_to_scheduler(regs);
		}
	}
}


void isr_exn_exregs_sc_bottom(struct x86_exregs *regs)
{
	printf("%s: ExchangeRegisters called\n", __func__);
	regs->eax = sys_exregs((L4_ThreadId_t)regs->eax, &regs->ecx,
		&regs->edx, &regs->esi, &regs->edi, &regs->ebx,
		(L4_ThreadId_t *)&regs->ebp);
}


void isr_exn_memctl_sc_bottom(struct x86_exregs *regs)
{
	printf("%s: MemoryControl called\n", __func__);
}


void isr_exn_gp_bottom(struct x86_exregs *regs)
{
	printf("#GP(0x%x) at eip 0x%x, esp 0x%x\n", regs->error,
		regs->eip, regs->esp);

	get_current_thread()->status = TS_STOPPED;
	return_to_scheduler(regs);
}


void isr_exn_pf_bottom(struct x86_exregs *regs)
{
	uint32_t fault_addr;
	__asm__ __volatile__("\tmovl %%cr2, %0\n" : "=r" (fault_addr)
		:: "memory");

	struct thread *current = get_current_thread();

	if(unlikely(!CHECK_FLAG(regs->error, 4))
		&& fault_addr >= KERNEL_SEG_START)
	{
		printf("KERNEL #PF (%s, %s, %s) @ 0x%x (eip 0x%x); current thread %d:%d\n",
			CHECK_FLAG(regs->error, 4) ? "user" : "super",
			CHECK_FLAG(regs->error, 2) ? "write" : "read",
			CHECK_FLAG(regs->error, 1) ? "access" : "presence",
			fault_addr, regs->eip,
			TID_THREADNUM(current->id), TID_VERSION(current->id));
		panic("KERNEL #PF");
	}


#if 0
	static uintptr_t last_fault = ~0;
	static int repeat_count = 0;
	if(last_fault == fault_addr && ++repeat_count == 3) {
		printf("faulted many times on the same address\n");
		thread_save_exregs(current, regs);
		current->status = TS_STOPPED;
		return_to_scheduler(regs);
	} else if(last_fault != fault_addr) {
		last_fault = fault_addr;
		repeat_count = 0;
	}
#endif

	const struct map_entry *e = mapdb_probe(&current->space->mapdb,
		fault_addr);
	if(e != NULL) {
		space_put_page(current->space, fault_addr & ~PAGE_MASK,
			e->page_id, e->flags & 0x7);
		space_commit(current->space);
	} else {
		thread_save_exregs(current, regs);
		void *utcb = thread_get_utcb(current);
		L4_ThreadId_t pager_id = { .raw = L4_VREG(utcb, L4_TCR_PAGER) };
		struct thread *pager = likely(!L4_IsNilThread(pager_id))
			? thread_find(pager_id.raw) : NULL;
		if(unlikely(pager == NULL)) {
			printf("thread %d:%d has no pager, stopping it\n",
				TID_THREADNUM(current->id), TID_VERSION(current->id));
			current->status = TS_STOPPED;
			return_to_scheduler(regs);
		} else {
			save_ipc_regs(current, 3, 1);
			L4_VREG(utcb, L4_TCR_BR(0)) = L4_CompleteAddressSpace.raw;
			L4_VREG(utcb, L4_TCR_MR(0)) = ((-2) & 0xfff) << 20		/* label */
				| (CHECK_FLAG(regs->error, 2) ? 0x2 : 0x4) << 16	/* access */
				| 2;		/* "u" for msgtag */
			L4_VREG(utcb, L4_TCR_MR(1)) = fault_addr;
			L4_VREG(utcb, L4_TCR_MR(2)) = regs->eip;
			return_to_ipc(regs, pager);
		}
	}
}

