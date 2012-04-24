
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
#include <ukernel/cpu.h>
#include <ukernel/kip.h>
#include <ukernel/util.h>
#include <ukernel/misc.h>


static struct thread *get_thread_pager(struct thread *t, void *utcb)
{
	L4_ThreadId_t pager_id = { .raw = L4_VREG(utcb, L4_TCR_PAGER) };
	return !L4_IsNilThread(pager_id) ? thread_find(pager_id.raw) : NULL;
}


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
	put_supervisor_page(heap_addr, mapdb_page_id_in_entry(e, regs->eip));
	int offset = regs->eip & PAGE_MASK;
	const uint8_t *mem = (void *)heap_addr;
	if(mem[offset] == 0xf0 && mem[offset + 1] == 0x90) {
		/* it is L4_KernelInterface().
		 * FIXME: proper values
		 */
		regs->eip += 2;
		regs->eax = L4_Address(current->space->kip_area);
		/* TODO: replace these with proper KIP accessors */
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
	static void (*const fn[])(struct x86_exregs *regs) = {
		[SC_IPC] = &sys_ipc,
		[SC_LIPC] = &sys_ipc,
		[SC_UNMAP] = &sys_unmap,
		[SC_THREADSWITCH] = &sys_threadswitch,
		[SC_SCHEDULE] = &sys_schedule,
		[SC_SPACECONTROL] = &sys_spacecontrol,
		[SC_THREADCONTROL] = &sys_threadcontrol,
		[SC_PROCESSORCONTROL] = &sys_processorcontrol,
	};

	int sc_num = regs->ebx;
	const int num_sc = sizeof(fn) / sizeof(fn[0]);
	if(unlikely(sc_num >= num_sc) || unlikely(fn[sc_num] == NULL)) {
		struct thread *current = get_current_thread();
		printf("unknown basic syscall %d (caller stopped)\n", regs->eax);
		current->status = TS_STOPPED;
		return_to_scheduler(regs);
	} else {
		(*fn[sc_num])(regs);
	}
}


void isr_exn_exregs_sc_bottom(struct x86_exregs *regs)
{
	regs->eax = sys_exregs((L4_ThreadId_t)regs->eax, &regs->ecx,
		&regs->edx, &regs->esi, &regs->edi, &regs->ebx,
		(L4_ThreadId_t *)&regs->ebp);
}


void isr_exn_memctl_sc_bottom(struct x86_exregs *regs)
{
	printf("%s: MemoryControl called\n", __func__);
}


static void handle_kdb_enter(struct thread *current, struct x86_exregs *regs)
{
	/* int3, i.e. KDB enter. there's a message we should decode. */
	const struct map_entry *e = mapdb_probe(&current->space->mapdb,
		regs->eip & ~PAGE_MASK);
	if(e == NULL) {
		printf("KDB callsite not mapped? what.\n");
		goto msgfail;
	}

	/* get string address from succeeding "MOV EAX, imm32" instruction. */
	uintptr_t heap_addr = reserve_heap_page();
	put_supervisor_page(heap_addr, mapdb_page_id_in_entry(e, regs->eip));
	int offset = regs->eip & PAGE_MASK;
	if(((const uint8_t *)heap_addr)[offset + 3] != 0xb8) {
		printf("KDB message not indicated with MOV EAX, imm32!\n");
		goto msgfail;
	}
	L4_Word_t strptr = *(const uint32_t *)(heap_addr + offset + 4);

	/* now crawl out at most 256 bytes of debug crap, not crossing a page
	 * boundary.
	 */
	e = mapdb_probe(&current->space->mapdb, strptr & ~PAGE_MASK);
	if(e == NULL) {
		printf("KDB message not mapped\n");
		goto msgfail;
	}
	put_supervisor_page(heap_addr, mapdb_page_id_in_entry(e, strptr));
	x86_flush_tlbs();
	assert((heap_addr & PAGE_MASK) == 0);
	const char *str = (void *)(heap_addr | (strptr & PAGE_MASK));
	char buf[257];
	memset(buf, 0, sizeof(buf));
	strlcpy(buf, str, MIN(size_t, 256, PAGE_SIZE - (strptr & PAGE_MASK)));
	printf("#KDB (eip %#x): [%#x] %s\n", regs->eip, strptr, buf);

msgfail:
	current->status = TS_STOPPED;
	return_to_scheduler(regs);
}


static void handle_io_fault(struct thread *current, struct x86_exregs *regs)
{
	thread_save_exregs(current, regs);

	printf("I/O fault in %d:%d\n", TID_THREADNUM(current->id),
		TID_VERSION(current->id));

	uint8_t insn[16];
	size_t n = space_memcpy_from(current->space, insn, regs->eip, 16);
#if 0
	printf("insn bytes [%u]:", (unsigned)n);
	for(int i=0; i < n; i++) printf(" %#02x", insn[i]);
	printf("\n");
#endif
	if(n == 0) {
		printf("can't read instructions at %#x; stopping thread\n",
			regs->eip);
		goto fail;
	}

	bool in;
	int port, size;
	switch(insn[0]) {
		case 0xe4:	/* IN AL, imm8 */
			port = insn[1];
			size = 1;
			in = true;
			break;

		case 0xe5:	/* IN AX, imm8 */
			port = insn[1];
			size = 2;
			in = true;
			/* FIXME: recognize IN EAX, imm8 also! */
			break;

		case 0xec:	/* IN AL, DX */
			port = regs->edx & 0xffff;
			size = 1;
			in = true;
			break;

		case 0xed:	/* IN AX, DX */
			port = regs->edx & 0xffff;
			size = 2;
			in = true;
			/* FIXME: recognize IN EAX, DX also! */
			break;

		default:
			printf("unknown instruction %#02x in I/O fault at %#x\n",
				insn[0], regs->eip);
			goto fail;
	}

	printf("#GP(IO): I/O fault; %s size %d in port %#x at eip %#x\n",
		in ? "in" : "out", size, port, regs->eip);
	void *utcb = thread_get_utcb(current);
	struct thread *pager = get_thread_pager(current, utcb);
	if(pager == NULL) goto fail;
	save_ipc_regs(current, 3, 1);
	L4_VREG(utcb, L4_TCR_BR(0)) = L4_CompleteAddressSpace.raw;
	L4_VREG(utcb, L4_TCR_MR(0)) = ((-8) & 0xfff) << 20 | 0x6 << 16 | 2;
	L4_VREG(utcb, L4_TCR_MR(1)) = L4_IoFpage(port, size).raw;
	L4_VREG(utcb, L4_TCR_MR(2)) = regs->eip;
	return_to_ipc(regs, pager);

	return;

fail:
	current->status = TS_STOPPED;
	return_to_scheduler(regs);
}


void isr_exn_gp_bottom(struct x86_exregs *regs)
{
	struct thread *current = get_current_thread();

	if(regs->error == 0) {
		handle_io_fault(current, regs);
	} else if(regs->error == 0x1a) {
		handle_kdb_enter(current, regs);
	} else {
		printf("#GP(0x%x) at eip 0x%x, esp 0x%x in %d:%d\n", regs->error,
			regs->eip, regs->esp, TID_THREADNUM(current->id),
			TID_VERSION(current->id));

		/* TODO: try to emit an exception message to the thread's exception
		 * handler
		 */

		current->status = TS_STOPPED;
		return_to_scheduler(regs);
	}
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

#ifndef NDEBUG
	static uintptr_t last_fault = ~0;
	static int repeat_count = 0;
	if(last_fault == fault_addr && ++repeat_count == 3) {
		printf("WARNING: faulted many times on the same address %#x\n",
			fault_addr);
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
		space_put_page(current->space, fault_addr,
			mapdb_page_id_in_entry(e, fault_addr), L4_Rights(e->range));
		space_commit(current->space);
	} else {
#ifndef NDEBUG
		repeat_count = 0;	/* sufficiently userspacey. */
#endif

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
