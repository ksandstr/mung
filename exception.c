
#include <stdio.h>
#include <stdint.h>
#include <ccan/likely/likely.h>

#include <l4/types.h>
#include <l4/vregs.h>

#include <ukernel/x86.h>
#include <ukernel/thread.h>
#include <ukernel/sched.h>
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
	printf("#DE(0x%lx) at eip 0x%lx, esp 0x%lx\n", regs->error,
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
	uint8_t buf[2];
	size_t n = space_memcpy_from(current->space, buf, regs->eip, 2);
	if(n < 2) {
		panic("can't read from #UD eip? what.");
	}
	if(buf[0] == 0xf0 && buf[1] == 0x90) {
		/* it is L4_KernelInterface().
		 * TODO: proper values
		 */
		regs->eip += 2;
		regs->eax = L4_Address(current->space->kip_area);
		/* TODO: replace these with proper KIP accessors */
		regs->ecx = *(L4_Word_t *)(kip_mem + 0x04);		/* API VERSION */
		regs->edx = *(L4_Word_t *)(kip_mem + 0x08);		/* API FLAGS */
		/* id = 23 (because 2 + 3 = 5); subid = 17
		 * TODO: get proper values at some point.
		 */
		regs->esi = (23 << 24) | (17 << 16);	/* KERNEL ID */
	} else {
		printf("#UD at eip 0x%lx, esp 0x%lx\n", regs->eip, regs->esp);
		/* TODO: pop an "invalid opcode" exception. */
		thread_halt(current);
		assert(current->status == TS_STOPPED);
		return_to_scheduler();
	}
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
		printf("unknown basic syscall %lu (caller stopped)\n", regs->eax);
		thread_halt(current);
		assert(current->status == TS_STOPPED);
		return_to_scheduler();
	} else {
		assert(x86_frame_len(regs) == sizeof(*regs));
		(*fn[sc_num])(regs);
	}
}


void isr_exn_exregs_sc_bottom(struct x86_exregs *regs)
{
	regs->eax = sys_exregs((L4_ThreadId_t){ .raw = regs->eax },
		&regs->ecx, &regs->edx, &regs->esi, &regs->edi, &regs->ebx,
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
		/* TODO: cause the #GP exception for int $3 instead */
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
	printf("#KDB (eip %#lx): [%#lx] %s\n", regs->eip, strptr, buf);

msgfail:
	thread_halt(current);
	assert(current->status == TS_STOPPED);
	return_to_scheduler();
}


static void handle_io_fault(struct thread *current, struct x86_exregs *regs)
{
	if(IS_KERNEL_THREAD(current)) panic("kernel IO fault!");
	thread_save_ctx(current, regs);

	uint8_t insn_buf[16], *insn = insn_buf;
	size_t n = space_memcpy_from(current->space, insn_buf, regs->eip, 16);
#if 0
	printf("insn bytes [%u]:", (unsigned)n);
	for(int i=0; i < n; i++) printf(" %#02x", insn[i]);
	printf("\n");
#endif
	if(n == 0) {
		printf("can't read instructions at %#lx; stopping thread\n",
			regs->eip);
		goto fail;
	}

	__attribute__((unused)) bool in = true;
	int port, size = 4;
	if(insn[0] == 0x66) {
		/* the "word-size" prefix */
		size = 2;
		insn++;
	}
	switch(insn[0]) {
		case 0xe4:	/* IN AL, imm8 */
			port = insn[1];
			size = 1;
			break;

		case 0xe5:	/* IN [E]AX, imm8 */
			port = insn[1];
			break;

		case 0xe6:	/* OUT imm8, AL */
			port = insn[1];
			size = 1;
			in = false;
			break;

		case 0xe7:	/* OUT imm8, [E]AX */
			port = insn[1];
			in = false;
			break;

		case 0xec:	/* IN AL, DX */
			port = regs->edx & 0xffff;
			size = 1;
			break;

		case 0xed:	/* IN [E]AX, DX */
			port = regs->edx & 0xffff;
			break;

		case 0xee:	/* OUT DX, AL */
			port = regs->edx & 0xffff;
			size = 1;
			in = false;
			break;

		case 0xef:	/* OUT DX, [E]AX */
			port = regs->edx & 0xffff;
			in = false;
			break;

		/* TODO: string variants */

		default:
			printf("unknown instruction %#02x in I/O fault at %#lx\n",
				insn[0], regs->eip);
			goto fail;
	}

#if 0
	/* TODO: make this a TRACE() */
	printf("#GP(IO): I/O fault; %s size %d in port %#lx at eip %#lx\n",
		in ? "in" : "out", size, port, regs->eip);
#endif

#if 0
	/* debugging aid for space_add_ioperm() */
	if(!space_add_ioperm(current->space, port, size)) {
		panic("couldn't add IO permissions");
	}
#else
	void *utcb = thread_get_utcb(current);
	struct thread *pager = get_thread_pager(current, utcb);
	if(pager == NULL) goto fail;
	save_ipc_regs(current, 3, 1);
	L4_VREG(utcb, L4_TCR_BR(0)) = L4_IoFpageLog2(0, 16).raw;
	L4_VREG(utcb, L4_TCR_MR(0)) = ((-8) & 0xfff) << 20 | 0x6 << 16 | 2;
	L4_VREG(utcb, L4_TCR_MR(1)) = L4_IoFpage(port, size).raw;
	L4_VREG(utcb, L4_TCR_MR(2)) = regs->eip;
	return_to_ipc(pager);
#endif

	return;

fail:
	thread_halt(current);
	assert(current->status == TS_STOPPED);
	return_to_scheduler();
}


static void receive_exn_reply(struct hook *hook, uintptr_t code, void *priv)
{
	hook_detach(hook);
	if(code != 0) return;

	struct thread *t = container_of(hook, struct thread, post_exn_call);

	void *utcb = thread_get_utcb(t);
	struct x86_exregs *regs = &t->ctx;
	L4_Word_t eflags;
	L4_Word_t *exvarptrs[] = {
		&regs->eip,
		&eflags,
		&regs->reason,		/* ExceptionNo */
		&regs->error,
		&regs->edi,
		&regs->esi,
		&regs->ebp,
		&regs->esp,
		&regs->ebx,
		&regs->edx,
		&regs->ecx,
		&regs->eax,
	};
	int num_vars = sizeof(exvarptrs) / sizeof(exvarptrs[0]);
	assert(num_vars == 12);
	for(int i=0; i < num_vars; i++) {
		*(exvarptrs[i]) = L4_VREG(utcb, L4_TCR_MR(i + 1));
	}
	/* retain privileged EFLAGS bits from thread context. */
	regs->eflags = (regs->eflags & 0xffffff00) | (eflags & 0xff);
	/* set reserved bits correctly */
	regs->eflags &= 0xffffffd7;
	regs->eflags |= 0x00000002;
}


void build_exn_ipc(
	struct thread *t,
	void *utcb,
	int label,
	const struct x86_exregs *regs)
{
	assert(label < 0);
	assert(utcb != NULL);

	save_ipc_regs(t, 13, 1);
	L4_VREG(utcb, L4_TCR_BR(0)) = 0;
	L4_VREG(utcb, L4_TCR_MR(0)) = (L4_MsgTag_t){
		.X.label = (label & 0xfff) << 4, .X.u = 12 }.raw;
	L4_Word_t exvars[] = {
		regs->eip,
		regs->eflags,
		regs->reason,		/* ExceptionNo */
		regs->error,
		regs->edi,
		regs->esi,
		regs->ebp,
		regs->esp,
		regs->ebx,
		regs->edx,
		regs->ecx,
		regs->eax,
	};
	int num_vars = sizeof(exvars) / sizeof(exvars[0]);
	assert(num_vars == 12);
	for(int i=0; i < num_vars; i++) {
		L4_VREG(utcb, L4_TCR_MR(i + 1)) = exvars[i];
	}

	/* this should be called before the save_ipc_regs() hook restores those
	 * MRs we're interested in.
	 */
	hook_push_front(&t->post_exn_call, &receive_exn_reply, NULL);
}


struct thread *get_thread_exh(struct thread *t, void *utcb)
{
	assert(utcb != NULL);

	L4_ThreadId_t exh_tid = {
		.raw = L4_VREG(utcb, L4_TCR_EXCEPTIONHANDLER),
	};
	return L4_IsNilThread(exh_tid) ? NULL : thread_find(exh_tid.raw);
}


void isr_exn_gp_bottom(struct x86_exregs *regs)
{
	struct thread *current = get_current_thread();

	if(IS_KERNEL_THREAD(current)) {
		printf("KERNEL #GP(%#lx) at eip %#lx, esp %#lx in %lu:%lu\n",
			regs->error, regs->eip, regs->esp,
			TID_THREADNUM(current->id), TID_VERSION(current->id));
		thread_halt(current);
		assert(current->status == TS_STOPPED);
		return_to_scheduler();
		assert(false);
	}

	if(regs->error == 0) {
		handle_io_fault(current, regs);
	} else if(regs->error == 0x1a) {
		handle_kdb_enter(current, regs);
	} else {
		thread_save_ctx(current, regs);

#if 0
		printf("#GP(%#lx) at eip %#lx, esp %#lx in %lu:%lu\n", regs->error,
			regs->eip, regs->esp, TID_THREADNUM(current->id),
			TID_VERSION(current->id));
#endif

		void *utcb = thread_get_utcb(current);
		struct thread *exh = get_thread_exh(current, utcb);
		if(exh != NULL) {
			build_exn_ipc(current, utcb, -5, regs);
			return_to_ipc(exh);
		} else {
			thread_halt(current);
			assert(current->status == TS_STOPPED);
			return_to_scheduler();
		}
	}
}


void isr_exn_pf_bottom(struct x86_exregs *regs)
{
	L4_Word_t fault_addr;
	__asm__ __volatile__("\tmovl %%cr2, %0\n" : "=r" (fault_addr)
		:: "memory");

	struct thread *current = get_current_thread();

	if(unlikely(!CHECK_FLAG(regs->error, 4))
		&& fault_addr >= KERNEL_SEG_START)
	{
		printf("KERNEL #PF (%s, %s, %s) @ %#lx (eip %#lx); current thread %lu:%lu\n",
			CHECK_FLAG(regs->error, 4) ? "user" : "super",
			CHECK_FLAG(regs->error, 2) ? "write" : "read",
			CHECK_FLAG(regs->error, 1) ? "access" : "presence",
			fault_addr, regs->eip,
			TID_THREADNUM(current->id), TID_VERSION(current->id));
		panic("KERNEL #PF");
	}

	/* NOTE: due to the way caches work, a write fault should also be a read
	 * fault except where an existing map_entry already grants read access.
	 * likewise read access should promote to execute access until NX bits
	 * (and PTE support in general) come about.
	 */
	const int fault_access = CHECK_FLAG(regs->error, 2)
		? L4_Writable : L4_Readable;

#ifndef NDEBUG
	static uintptr_t last_fault = ~0;
	static int repeat_count = 0;
	if(last_fault == fault_addr && ++repeat_count == 3) {
		printf("WARNING: faulted many times on the same address %#lx\n",
			fault_addr);
		thread_save_ctx(current, regs);
		thread_halt(current);
		assert(current->status == TS_STOPPED);
		return_to_scheduler();
		assert(false);
	} else if(last_fault != fault_addr) {
		last_fault = fault_addr;
		repeat_count = 0;
	}
#endif

	const struct map_entry *e = mapdb_probe(&current->space->mapdb,
		fault_addr);
	if(e != NULL && CHECK_FLAG_ALL(L4_Rights(e->range), fault_access)) {
		space_put_page(current->space, fault_addr,
			mapdb_page_id_in_entry(e, fault_addr), L4_Rights(e->range));
		space_commit(current->space);
	} else {
#ifndef NDEBUG
		repeat_count = 0;	/* sufficiently userspacey. */
#endif

		thread_save_ctx(current, regs);
		void *utcb = thread_get_utcb(current);
		struct thread *pager = get_thread_pager(current, utcb);
		if(unlikely(pager == NULL)) {
			printf("thread %lu:%lu has no pager, stopping it\n",
				TID_THREADNUM(current->id), TID_VERSION(current->id));
			printf("  (fault was at %#lx, ip %#lx)\n", fault_addr, regs->eip);
			thread_halt(current);
			assert(current->status == TS_STOPPED);
			return_to_scheduler();
		} else {
			save_ipc_regs(current, 3, 1);
			L4_VREG(utcb, L4_TCR_BR(0)) = L4_CompleteAddressSpace.raw;
			L4_VREG(utcb, L4_TCR_MR(0)) = ((-2) & 0xfff) << 20		/* label */
				| (CHECK_FLAG(regs->error, 2) ? 0x2 : 0x4) << 16	/* access */
				| 2;		/* "u" for msgtag */
			L4_VREG(utcb, L4_TCR_MR(1)) = fault_addr;
			L4_VREG(utcb, L4_TCR_MR(2)) = regs->eip;
			return_to_ipc(pager);
		}
	}
}
