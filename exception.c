
#include <stdio.h>
#include <stdint.h>

#include <ccan/likely/likely.h>
#include <ccan/compiler/compiler.h>

#include <l4/types.h>
#include <l4/vregs.h>

#include <ukernel/x86.h>
#include <ukernel/thread.h>
#include <ukernel/sched.h>
#include <ukernel/space.h>
#include <ukernel/slab.h>
#include <ukernel/ipc.h>
#include <ukernel/interrupt.h>
#include <ukernel/syscall.h>
#include <ukernel/cpu.h>
#include <ukernel/kip.h>
#include <ukernel/util.h>
#include <ukernel/misc.h>


/* the thread the current FPU context belongs to. usually not NULL, but may
 * become so through a deleting ThreadControl.
 */
static struct thread *fpu_thread = NULL;

static struct kmem_cache *fpu_context_slab = NULL;
static void *next_fpu_context = NULL;


/* this must be the last line of an exception handler that doesn't call one of
 * the return_to_*() family of kernel exits. it tests for and handles latent
 * interrupts, restoring the interrupt flag from saved user context. (the
 * reason why return_to_*() callers don't need it is that return_to_*() family
 * already does this, as they must.)
 *
 * NOTE: this'll likely be expanded to a more general, preemption-handling ISR
 * exit path.
 *
 * NOTE: exported in <ukernel/sched.h> .
 */
void return_from_exn(void)
{
	x86_irq_disable();
	if(unlikely(kernel_irq_deferred)) {
		do {
			x86_irq_enable();
			if(int_latent()) {
				/* FIXME: or, like, pop off to another thread entirely? that
				 * could happen.
				 */
				printf("%s: preemption! (caller %p)\n", __func__,
					__builtin_return_address(0));
			}
			x86_irq_disable();
		} while(kernel_irq_deferred);
	}
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
		return_from_exn();
	} else {
		printf("#UD at eip 0x%lx, esp 0x%lx\n", regs->eip, regs->esp);
		/* TODO: pop an "invalid opcode" exception. */
		thread_halt(current);
		assert(current->status == TS_STOPPED);
		return_to_scheduler();
	}
}


COLD void cop_init(void)
{
	bool long_fsave = CPU_HAS_FXSR();
	fpu_context_slab = kmem_cache_create("fpuctx",
		long_fsave ? 512 : 108, 64, 0, NULL, NULL);

	if(CPU_HAS_FXSR() && CPU_HAS_SSE()) {
		/* set CR4.OSFXSR and CR4.OSXMMEXCPT */
		asm volatile (
			"movl %%cr4, %%eax\n"
			"orl $0x600, %%eax\n"
			"movl %%eax, %%cr4\n"
			::: "eax");
	}
}


void cop_switch(struct thread *next)
{
	assert(next != NULL);

	if(fpu_thread == next) {
		/* re-enable the unsaved context */
		x86_alter_cr0(~X86_CR0_TS, 0);
	} else {
		/* lazy transition, or initialization */
		x86_alter_cr0(~0ul, X86_CR0_TS);
		if(next_fpu_context == NULL) {
			next_fpu_context = kmem_cache_alloc(fpu_context_slab);
		}
	}
}


void cop_killa(struct thread *dead)
{
	if(fpu_thread == dead) {
		fpu_thread = NULL;
		x86_alter_cr0(~0ul, X86_CR0_TS);
	}
	if(dead->fpu_context != NULL) {
		if(next_fpu_context == NULL) {
			next_fpu_context = dead->fpu_context;
		} else {
			kmem_cache_free(fpu_context_slab, dead->fpu_context);
		}
	}

#ifndef NDEBUG
	dead->fpu_context = (void *)0xDEADBEEF;
#endif
}


/* device not available exception (fpu/mmx context switch) */
void isr_exn_nm_bottom(struct x86_exregs *regs)
{
	x86_alter_cr0(~X86_CR0_TS, 0);

	struct thread *current = get_current_thread(), *prev = fpu_thread;

	assert(prev == NULL || prev->fpu_context != (void *)0xDEADBEEF);
	if(prev != current && likely(prev != NULL)) {
		if(prev->fpu_context == NULL) {
			assert(next_fpu_context != NULL);
			prev->fpu_context = next_fpu_context;
			next_fpu_context = NULL;
		}

		if(CPU_HAS_FXSR()) {
			x86_fxsave(prev->fpu_context);
		} else {
			x86_fsave(prev->fpu_context);
		}
	}

	if(current->fpu_context != NULL) {
		assert(current->fpu_context != (void *)0xDEADBEEF);
		if(CPU_HAS_FXSR()) {
			x86_fxrstor(current->fpu_context);
		} else {
			x86_frstor(current->fpu_context);
		}
	} else {
		/* TODO: set rounding mode to truncate, etc. default FPU state */
	}

	fpu_thread = current;

	return_from_exn();
}


/* x87 fpu exceptions */
void isr_exn_mf_bottom(struct x86_exregs *regs)
{
	printf("#MF\n");
	thread_halt(get_current_thread());
	return_to_scheduler();
}


/* SSE fpu exceptions */
void isr_exn_xm_bottom(struct x86_exregs *regs)
{
	printf("#XM\n");
	thread_halt(get_current_thread());
	return_to_scheduler();
}


static void sys_unmap_wrap(struct x86_exregs *regs)
{
	L4_Word_t control = regs->eax;

	/* TODO: pass utcb to sys_unmap()? */
	void *utcb = thread_get_utcb(get_current_thread());
	if((control & 0x3f) > 0) L4_VREG(utcb, L4_TCR_MR(0)) = regs->esi;
	sys_unmap(control);

	regs->esi = L4_VREG(utcb, L4_TCR_MR(0));
}


static void sys_spacecontrol_wrap(struct x86_exregs *regs)
{
	regs->eax = sys_spacecontrol(
		(L4_ThreadId_t){ .raw = regs->eax },	/* spacespec */
		regs->ecx,								/* control */
		(L4_Fpage_t){ .raw = regs->edx },		/* kip_area */
		(L4_Fpage_t){ .raw = regs->esi },		/* utcb_area */
		(L4_ThreadId_t){ .raw = regs->edi },	/* redirector */
		&regs->ecx);							/* old_control */
}


/* basically the slowest possible syscall wrappers. */
void isr_exn_basic_sc_bottom(struct x86_exregs *regs)
{
	assert(x86_irq_is_enabled());

	static void (*const fn[])(struct x86_exregs *regs) = {
		[SC_IPC] = &sys_ipc,
		[SC_LIPC] = &sys_ipc,
		[SC_UNMAP] = &sys_unmap_wrap,
		[SC_THREADSWITCH] = &sys_threadswitch,
		[SC_SCHEDULE] = &sys_schedule,
		[SC_SPACECONTROL] = &sys_spacecontrol_wrap,
		[SC_THREADCONTROL] = &sys_threadcontrol,
		[SC_PROCESSORCONTROL] = &sys_processorcontrol,
	};

	int sc_num = regs->ebx;
	const int num_sc = sizeof(fn) / sizeof(fn[0]);
	if(unlikely(sc_num >= num_sc || fn[sc_num] == NULL)) {
		struct thread *current = get_current_thread();
		printf("unknown basic syscall %lu (caller stopped)\n", regs->eax);
		thread_halt(current);
		assert(current->status == TS_STOPPED);
		return_to_scheduler();
	} else {
		assert(x86_frame_len(regs) == sizeof(*regs));
		(*fn[sc_num])(regs);
		return_from_exn();
	}
}


void isr_exn_exregs_sc_bottom(struct x86_exregs *regs)
{
	assert(x86_irq_is_enabled());
	regs->eax = sys_exregs((L4_ThreadId_t){ .raw = regs->eax },
		&regs->ecx, &regs->edx, &regs->esi, &regs->edi, &regs->ebx,
		(L4_ThreadId_t *)&regs->ebp);
	return_from_exn();
}


void isr_exn_memctl_sc_bottom(struct x86_exregs *regs)
{
	assert(x86_irq_is_enabled());
	printf("%s: MemoryControl called\n", __func__);
	return_from_exn();
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
	assert(!IS_KERNEL_THREAD(current));
	thread_save_ctx(current, regs);

	uint8_t insn_buf[16], *insn = insn_buf;
	size_t n = space_memcpy_from(current->space, insn_buf, regs->eip, 16);
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
			/* FIXME: pop an "illegal instruction" exception message,
			 * somehow
			 */
			goto fail;
	}

	void *utcb = thread_get_utcb(current);
	struct thread *pager = thread_get_pager(current, utcb);
	if(pager == NULL) goto fail;
	save_ipc_regs(current, 3, 1);
	L4_VREG(utcb, L4_TCR_BR(0)) = L4_IoFpageLog2(0, 16).raw;
	L4_VREG(utcb, L4_TCR_MR(0)) = ((-8) & 0xfff) << 20 | 0x6 << 16 | 2;
	L4_VREG(utcb, L4_TCR_MR(1)) = L4_IoFpage(port, size).raw;
	L4_VREG(utcb, L4_TCR_MR(2)) = regs->eip;
	return_to_ipc(pager);

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

	thread_wake(t);
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


static void receive_pf_reply(struct hook *hook, uintptr_t code, void *priv)
{
	hook_detach(hook);
	if(likely(code == 0)) {
		struct thread *t = container_of(hook, struct thread, post_exn_call);
		thread_wake(t);
	}
}


void isr_exn_int3_bottom(struct x86_exregs *regs) {
	handle_kdb_enter(get_current_thread(), regs);
}


void isr_exn_gp_bottom(struct x86_exregs *regs)
{
	struct thread *current = get_current_thread();

	if(unlikely(x86_frame_len(regs) < sizeof(*regs))) {
		/* this shouldn't happen. stop the thread in question & limp along
		 * regardless.
		 */
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
	} else {
		thread_save_ctx(current, regs);

#if 0
		printf("#GP(%#lx) at eip %#lx, esp %#lx in %lu:%lu\n", regs->error,
			regs->eip, regs->esp, TID_THREADNUM(current->id),
			TID_VERSION(current->id));
#endif

		void *utcb = thread_get_utcb(current);
		struct thread *exh = thread_get_exnh(current, utcb);
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
	asm volatile ("movl %%cr2, %0": "=r" (fault_addr));

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

	int fault_access = L4_Readable;		/* the cache always reads. */
	if(CHECK_FLAG(regs->error, 2)) fault_access |= L4_Writable;
	/* TODO: is there some way to catch this explicitly e.g. with NX-enabled
	 * PTEs?
	 *
	 * see also the x86 hack in mapdb_add_map().
	 */
	if(regs->eip == fault_addr) fault_access |= L4_eXecutable;

#if 0
	printf("#PF (%s, %s, %s) @ %#lx (eip %#lx); current thread %lu:%lu\n",
		CHECK_FLAG(regs->error, 4) ? "user" : "super",
		CHECK_FLAG(regs->error, 2) ? "write" : "read",
		CHECK_FLAG(regs->error, 1) ? "access" : "presence",
		fault_addr, regs->eip,
		TID_THREADNUM(current->id), TID_VERSION(current->id));
#endif

#ifndef NDEBUG
	static uintptr_t last_fault = ~0;
	static int repeat_count = 0;
	if(last_fault == fault_addr && ++repeat_count == 10) {
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
		return_from_exn();
	} else {
#ifndef NDEBUG
		repeat_count = 0;	/* sufficiently userspacey. */
#endif

		thread_save_ctx(current, regs);
		void *utcb = thread_get_utcb(current);
		struct thread *pager = thread_get_pager(current, utcb);
		if(unlikely(pager == NULL)) {
			printf("thread %lu:%lu has no pager, stopping it\n",
				TID_THREADNUM(current->id), TID_VERSION(current->id));
			printf("  (fault was at %#lx, ip %#lx)\n", fault_addr, regs->eip);
			thread_halt(current);
			assert(current->status == TS_STOPPED);
			return_to_scheduler();
		} else {
			set_pf_msg(current, utcb, fault_addr, regs->eip, fault_access);
			hook_push_back(&current->post_exn_call, &receive_pf_reply, NULL);
			return_to_ipc(pager);
		}
	}
}


/* functions exported in <ukernel/ipc.h> */

/* FIXME: if a pagefault is replied to with more than 3 MRs (tag, single
 * MapItem), the reply's data overwrites that present in the recipient's TCB.
 * this can disrupt ongoing IPC by overwriting registers that haven't been
 * copied, and munge MRs in a pre-IPC condition when a pagefault or exception
 * is generated while the thread loads its MRs.
 *
 * proposed solution: when !hook_empty(&t->post_exn_call), store existing MRs
 * into a buffer that gets restored when post_exn_ok() returns true.
 */
void set_pf_msg(
	struct thread *t,
	void *utcb,
	L4_Word_t fault_addr,
	L4_Word_t ip,
	int fault_access)
{
	assert(utcb != NULL);

	save_ipc_regs(t, 3, 1);
	L4_VREG(utcb, L4_TCR_BR(0)) = L4_CompleteAddressSpace.raw;
	L4_VREG(utcb, L4_TCR_MR(0)) = ((-2) & 0xfff) << 20		/* label */
		| fault_access << 16	/* access */
		| 2;		/* "u" for msgtag */
	L4_VREG(utcb, L4_TCR_MR(1)) = fault_addr;
	L4_VREG(utcb, L4_TCR_MR(2)) = ip;
}
