
#include <stdio.h>
#include <stdint.h>

#include <ccan/likely/likely.h>
#include <ccan/compiler/compiler.h>
#include <ccan/str/str.h>
#include <ccan/container_of/container_of.h>

#include <l4/types.h>
#include <l4/vregs.h>

#include <ukernel/x86.h>
#include <ukernel/setjmp.h>
#include <ukernel/thread.h>
#include <ukernel/sched.h>
#include <ukernel/space.h>
#include <ukernel/mapdb.h>
#include <ukernel/ptab.h>
#include <ukernel/slab.h>
#include <ukernel/ipc.h>
#include <ukernel/interrupt.h>
#include <ukernel/syscall.h>
#include <ukernel/cpu.h>
#include <ukernel/kip.h>
#include <ukernel/util.h>
#include <ukernel/misc.h>
#include <ukernel/ktest.h>
#include <ukernel/config.h>


/* the thread the current FPU context belongs to. usually not NULL, but may
 * become so through a deleting ThreadControl.
 */
static struct thread *fpu_thread = NULL;

static struct kmem_cache *fpu_context_slab = NULL;
static void *next_fpu_context = NULL;

jmp_buf catch_pf_env;
volatile bool catch_pf_ok = false;


static void receive_pf_reply(
	struct hook *hook,
	void *param, uintptr_t code, void *priv);


void isr_exn_de_bottom(struct x86_exregs *regs)
{
	printf("#DE(0x%lx) at eip 0x%lx, esp 0x%lx\n", regs->error,
		regs->eip, regs->esp);
	panic("#DE");
}


static NORETURN void return_from_gp(struct thread *current, struct x86_exregs *regs)
{
	void *utcb = thread_get_utcb(current);
	struct thread *exh = thread_get_exnh(current, utcb);
	if(likely(exh != NULL)) {
		return_to_ipc(send_exn_ipc(current, utcb, -5, regs, &exh), exh);
	} else {
		printf("#GP(%#lx) unhandled at eip=%#lx, esp=%#lx, tid=%lu:%lu\n",
			regs->error, regs->eip, regs->esp,
			TID_THREADNUM(current->id), TID_VERSION(current->id));

		thread_halt(current);
		assert(current->status == TS_STOPPED);
		return_to_scheduler();
	}

	assert(false);
}


/* TODO: could this be merged with return_from_gp()? */
static NORETURN void return_from_ud(struct thread *current, struct x86_exregs *regs)
{
	void *utcb = thread_get_utcb(current);
	struct thread *exh = thread_get_exnh(current, utcb);
	if(likely(exh != NULL)) {
		/* indicate "invalid opcode" as though it was an INT# GP on line 6
		 * (#UD). label will be an architecture-specific exception despite
		 * invalid opcodes occurring on all architectures.
		 */
		regs->error = (6 << 3) + 2;
		return_to_ipc(send_exn_ipc(current, utcb, -5, regs, &exh), exh);
	} else {
		printf("#UD unhandled in %lu:%lu; eip=%#lx, esp=%#lx\n",
			TID_THREADNUM(current->id), TID_VERSION(current->id),
			regs->eip, regs->esp);
		thread_halt(current);
		assert(current->status == TS_STOPPED);
		return_to_scheduler();
	}

	assert(false);
}


void isr_exn_ud_bottom(struct x86_exregs *regs)
{
	assert(x86_irq_is_enabled());

	/* see if it's a LOCK NOP. (this is why the "syscall" is so slow.) */
	struct thread *current = get_current_thread();
	uint8_t buf[2];
	size_t n = space_memcpy_from(current->space, buf, regs->eip, 2);
	if(n == 2 && buf[0] == 0xf0 && buf[1] == 0x90) {
		/* it is L4_KernelInterface(). */
		const L4_KernelInterfacePage_t *kip = kip_mem;
		regs->eip += 2;
		regs->r.eax = L4_Address(current->space->kip_area);
		regs->r.ecx = kip->ApiVersion.raw;
		regs->r.edx = kip->ApiFlags.raw;
		const L4_KernelDesc_t *kdesc = kip_mem + kip->KernelVerPtr;
		regs->r.esi = kdesc->KernelId.raw;
		return_from_exn();
	} else {
		/* short read, or not a KernelInterface sequence. */
		return_from_ud(current, regs);
		assert(false);
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
	assert(x86_irq_is_enabled());

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
			x86_fnsave(prev->fpu_context);
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
	assert(x86_irq_is_enabled());

	/* indicate as INT# GP on line 16 (#MF). */
	regs->error = (16 << 3) + 2;
	return_from_gp(get_current_thread(), regs);
}


/* SSE fpu exceptions */
void isr_exn_xm_bottom(struct x86_exregs *regs)
{
	assert(x86_irq_is_enabled());

	/* indicate SIMD exception like an INT# GP on line 19 (#XM). */
	regs->error = (19 << 3) + 2;
	return_from_gp(get_current_thread(), regs);
}


static void glue_unmap(struct x86_regs *regs)
{
	L4_Word_t control = regs->eax;

	void *utcb = thread_get_utcb(get_current_thread());
	if((control & 0x3f) > 0) L4_VREG(utcb, L4_TCR_MR(0)) = regs->esi;
	sys_unmap(control, utcb);

	regs->esi = L4_VREG(utcb, L4_TCR_MR(0));
}


static void glue_threadcontrol(struct x86_regs *regs)
{
	L4_ThreadId_t dest_tid = { .raw = regs->eax },
		pager = { .raw = regs->ecx },
		scheduler = { .raw = regs->edx },
		spacespec = { .raw = regs->esi };
	L4_Word_t utcb_loc = regs->edi;

	regs->eax = sys_threadcontrol(dest_tid, pager, scheduler, spacespec,
		(void *)utcb_loc);
}


static void glue_spacecontrol(struct x86_regs *regs)
{
	regs->eax = sys_spacecontrol(
		(L4_ThreadId_t){ .raw = regs->eax },	/* spacespec */
		regs->ecx,								/* control */
		(L4_Fpage_t){ .raw = regs->edx },		/* kip_area */
		(L4_Fpage_t){ .raw = regs->esi },		/* utcb_area */
		(L4_ThreadId_t){ .raw = regs->edi },	/* redirector */
		&regs->ecx);							/* old_control */
}


static void glue_ipc(struct x86_regs *regs)
{
	struct thread *current = get_current_thread();
	/* TODO: instead of get_utcb, validate caller_utcb in the kernel */
	void // *caller_utcb = (void *)regs->edi,
		*utcb = thread_get_utcb(current);

	/* preserve registers. eip set by caller for return from scheduling. */
	current->ctx.r.edi = regs->edi;
	current->ctx.r.esp = regs->esp;

	L4_ThreadId_t to = { .raw = regs->eax }, from = { .raw = regs->edx };
	L4_Word_t timeouts = regs->ecx, mr0 = regs->esi;
	current->flags |= TF_SYSCALL;
	regs->eax = sys_ipc(to, from, timeouts, utcb, mr0);
	current->flags &= ~TF_SYSCALL;
	regs->esi = L4_VREG(utcb, L4_TCR_MR(0));
	regs->ebx = L4_VREG(utcb, L4_TCR_MR(1));
	regs->ebp = L4_VREG(utcb, L4_TCR_MR(2));
}


static void glue_threadswitch(struct x86_regs *regs)
{
	struct thread *current = get_current_thread();
	thread_save_ctx(current, container_of(regs, struct x86_exregs, r));

	L4_ThreadId_t target = { .raw = regs->eax };
	current->flags |= TF_SYSCALL;
	sys_threadswitch(target);
	current->flags &= ~TF_SYSCALL;
}


static void glue_processorcontrol(struct x86_regs *regs)
{
	regs->eax = sys_processorcontrol(regs->eax, regs->ecx,
		regs->edx, regs->esi);
}


static void glue_schedule(struct x86_regs *regs)
{
	L4_ThreadId_t dest_tid = { .raw = regs->eax };
	regs->eax = sys_schedule(dest_tid, regs->ecx,
		&regs->edx, regs->esi, regs->edi);
}


/* FIXME: implement sys_memctl() in a memory.c, or some such */
static void glue_memctl(struct x86_regs *regs)
{
	static bool first = true;
	if(first) {
		first = false;
		printf("MemoryControl not implemented\n");
	}
}


static void glue_exregs(struct x86_regs *regs)
{
	assert(x86_irq_is_enabled());
	regs->eax = sys_exregs((L4_ThreadId_t){ .raw = regs->eax },
		&regs->ecx, &regs->edx, &regs->esi, &regs->edi, &regs->ebx,
		(L4_ThreadId_t *)&regs->ebp);
}


static void (*const sys_fns[])(struct x86_regs *regs) = {
	[SC_IPC] = &glue_ipc,
	[SC_LIPC] = &glue_ipc,
	[SC_UNMAP] = &glue_unmap,
	[SC_THREADSWITCH] = &glue_threadswitch,
	[SC_SCHEDULE] = &glue_schedule,
	[SC_SPACECONTROL] = &glue_spacecontrol,
	[SC_THREADCONTROL] = &glue_threadcontrol,
	[SC_PROCESSORCONTROL] = &glue_processorcontrol,
	[SC_EXREGS] = &glue_exregs,
	[SC_MEMCTL] = &glue_memctl,
};


void isr_exn_basic_sc_bottom(struct x86_exregs *regs)
{
	assert(x86_irq_is_enabled());
	assert(x86_frame_len(regs) == sizeof(*regs));

	struct thread *current = get_current_thread();
	current->ctx.eip = regs->eip;

	switch(regs->r.ebx & 0xff) {
		case SC_IPC:
		case SC_LIPC:
			glue_ipc(&regs->r);
			break;
		case SC_UNMAP: glue_unmap(&regs->r); break;
		case SC_THREADSWITCH: glue_threadswitch(&regs->r); break;
		case SC_SCHEDULE: glue_schedule(&regs->r); break;
		case SC_SPACECONTROL: glue_spacecontrol(&regs->r); break;
		case SC_THREADCONTROL: glue_threadcontrol(&regs->r); break;
		case SC_PROCESSORCONTROL: glue_processorcontrol(&regs->r); break;
		default: {
			printf("unknown basic syscall %lu (caller stopped)\n",
				regs->r.eax);
			thread_halt(current);
			assert(current->status == TS_STOPPED);
			return_to_scheduler();
		}
	}

	return_from_exn();
}


void isr_exn_exregs_sc_bottom(struct x86_exregs *regs)
{
	/* a thread-starting ExchangeRegisters may cause preemption. */
	get_current_thread()->ctx.eip = regs->eip;

	glue_exregs(&regs->r);
	return_from_exn();
}


void isr_exn_memctl_sc_bottom(struct x86_exregs *regs)
{
	assert(x86_irq_is_enabled());
	glue_memctl(&regs->r);
	return_from_exn();
}


void isr_exn_lipc_sc_bottom(struct x86_exregs *regs)
{
	assert(x86_irq_is_enabled());
	/* FIXME: use glue_lipc() instead */
	glue_ipc(&regs->r);
	return_from_exn();
}


#ifdef CONFIG_X86_SYSENTER
/* NOTE: the sysenter top half is a good candidate for a rewrite in assembly.
 * it'd combine the functions of both with sys_*() functions that take the
 * right parameters on the stack instead of ad-hockery and glue as we have
 * now.
 */
void sysenter_bottom(struct x86_exregs *regs)
{
	struct thread *current = get_current_thread();
	unsigned int target = regs->error & 0xff;
	L4_Word_t kip_base = L4_Address(current->space->kip_area);

#if 0
	printf("got SYSENTER [target=%u, current=%lu:%lu, esp=%#lx]\n",
		target, TID_THREADNUM(current->id), TID_VERSION(current->id),
		regs->esp);
#endif

	assert(target > 2);	/* Ipc & Lipc have fastpaths in _sysenter_top */
	if(target == SC_THREADSWITCH) {
		/* special handling of ThreadSwitch. */
		void *utcb = thread_get_utcb(current);
		regs->r.ebp = L4_VREG(utcb, TCR_SYSENTER_EBP);
		regs->r.ebx = L4_VREG(utcb, TCR_SYSENTER_EBX);
		regs->eip = kip_base + sysexit_epilogs.ecdx;
		L4_VREG(utcb, TCR_SYSEXIT_ECX) = regs->r.ecx;
		L4_VREG(utcb, TCR_SYSEXIT_EDX) = regs->r.edx;
		glue_threadswitch(&regs->r);
		return_from_exn();
	} else if(unlikely(target >= NUM_ELEMENTS(sys_fns)
		|| sys_fns[target] == NULL))
	{
		printf("unknown sysenter target %u (caller stopped)\n", target);
		/* context is not saved. */
		thread_halt(get_current_thread());
		return_to_scheduler();
	} else {
		void *utcb = thread_get_utcb(current);
		int ret_offset;
		/* special cases for syscalls that either accept ebx or ebp, or return
		 * ecx or edx.
		 */
		switch(target) {
			case SC_SCHEDULE:
				glue_schedule(&regs->r);
				L4_VREG(utcb, TCR_SYSEXIT_EDX) = regs->r.edx;
				ret_offset = sysexit_epilogs.edx;
				break;
			case SC_SPACECONTROL:
				glue_spacecontrol(&regs->r);
				L4_VREG(utcb, TCR_SYSEXIT_ECX) = regs->r.ecx;
				ret_offset = sysexit_epilogs.ecx;
				break;
			case SC_EXREGS:
			case SC_MEMCTL:
				regs->r.ebp = L4_VREG(utcb, TCR_SYSEXIT_ECX);
				regs->r.ebx = L4_VREG(utcb, TCR_SYSEXIT_EDX);
				if(unlikely(target == SC_MEMCTL)) {
					glue_memctl(&regs->r);
					ret_offset = sysexit_epilogs.fast;
				} else {
					glue_exregs(&regs->r);
					L4_VREG(utcb, TCR_SYSEXIT_ECX) = regs->r.ecx;
					L4_VREG(utcb, TCR_SYSEXIT_EDX) = regs->r.edx;
					ret_offset = sysexit_epilogs.ecdx;
				}
				break;
			default:
				ret_offset = sysexit_epilogs.fast;
				(*sys_fns[target])(&regs->r);
		}
		regs->eip = kip_base + ret_offset;
		return_from_exn();
	}
}
#endif


static void kdb_print_char(struct x86_exregs *regs) {
	printf("KDB: %s not implemented\n", __func__);
}


static void kdb_print_string(struct x86_exregs *regs)
{
	struct thread *current = get_current_thread();

	char sbuf[512];
	size_t n = space_memcpy_from(current->space, sbuf, regs->r.eax,
		sizeof(sbuf));
	sbuf[MIN(size_t, n, sizeof(sbuf) - 1)] = '\0';
	int len = strlen(sbuf);
	assert(len <= n);
	while(len > 0 && sbuf[len - 1] == '\n') sbuf[--len] = '\0';
	if(current->space == sigma0_space) {
		printf("[sigma0]: %s\n", sbuf);
		return;
	}

#ifdef ENABLE_SELFTEST
	if(CHECK_FLAG(current->space->flags, SF_PRIVILEGE)) {
		if(strstr(sbuf, "mung self-tests") != NULL) {
			if(strstr(sbuf, "describe option") != NULL) describe_all_tests();
			run_all_tests();
			return;
		}
	}
#endif

	printf("KDB[PrintString]: %s\n", sbuf);
}


static void kdb_clear_page(struct x86_exregs *regs) {
	printf("KDB: %s not implemented\n", __func__);
}


static void kdb_toggle_breakin(struct x86_exregs *regs) {
	printf("KDB: %s not implemented\n", __func__);
}


static void kdb_read_char(struct x86_exregs *regs) {
	printf("KDB: %s not implemented\n", __func__);
	regs->r.eax = 0;
}


static void kdb_read_char_blocked(struct x86_exregs *regs) {
	printf("KDB: %s not implemented\n", __func__);
	regs->r.eax = 0;
}


/* returns true if a valid #KDB operation was performed, false otherwise. */
static bool kdb_op(struct x86_exregs *regs)
{
	struct thread *current = get_current_thread();

	/* check the instruction sequence used. L4_KDB_Enter() makes cc eb 05 b8,
	 * i.e. int3; jmp +5; mov imm32, %eax. everything else is cc 3c, i.e.
	 * int3; cmpb $xx, %al; where xx appears after and identifies the
	 * operation.
	 */
	static const uint8_t enter_seq[4] = { 0xcc, 0xeb, 0x05, 0xb8 },
		cmd_seq[2] = { 0xcc, 0x3c };
	union {
		L4_Word_t w[2];
		uint8_t b[8];
	} mem;
	size_t n = space_memcpy_from(current->space, mem.b, regs->eip, 8);
	if(n < 8) {
		printf("KDB: can't memcpy 8 bytes from %#lx (got %u)\n",
			regs->eip, (unsigned)n);
		/* advance by one byte just to cause something besides an infinite
		 * loop through the kernel.
		 */
		regs->eip++;
		return false;
	}
	if(n >= 4 && memcmp(mem.b, enter_seq, 4) == 0) {
		/* copy out at most 256 bytes of KDB entry string */
		L4_Word_t strptr = mem.w[1];
		char strbuf[257];
		n = space_memcpy_from(current->space, strbuf, strptr, 256);
		strbuf[MIN(size_t, n, 256)] = '\0';
		if(strstarts(strbuf, "mung ") && strends(strbuf, " do nothing")) {
			printf("#KDB entry ignored\n");
			regs->eip++;
		} else {
			printf("#KDB (eip %#lx): [%#lx] %s\n", regs->eip, strptr, strbuf);
			printf(" ... (not implemented, halting thread)\n");
			thread_save_ctx(current, regs);
			thread_halt(current);
			assert(current->status == TS_STOPPED);
			return_to_scheduler();
			assert(false);
		}
	} else if(n >= 3 && memcmp(mem.b, cmd_seq, 2) == 0) {
		/* other KDB functions. */
		regs->eip += 3;
		static void (*const kdb_ops[])(struct x86_exregs *regs) = {
			[0] = &kdb_print_char,
			[1] = &kdb_print_string,
			[2] = &kdb_clear_page,
			[3] = &kdb_toggle_breakin,
			[4] = &kdb_read_char,
			[5] = &kdb_read_char_blocked,
		};
		int op_id = mem.b[2];
		if(op_id < 0 || op_id >= NUM_ELEMENTS(kdb_ops)
			|| kdb_ops[op_id] == NULL)
		{
			printf("KDB: unknown op_id=%d\n", op_id);
			goto fail;
		}
		(*kdb_ops[op_id])(regs);
		return true;
	} else {
		/* KDB message not indicated with the right sequence, not mapped, or
		 * there was some other error.
		 */
		if(n < 4) {
			printf("KDB: entry/cmd sequence not mapped (n=%d)\n", (int)n);
		}
fail:
		regs->error = 3 * 8 + 2;
		return_from_gp(current, regs);
		assert(false);
	}

	return true;
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
			port = regs->r.edx & 0xffff;
			size = 1;
			break;

		case 0xed:	/* IN [E]AX, DX */
			port = regs->r.edx & 0xffff;
			break;

		case 0xee:	/* OUT DX, AL */
			port = regs->r.edx & 0xffff;
			size = 1;
			in = false;
			break;

		case 0xef:	/* OUT DX, [E]AX */
			port = regs->r.edx & 0xffff;
			in = false;
			break;

		/* TODO: string variants */

		default:
			/* FIXME: see callsite in isr_exn_gp_bottom() */
			printf("unknown instruction %#02x in I/O fault at %#lx\n",
				insn[0], regs->eip);
			return_from_ud(current, regs);
			assert(false);
	}

	void *cur_utcb = thread_get_utcb(current);
	struct thread *pager = thread_get_pager(current, cur_utcb);
	if(unlikely(pager == NULL)) goto fail;
	L4_Word_t old_br0 = L4_VREG(cur_utcb, L4_TCR_BR(0));
	L4_VREG(cur_utcb, L4_TCR_BR(0)) = L4_IoFpageLog2(0, 16).raw;
	void *utcb = ipc_user(
		(L4_MsgTag_t){ .raw = ((-8) & 0xfff) << 20 | 0x6 << 16 | 2 },
		current, cur_utcb, &pager, 3);
	hook_push_back(&current->post_exn_call,
		&receive_pf_reply, (void *)old_br0);
	L4_VREG(utcb, L4_TCR_MR(1)) = L4_IoFpage(port, size).raw;
	L4_VREG(utcb, L4_TCR_MR(2)) = regs->eip;
	return_to_ipc(utcb, pager);
	NOT_REACHED;

fail:
	thread_halt(current);
	assert(current->status == TS_STOPPED);
	return_to_scheduler();
}


static void receive_exn_reply(
	struct hook *hook,
	void *param, uintptr_t code, void *priv)
{
	hook_detach(hook);
	struct thread *t = container_of(hook, struct thread, post_exn_call),
		*sender = param;
	L4_VREG(thread_get_utcb(t), L4_TCR_BR(0)) = (L4_Word_t)priv;

	if(code != 0) {
		/* failed exception IPC happens under two conditions: either the
		 * thread was deleted, or the exception handler was deleted. in the
		 * former case TF_HALT will be set already; in the latter, the thread
		 * should be halted.
		 */
		if(!CHECK_FLAG(t->flags, TF_HALT)) {
			/* thread_halt(), on the other hand, doesn't transition RECV_WAIT
			 * threads to STOPPED. so we'll have to force the issue a bit.
			 */
			t->status = TS_R_RECV;
			thread_halt(t);
			assert(t->status == TS_STOPPED);
		}
		return;
	}

	void *msg_utcb = thread_get_utcb(sender);

	/* only do a valid thing for a valid reply. seems reasonable, right? */
	L4_MsgTag_t tag = { .raw = L4_VREG(msg_utcb, L4_TCR_MR(0)) };
	if(unlikely(L4_UntypedWords(tag) < 12 || sender == NULL)) return;

	/* ignores "ExceptionNo", "ErrorCode" in MR3, MR4 resp. */
	struct x86_ctx *c = &t->ctx;
	c->eflags = x86_clean_eflags(c->eflags, L4_VREG(msg_utcb, L4_TCR_MR(2)));
	c->eip = L4_VREG(msg_utcb, L4_TCR_MR(1));
	L4_Word_t *rpos = &c->r.edi;
	for(int i=5; i <= 12; i++) *(rpos++) = L4_VREG(msg_utcb, L4_TCR_MR(i));
	assert(rpos == &c->r.eax + 1);

	thread_wake(t);
}


void *send_exn_ipc(
	struct thread *t, void *t_utcb, int label,
	const struct x86_exregs *regs,
	struct thread **handler_p)
{
	assert(label < 0);

	L4_Word_t old_br0 = L4_VREG(t_utcb, L4_TCR_BR(0));
	L4_VREG(t_utcb, L4_TCR_BR(0)) = L4_UntypedWordsAcceptor.raw;
	void *utcb = ipc_user(
		(L4_MsgTag_t){ .X.label = (label & 0xfff) << 4, .X.u = 12 },
		t, t_utcb, handler_p, 13);
	L4_Word_t exvars[] = {
		regs->eip,
		regs->eflags,
		regs->reason,	/* ExceptionNo */
		regs->error,
		regs->r.edi, regs->r.esi, regs->r.ebp,
		regs->esp,		/* NOTE: this is correct. */
		regs->r.ebx, regs->r.edx, regs->r.ecx, regs->r.eax,
	};
	int num_vars = sizeof(exvars) / sizeof(exvars[0]);
	assert(num_vars == 12);
	for(int i=0; i < num_vars; i++) {
		L4_VREG(utcb, L4_TCR_MR(i + 1)) = exvars[i];
	}

	/* back, because BR0 must be restored after save_ipc_regs()' hook function
	 * completes (if applicable).
	 */
	hook_push_back(&t->post_exn_call, &receive_exn_reply, (void *)old_br0);

	return utcb;
}


static void receive_pf_reply(
	struct hook *hook,
	void *param, uintptr_t code, void *priv)
{
	hook_detach(hook);
	struct thread *t = container_of(hook, struct thread, post_exn_call);
	void *utcb = thread_get_utcb(t);
	L4_VREG(utcb, L4_TCR_BR(0)) = (L4_Word_t)priv;
	if(likely(code == 0)) {
		thread_wake(t);
	} else {
		/* failed pager IPC. there are two cases: either the thread itself was
		 * deleted, or the peer disappeared. in the former case the thread is
		 * already halted; in the latter, we should do it here.
		 */
		if(!CHECK_FLAG(t->flags, TF_HALT)) {
			/* (this is required to force a transition to STOPPED. see
			 * receive_exn_reply() for the ugly details.)
			 */
			t->status = TS_R_RECV;
			thread_halt(t);
			assert(t->status == TS_STOPPED);
		}
	}
}


/* NOTE: this doesn't activate from the INT3 instruction under qemu-kvm.
 * (wondering why that is.) anyway, the #GP handler also does kdb_op() to
 * cover for it.
 */
void isr_exn_int3_bottom(struct x86_exregs *regs)
{
	kdb_op(regs);
	return_from_exn();
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
		/* FIXME: things like unaligned MOVAPS instructions pop #GP(0). these
		 * should be recognized by instruction prefix 0x0f, or VEX.128/256 on
		 * AVX targets.
		 *
		 * correcting the problem above requires modification of
		 * handle_io_fault() so that it'll return with an indicator when it
		 * doesn't recognize an I/O fault, rather than being the noreturn
		 * bog-void it currently is.
		 */
#if 0
		printf("#GP(%#lx) eip=%#lx, esp=%#lx, current=%lu:%lu\n",
			regs->error, regs->eip, regs->esp,
			TID_THREADNUM(current->id), TID_VERSION(current->id));
		printf("  eax=%#lx\n", regs->eax);
#endif
		handle_io_fault(current, regs);
	} else if(regs->error == 3 * 8 + 2 && kdb_op(regs)) {
		/* INT3 via #GP, was valid KDB operation; return to userspace */
	} else {
		thread_save_ctx(current, regs);
		return_from_gp(current, regs);
	}
}


void isr_exn_pf_bottom(struct x86_exregs *regs)
{
	L4_Word_t fault_addr;
	asm volatile ("movl %%cr2, %0": "=r" (fault_addr));

	struct thread *current = get_current_thread();

	if(unlikely(!CHECK_FLAG(regs->error, 4)) && catch_pf_ok) {
		if(likely(fault_addr < KERNEL_SEG_START)
			&& space_prefill_upper(current->space, fault_addr))
		{
			/* lazy-mode kernel pf repair */
			return;
		} else {
			catch_pf_ok = false;
			longjmp(catch_pf_env, fault_addr);
			assert(false);
		}
	}

#if 0
	printf("#PF (%s, %s, %s) @ %#lx (eip %#lx); current thread %lu:%lu\n",
		CHECK_FLAG(regs->error, 4) ? "user" : "super",
		CHECK_FLAG(regs->error, 2) ? "write" : "read",
		CHECK_FLAG(regs->error, 1) ? "access" : "presence",
		fault_addr, regs->eip,
		TID_THREADNUM(current->id), TID_VERSION(current->id));
#endif

	assert(x86_irq_is_enabled());

	if(unlikely(!CHECK_FLAG(regs->error, 4))) {
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

#ifndef NDEBUG
	static uintptr_t last_fault = ~0;
	static int repeat_count = 0;
	if(last_fault == fault_addr && ++repeat_count == 10) {
		printf("WARNING: faulted many times on faddr=%#lx, fip=%#lx\n",
			fault_addr, regs->eip);
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

	struct pt_iter it;
	pt_iter_init(&it, current->space);
	if(fault_addr < KERNEL_SEG_START && !pt_upper_present(&it, fault_addr)) {
		if(space_prefill_upper(current->space, fault_addr)) {
			assert(pt_upper_present(&it, fault_addr));
			pt_iter_destroy(&it);
			return_from_exn();
			return;
		} else {
			/* fall out into the fault-generating part. */
			assert(!pt_upper_present(&it, fault_addr));
		}
	}

#ifndef NDEBUG
	const struct map_entry *e = mapdb_probe(current->space, fault_addr);
	if(e != NULL && CHECK_FLAG_ALL(L4_Rights(e->range), fault_access)) {
		printf("#PF fault_addr=%#lx, eip=%#lx\n", fault_addr, regs->eip);
		printf("    mapdb_id=%u, ptab_id=%u\n",
			mapdb_page_id_in_entry(e, fault_addr),
			pt_get_pgid(&it, NULL, fault_addr));

		panic("inconsistent mapping database or page table");
	}

	repeat_count = 0;
#endif

	pt_iter_destroy(&it);

	thread_save_ctx(current, regs);
	void *utcb = thread_get_utcb(current);
	struct thread *pager = thread_get_pager(current, utcb);
	if(unlikely(pager == NULL)) {
		thread_halt(current);
		assert(current->status == TS_STOPPED);
		return_to_scheduler();
	} else {
		struct thread *dest = pager;
		L4_Word_t old_br0 = L4_VREG(utcb, L4_TCR_BR(0));
		void *msg_utcb = send_pf_ipc(current, utcb,
			fault_addr, regs->eip, fault_access, &dest);
		hook_push_back(&current->post_exn_call,
			&receive_pf_reply, (void *)old_br0);
		return_to_ipc(msg_utcb, dest);
	}
}


/* exported in <ukernel/ipc.h> */

void *send_pf_ipc(
	struct thread *t, void *t_utcb,
	L4_Word_t fault_addr, L4_Word_t fault_ip, int fault_access,
	struct thread **handler_p)
{
	L4_VREG(t_utcb, L4_TCR_BR(0)) = L4_CompleteAddressSpace.raw;
	void *utcb = ipc_user(
		(L4_MsgTag_t){ .raw = ((-2) & 0xfff) << 20		/* label */
			| fault_access << 16	/* access */
			| 2 },		/* # of untyped words */
		t, t_utcb, handler_p, 3);
	L4_VREG(utcb, L4_TCR_MR(1)) = fault_addr;
	L4_VREG(utcb, L4_TCR_MR(2)) = fault_ip;

	return utcb;
}
