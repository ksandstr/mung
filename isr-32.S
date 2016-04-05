#include <ukernel/gdt.h>
#include <ukernel/config.h>
#include <ukernel/syscall.h>

	/* NOTE: when using the "regs" parameter for task switching, the callee
	 * should also fill in the correct %fs and %gs selectors! this is not
	 * handled by interrupt service routines, unlike %es and %ds are.
	 */


	/* ISR_IRQ_FAST produces IRQ interrupt handlers that call a specific
	 * bottom half. it puts $vecn in the "reason" field of the parameter
	 * <struct x86_exregs *>.
	 */
	.macro ISR_IRQ_FAST vecn, name
	.global isr_\name\()_top
	.type isr_\name\()_top, @function
	.align 16,0
isr_\name\()_top:
	subl $4, %esp		# trash errorcode
	pushal
	pushl %ds
	pushl %es
	pushl $\vecn
	pushl %esp
	movl $(SEG_KERNEL_DATA_HIGH << 3), %eax
	movl %eax, %ds
	movl %eax, %es
	call isr_\name\()_bottom
	addl $8, %esp		# pop regs, reason parameters
	popl %es
	popl %ds
	popal
	addl $4, %esp		# pop errorcode
	iret
	.endm

	# timer interrupt is consumed by microkernel. applications may bind to
	# interrupt 0, but it'll never fire.
	ISR_IRQ_FAST 0x20, irq0
	ISR_IRQ_FAST 0xb00bface, xtpic	# ye olde XT-PIC (or two)
	ISR_IRQ_FAST 0xdeadd065, apic	# fixed interrupts via the I/O APIC


	.equ USER_DATA_SEL, (SEG_USER_DATA << 3 + 3)
	.equ USER_CODE_SEL, (SEG_USER_CODE << 3 + 3)

	/* ISR_EXN produces exception interrupt handlers. these have individual
	 * bottom halves on the C side.
	 */
	.macro ISR_EXN vecn, name, ecval
	.global isr_exn_\name\()_top
	.type isr_exn_\name\()_top, @function
	.align 16,0
isr_exn_\name\()_top:
	# what is this rigamarole necessary for? apparently pushal references
	# data segments (rather than the stack segment replaced by the
	# interrupt trap), which has the effect of triple-faulting the CPU.
	# load data segments redundantly to avoid this.
.ifnb \ecval
	pushl $\ecval		# used for exceptions that don't push a code
.endif
	pushl %eax
	movl $(SEG_KERNEL_DATA_HIGH << 3), %eax
	movl %eax, %ds
	movl %eax, %es
	popl %eax
	pushal
	pushl $USER_DATA_SEL	# ds (surrogate)
	pushl $USER_DATA_SEL	# es (surrogate)
	pushl $\vecn
	pushl %esp
	call isr_exn_\name\()_bottom
	addl $8, %esp		# pop regs, reason parameters
	popl %es
	popl %ds
	popal
	addl $4, %esp		# the error code
	iret
	.endm

	ISR_EXN 0, de, 0	# divide error
	ISR_EXN 3, int3, 0	# int3 (KDB)
	ISR_EXN 6, ud, 0	# invalid opcode
	ISR_EXN 7, nm, 0	# device not available (fpu/mmx etc)
	ISR_EXN 13, gp		# general protection
	ISR_EXN 14, pf		# pagefault
	ISR_EXN 16, mf, 0	# x87 floating point thing
	ISR_EXN 19, xm, 0	# SIMD floating point thing
	ISR_EXN 0x8c, lipc_sc, 0	# Lipc
	ISR_EXN 0x8d, exregs_sc, 0	# ExchangeRegisters
	ISR_EXN 0x8e, memctl_sc, 0	# MemoryControl
	ISR_EXN 0x8f, basic_sc, 0	# basic syscall


#ifdef CONFIG_X86_SYSENTER

# SYSENTER/SYSEXIT support
#
# after SYSENTER, the CPU will be in ring 0, with CS and SS set to the kernel
# high segments. furthermore, the base address of CS and SS will be destroyed;
# instead the processor loads a base address of 0. therefore the address of
# _sysenter_top will be somewhere in the kernel's linear range, meaning that
# calling sysenter_bottom must also reload the high segments and reset kernel
# %esp and %eip to the low address.
#
# DS, ES must be loaded with the high data segment as well.
#
# this routine calls sysenter_bottom(regs), which may return either nonlocally
# or locally; in the latter case it must set regs->eip to a correct sysexit
# epilogue. EBX will be pushed as the error field to indicate syscall target.
# the EBX field in the caller's frame will be filled in (for ThreadSwitch) or
# substituted for (everything else) by syscall glue.
#
# it'll also call sys_ipc() from a fast-path.
#
# TODO: add a fastpath for sys_threadswitch() as well; it should save the
# caller's context in the current thread (like thread_save_ctx() would), set
# TF_SYSCALL, and call sys_threadswitch().
#
# TODO: it'd be more efficient to use a per-syscall return path instead of one
# that restores all registers except EDX and ECX, if only by a couple of load
# instructions.

#define UTCB_SIZE 512		/* FIXME: get this from an asm header */
#define THREAD_CTX_OFF (4 + 4)
#define THREAD_FLAGS_OFF 4

	.global _sysenter_top
	.type _sysenter_top, @function
	.align 16,0
_sysenter_top:
	ljmp $(SEG_KERNEL_CODE_HIGH << 3), $1f	# restore kernel %cs, %eip
1:	pushl %ebx		# save syscall target
	andl $0x0fffffff, %esp	# drop back to kernel %ss (EXTRA FUCKY)
	movl $(SEG_KERNEL_DATA_HIGH << 3), %ebx
	movl %ebx, %ss
	movl %ebx, %ds
	movl %ebx, %es
	sti

	# direct dispatch. requires that SC_IPC=1, SC_LIPC=2, SC_THREADSWITCH=3
	# and that no syscall is at 0.
	movzbl (%esp), %ebx
	cmpl $SC_THREADSWITCH, %ebx
	# ja 2f
	# je _sys_threadswitch_fast
	jae 2f

	# IPC fastpath.
	#
	# the spec has a 1:1 mapping between GCC regparms and the
	# to/fromspec/timeouts parameters to Ipc and Lipc. so that's quite
	# convenient.
	cmpl $SC_LIPC, %ebx
	je _sysenter_lipc_fast

	pushl %esi	# arg: mr0
	leal 0x10000000(%edi), %esi
	pushl %esi	# arg: user UTCB in kernel segment
	# retain caller %esp (in %ebp) and %edi for when sys_ipc() goes to
	# scheduler instead. generate %eip for the same reason, and keep it in
	# %esi for use in the epilogue.
	movl current_thread, %ebx
	movl (%ebx), %esi	# ->space
	movl 8(%esi), %esi	# ->kip_area
	andl $0xfffffc00, %esi	# ... address
	addl sysexit_epilogs, %esi	# + no-restore epilog offset
	movl %edi, THREAD_CTX_OFF(%ebx)
	movl %esi, THREAD_CTX_OFF+32(%ebx)
	movl %ebp, THREAD_CTX_OFF+12(%ebx)

	# utcb validation. compare %edi to utcb_top, and to utcb_area +
	# UTCB_SIZE / 2.
	movl (%ebx), %ebx	# ->space
	cmpl (%ebx), %edi
	ja 3f			# if %edi > utcb_top
	movl 4(%ebx), %ebx	# ->utcb_area
	andl $0xfffffc00, %ebx	# its address
	addl $(UTCB_SIZE / 2), %ebx	# + UTCB_SIZE / 2
	cmpl %ebx, %edi
	jb 3f			# if %edi < %ebx, halt & schedule

	movl current_thread, %ebx
	orb $0x10, THREAD_FLAGS_OFF(%ebx)	# set TF_SYSCALL
	call sys_ipc		# OUT %eax=retval
	andb $0xef, THREAD_FLAGS_OFF(%ebx)	# clear TF_SYSCALL

	# check for and process interrupts that occurred while IRQ processing
	# was soft disabled.
	cli
10:	movzbl kernel_irq_deferred, %ecx
	test %cl, %cl
	jz 11f
	sti
	call int_latent
	cli
	jmp 10b

	# now test for a pending preemption & handle it like the C part would.
11:	movzbl kernel_preempt_pending, %ecx
	test %cl, %cl
	jnz 12f

	movl %ebp, %ecx		# OUT caller %esp
	movl %esi, %edx		# OUT epilog %eip
	# loads from visible UTCB: %esi=MR0, %ebx=MR1, %ebp=MR2.
	leal 0x10000000(%edi), %esi
	pushl 4(%esi)		# MR1 (stashed)
	movl 8(%esi), %ebp	# OUT MR2
	movl (%esi), %esi	# OUT MR0
13:	movl $USER_DATA_SEL, %ebx
	movl %ebx, %ds
	movl %ebx, %es
	movl %ebx, %fs
	popl %ebx		# OUT MR1
	sti
	sysexit

12:	# store caller's return info for rescheduling
	#
	# TODO: the current unit tests never hit this path, and it can't be
	# properly hand-tested because of how return_from_exn will actually
	# return to caller when preemption didn't happen. to test it, somehow
	# Ipc processing would have to take long enough for a
	# preemption-causing interrupt to occur.
	movl %ebp, THREAD_CTX_OFF+12(%ebx)	# caller %esp
	movl %esi, THREAD_CTX_OFF+32(%ebx)	# caller %eip
	# no need for a proper call if kernel_preempt_pending is set; then the
	# function is guaranteed to never return (except if current ==
	# scheduler_thread, but... how would it end up in the sysenter tophalf
	# anyway).
	sti
	jmp return_from_exn

	.align 16,0
2:	# construct generic x86_exregs frame
	pushl $USER_DATA_SEL	# .ss
	pushl %ebp		# .esp
	pushf			# .eflags
	pushl $USER_CODE_SEL	# .cs (future)
	pushl $0		# .eip (false)
	pushl 20(%esp)		# .error (syscall target)
	pushal			# .r
	pushl $USER_DATA_SEL	# .ds (surrogate)
	pushl $USER_DATA_SEL	# .es (surrogate)
	pushl $0		# .reason (not used)
	pushl %esp		# frame address on sysenter stack
	call sysenter_bottom

	# offsets at +4 because of sysenter_bottom() parameter ptr.
	# TODO: change it to regparms.
	movl 16(%esp), %edi
	movl 20(%esp), %esi
	movl 24(%esp), %ebp
	movl 32(%esp), %ebx

	movl 52(%esp), %edx	# eip
	movl 64(%esp), %ecx	# esp

	pushl 60(%esp)		# eflags
	pushl 48(%esp)		# temporary eax (at +8)
	movl $USER_DATA_SEL, %eax
	movl %eax, %ds
	movl %eax, %es
	movl %eax, %fs
	popl %eax
	popf
	sysexit

	# the "utcb wasn't valid at all" branch.
3:	pushl %ecx
	pushl %edx
	pushl %eax
	pushl %edi
	pushl $4f
	call printf
	pushl current_thread
	call thread_halt
	call return_to_scheduler

4:	.ascii "utcb=%#lx wasn't valid at all (to=%#lx, from=%#lx, timeouts=%#lx)"
	.byte 10, 0

	.align 16, 0
_sysenter_lipc_fast:
	# input registers:
	#   %ebp = caller %esp, %eax = to, %ecx = timeouts,
	#   %edx = fromspec, %esi = mr0, %edi = caller UTCB
	#   ... also, (%ebp) and 4(%ebp) are MR1 and MR2 resp.
	#
	# stash pass-through %esp, %edi regs first.
	movl current_thread, %ebx
	movl %edi, THREAD_CTX_OFF(%ebx)
	movl %ebp, THREAD_CTX_OFF+12(%ebx)
	# call sys_lipc().
	#
	# (the user-to-kernel transfer, here, is as valid as the sysenter stub
	# can guarantee. however, if the caller's stack broke in between, we're
	# screwed. TODO: set a catch_pf()-alike flag to indicate that a fault
	# in ring 0 isn't the kernel breaking, just userspace behaving badly.)
	leal 0x10000000(%ebp), %ebp	# transfer to kernel segment
	pushl 4(%ebp)		# MR2
	pushl (%ebp)		# MR1
	pushl %esi		# MR0 (the message tag)
	leal 0x10000000(%edi), %esi
	pushl %esi		# caller UTCB in kernel space
	# pass to, fromspec, timeouts as-is
	orb $0x10, THREAD_FLAGS_OFF(%ebx)	# set TF_SYSCALL
	call sys_lipc		# OUT %eax=from (retval)

	# we get here because Lipc's fallback to Ipc failed or returned
	# immediately. on success, sys_lipc() switches to a different thread or
	# enters the scheduler.
	#
	# registers:
	#   %eax = sys_ipc() return value, %ebx = current_thread,
	#   %ebp = caller %esp,
	#   %esi = caller UTCB in kernelspace, %edi = caller UTCB in userspace
	andb $0xef, THREAD_FLAGS_OFF(%ebx)	# clear TF_SYSCALL

	# check for and process interrupts that occurred while IRQ processing
	# was soft disabled.
	cli
10:	movzbl kernel_irq_deferred, %ecx
	test %cl, %cl
	jz 11f
	sti
	call int_latent
	cli
	jmp 10b

	# now test for a pending preemption & handle it like the C part would.
11:	movzbl kernel_preempt_pending, %ecx
	test %cl, %cl
	jnz 12f		# return_from_exn() path
	movl THREAD_CTX_OFF+12(%ebx), %ecx	# OUT caller %esp
	movl THREAD_CTX_OFF+32(%ebx), %edx	# OUT epilog %eip
	# load MR0, MR1, MR2 from visible UTCB
	movl 8(%esi), %ebp	# OUT MR2
	pushl 4(%esi)		# OUT MR1 (popped before sysexit)
	movl (%esi), %esi	# OUT MR0
	jmp 13b			# sysexit from the Ipc fastpath

12:	sti
	jmp return_from_exn

#endif
