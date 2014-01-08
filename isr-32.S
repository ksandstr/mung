#include <ukernel/gdt.h>

	/* NOTE: when using the "regs" parameter for task switching, the callee
	 * should also fill in the correct %fs and %gs selectors! this is not
	 * handled by interrupt service routines, unlike %es and %ds are.
	 */

	/* ISR_IRQ produces IRQ interrupt handlers that call a common bottom
	 * half while pushing $vecn as the "reason" field in the parameter
	 * <struct x86_exregs *>. this is useful in the common case because
	 * non-fast IRQs being caught generally indicates that a device driver,
	 * running at a higher priority than the current thread, will
	 * immediately pre-empt the running thread.
	 */
	.macro ISR_IRQ vecn, name
	.global isr_\name\()_top
	.type isr_\name\()_top, @function
	.align 16,0
isr_\name\()_top:
	subl $4, %esp		# trash error
	pushal
	pushl %ds
	pushl %es
	pushl $\vecn
	pushl %esp
	movl $(SEG_KERNEL_DATA_HIGH << 3), %eax
	movl %eax, %ds
	movl %eax, %es
	jmp isr_irq_common
	.endm


	.align 16,0
isr_irq_common:
	call isr_irq_bottom
	addl $8, %esp		# pop regs, reason parameters
	popl %es
	popl %ds
	popal
	addl $4, %esp		# pop trash error
	iret


	/* ISR_IRQ_FAST produces the same as ISR_IRQ, but calls a specific
	 * bottom half.
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
	addl $8, %esp
	popl %es
	popl %ds
	popal
	addl $4, %esp		# pop errorcode
	iret
	.endm

	# timer interrupt is consumed by microkernel. applications may bind to
	# interrupt 0, but it'll never fire.
	ISR_IRQ_FAST 0x20, irq0
	ISR_IRQ 0x21, irq1		# keyboard
	# TODO: add the other XT-PIC things. perhaps a routine that checks the
	# interrupt service field in the PIC to find out which one fired,
	# rather than optimizing it by the vector.

	ISR_IRQ_FAST 0x30, apic	# fixed interrupts via the I/O APIC


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
	ISR_EXN 0x8d, exregs_sc, 0	# ExchangeRegisters
	ISR_EXN 0x8e, memctl_sc, 0	# MemoryControl
	ISR_EXN 0x8f, basic_sc, 0	# basic syscall


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
# TODO: it'd be more efficient to use a per-syscall return path instead of one
# that restores all registers except EDX and ECX, if only by a couple of load
# instructions.
#
# TODO: similarly, _sysenter_top could dispatch for sys_ipc() and
# sys_threadswitch() directly, without going through sysenter_bottom(). this
# would have a considerable advantage due to no construction of the x86_exregs
# frame.

	.global _sysenter_top
	.type _sysenter_top, @function
	.align 16,0
_sysenter_top:
	ljmp $(SEG_KERNEL_CODE_HIGH << 3), $1f	# restore kernel %cs, %eip
1:	pushl %ebx		# save syscall target
	andl $0x0fffffff, %esp	# drop back to kernel %ss (EXTRA FUCKY)
	movl $(SEG_KERNEL_DATA_HIGH << 3), %ebx
	movl %ebx, %ss
	sti
	movl %ebx, %ds
	movl %ebx, %es
	# construct generic x86_exregs frame
	pushl $USER_DATA_SEL	# .ss
	pushl %ebp		# .esp
	pushf			# .eflags
	pushl $USER_CODE_SEL	# .cs (future)
	pushl $0		# .eip (false)
	pushl 20(%esp)		# .error (syscall target)
	pushal
	pushl $USER_DATA_SEL	# .ds (surrogate)
	pushl $USER_DATA_SEL	# .es (surrogate)
	pushl $0		# .reason (not used)
	pushl %esp		# frame address on sysenter stack
	call sysenter_bottom

	addl $4, %esp		# TODO: remove this, adjust offsets below
	movl 12(%esp), %edi
	movl 16(%esp), %esi
	movl 20(%esp), %ebp
	movl 28(%esp), %ebx

	movl 48(%esp), %edx	# eip
	movl 60(%esp), %ecx	# esp

	pushl 56(%esp)		# eflags
	pushl 44(%esp)		# eax (temporary, at +4 due to preceding push)
	movl $USER_DATA_SEL, %eax
	movl %eax, %ds
	movl %eax, %es
	movl %eax, %fs
	popl %eax
	popf
	sysexit
