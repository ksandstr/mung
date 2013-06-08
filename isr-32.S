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

	ISR_IRQ 0x21, irq1
	ISR_IRQ 0x22, irq2
	ISR_IRQ 0x23, irq3
	/* ... TODO */

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

	ISR_IRQ_FAST 0x20, irq0


	.equ USER_DATA_SEL, (SEG_USER_DATA << 3 + 3)

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
