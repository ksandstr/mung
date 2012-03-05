
	/* ISR_IRQ produces IRQ interrupt handlers that call a common bottom
	 * half while pushing $vecn as the "reason" field in the parameter
	 * <struct x86_exregs *>.
	 */

	.macro ISR_IRQ vecn, name
	.global isr_\name\()_top
	.type isr_\name\()_top, @function
	.align 16,0
isr_\name\()_top:
	pushal
	pushl %ds
	pushl %es
	pushl $\vecn
	pushl %esp
	jmp isr_irq_common
	.endm

	ISR_IRQ 0x21, irq1
	ISR_IRQ 0x22, irq2
	ISR_IRQ 0x23, irq3
	/* ... TODO */

	.align 16,0
isr_irq_common:
	call isr_irq_bottom
	addl $8, %esp
	popl %es
	popl %ds
	popal
	iret


	/* ISR_IRQ_FAST produces the same as ISR_IRQ, but calls a specific
	 * bottom half.
	 */
	.macro ISR_IRQ_FAST vecn, name
	.global isr_\name\()_top
	.type isr_\name\()_top, @function
	.align 16,0
isr_\name\()_top:
	pushal
	call isr_\name\()_bottom
	popal
	iret
	.endm

	ISR_IRQ_FAST 0x20, irq0		# the timer gets speed, reqd or not.


	/* ISR_EXN produces exception interrupt handlers. these have individual
	 * bottom halves on the C side.
	 */
	.macro ISR_EXN vecn, name
	.global isr_exn_\name\()_top
	.type isr_exn_\name\()_top, @function
	.align 16,0
isr_exn_\name\()_top:
	# NOTE: what is this rigamarole necessary for? apparently pushal
	# references a segment other than SS, which triple-faults the CPU. this
	# seems to avoid it by loading the other data segments with
	# SEG_KERNEL_DATA_HIGH .
	pushl %eax
	movl $(7 << 3), %eax
	movl %eax, %ds
	movl %eax, %es
	movl %eax, %fs
	movl %eax, %gs
	popl %eax
	pushal
	pushl %ds		# no need to save these, really
	pushl %es
	pushl $\vecn
	pushl %esp
	call isr_exn_\name\()_bottom
	addl $8, %esp
	popl %es
	popl %ds
	popal
	addl $4, %esp		# the error code
	iret
	.endm

	ISR_EXN 0, de		# divide error
	ISR_EXN 13, gp		# general protection
	ISR_EXN 14, pf		# pagefault
