
	.global isr_top
	.type isr_top, @function

# NOTE: this is broken. should be reworked to resemble isr14_top instead.
	.align 16,0
isr_top:
	pushal
	pushl %gs
	pushl %fs
	pushl %ds
	pushl %es
	call isr_bottom
	popl %es
	popl %ds
	popl %fs
	popl %gs
	popal
	iret


	.global isr14_top
	.type isr14_top, @function

	.align 16,0
isr14_top:
	pushal
	pushl %ds
	pushl %es
	pushl $14		# exception 14, #PF
	pushl %esp
	call isr14_bottom
	addl $8, %esp
	popl %es
	popl %ds
	popal
	addl $4, %esp		# the error code
	iret


	.global isr_irq1_top
	.type isr_irq1_top, @function

	.align 16, 0
isr_irq1_top:
	pushal
	pushl %ds
	pushl %es
	pushl $0x8001		# IRQ 1
	pushl %esp
	call isr_irq1_bottom
	addl $8, %esp
	popl %es
	popl %ds
	popal
	iret
