
	.global isr_top
	.type isr_top, @function

# NOTE: this is broken. should be reworked to resemble isr14_top instead.
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
