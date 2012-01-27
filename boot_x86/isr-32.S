
	.global isr_top

isr_top:
	pushal
	pushl %gs
	pushl %fs
	pushl %ds
	pushl %es
	call isr_bottom
	popl %gs
	popl %fs
	popl %ds
	popl %es
	popal
	iret


	.global isr14_top

isr14_top:
	pushal
	pushl %gs
	pushl %fs
	pushl %ds
	pushl %es
	movl %cr2, %eax
	pushl %eax
	call isr14_bottom
	popl %eax
	popl %gs
	popl %fs
	popl %ds
	popl %es
	popal
	iret
