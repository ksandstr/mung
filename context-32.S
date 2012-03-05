
	.equ USER_CODE_SEG, 1
	.equ USER_DATA_SEG, 2

	.global swap_context
	.type swap_context, @function

	.align 16,0
swap_context:
	movl 4(%esp), %eax
	movl %ebp, 24(%eax)
	movl %ebx, 4(%eax)
	movl %edi, 20(%eax)
	movl %esi, 16(%eax)
	movl %esp, 28(%eax)
	pushf
	popl 36(%eax)

	movl 8(%esp), %eax
	movl 24(%eax), %ebp
	movl 4(%eax), %ebx
	movl 20(%eax), %edi
	movl 16(%eax), %esi
	movl 28(%eax), %esp
	/* no need to restore flags over swap_context() */
	xorl %eax, %eax

	/* bombs away! */
	ret


	.global swap_to_ring3
	.type swap_to_ring3, @function

	.align 16,0
swap_to_ring3:
	movl 4(%esp), %eax	# the kernelspace (function call) context
	movl %ebp, 24(%eax)
	movl %ebx, 4(%eax)
	movl %edi, 20(%eax)
	movl %esi, 16(%eax)
	movl %esp, 28(%eax)
	pushf
	popl 36(%eax)
	# ... segments, too?

	movl 8(%esp), %eax	# the userspace context
	movl 4(%eax), %ebx
	movl 8(%eax), %ecx
	movl 12(%eax), %edx
	movl 16(%eax), %esi
	movl 20(%eax), %edi
	movl 24(%eax), %ebp
	pushl $((USER_DATA_SEG << 3) + 0x3)	# stack segment, ring 3
	pushl 28(%eax)		# sp
	pushl 36(%eax)		# eflags
	pushl $((USER_CODE_SEG << 3) + 0x3)	# code segment, ring 3
	pushl 32(%eax)		# ip
	pushl (%eax)		# eax (temporary)
	movl $((USER_DATA_SEG << 3) + 0x3), %eax
	movl %eax, %ds
	movl %eax, %es
	movl %eax, %fs
	movl %eax, %gs
	popl %eax
	iret
