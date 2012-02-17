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

	movl 8(%esp), %eax
	movl 24(%eax), %ebp
	movl 4(%eax), %ebx
	movl 20(%eax), %edi
	movl 16(%eax), %esi
	movl 28(%eax), %esp
	xorl %eax, %eax

	/* bombs away! */
	ret
