
/* setjmp() and longjmp() */

	.global setjmp
	.type setjmp, @function
	.align 16,0
setjmp:
	movl 4(%esp), %eax
	movl (%esp), %edx	# return address
	movl %ebx, (%eax)
	movl %esi, 4(%eax)
	movl %edi, 8(%eax)
	movl %ebp, 12(%eax)
	movl %edx, 16(%eax)
	movl %esp, 20(%eax)
	xorl %eax, %eax
	ret


	.global longjmp
	.type longjmp, @function
	.align 16,0
longjmp:
	movl 4(%esp), %ecx	# context
	movl 16(%ecx), %edx	# return address
	movl 8(%esp), %eax	# setjmp return value
	movl 20(%ecx), %esp
	addl $4, %esp
	movl (%ecx), %ebx
	movl 4(%ecx), %esi
	movl 8(%ecx), %edi
	movl 12(%ecx), %ebp
	jmp *%edx
