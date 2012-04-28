
	.section .text

	.global exn_store_regs
	.align 16,0
exn_store_regs:
	movl %eax, (%eax)
	movl %ebx, 4(%eax)
	movl %ecx, 8(%eax)
	movl %edx, 12(%eax)
	movl %esi, 16(%eax)
	movl %edi, 20(%eax)
	movl %ebp, 24(%eax)
	movl %esp, 28(%eax)
	jmp *%ebx
