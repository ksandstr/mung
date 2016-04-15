#include <ukernel/gdt.h>


/* L4_Word_t save_kth_context(void) */
	.global save_kth_context
	.type save_kth_context, @function
	.align 16,0
save_kth_context:
	movl current_thread, %eax	# context at +8
	movl (%esp), %edx	# return address
	movl %edx, 40(%eax)	# eip
	pushf
	popl 44(%eax)		# eflags
	movl %edi, 8(%eax)
	movl %esi, 12(%eax)
	movl %ebp, 16(%eax)
	movl %esp, 20(%eax)
	movl %ebx, 24(%eax)
	movl $1, 36(%eax)
	xorl %eax, %eax
	ret


/* void load_context(const struct x86_ctx *ctx) */
	.global load_context
	.type load_context, @function

	.align 16,0
load_context:
	movl 4(%esp), %eax
	movl 12(%eax), %esp
	addl $4, %esp
	movl (%eax), %edi
	movl 4(%eax), %esi
	movl 8(%eax), %ebp
	movl 16(%eax), %ebx
	/* restore flags just 'cause */
	pushl 36(%eax)
	popf
	/* bombs away! */
	movl 32(%eax), %ecx
	movl 28(%eax), %eax
	jmp *%ecx


/* void iret_to_ring3(struct x86_ctx *ctx, int gs_sel) */
	.global iret_to_ring3
	.type iret_to_ring3, @function

	.equ USER_DATA_SEL, (SEG_USER_DATA << 3 + 0x3)
	.equ USER_CODE_SEL, (SEG_USER_CODE << 3 + 0x3)

	.align 16,0
iret_to_ring3:
	# %gs
	movl 8(%esp), %eax
	movl %eax, %gs

	movl 4(%esp), %eax	# the userspace context
	movl (%eax), %edi
	movl 4(%eax), %esi
	movl 8(%eax), %ebp
	movl 16(%eax), %ebx
	movl 20(%eax), %edx
	movl 24(%eax), %ecx

	pushl $USER_DATA_SEL	# stack segment, ring 3
	pushl 12(%eax)		# esp
	pushl 36(%eax)		# eflags
	pushl $USER_CODE_SEL	# code segment, ring 3
	pushl 32(%eax)		# ip
	pushl 28(%eax)		# eax (temporary)
	movl $USER_DATA_SEL, %eax
	movl %eax, %ds
	movl %eax, %es
	movl %eax, %fs
	popl %eax
	iret


/* exit from kernel to userspace syscall epilogue. */
/* void sysexit_to_ring3(struct x86_ctx *ctx, int gs_sel) */
	.global sysexit_to_ring3
	.type sysexit_to_ring3, @function

	.equ USER_DATA_SEL, (SEG_USER_DATA << 3 + 0x3)
	.equ USER_CODE_SEL, (SEG_USER_CODE << 3 + 0x3)

	.align 16,0
sysexit_to_ring3:
	movl 8(%esp), %eax
	movl %eax, %gs

	movl 4(%esp), %eax
	movl (%eax), %edi
	movl 4(%eax), %esi
	movl 8(%eax), %ebp
	movl 16(%eax), %ebx

	movl 32(%eax), %edx	# eip
	movl 12(%eax), %ecx	# esp

	pushl 36(%eax)		# eflags
	pushl 28(%eax)		# eax (temporary)
	movl $USER_DATA_SEL, %eax
	movl %eax, %ds
	movl %eax, %es
	movl %eax, %fs
	popl %eax
	popf
	sysexit


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
