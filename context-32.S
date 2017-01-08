#include <ukernel/gdt.h>


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
