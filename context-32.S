#define IN_ASM_SOURCE
#include <ukernel/gdt.h>

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
	movl (%esp), %ebx
	movl %ebx, 32(%eax)
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

	.equ USER_DATA_SEL, (SEG_USER_DATA << 3 + 0x3)
	.equ USER_CODE_SEL, (SEG_USER_CODE << 3 + 0x3)

	.align 16,0
swap_to_ring3:
	movl 4(%esp), %eax	# the kernelspace (function call) context
	movl %ebp, 24(%eax)
	movl %ebx, 4(%eax)
	movl %edi, 20(%eax)
	movl %esi, 16(%eax)
	movl %esp, 28(%eax)
	movl (%esp), %ebx	# return address
	movl %ebx, 32(%eax)
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
	pushl $USER_DATA_SEL	# stack segment, ring 3
	pushl 28(%eax)		# sp
	pushl 36(%eax)		# eflags
	pushl $USER_CODE_SEL	# code segment, ring 3
	pushl 32(%eax)		# ip
	pushl (%eax)		# eax (temporary)
	movl $USER_DATA_SEL, %eax
	movl %eax, %ds
	movl %eax, %es
	movl %eax, %fs
	movl %eax, %gs
	popl %eax
	iret


/* NORETURN void iret_to_scheduler(const struct x86_context *sched_ctx) */
	.global iret_to_scheduler
	.type iret_to_scheduler, @function

	.equ KERNEL_CODE_SEL, (SEG_KERNEL_CODE_HIGH << 3)
	.equ KERNEL_DATA_SEL, (SEG_KERNEL_DATA_HIGH << 3)

	.align 16,0
iret_to_scheduler:
	movl 4(%esp), %eax
	# switch to the scheduler's stack.
	movl 28(%eax), %esp
	addl $4, %esp		# pop return address from swap_to_ring3()
	# restore scheduler frame
	movl 4(%eax), %ebx
	movl 8(%eax), %ecx
	movl 12(%eax), %edx
	movl 16(%eax), %esi
	movl 20(%eax), %edi
	movl 24(%eax), %ebp
	# prepare intra-PL iret stack
	pushl 36(%eax)		# eflags
	pushl $KERNEL_CODE_SEL
	pushl 32(%eax)
	# we can overwrite eax as swap_to_ring3() returns void.
	movl $KERNEL_DATA_SEL, %eax
	movl %eax, %ds
	movl %eax, %es
	movl %eax, %fs
	movl %eax, %gs
	iret
