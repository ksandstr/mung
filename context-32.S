#include <ukernel/gdt.h>

	.global swap_context
	.type swap_context, @function


	.align 16,0
swap_context:
	movl 4(%esp), %eax
	movl %edi, (%eax)
	movl %esi, 4(%eax)
	movl %ebp, 8(%eax)
	movl %esp, 12(%eax)
	movl %ebx, 16(%eax)
	/* these two are for that function that switches back from user
	 * space. (maybe.)
	 */
	movl (%esp), %ebx
	movl %ebx, 32(%eax)
	pushf
	popl 36(%eax)

	movl 8(%esp), %eax
	movl 12(%eax), %esp
	addl $4, %esp
	movl (%eax), %edi
	movl 4(%eax), %esi
	movl 8(%eax), %ebp
	movl 16(%eax), %ebx
	/* no need to restore flags over swap_context() */
	/* bombs away! */
	movl 32(%eax), %eax
	jmp *%eax


/* void swap_to_ring3(struct x86_ctx *store, *load, int gs_sel) */
	.global swap_to_ring3
	.type swap_to_ring3, @function

	.equ USER_DATA_SEL, (SEG_USER_DATA << 3 + 0x3)
	.equ USER_CODE_SEL, (SEG_USER_CODE << 3 + 0x3)

	.align 16,0
swap_to_ring3:
	movl 4(%esp), %eax
	movl %edi, (%eax)
	movl %esi, 4(%eax)
	movl %ebp, 8(%eax)
	movl %esp, 12(%eax)
	movl %ebx, 16(%eax)
	/* edx, ecx, eax are caller-saved. */

	/* eip and eflags for iret_to_scheduler() */
	movl (%esp), %ebx
	movl %ebx, 32(%eax)
	pushf
	popl 36(%eax)

	# %gs
	movl 12(%esp), %eax
	movl %eax, %gs

	movl 8(%esp), %eax	# the userspace context
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


/* exit from kthread (e.g. the scheduler) to userspace syscall epilogue. */
/* void sysexit_from_kth(struct x86_ctx *store, *load, int gs_sel) */
	.global sysexit_from_kth
	.type sysexit_from_kth, @function

	.equ USER_DATA_SEL, (SEG_USER_DATA << 3 + 0x3)
	.equ USER_CODE_SEL, (SEG_USER_CODE << 3 + 0x3)

	.align 16,0
sysexit_from_kth:
	movl 4(%esp), %eax
	movl %edi, (%eax)
	movl %esi, 4(%eax)
	movl %ebp, 8(%eax)
	movl %esp, 12(%eax)
	movl %ebx, 16(%eax)
	/* edx, ecx, eax are caller-saved. */

	/* eip and eflags for iret_to_scheduler() */
	movl (%esp), %ebx
	movl %ebx, 32(%eax)
	pushf
	popl 36(%eax)
	# ... segments, too?

	addl $4, %esp	# make stack look like it's for sysexit_to_ring3
	jmp sysexit_to_ring3


/* NORETURN void sysexit_to_ring3(const struct x86_ctx *r3_ctx, int gs_sel) */
	.global sysexit_to_ring3
	.type sysexit_to_ring3, @function
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


/* NORETURN void iret_to_scheduler(const struct x86_ctx *sched_ctx) */
/* this function is called from isr-32.S's macros. it should be renamed to
 * "iret_to_kthread()" or some such.
 */
	.global iret_to_scheduler
	.type iret_to_scheduler, @function

	.equ KERNEL_CODE_SEL, (SEG_KERNEL_CODE_HIGH << 3)
	.equ KERNEL_DATA_SEL, (SEG_KERNEL_DATA_HIGH << 3)

	.align 16,0
iret_to_scheduler:
	movl 4(%esp), %eax
	# switch to the scheduler's stack and pop the return address
	movl 12(%eax), %esp
	addl $4, %esp
	# restore scheduler frame
	movl (%eax), %edi
	movl 4(%eax), %esi
	movl 8(%eax), %ebp
	movl 16(%eax), %ebx
	# prepare intra-PL iret stack
	pushl 36(%eax)		# eflags
	pushl $KERNEL_CODE_SEL	# cs
	pushl 32(%eax)		# eip
	# we can overwrite eax as swap_to_ring3() returns void.
	movl $KERNEL_DATA_SEL, %eax
	movl %eax, %ds
	movl %eax, %es
	iret


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
