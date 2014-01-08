
	.global _sc_ipc
	.global _sc_exregs
	.global _sc_exr_epilog
	.global _sc_sched_epilog

_sc_ipc:
	movb $1, %bl
	movl %esp, %ebp
	sysenter


_sc_exregs:
	pushl %ebp
	movl %gs:0, %ebp
	movl %ebx, -8(%ebp)
	popl -4(%ebp)
	movb $0xdd, %bl
	movl %esp, %ebp
	sysenter

_sc_exr_epilog:
	movl %gs:0, %ecx
	movl -8(%ecx), %edx
	movl -4(%ecx), %ecx
	ret

_sc_sched_epilog:
	movl %gs:0, %edx
	movl -8(%edx), %edx
	ret

