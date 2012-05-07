
# source used to generate the values used in kip.c/make_syscall_stub().

	.global regular_syscall
regular_syscall:
	pushl %ebx		# only for ThreadSwitch (see kip.c)
	movl $0xdd, %ebx
	int $0x8f
	popl %ebx
	ret


# this reads the last 8 bytes of the kernel interface page, located via %eip.
#
# NOTE: this might end up acting super wonky if a timer interrupt starts right
# between the instructions that load the clock's halves into eax:edx, and that
# interrupt causes a carry between the low 32 bits and the high 32 bits. which
# for a millisecond timer is once about every 49 days. more seriously this
# segment is not safe against concurrent preemption, a major flaw in system
# call interface code.
#
# these cases could be prevented by testing %edx against the original value
# after reading the less significant half, and restarting if it isn't the same.
# if it is the same, the lower half will be current even if there was an
# interrupt or other preemption event in between.
	.align 16,0
	.global soft_systemclock
soft_systemclock:
	call 1f
	movl 4(%eax), %edx
	movl (%eax), %eax
	ret
1:
	# space the returns apart a bit for some hardware's benefit.
	movl (%esp), %eax
	orl $0xfff, %eax
	andl $~7, %eax
	ret
