
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
# NOTE: a simpler version of this syscall would end up acting super wonky if an
# interrupt started right in between the two instructions that load the clock's
# halves into eax:edx, and that the process context was only returned into
# after the high 32 bits had been carried over into. (that occurs for once in
# about 49 days for the timer interrupt alone.)
#
# this case is remedied by by testing %edx against the original value after
# reading the less significant half, and restarting if it isn't the same. if it
# is the same, the lower half will be current even if there was an interrupt or
# other preemption event in between.
	.align 16,0
	.global soft_systemclock
soft_systemclock:
	call 1f
2:
	movl 4(%ecx), %edx	# most significant half
	movl (%ecx), %eax	# least significant half
	cmpl %edx, 4(%ecx)
	jne 2b
	ret
1:
	# space the returns apart a bit for some hardware's benefit.
	movl (%esp), %ecx
	orl $0xfff, %ecx
	andl $~7, %ecx
	ret
