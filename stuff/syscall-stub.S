
# source used to generate the values used in kip.c/make_syscall_stub().

	.global regular_syscall
regular_syscall:
	pushl %ebx		# only for ThreadSwitch (see kip.c)
	movl $0xdd, %ebx
	int $0x8f
	popl %ebx
	ret


# this reads the last 8 bytes of the kernel interface page, located via %eip.
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
