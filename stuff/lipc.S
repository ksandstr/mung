
	.global user_lipc, regular_old_ipc, lipc_epilog

	utcb.processorno = -14 * 4
	utcb.mr1 = 1 * 4
	utcb.mr2 = 2 * 4

	# input registers:
	#   %eax = to, %ecx = timeouts, %edx = fromspec,
	#   %esi = mr0, %edi = caller UTCB
	# must preserve caller's %esp somewhere before entering kernel.
user_lipc:
	movl %eax, %ebp
	andl $0x3f, %ebp	# mask off version bits for LTID test
	xorl %ebx, %ebx
	testl %eax, %eax	# test for send phase
	sete %bl
	orl %ebx, %ebp		# %ebp = !localtid | !send
	xorl %ebx, %ebx
	testl %edx, %edx	# test for receive phase
	sete %bl
	orl %ebx, %ebp		# %ebp = !localtid | !send | !recv
	movl %esi, %ebx
	andl $0xffc0, %ebx	# mask tag.t, tag.flags
	orl %ebx, %ebp		# %ebp |= !no_typed | !no_flags
#if 0
	movl %ecx, %ebx
	andl $0xffff, %ebx	# mask RcvTimeout
	orl %ebx, %ebp		# %ebp |= !recvto_is_never
#endif
	testl %ebp, %ebp
	jne regular_old_ipc
	# now that it's known that dest is a LTID, dig around in its UTCB.
	movl utcb.processorno(%eax), %ebx
	cmpl utcb.processorno(%edi), %ebx
	jne regular_old_ipc
	movl utcb.mr1(%edi), %ebx
	movl utcb.mr2(%edi), %ebp
	int $0x8c

	# the sysenter alternative...
	movb $2, %ebx
	pushl utcb.mr2(%edi)
	pushl utcb.mr1(%edi)
	movl %esp, %ebp
	sysenter

regular_old_ipc:
	int $42		# FFFFUUUUUU

lipc_epilog:
	# userspace returns here when a sender's kernel-side Lipc checks were
	# successful. this code copies the sender's untyped message from its
	# UTCB so that the kernel need not access either thread's UTCB
	# directly. (that saves a few cache lines' worth of remapping work in
	# the kernel, which might be significant under load.)
	#
	# registers: %edi = receiver's UTCB, %eax = sender's LTID = UTCB
	#            %esi = message tag = MR0, %ebx = MR1, %ebp = MR2
	cmpw $2, %si
	jle 2f		# in-reg transfer of up to 2 MRs (iff no flags apply)
	pushl %esi
	pushl %edi
	andl $0x3f, %esi
	leal -2(%esi), %ecx	# u - 2
	leal 12(%eax), %esi	# read from src:MR2..
	leal 12(%edi), %edi	# write to dst:MR2..
	rep
	movsl
	popl %edi
	popl %esi
2:	ret

