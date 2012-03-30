
# loader program
	.global loader
	.global _start
_start:
loader:
	movl $(stack + STACKSIZE), %esp

	/* clear EFLAGS */
	pushl $0
	popf

	/* call kmain(*P, magic) */
	pushl %eax
	pushl %ebx
	call kmain
	cli
hang:
	hlt
	jmp hang


# stack for main
	.set STACKSIZE, 0x4000	# 16k
	.comm stack, STACKSIZE, 32
