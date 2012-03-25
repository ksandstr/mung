
# multiboot header
	.set ALIGN, 1<<0
	.set MEMINFO, 1<<1
	.set FLAGS, ALIGN | MEMINFO
	.set MAGIC, 0x1BADB002
	.set CHECKSUM, -(MAGIC + FLAGS)

	.align 4
	.long MAGIC
	.long FLAGS
	.long CHECKSUM


# loader program
	.global loader
	.global _start
_start:
loader:
	movl $(stack + STACKSIZE), %esp

	/* clear EFLAGS */
	pushl $0
	popf

	/* call bootmain(mbi, magic) */
	pushl %eax
	pushl %ebx
	call bootmain
	cli
hang:
	hlt
	jmp hang


# stack for main
	.set STACKSIZE, 0x4000	# 16k
	.comm stack, STACKSIZE, 32
