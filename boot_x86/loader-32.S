

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


# stack for main
	.set STACKSIZE, 0x4000	# 16k
	.comm stack, STACKSIZE, 32


# loader program
	.global loader
loader:
	movl $(stack + STACKSIZE), %esp
	pushl %eax
	pushl %ebx
	call kmain
	cli
hang:
	hlt
	jmp hang
