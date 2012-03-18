

	.text
	.global _start

	.align 16
_start:
	leal __stack, %esp
	pushl $__return_from_main
	jmp main


__return_from_main:
	movl $1, %ebx
	int $1
	jmp __return_from_main

# two pages' worth should be well enough.
	.bss
	.space 8192
__stack:
