
#ifndef __L4__KDEBUG_H__
#define __L4__KDEBUG_H__

/* via L4Ka::Pistachio, slightly modified */
#define L4_KDB_Enter(...) \
	do { \
		__asm__ __volatile__ ( \
			"/* L4_KDB_Enter() */\n" \
			"\tint $3\n" \
			"\tjmp 2f\n" \
			"\tmov 1f, %eax\n" \
			".section .rodata\n" \
			"1: .ascii \"" __VA_ARGS__ "\"\n" \
			"   .byte 0\n" \
			".previous\n" \
			"2:\n"); \
	} while(0)


/* rest of the KDB interface left unimplemented. */

#endif
