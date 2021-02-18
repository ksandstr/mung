
#ifndef SEEN_FAKE_CLIB_SETJMP_H
#define SEEN_FAKE_CLIB_SETJMP_H

#include <stdint.h>
#include <stdnoreturn.h>


typedef struct jmp_buf_s {
	uint32_t regs[6];		/* ebx, esi, edi, ebp, eip, esp */
} jmp_buf[1];

extern __attribute__((__returns_twice__)) int setjmp(jmp_buf env);
extern noreturn void longjmp(jmp_buf env, int val);


#endif
