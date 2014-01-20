/* microkernel setjmp. allows recovery from in-kernel pagefaults. semantics
 * are like regular setjmp/longjmp.
 */

#ifndef SEEN_UKERNEL_SETJMP_H
#define SEEN_UKERNEL_SETJMP_H

#include <ccan/compiler/compiler.h>

#include <ukernel/x86.h>

typedef struct x86_exregs jmp_buf[1];

extern int setjmp(jmp_buf env);
extern NORETURN void longjmp(jmp_buf env, int val);


#endif
