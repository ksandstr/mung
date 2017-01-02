
#ifndef SEEN_UKERNEL_TRAMPOLINE_H
#define SEEN_UKERNEL_TRAMPOLINE_H

#include <stdbool.h>

/* usage:
 *
 * in a header file:
 *
 *     extern int determined_at_runtime(char *, ...);
 *
 * in the defining module, toplevel:
 *
 *     #include <ukernel/trampoline.h>
 *
 *     DEFINE_TRAMPOLINE(determined_at_runtime);
 *
 *     static void int impl_0(char *, ...) {
 *         panic("cat vom is mostly fur");
 *     }
 *
 *     void initialize(void) {
 *         SET_TRAMPOLINE(determined_at_runtime, impl_0);
 *     }
 *
 * note that SET_TRAMPOLINE works with the value of impl_0 rather than the
 * symbol. calling a trampoline function before it is set panics the kernel.
 */

#define DEFINE_TRAMPOLINE(tname) \
	asm (".text\n" \
		 ".global " #tname "\n" \
		 ".type " #tname ", @function\n" \
		 ".align 16,0\n" \
		 #tname ":\n" \
		 "\tcall undefined_trampoline\n" \
		 ".align 16,0\n" \
		 ".previous\n")

#define SET_TRAMPOLINE(tname, fnaddr) __set_trampoline_once(&tname, (fnaddr))


extern void __set_trampoline_once(void *tptr, void *fnaddr);
extern void undefined_trampoline(void);

#endif
