
#include <stdio.h>

#include <ccan/compiler/compiler.h>

#include <ukernel/bug.h>
#include <ukernel/misc.h>
#include <ukernel/trampoline.h>


void undefined_trampoline(void)
{
	char msg[128];
	snprintf(msg, sizeof(msg), "%s called %s %p",
		__func__,
#ifndef NDEBUG
		"per",		/* address of trampoline */
#else
		"from",		/* address of call site */
#endif
		__builtin_return_address(0) - 5);
	panic(msg);
}


COLD void __set_trampoline_once(void *tptr, void *fnaddr)
{
	/* this allows re-set via undefined_trampoline, which is exploited in
	 * early paravirt support. in any case BUG_ON() is supposed to be just a
	 * developer tool anyway, so this doesn't matter too much.
	 *
	 * "+ 5" is for the jmp insn's length.
	 */
	BUG_ON(*(uint8_t *)tptr == 0xe9
		&& *(uint32_t *)(tptr + 1) !=
			(uintptr_t)&undefined_trampoline - ((uintptr_t)tptr + 5)
		&& fnaddr != &undefined_trampoline,
		"trampoline %p already set!", tptr);

	*(uint8_t *)tptr = 0xe9;	/* jmp rel32 */
	*(uint32_t *)(tptr + 1) = (uintptr_t)fnaddr - ((uintptr_t)tptr + 5);

	asm volatile ("clflush (%0)" :: "r" (tptr) : "memory");

	assert(*(uint8_t *)tptr == 0xe9);
}
