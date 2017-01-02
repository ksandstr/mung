
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
	/* NOTE: this allows re-set via undefined_trampoline, but assert() down
	 * below forbids it under !NDEBUG. not sure if that's useful or
	 * exploitable or what; BUG_ON() is supposed to be a developer tool
	 * anyway, not a security guarantee.
	 *
	 * "+ 5" is for the jmp insn's length.
	 */
	BUG_ON(*(uint8_t *)tptr == 0xe9
		&& *(uint32_t *)(tptr + 1) !=
			(uintptr_t)&undefined_trampoline - ((uintptr_t)tptr + 5),
		"trampoline %p already set!", tptr);

	*(uint8_t *)tptr = 0xe9;	/* jmp rel32 */
	*(uint32_t *)(tptr + 1) = (uintptr_t)fnaddr - ((uintptr_t)tptr + 5);

	asm volatile ("clflush (%0)" :: "r" (tptr) : "memory");

	assert(*(uint8_t *)tptr == 0xe9);
	assert(*(uint32_t *)(tptr + 1) !=
		(uintptr_t)&undefined_trampoline - ((uintptr_t)tptr + 5));
}
