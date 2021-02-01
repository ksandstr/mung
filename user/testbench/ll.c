
#include <ccan/compiler/compiler.h>
#include <l4/types.h>

#include <ukernel/mm.h>


COLD void fault_pages_in(L4_Word_t start, L4_Word_t end)
{
	for(L4_Word_t addr = start; addr < end; addr += PAGE_SIZE) {
		*(volatile L4_Word_t *)addr = *(volatile L4_Word_t *)addr;
	}
}


COLD void fault_own_pages(void)
{
	extern char _start, _end;
	fault_pages_in((L4_Word_t)&_start & ~PAGE_MASK,
		((L4_Word_t)&_end + PAGE_SIZE - 1) & ~PAGE_MASK);
}
