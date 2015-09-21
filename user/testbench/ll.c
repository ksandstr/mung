
#include <ccan/compiler/compiler.h>
#include <l4/types.h>

#include <ukernel/mm.h>


COLD void fault_own_pages(void)
{
	extern char _start, _end;
	for(L4_Word_t addr = (L4_Word_t)&_start & ~PAGE_MASK;
		addr < (((L4_Word_t)&_end + PAGE_SIZE - 1) & ~PAGE_MASK);
		addr += PAGE_SIZE)
	{
		*(volatile L4_Word_t *)addr = *(volatile L4_Word_t *)addr;
	}
}
