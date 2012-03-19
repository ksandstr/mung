
#include <l4/types.h>

void *L4_KernelInterface(
	L4_Word_t *apiver,
	L4_Word_t *apiflags,
	L4_Word_t *kernelid)
{
	L4_Word_t baseaddr;
	asm volatile (
		"lock; nop"
		: "=a" (baseaddr), "=c" (*apiver), "=d" (*apiflags), "=S" (*kernelid));
	return (void *)baseaddr;
}


int main(void)
{
	L4_Word_t apiver, apiflags, kernelid;
	void *kip = L4_KernelInterface(&apiver, &apiflags, &kernelid);

	*(char *)(kip - 0x1000) = '*';	/* pagefault at a definite spot */

	/* this causes a huge ugly page fault. */
	char *ptr = (char *)0xb0a7face;
	*ptr = '@';
}
