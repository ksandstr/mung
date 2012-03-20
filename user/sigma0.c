
#include <l4/types.h>
#include <l4/vregs.h>


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


L4_Word64_t L4_SystemClock(void *kip)
{
	L4_Word_t low, high;
	asm volatile (
		"call *%2"
		: "=a" (low), "=d" (high)
		: "r" (kip + *(L4_Word_t *)(kip + 0xf0))
		: "ecx", "esi", "edi");
	return (L4_Word64_t)high << 32 | low;
}


void *__L4_Get_UtcbAddress(void) {
	void *ptr;
	asm volatile (
		"\tmovl %%gs:0, %0\n"
		: "=r" (ptr));
	return ptr;
}


int main(void)
{
	L4_Word_t apiver, apiflags, kernelid;
	void *kip = L4_KernelInterface(&apiver, &apiflags, &kernelid);

	void *utcb = __L4_Get_UtcbAddress();
	L4_VREG(utcb, L4_TCR_MR(1)) = 0xdeadbeef;

	/* L4_Word64_t now = */ L4_SystemClock(kip);

	*(char *)(kip - 0x1000) = '*';	/* pagefault at a definite spot */
}
