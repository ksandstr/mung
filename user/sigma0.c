
#include <ccan/compiler/compiler.h>

#include <l4/types.h>
#include <l4/ipc.h>
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


#if 0
L4_ThreadId_t L4_ExchangeRegisters(
	void *kip,
	L4_ThreadId_t dest,
	L4_Word_t *control_p,
	L4_Word_t *sp_p, L4_Word_t *ip_p, L4_Word_t *flags_p,
	L4_Word_t *udh_p,
	L4_ThreadId_t *pager_p)
{
	/* TODO: lift this from Pistachio. shit sucks. */
	L4_ThreadId_t result;
	asm volatile (
		"\tpushl %[scvec]\n"
		"\tmovl %[pager], %%ebp\n"
		"\tcall *(%%esp)\n"
		"\tmovl %%ebp, %[pager]\n"
		: [pager] "=m" (*pager_p),
		  "=a" (result.raw), "=c" (*control_p), "=d" (*sp_p),
		  "=S" (*ip_p), "=D" (*flags_p), "=b" (*udh_p)
		: [scvec] "r" (kip + *(L4_Word_t *)(kip + 0xec))
		: "ebp", "memory");

	return result;
}
#endif


CONST_FUNCTION void *__L4_Get_UtcbAddress(void) {
	void *ptr;
	asm volatile (
		"\tmovl %%gs:0, %0\n"
		: "=r" (ptr));
	return ptr;
}


L4_MsgTag_t L4_Ipc(
	void *kip,
	L4_ThreadId_t to,
	L4_ThreadId_t fromspec,
	L4_Word_t timeouts,
	L4_ThreadId_t *from_p)
{
	void *utcb = __L4_Get_UtcbAddress();
	L4_MsgTag_t tag;
	asm volatile (
		"\tcall *%[scvec]\n"
		"\tmovl %%ebp, %[mr2_out]\n"
#if 0
		"\tmovl %%eax, %[from_out]\n"
		"\tmovl %%esi, %[mr0_out]\n"
		"\tmovl %%ebx, %[mr1_out]\n"
		: [from_out] "=r" (*from_p),
		  [mr0_out] "=r" (tag.raw),
		  [mr1_out] "=m" (L4_VREG(utcb, L4_TCR_MR(1))),
		  [mr2_out] "=m" (L4_VREG(utcb, L4_TCR_MR(2)))
#else
		: "=a" (*from_p), "=S" (tag.raw), "=b" (L4_VREG(utcb, L4_TCR_MR(1))),
		  [mr2_out] "=m" (L4_VREG(utcb, L4_TCR_MR(2)))
#endif
		: "a" (to.raw), "c" (timeouts), "d" (fromspec.raw),
		  "S" (L4_VREG(utcb, L4_TCR_MR(0))), "D" (utcb),
		  [scvec] "r" (kip + *(L4_Word_t *)(kip + 0xe0))
		: "ebp");
	return tag;
}


#if 0
L4_ThreadId_t L4_MyGlobalId(void) {
	void *utcb = __L4_Get_UtcbAddress();
	return (L4_ThreadId_t){ .raw = L4_VREG(utcb, L4_TCR_MYGLOBALID) };
}


L4_ThreadId_t L4_MyLocalId(void *kip) {
	L4_Word_t dummy = 0;
	L4_ThreadId_t t_dummy = L4_nilthread;
	return L4_ExchangeRegisters(kip, L4_MyGlobalId(),
		&dummy, &dummy, &dummy, &dummy, &dummy, &t_dummy);
}
#endif


L4_ThreadId_t L4_Pager(void) {
	void *utcb = __L4_Get_UtcbAddress();
	return (L4_ThreadId_t){ .raw = L4_VREG(utcb, L4_TCR_PAGER) };
}


L4_Word_t L4_ErrorCode(void) {
	void *utcb = __L4_Get_UtcbAddress();
	return L4_VREG(utcb, L4_TCR_ERRORCODE);
}


static L4_Word_t send_test(void *kip, L4_Word_t payload)
{
	void *utcb = __L4_Get_UtcbAddress();
	L4_VREG(utcb, L4_TCR_MR(0)) = ((L4_MsgTag_t){
		.X.label = 0x2369, .X.u = 1 }).raw;
	L4_VREG(utcb, L4_TCR_MR(1)) = payload;
	L4_ThreadId_t dummy;
	L4_MsgTag_t tag = L4_Ipc(kip, L4_Pager(), L4_Pager(),
		L4_Timeouts(L4_Never, L4_Never), &dummy);
	if(L4_IpcFailed(tag)) return 0; else return L4_VREG(utcb, L4_TCR_MR(1));
}


int main(void)
{
	L4_Word_t apiver, apiflags, kernelid;
	void *kip = L4_KernelInterface(&apiver, &apiflags, &kernelid);

	send_test(kip, 0xdeadbeef);
	send_test(kip, L4_SystemClock(kip));

	/* L4_Word64_t now = */ L4_SystemClock(kip);

	*(char *)(kip - 0x1000) = '*';	/* pagefault at a definite spot */

	return 0;
}
