
#include <ccan/compiler/compiler.h>

#include <l4/types.h>
#include <l4/ipc.h>
#include <l4/vregs.h>
#include <l4/syscall.h>


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


static L4_Word_t send_test(L4_Word_t payload)
{
	void *utcb = __L4_Get_UtcbAddress();
	L4_VREG(utcb, L4_TCR_MR(0)) = ((L4_MsgTag_t){
		.X.label = 0x2369, .X.u = 1 }).raw;
	L4_VREG(utcb, L4_TCR_MR(1)) = payload;
	L4_ThreadId_t dummy;
	L4_MsgTag_t tag = L4_Ipc(L4_Pager(), L4_Pager(),
		L4_Timeouts(L4_Never, L4_Never), &dummy);
	if(L4_IpcFailed(tag)) return 0; else return L4_VREG(utcb, L4_TCR_MR(1));
}


int main(void)
{
	L4_Word_t apiver, apiflags, kernelid;
	void *kip = L4_KernelInterface(&apiver, &apiflags, &kernelid);

	send_test(0xdeadbeef);
	send_test(L4_SystemClock().raw);

	/* L4_Word64_t now = */ L4_SystemClock();

	*(char *)(kip - 0x1000) = '*';	/* pagefault at a definite spot */

	return 0;
}
