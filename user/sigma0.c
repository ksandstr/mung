
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <ccan/compiler/compiler.h>

#include <l4/types.h>
#include <l4/ipc.h>
#include <l4/thread.h>
#include <l4/vregs.h>
#include <l4/syscall.h>


static void con_putstr(const char *str, size_t len);


int vprintf(const char *fmt, va_list al)
{
	char buffer[256];
	int n = vsnprintf(buffer, sizeof(buffer), fmt, al);
	if(n > 0) con_putstr(buffer, n);
	return n;
}


int printf(const char *format, ...)
{
	va_list al;
	va_start(al, format);
	int n = vprintf(format, al);
	va_end(al);
	return n;
}


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


static void con_putstr(const char *str, size_t len)
{
	void *utcb = __L4_Get_UtcbAddress();
	L4_VREG(utcb, L4_TCR_MR(0)) = ((L4_MsgTag_t){
		.X.label = 0x5370, /* "pS" */
		.X.u = (len + 3) / 4,
	}).raw;
	for(int i=0; i * 4 < len; i++) {
		L4_VREG(utcb, L4_TCR_MR(i + 1)) = *(L4_Word_t *)&str[i * 4];
	}
	L4_ThreadId_t dummy;
	L4_Ipc(L4_Pager(), L4_Pager(), L4_Timeouts(L4_Never, L4_Never), &dummy);
}


void *malloc(size_t size) {
	printf("malloc() in sigma0 is a stub\n");
	return NULL;
}


void __assert_failure(
	const char *condition,
	const char *file,
	unsigned int line,
	const char *function)
{
	printf("assert(%s) failed in `%s' (%s:%u)\n", condition, function,
		file, line);
	for(;;) { /* spin */ }
}


static void tid_test(void)
{
	printf("threadid test start.\n");

	printf("\tL4_Myself() == %#x\n", (unsigned)L4_Myself().raw);
	printf("\tL4_MyLocalId() == %#x\n", (unsigned)L4_MyLocalId().raw);
	printf("\tL4_LocalIdOf(L4_MyGlobalId()) == %#x\n",
		(unsigned)L4_LocalIdOf(L4_MyGlobalId()).raw);
	printf("\tL4_GlobalIdOf(L4_MyLocalId()) == %#x\n",
		(unsigned)L4_GlobalIdOf(L4_MyLocalId()).raw);

	printf("threadid test ends.\n");
}


int main(void)
{
	printf("hello, world!\n");

	L4_Word_t apiver, apiflags, kernelid;
	void *kip = L4_KernelInterface(&apiver, &apiflags, &kernelid);

	send_test(0xdeadbeef);
	send_test(L4_SystemClock().raw);

	tid_test();

	/* L4_Word64_t now = */ L4_SystemClock();

	*(char *)(kip - 0x1000) = '*';	/* pagefault at a definite spot */

	return 0;
}
