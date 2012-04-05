
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <ccan/compiler/compiler.h>

#include <l4/types.h>
#include <l4/ipc.h>
#include <l4/thread.h>
#include <l4/vregs.h>
#include <l4/syscall.h>

#define CHECK_FLAG(mask, bit) (((mask) & (bit)) != 0)


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


static int sigma0_ipc_loop(void *kip_base)
{
	/* FIXME: add "proper" L4_LoadMR(), L4_StoreMR() functions */
	void *utcb = __L4_Get_UtcbAddress();
	for(;;) {
		L4_ThreadId_t from = L4_nilthread;
		L4_MsgTag_t tag = L4_Ipc(L4_nilthread, L4_anythread,
			L4_Timeouts(L4_Never, L4_Never), &from);

		for(;;) {
			if(L4_IpcFailed(tag)) {
				printf("pager ipc failed (errorcode %#x)\n", L4_ErrorCode());
				break;
			}

			L4_ThreadId_t sender = from;
			if((tag.X.label & 0xfff0) == 0xffe0
				&& tag.X.u == 2 && tag.X.t == 0)
			{
				L4_Word_t ip = L4_VREG(utcb, L4_TCR_MR(2)),
					addr = L4_VREG(utcb, L4_TCR_MR(1));
				printf("pagefault from %d:%d (ip %#x, addr %#x)\n",
					from.global.X.thread_no, from.global.X.version,
					ip, addr);
				static L4_Word_t last_fault = 0;
				if(last_fault == addr) {
					printf("... two faults at the same address! s0 rejects this.\n");
					break;
				} else {
					last_fault = addr;
				}
				L4_Fpage_t page = L4_FpageLog2(addr, 12);
				L4_Set_Rights(&page, L4_FullyAccessible);
				L4_MapItem_t idemp = L4_MapItem(page,
					addr & ~((1 << 12) - 1));
				L4_VREG(utcb, L4_TCR_MR(0)) = (L4_MsgTag_t){ .X.t = 2 }.raw;
				L4_VREG(utcb, L4_TCR_MR(1)) = idemp.raw[0];
				L4_VREG(utcb, L4_TCR_MR(2)) = idemp.raw[1];
			} else if((tag.X.label & 0xfff0) == 0xffa0
				&& tag.X.u == 2 && tag.X.t == 0)
			{
				/* s0 user protocol: fpage request */
				L4_Fpage_t req_fpage = { .raw = L4_VREG(utcb, L4_TCR_MR(1)) };
				L4_Word_t req_attr = L4_VREG(utcb, L4_TCR_MR(2));
				printf("roottask requested page %#x:%#x attr %#x\n",
					L4_Address(req_fpage), L4_Size(req_fpage), req_attr);
				/* FIXME: implement this by allocating a "struct free_page"
				 * per aligned fpage that the KIP's memorydesc ranges leave,
				 * up to the largest page size that the KIP declares native
				 * support for, or 4 MiB if smaller. stick these in buckets.
				 * allocate new slab pages from those buckets, smallest first,
				 * breaking up pages as necessary, with the same allocator as
				 * used by the fpage request protocol and the pagefault
				 * protcol handlers, above.
				 */
				break;
			} else {
				printf("unknown IPC label %#x (u %d, t %d) from %u:%u\n",
					tag.X.label, tag.X.u, tag.X.t, from.global.X.thread_no,
					from.global.X.version);
				break;
			}

			/* ReplyWait */
			from = L4_nilthread;
			tag = L4_Ipc(sender, L4_anythread,
				L4_Timeouts(L4_ZeroTime, L4_Never), &from);
		}
	}

	/* never reached, but whatever. */
	return 0;
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


void unmap_test(void)
{
	printf("unmap test start.\n");

	void *utcb = __L4_Get_UtcbAddress();
	L4_Fpage_t pages[] = {
		L4_FpageLog2(0xdeadbeef, 12),
		L4_FpageLog2(0xcafebabe, 12),
	};
	const int n_pages = sizeof(pages) / sizeof(pages[0]);
	for(int i=0; i < n_pages; i++) {
		L4_Set_Rights(&pages[i], L4_FullyAccessible);
		L4_VREG(utcb, L4_TCR_MR(i)) = pages[i].raw;
	}
	L4_Fpage_t out_pages[n_pages];
	L4_Unmap(1);
	for(int i=0; i < n_pages; i++) {
		out_pages[i].raw = L4_VREG(utcb, L4_TCR_MR(i));
	}

	for(int i=0; i < n_pages; i++) {
		L4_Fpage_t fp = out_pages[i];
		printf("page %d: %#x:%#x, was %c%c%c\n", i,
			(unsigned)L4_Address(fp), (unsigned)L4_Size(fp),
			CHECK_FLAG(L4_Rights(fp), L4_Readable) ? 'r' : '-',
			CHECK_FLAG(L4_Rights(fp), L4_Writable) ? 'w' : '-',
			CHECK_FLAG(L4_Rights(fp), L4_eXecutable) ? 'x' : '-');
	}

	printf("unmap test ends.\n");
}


static void threadctl_test(void)
{
	printf("threadcontrol test start.\n");

	/* thread creation. */
	L4_ThreadId_t dest = { .global = { .X.version = 1, .X.thread_no = 129 } };
	L4_Word_t result = L4_ThreadControl(dest, L4_Myself(), L4_nilthread,
		L4_Pager(), __L4_Get_UtcbAddress() + 512);
	printf("creating threadcontrol result %u\n", (unsigned)result);
	if(result == 0) {
		printf("error code %#x\n", L4_ErrorCode());
	}

	printf("threadcontrol test ends.\n");
}


void spacectl_test(void)
{
	printf("spacecontrol test start.\n");

	L4_Word_t old_ctl, result = L4_SpaceControl(L4_Myself(),
		0xcafeb00b, L4_FpageLog2(0x10000, 12), L4_FpageLog2(0x11000, 15),
		L4_nilthread, &old_ctl);
	printf("spacecontrol result %#x, old_ctl %#x\n", (unsigned)result,
		(unsigned)old_ctl);

	printf("spacecontrol test ends.\n");
}


static void threadswitch_test(void)
{
	printf("threadswitch test start.\n");
	L4_ThreadSwitch(L4_nilthread);
	printf("threadswitch test end.\n");
}


static void schedule_test(void)
{
	printf("schedule test start.\n");
	L4_Word_t old_tc;
	L4_Word_t res = L4_Schedule(L4_Myself(), 1, 2, 3, 4, &old_tc);
	printf("L4_Schedule() returned %#x (old_timectl %#x)\n",
		(unsigned)res, (unsigned)old_tc);
	printf("schedule test end.\n");
}


int main(void)
{
	printf("hello, world!\n");

	L4_Word_t apiver, apiflags, kernelid;
	void *kip = L4_KernelInterface(&apiver, &apiflags, &kernelid);

	send_test(0xdeadbeef);
	send_test(L4_SystemClock().raw);

	tid_test();
	unmap_test();
	threadctl_test();
	threadswitch_test();
	schedule_test();
	spacectl_test();

	return sigma0_ipc_loop(kip);
}
