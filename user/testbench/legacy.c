
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

#include <l4/types.h>
#include <l4/thread.h>
#include <l4/ipc.h>
#include <l4/syscall.h>

#include <ukernel/util.h>

#include "test.h"
#include "defs.h"


static void malloc_test(void)
{
	void *ptr = malloc(66);
	if(ptr != NULL) {
		printf("%s: allocated memory at %p\n", __func__, ptr);
		strlcpy(ptr, "hello, malloc!", 66);
		free(ptr);
	} else {
		printf("%s: malloc failed!\n", __func__);
	}
	fail_if(ptr == NULL);
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


/* NOTE: this test is useless in the absence of a serious sys_unmap() in the
 * kernel.
 */
void unmap_test(void)
{
	printf("unmap test start.\n");

	L4_Fpage_t pages[] = {
		L4_FpageLog2(0xdeadbeef, 12),
		L4_FpageLog2(0xcafebabe, 12),
	};
	const int n_pages = sizeof(pages) / sizeof(pages[0]);
	for(int i=0; i < n_pages; i++) {
		L4_Set_Rights(&pages[i], L4_FullyAccessible);
		L4_LoadMR(i, pages[i].raw);
	}
	L4_Fpage_t out_pages[n_pages];
	L4_Unmap(1);
	for(int i=0; i < n_pages; i++) L4_StoreMR(i, &out_pages[i].raw);

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


void sleep_test(void)
{
	L4_Time_t t = L4_TimePeriod(500000);
	printf("half a second is e %#x, m %#x\n", t.period.e, t.period.m);

	printf("testbench sleeping for 0.5s...\n");
	L4_Word64_t sleep_start = L4_SystemClock().raw;
	L4_Sleep(L4_TimePeriod(500000));
	L4_Word64_t wake = L4_SystemClock().raw;
	printf("testbench woke up at %llu; slept for %llu ticks\n", wake,
		wake - sleep_start);
}


void legacy_tests(void)
{
	malloc_test();

	tid_test();
	threadctl_test();

	threadswitch_test();
	sleep_test();
	unmap_test();
	spacectl_test();
}
