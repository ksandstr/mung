
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
	tid_test();
	sleep_test();
}
