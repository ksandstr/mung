
#include <stdio.h>
#include <assert.h>

#include <l4/types.h>
#include <l4/thread.h>
#include <l4/ipc.h>
#include <l4/syscall.h>

#include "defs.h"


static void helper_fn(void *param)
{
	printf("%s: sleeping for 15 ms\n", __func__);
	L4_Sleep(L4_TimePeriod(15000));
}


void sched_test(void)
{
	printf("%s: starting helper thread\n", __func__);
	L4_ThreadId_t tid = start_thread(&helper_fn, NULL);
	if(L4_IsNilThread(tid)) {
		printf("%s: helper thread didn't start\n", __func__);
		return;
	}

	printf("%s: whee!\n", __func__);

	join_thread(tid);
	printf("%s: after helper exit!\n", __func__);
}
