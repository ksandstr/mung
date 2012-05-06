
#include <stdio.h>
#include <assert.h>

#include <l4/types.h>
#include <l4/thread.h>
#include <l4/ipc.h>
#include <l4/schedule.h>

#include "defs.h"


/* test case for the correct ipc error result in threads that time out in the
 * TS_R_RECV state.
 *
 * two cases: with an incoming message, and without. the former tests whether
 * the TS_R_RECV timeout is handled correctly in the receive override case,
 * which isn't implemented as of 2012-05-06.
 */
static void r_recv_timeout_fn(void *param_ptr)
{
	printf("%s: entered; param is %p\n", __func__, param_ptr);

	/* in-parameters: [0] = partner TID, [1] = timeout (L4_Time_t) */
	L4_Word_t *param = param_ptr;
	L4_ThreadId_t partner = { .raw = param[0] };
	L4_Set_UserDefinedHandle(partner.raw);
	L4_Time_t timeout = { .raw = param[1] };

	L4_LoadMR(0, (L4_MsgTag_t){ .X.u = 1 }.raw);
	L4_LoadMR(1, 0xfedcba98);
	L4_ThreadId_t dummy;
	L4_MsgTag_t tag = L4_Ipc(partner, partner,
		L4_Timeouts(L4_Never, timeout), &dummy);
	if(L4_IpcSucceeded(tag)) param[0] = 0; else param[0] = L4_ErrorCode();

	printf("%s: exiting (return value %#x)\n", __func__, param[0]);
}


static L4_Word_t r_recv_timeout_case(int priority, bool spin, bool send)
{
	const int timeout_ms = 15;

	static L4_Word_t param[2];
	param[0] = L4_Myself().raw;
	param[1] = L4_TimePeriod(timeout_ms * 1000).raw;
	L4_ThreadId_t helper = start_thread(&r_recv_timeout_fn, param);
	fail_if(L4_IsNilThread(helper), "can't start helper thread");

	L4_Word_t res = L4_Set_Priority(helper, priority);
	fail_if((res & 0xff) == 0, "L4_Set_Priority(..., %d) failed: ec %#lx",
		priority, L4_ErrorCode());

	L4_MsgTag_t tag = L4_Receive(helper);
	fail_if(L4_IpcFailed(tag), "error in receive from helper: ec %#lx",
		L4_ErrorCode());

	if(spin) {
		L4_Clock_t start = L4_SystemClock();
		volatile int foo = 0;
		do {
			/* ishygddt */
			for(int i=0; i < 2048; i++) foo += i;
		} while(L4_SystemClock().raw < start.raw + timeout_ms * 1000);
	}

	if(send) {
		L4_LoadMR(0, 0);
		L4_Reply(helper);
	}

	join_thread(helper);

	return param[0];
}


static void r_recv_timeout_test(void)
{
	fail_if(r_recv_timeout_case(98, false, false) != 0x3,
		"expected timeout in immediate nosend");
	fail_if(r_recv_timeout_case(98, false, true) != 0,
		"expected successful immediate send");
	fail_if(r_recv_timeout_case(98, true, false) != 0x3,
		"expected timeout in spin, nosend");
	fail_if(r_recv_timeout_case(98, true, true) != 0x3,
		"expected timeout in spin, send");
}


/* TODO: move this into thread_test.c */
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


	/* actual test cases */
	r_recv_timeout_test();
}
