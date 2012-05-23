
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
}


static L4_Word_t r_recv_timeout_case(int priority, bool spin, bool send)
{
	const int timeout_ms = 20;

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
		do {
			delay_loop(iters_per_tick / 100);
		} while(start.raw == L4_SystemClock().raw);

		start = L4_SystemClock();
		do {
			/* ishygddt */
			delay_loop(iters_per_tick);
		} while(start.raw + timeout_ms > L4_SystemClock().raw + 1);
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


static void spinner_fn(void *param_ptr)
{
	const L4_Word_t *param = param_ptr;
	printf("spinner spins for %u ms...\n", param[1]);

	L4_Clock_t start = L4_SystemClock();
	do {
		delay_loop(iters_per_tick / 4);
	} while(start.raw + param[1] > L4_SystemClock().raw);

	printf("spinner thread exiting.\n");

	L4_LoadMR(0, 0);
	L4_Reply((L4_ThreadId_t){ .raw = param[0] });
}


static void preempt_test(void)
{
	static L4_Word_t param[2];
	param[0] = L4_Myself().raw;
	param[1] = 15;		/* # of ms to spin */
	L4_ThreadId_t spinner = start_thread(&spinner_fn, param);
	fail_if(L4_IsNilThread(spinner), "can't start spinner thread");

	/* give it a 5ms slice, and a neverending total quantum */
	L4_Word_t res = L4_Set_Timeslice(spinner, L4_TimePeriod(5 * 1000),
		L4_Never);
	if(res == L4_SCHEDRESULT_ERROR) {
		printf("%s: L4_Set_Timeslice() failed; errorcode %u\n", __func__,
			L4_ErrorCode());
		return;
	}
	printf("spinner thread state is %u\n", res);

	/* now wait until the preemption thing happens. */
	L4_MsgTag_t tag = L4_Receive(spinner);
	fail_if(L4_IpcFailed(tag));
}


#if 0
/* TODO: move this into thread_test.c */
static void helper_fn(void *param)
{
	printf("%s: sleeping for 15 ms\n", __func__);
	L4_Sleep(L4_TimePeriod(15000));
}
#endif


void sched_test(void)
{
#if 0
	printf("%s: starting helper thread\n", __func__);
	L4_ThreadId_t tid = start_thread(&helper_fn, NULL);
	if(L4_IsNilThread(tid)) {
		printf("%s: helper thread didn't start\n", __func__);
		return;
	}

	printf("%s: whee!\n", __func__);

	join_thread(tid);
	printf("%s: after helper exit!\n", __func__);
#endif

	r_recv_timeout_test();
	preempt_test();
}
