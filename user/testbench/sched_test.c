
#include <stdio.h>
#include <stdlib.h>
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


struct spinner_param
{
	L4_ThreadId_t parent;
	L4_Word_t spin_ms;
	bool signal_preempt;
};


static void spinner_fn(void *param_ptr)
{
	struct spinner_param *param = param_ptr;

	if(param->signal_preempt) {
		L4_EnablePreemptionFaultException();
	}

#if 0
	printf("spinner spins for %lu ms from %llu...\n", param->spin_ms,
		L4_SystemClock().raw);
#endif

	L4_Clock_t start = L4_SystemClock();
	do {
		delay_loop(iters_per_tick / 4);
	} while(start.raw + param->spin_ms > L4_SystemClock().raw);

#if 0
	printf("spinner thread exiting at %llu.\n",
		L4_SystemClock().raw);
#endif

	if(!L4_IsNilThread(param->parent)) {
		L4_LoadMR(0, 0);
		L4_Send_Timeout(param->parent, L4_TimePeriod(5 * 1000));
	}

	free(param);
}


/* NOTE: copypasta'd from <ukernel/util.h> */
static uint64_t time_in_us(L4_Time_t t)
{
	/* only defined for periods. c'mon. that's what "in" means. */
	assert(t.period.a == 0);
	if(t.raw == L4_ZeroTime.raw) return 0;
	else return (uint32_t)t.period.m * (1u << t.period.e);
}


static L4_ThreadId_t start_spinner(
	int priority,
	int spin_ms,
	L4_Time_t timeslice,
	bool signal_preempt)
{
	assert(spin_ms > 0);
	assert(time_in_us(timeslice) >= 1000);
	assert(priority >= 0 && priority <= 0xff);

	struct spinner_param *p = malloc(sizeof(*p));
	if(p == NULL) return L4_nilthread;

	*p = (struct spinner_param){
		.parent = L4_Myself(),
		.spin_ms = spin_ms,
		.signal_preempt = signal_preempt,
	};
	L4_ThreadId_t spinner = start_thread_long(&spinner_fn, p,
		priority, timeslice, L4_Never);
	if(L4_IsNilThread(spinner)) free(p);
	return spinner;
}


static void preempt_test(void)
{
	L4_ThreadId_t spinner = start_spinner(2, 15,
		L4_TimePeriod(5 * 1000), false);
	printf("returned to %s after spinner start\n", __func__);
	fail_if(L4_IsNilThread(spinner), "can't start spinner thread");

	L4_Time_t ts, tq;
	L4_Word_t res = L4_Timeslice(spinner, &ts, &tq);
	if(res == L4_SCHEDRESULT_ERROR) {
		printf("%s: L4_Timeslice() failed; errorcode %#lx\n", __func__,
			L4_ErrorCode());
		return;
	}
	printf("spinner thread state is %lu; has %u µs quantum\n",
		res, (unsigned)time_in_us(ts));

	/* now wait until the preemption thing happens, but wake up every 3 ms
	 * just to cause preemptions in the spinner thread.
	 */
	L4_Clock_t start = L4_SystemClock();
	L4_MsgTag_t tag;
	do {
		tag = L4_Receive_Timeout(spinner, L4_TimePeriod(3 * 1000));
		if(L4_IpcFailed(tag)) {
			printf("spinner didn't send yet at %llu\n",
				L4_SystemClock().raw);
		} else if(tag.X.label >> 4 == (-4u & 0xfff)) {
			L4_Word_t words[63];
			int num = tag.X.u + tag.X.t;
			if(num > 64) num = 64;
			L4_StoreMRs(1, num, words);
			printf("got exception of %d words: ip %#lx\n", num, words[0]);

			/* should reply, or the spinner will stop. */
			tag.X.label = 0;
			L4_LoadMR(0, tag.raw);
			L4_LoadMRs(1, num, words);
			tag = L4_Reply(spinner);
			if(L4_IpcFailed(tag)) {
				printf("spinner preempt reply failed: ec %#lx\n",
					L4_ErrorCode());
			}
		} else if(tag.X.u == 0) {
			printf("got regular spinner exit\n");
			break;
		} else {
			printf("%s: got unexpected message (label %#lx, u %#lx, t %#lx)\n",
				__func__, (L4_Word_t)tag.X.label, (L4_Word_t)tag.X.u,
				(L4_Word_t)tag.X.t);
		}
	} while(start.raw + 100 > L4_SystemClock().raw);

	join_thread(spinner);
}


/* feh. */
static int find_own_priority(void)
{
	int pri = 254;
	L4_ThreadId_t self = L4_Myself();
	while(pri > 0 && (L4_Set_Priority(self, pri) & 0xff) == 0
		&& L4_ErrorCode() == 5)
	{
		/* invalid parameter means the priority was too high. this loop stops
		 * when "pri" is the actual priority of this thread, without altering
		 * the kernel setting.
		 */
		pri--;
	}
	if(pri == 0) {
		printf("%s: found zero? wtf.\n", __func__);
		pri = 100;		/* make shit up! */
	}
	return pri;
}


static void preempt_fn(void *param_ptr)
{
	unsigned int sleep_ms = (L4_Word_t)param_ptr;
#if 0
	printf("%s: sleeping for %u ms\n", __func__, sleep_ms);
#endif
	L4_Sleep(L4_TimePeriod(sleep_ms * 1000));
}


#define start_preempt(sleep_ms) \
	start_thread(&preempt_fn, (void *)((L4_Word_t)(sleep_ms)))


struct preempt_exn_result
{
	L4_Clock_t loop_start, first_preempt;
	int num;
};


/* test preemption exceptions. records just their time of occurrence, which
 * should coincide with preempt_fn's sleep or the thread's timeslice ending.
 */
static bool preempt_exn_case(
	struct preempt_exn_result *r,
	L4_Time_t spinner_ts,
	bool signal_preempt,
	int preempt_delay)
{
	bool ok = true;
	int my_pri = find_own_priority();
	L4_ThreadId_t spinner = start_spinner(my_pri - 2, 25, spinner_ts,
		signal_preempt);
	fail_if(L4_IsNilThread(spinner));

	L4_ThreadId_t preempt;
	if(preempt_delay > 0) {
		preempt = start_preempt(preempt_delay);
		fail_if(L4_IsNilThread(preempt));
	} else {
		preempt = L4_nilthread;
	}

	r->num = 0;
	r->first_preempt.raw = 0;

	r->loop_start = L4_SystemClock();
	L4_MsgTag_t tag;
	do {
		/* the timeout is long enough to preempt the spinner just once toward
		 * the end.
		 */
		tag = L4_Receive_Timeout(spinner, L4_TimePeriod(22 * 1000));
		if(L4_IpcFailed(tag)) {
			if(L4_ErrorCode() != 0x3) {
				printf("%s: ipc error (code %#lx)\n", __func__,
					L4_ErrorCode());
				ok = false;
				break;
			}
		} else if(tag.X.label >> 4 == (-4u & 0xfff)) {
			L4_Word_t words[63];
			int num = tag.X.u + tag.X.t;
			if(num > 64) num = 64;
			L4_StoreMRs(1, num, words);

			/* should reply or the thread will stop. */
			tag.X.label = 0;
			L4_LoadMR(0, tag.raw);
			L4_LoadMRs(1, num, words);
			tag = L4_Reply(spinner);
			if(L4_IpcFailed(tag)) {
				printf("spinner preempt reply failed: ec %#lx\n",
					L4_ErrorCode());
				ok = false;
				break;
			}
			if(r->num == 0) r->first_preempt = L4_SystemClock();
			r->num++;
		} else if(tag.X.u == 0) {
			/* ordinary regular spinner exit */
			break;
		} else {
			printf("%s: got unexpected message (label %#lx, u %#lx, t %#lx)\n",
				__func__, (L4_Word_t)tag.X.label, (L4_Word_t)tag.X.u,
				(L4_Word_t)tag.X.t);
		}
	} while(r->loop_start.raw + 100 > L4_SystemClock().raw);

	join_thread(spinner);
	join_thread(preempt);

	return ok;
}


static void preempt_exn_test(void)
{
	struct preempt_exn_result *res = malloc(sizeof(*res));
	printf("%s: started\n", __func__);

	/* there are three boolean variables here. the first is the timeslice
	 * length, the second is the preemption signaling switch, and the third is
	 * whether there'll be a higher-priority wakeup 10 ms in.
	 */
	for(int t=0; t < 8; t++) {
		bool big_ts = (t & 0x01) != 0,
			sig_pe = (t & 0x02) != 0,
			has_pe = (t & 0x04) != 0;

		fail_if(!preempt_exn_case(res,
			L4_TimePeriod((big_ts ? 120 : 4) * 1000),
			sig_pe, has_pe ? 10 : 0));

		/* no preemption signaling implies no preemptions were signaled,
		 * regardless of the other variables.
		 */
		if(!sig_pe) {
			fail_unless(res->num == 0);
			continue;
		}

		fail_unless(res->num > 0);
		uint32_t diff = res->first_preempt.raw - res->loop_start.raw;
#if 0
		printf("started at %llu, first preempt at %llu (diff %u), preempted %d time(s)\n",
			res->loop_start.raw, res->first_preempt.raw, diff, res->num);
#endif
		if(big_ts && has_pe) {
			/* the preempt_fn causes a preemption, which satisifes the 22-ms
			 * receive function, which then doesn't preempt the thread again.
			 *
			 * it's conceivable this might cause two preemptions, though.
			 */
			fail_unless(res->num == 1 || res->num == 2,
				"spinner should be preempted once or twice");
			fail_unless(diff >= 10 && diff <= 13,
				"first spinner preempt should occur at between 10..13 ms");
		} else if(big_ts && !has_pe) {
			fail_unless(res->num == 1,
				"spinner should be preempted once");
			fail_unless(diff >= 20,
				"first spinner preempt should occur at 20 ms or later");
		} else if(!big_ts && has_pe) {
			/* six times for timeslice (25 / 4 = 6.25) and once for
			 * preempt_fn.
			 */
			fail_unless(res->num == 7);
			fail_unless(diff < 8,
				"short timeslice should cause preemption before 10 ms");
		} else {
			assert(!big_ts && !has_pe);
			fail_unless(res->num == 6);		/* see above */
			fail_unless(diff < 8);
		}
	}

	printf("%s: ending\n", __func__);
	free(res);
}


/* returns the difference between spinner switch and return therefrom. */
static int yield_timeslice_case(bool preempt_spinner)
{
	int my_pri = find_own_priority();
	printf("%s: starting, my_pri is %d\n", __func__, my_pri);

	L4_ThreadId_t spinner = start_spinner(my_pri - 2, 15,
		L4_TimePeriod(2 * 1000), false);
	printf("%s: returned after spinner start\n", __func__);

	L4_ThreadId_t preempt = L4_nilthread;
	if(preempt_spinner) {
		preempt = start_thread_long(&preempt_fn, (void *)4,
			my_pri - 1, L4_TimePeriod(2 * 1000), L4_Never);
		L4_ThreadSwitch(preempt);
	}

	L4_Clock_t start = L4_SystemClock();
	L4_ThreadSwitch(spinner);
	L4_Clock_t end = L4_SystemClock();
	printf("%s: switched to spinner at %llu, returned at %llu\n", __func__,
		start.raw, end.raw);

	L4_Receive_Timeout(spinner, L4_TimePeriod(45 * 1000));
	join_thread(spinner);

	if(preempt_spinner) join_thread(preempt);

	return end.raw - start.raw;
}


static void yield_timeslice_test(void)
{
	fail_if(yield_timeslice_case(false) < 10,
		"expected yield would schedule out for at least 10 ms");
	fail_if(yield_timeslice_case(true) > 5,
		"expected extraordinary scheduling to be preempted within 5 ms");
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
	preempt_exn_test();
	yield_timeslice_test();
}
