
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <ccan/list/list.h>

#include <l4/types.h>
#include <l4/thread.h>
#include <l4/ipc.h>
#include <l4/schedule.h>

#include "defs.h"
#include "test.h"


#define printf(fmt, ...) log_f(fmt, ##__VA_ARGS__)


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
	/* FIXME: replace these with nonlocal, diag()-printing exits */
	assert(!L4_IsNilThread(helper));

	L4_Word_t res = L4_Set_Priority(helper, priority);
	assert((res & 0xff) != 0);

	L4_MsgTag_t tag = L4_Receive(helper);
	assert(L4_IpcSucceeded(tag));

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


START_TEST(r_recv_timeout_test)
{
	plan_tests(4);

	ok(r_recv_timeout_case(98, false, false) == 0x3,
		"timeout in immediate nosend");
	ok(r_recv_timeout_case(98, false, true) == 0,
		"successful immediate send");
	ok(r_recv_timeout_case(98, true, false) == 0x3,
		"timeout in spin, nosend");
	ok(r_recv_timeout_case(98, true, true) == 0x3,
		"timeout in spin, send");
}
END_TEST


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
	printf("%s: sleeping for %u ms\n", __func__, sleep_ms);
	L4_Sleep(L4_TimePeriod(sleep_ms * 1000));
}


#define start_preempt(sleep_ms) \
	start_thread(&preempt_fn, (void *)((L4_Word_t)(sleep_ms)))


struct preempt_exn_result
{
	L4_Clock_t loop_start, first_preempt;
	int num_exn, num_wake;
	struct list_head wakeups;
};


struct preempt_wakeup {
	struct list_node result_link;
	L4_Clock_t clock;
};


/* test preemption exceptions. records just their time of occurrence, which
 * should coincide with preempt_fn's sleep or the thread's timeslice ending.
 */
static bool preempt_exn_case(
	struct preempt_exn_result *r,
	L4_Time_t spinner_ts,
	bool signal_preempt,
	int preempt_delay,
	int spin_time_ms,
	int receive_wait_ms)
{
	bool ok = true;
	int my_pri = find_own_priority();
	L4_ThreadId_t spinner = start_spinner(my_pri - 2, spin_time_ms,
		spinner_ts, signal_preempt);
	assert(!L4_IsNilThread(spinner));

	L4_ThreadId_t preempt;
	if(preempt_delay > 0) {
		preempt = start_preempt(preempt_delay);
		assert(!L4_IsNilThread(preempt));
	} else {
		preempt = L4_nilthread;
	}

	r->num_exn = 0;
	r->num_wake = 0;
	r->first_preempt.raw = 0;

	r->loop_start = L4_SystemClock();
	L4_Clock_t now;
	L4_MsgTag_t tag;
	do {
		tag = L4_Receive_Timeout(spinner,
			L4_TimePeriod(receive_wait_ms * 1000));
		L4_Word_t mrs[64];
		int num_words = tag.X.u + tag.X.t;
		if(num_words > 63) num_words = 63;
		if(L4_IpcSucceeded(tag)) L4_StoreMRs(0, num_words + 1, mrs);

		r->num_wake++;
		now = L4_SystemClock();
		struct preempt_wakeup *w = malloc(sizeof(*w));
		if(w != NULL) {
			w->clock = now;
			list_add_tail(&r->wakeups, &w->result_link);
		} else {
			printf("wakeup at %llu not recorded due to malloc() issue",
				now.raw);
		}

		if(L4_IpcFailed(tag)) {
			if(L4_ErrorCode() != 0x3) {
				printf("%s: ipc error (code %#lx)\n", __func__,
					L4_ErrorCode());
				ok = false;
				break;
			}
		} else if(tag.X.label >> 4 == (-4u & 0xfff)) {
			const L4_Word_t *words = &mrs[1];

			/* should reply or the thread will stop. */
			tag.X.label = 0;
			L4_LoadMR(0, tag.raw);
			L4_LoadMRs(1, num_words, words);
			tag = L4_Reply(spinner);
			if(L4_IpcFailed(tag)) {
				printf("spinner preempt reply failed: ec %#lx\n",
					L4_ErrorCode());
				ok = false;
				break;
			}
			if(r->num_exn == 0) r->first_preempt = L4_SystemClock();
			r->num_exn++;
		} else if(tag.X.u == 0) {
			/* ordinary regular spinner exit */
			break;
		} else {
			printf("%s: got unexpected message (label %#lx, u %#lx, t %#lx)\n",
				__func__, (L4_Word_t)tag.X.label, (L4_Word_t)tag.X.u,
				(L4_Word_t)tag.X.t);
		}
	} while(r->loop_start.raw + 100 > now.raw);

	join_thread(spinner);
	join_thread(preempt);

	return ok;
}


static void free_preempt_exn_result(struct preempt_exn_result *res)
{
	struct preempt_wakeup *w, *w_next;
	list_for_each_safe(&res->wakeups, w, w_next, result_link) {
		list_del_from(&res->wakeups, &w->result_link);
		free(w);
	}
	assert(list_empty(&res->wakeups));
	free(res);
}


START_TEST(simple_preempt_test)
{
	plan_tests(6);

	struct preempt_exn_result *res = malloc(sizeof(*res));
	list_head_init(&res->wakeups);
	if(!preempt_exn_case(res, L4_TimePeriod(120 * 1000),
		false, 0, 25, 4))
	{
		skip(666, "preempt_exn_case() failed");
		goto end;
	}

	/* the result should be at least five wakeups with at least three ms in
	 * between.
	 */
	ok1(res->num_wake >= 5);

	uint64_t prev = 0;
	int count = 0;	/* # of intervals seen */
	struct preempt_wakeup *w;
	list_for_each(&res->wakeups, w, result_link) {
		if(prev == 0) {
			assert(w == list_top(&res->wakeups, struct preempt_wakeup,
				result_link));
			prev = w->clock.raw;
		} else {
			ok1(w->clock.raw - prev >= 3);
			if(++count == 4) break;
		}
	}
	ok1(count >= 4);

end:
	free_preempt_exn_result(res);
}
END_TEST


START_LOOP_TEST(preempt_exn_test, t)
{
	assert(t >= 0 && t < 8);

	/* there are three boolean variables here. the first is timeslice length,
	 * the second is the preemption signaling switch, and the third is whether
	 * there'll be a higher-priority wakeup 10 ms in.
	 */
	bool big_ts = (t & 0x01) != 0,
		sig_pe = (t & 0x02) != 0,
		has_pe = (t & 0x04) != 0;

	plan_tests(!sig_pe ? 2 : 3);

	struct preempt_exn_result *res = calloc(1, sizeof(*res));
	list_head_init(&res->wakeups);
	/* receive preempts the spinner once towards the end. */
	if(!preempt_exn_case(res, L4_TimePeriod((big_ts ? 120 : 4) * 1000),
		sig_pe, has_pe ? 10 : 0, 25, 22))
	{
		/* TODO: replace with skip_all() once nonlocal exits have been
		 * implemented
		 */
		skip(!sig_pe ? 2 : 3, "preempt_exn_case() failed in iter %d", t);
		goto end;
	}

	assert(res->num_exn <= res->num_wake);
	if(!sig_pe) {
		/* no preemption signaling implies no preemptions were signaled,
		 * regardless of the other variables.
		 */
		ok(res->num_exn == 0, "only signal exceptions on request");
		/* should wake up once due to the receive timeout at 22ms, and another
		 * time when the spinner exits.
		 */
		ok(res->num_wake == 2, "exn wrapper should return from ipc twice");
	} else {
		ok1(res->num_exn > 0);
		uint32_t diff = res->first_preempt.raw - res->loop_start.raw;
#if 1
		printf("started at %llu, first preempt at %llu (diff %u), preempted %d time(s)\n",
			res->loop_start.raw, res->first_preempt.raw, diff, res->num_exn);
#endif
		if(big_ts && has_pe) {
			/* the preempt_fn causes a preemption, which satisifes the 22-ms
			 * receive function, which then doesn't preempt the thread again.
			 *
			 * it's conceivable this might cause two preemptions, though.
			 */
			ok(res->num_exn == 1 || res->num_exn == 2,
				"spinner should be preempted once or twice");
			ok(diff >= 10 && diff <= 13,
				"first spinner preempt should occur at between 10..13 ms");
		} else if(big_ts && !has_pe) {
			ok(res->num_exn == 1,
				"spinner should be preempted once");
			ok(diff >= 20,
				"first spinner preempt should occur at 20 ms or later");
		} else if(!big_ts && has_pe) {
			/* six times for timeslice (25 / 4 = 6.25) and once for
			 * preempt_fn.
			 */
			ok1(res->num_exn == 7);
			ok(diff < 8,
				"short timeslice should cause preemption before 10 ms");
		} else {
			assert(!big_ts && !has_pe);
			ok1(res->num_exn == 6);		/* see above */
			ok1(diff < 8);
		}
	}

end:
	free_preempt_exn_result(res);
}
END_TEST


/* setup: spinner thread at priority P-2 for 25 ms; preempt thread at P-1 to
 * interrupt at 5ms. spinner thread set to signal preemptions. spinner
 * thread's maximum delay set to 10 ms.
 *
 * variables:
 *   - spinner thread set to delay preemptions, or not
 *   - spinner thread's sens_pri set to level of preempt thread, or not
 *   - whether spinner thread is polite (i.e. yields on first observation of
 *     pending interrupt), or not
 *   - whether the spinner thread's quantum runs out during the preemption
 *     delay time, or not
 *
 * observe: times when preemptions occur. these should cover:
 *   1) whether preemption by higher-priority thread occurs
 *   2) time until preempted by higher-priority thread
 *   3) whether preemption by quantum works despite delay on priority
 *      preemption
 *
 * requisite: when the microkernel preempts a lower-priority thread and this
 * causes it to do IPC to a thread with higher priority than the preemptor's,
 * the IPC recipient should be scheduled first. any selection of preemptor,
 * such as for checking against sens_pri, may not result in a switch to the
 * preemptor thread until after the higher-or-equal priority exception handler
 * has switched off.
 */
START_LOOP_TEST(delay_preempt, t)
{
	bool delay_pe = (t & 0x1) != 0,
		high_sens_pri = (t & 0x2) != 0,
		polite = (t & 0x4) != 0,
		small_ts = (t & 0x8) != 0;

	if(t == 4) {
		plan_no_plan();
		fail("we're going down");
	} else {
		plan_skip_all("nothing is implemented");
		return;
	}

	/* TODO: everything specified above */
}
END_TEST


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


START_TEST(yield_timeslice_test)
{
	plan_tests(2);

	ok(yield_timeslice_case(false) >= 10,
		"yield should schedule out for at least 10 ms");
	ok(yield_timeslice_case(true) <= 5,
		"extraordinary scheduling should be preempted within 5 ms");
}
END_TEST


#if 0
/* TODO: move this into thread_test.c */
static void helper_fn(void *param)
{
	printf("%s: sleeping for 15 ms\n", __func__);
	L4_Sleep(L4_TimePeriod(15000));
}
#endif


Suite *sched_suite(void)
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

	Suite *s = suite_create("sched");

	TCase *ipc_case = tcase_create("ipc");
	tcase_add_test(ipc_case, r_recv_timeout_test);
	suite_add_tcase(s, ipc_case);

	TCase *preempt_case = tcase_create("preempt");
	tcase_add_test(preempt_case, simple_preempt_test);
	tcase_add_loop_test(preempt_case, preempt_exn_test, 0, 7);
	tcase_add_loop_test(preempt_case, delay_preempt, 0, 15);
	suite_add_tcase(s, preempt_case);

	TCase *yield_case = tcase_create("yield");
	tcase_add_test(yield_case, yield_timeslice_test);
	suite_add_tcase(s, yield_case);

	return s;
}
