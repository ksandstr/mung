
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>

#include <ccan/list/list.h>
#include <ccan/compiler/compiler.h>
#include <ccan/darray/darray.h>
#include <ccan/talloc/talloc.h>

#include <ukernel/util.h>

#include <l4/types.h>
#include <l4/kip.h>
#include <l4/thread.h>
#include <l4/ipc.h>
#include <l4/schedule.h>

#include "defs.h"
#include "test.h"


#define log(fmt, ...) log_f("%s: " fmt, __func__, ##__VA_ARGS__)


struct preempt_wakeup
{
	struct list_node result_link;
	L4_Clock_t clock;
	bool was_exn;
};


static bool delay_preempt_case(
	struct list_head *result_list,	/* adds <struct preempt_wakeup> */
	L4_Clock_t *start_time_p,
	bool delay_pe,
	bool high_sens_pri,
	bool polite,
	bool small_ts);


/* api tests */

static void starvin_marvin(void *param UNUSED)
{
	L4_MsgTag_t tag;
	do {
		L4_ThreadId_t somebody;
		tag = L4_Wait(&somebody);
		if(L4_IpcFailed(tag)) {
			diag("%s: ipc fail, ec %#lx", __func__, L4_ErrorCode());
			continue;
		}
	} while(tag.X.label != QUIT_LABEL);
}


static bool try_sched(L4_ThreadId_t o_tid)
{
	const int my_pri = find_own_priority();
	L4_Word_t dummy, res = L4_Schedule(o_tid,
		(L4_Word_t)L4_TimePeriod(10000).raw << 16 | L4_Never.raw,
		0, my_pri, my_pri << 16 | L4_ZeroTime.raw, &dummy);
	return res != L4_SCHEDRESULT_ERROR;
}


/* provoke as many distinct errors as possible in a single address space. */
START_TEST(single_as_errors)
{
	const int ref_pri = find_own_priority();
	const L4_Word_t ref_timectl = (L4_Word_t)L4_TimePeriod(15 * 1000).raw << 16
			| L4_Never.raw,
		ref_pectl = (ref_pri - 1) << 8 | L4_Never.raw;

	plan_tests(1 + 2*2 + 1 + 1);

	L4_ThreadId_t other = xstart_thread(&starvin_marvin, NULL);

	/* base case: pass (unexcitingly) */
	L4_Word_t timectl_out, ret = L4_Schedule(other, ref_timectl, 0, ref_pri,
		ref_pectl, &timectl_out);
	if(!ok(ret != L4_SCHEDRESULT_ERROR, "can succeed")) {
		diag("err %d, ec %#lx", ret, L4_ErrorCode());
	}

	/* part 1: fuck with time control.
	 * two halves: one for ts_len, another for total_quantum. same rules apply
	 * to both.
	 */
	for(int half = 0; half <= 1; half++) {
		const char *what = half == 0 ? "ts_len" : "total_quantum";
		/* the other thing, what's not tested. */
		L4_Word_t base = (L4_Word_t)L4_Never.raw << (half * 16);
		int shift = (1 - half) * 16;

		const struct {
			L4_Time_t t;
			const char *desc;
		} fails[] = {
			{ .desc = "zero", .t = L4_ZeroTime },
			{ .desc = "timepoint value", .t = L4_TimePoint((L4_Clock_t){
				.raw = L4_SystemClock().raw + 30 * 1000 }) },
		};
		for(int f=0; f < sizeof(fails) / sizeof(fails[0]); f++) {
			L4_Word_t val = base | (L4_Word_t)fails[f].t.raw << shift;
			ret = L4_Schedule(other, val, 0, ref_pri, ref_pectl,
				&timectl_out);
			L4_Word_t ec = L4_ErrorCode();
			if(!ok(ret == L4_SCHEDRESULT_ERROR && ec == 5,
				"reject %s for %s", fails[f].desc, what))
			{
				diag("ret=%lu, ec=%#lx", ret, ec);
			}
		}
	}

	/* part 2: screw with prioctl. */
	ret = L4_Schedule(other, ref_timectl, 0, ref_pri + 1, ref_pectl,
		&timectl_out);
	L4_Word_t ec = L4_ErrorCode();
	if(!ok(ret == L4_SCHEDRESULT_ERROR && ec == 5,
		"reject prio = ref_pri + 1"))
	{
		diag("ret=%lu, ec=%#lx", ret, ec);
	}

	/* part 3: sexually penetrate sens_pri. */
	ret = L4_Schedule(other, ref_timectl, 0, ref_pri,
		(ref_pectl & 0xffff) | (ref_pri + 1) << 16, &timectl_out);
	ec = L4_ErrorCode();
	if(!ok(ret == L4_SCHEDRESULT_ERROR && ec == 5,
		"reject sens_prio = ref_pri + 1"))
	{
		diag("ret=%lu, ec=%#lx", ret, ec);
	}

	send_quit(other);
	xjoin_thread(other);
}
END_TEST


/* test for the correct error results for attempting to schedule the interrupt
 * and system TID ranges.
 */
START_TEST(range_errors)
{
	plan_tests(5);
	L4_KernelInterfacePage_t *kip = L4_GetKernelInterface();
	const int last_int = kip->ThreadInfo.X.SystemBase - 1,
		last_sys = kip->ThreadInfo.X.UserBase - 1;
	diag("last_int=%d, last_sys=%d", last_int, last_sys);

	/* base case: can schedule self. */
	ok1(try_sched(L4_Myself()));

	/* part 1: scheduling an interrupt ain't cool. */
	skip_start(last_int < 0, 2, "no interrupt range!");
		ok(!try_sched(L4_GlobalId(last_int, 1)),
			"can't schedule an interrupt");
		L4_Word_t ec = L4_ErrorCode();
		if(!ok(ec == 2, "code is ``invalid thread''")) diag("ec=%#lx", ec);
	skip_end;

	/* part 2: same for the system range. */
	skip_start(last_sys <= last_int, 2, "no system range!");
		ok(!try_sched(L4_GlobalId(last_sys, 17)),
			"can't schedule a system thread");
		L4_Word_t ec = L4_ErrorCode();
		if(!ok(ec == 2, "code is ``invalid thread''")) diag("ec=%#lx", ec);
	skip_end;
}
END_TEST


/* simple ReadPrecision (SystemClock latency) test. */
START_TEST(kip_sysclock_latency)
{
	plan_tests(1);

	const int n_samples = 64;
	L4_KernelInterfacePage_t *kip = L4_GetKernelInterface();
	const L4_Time_t readprec = { .raw = kip->ClockInfo.X.ReadPrecision };
	unsigned readprec_us = L4_PeriodUs_NP(readprec);
	diag("readprec=%u µs", readprec_us);

	L4_Clock_t samples[n_samples], first = L4_SystemClock();
	for(int i=0; i < n_samples; i++) {
		samples[i] = L4_SystemClock();
	}

	int64_t max_us = 0;
	L4_Clock_t prev = first;
	for(int i=0; i < n_samples; i++) {
		int64_t lat = samples[i].raw - prev.raw;
		if(lat < 0 || lat > readprec_us) {
			diag("sample %d: lat=%u µs", i, (unsigned)lat);
		}
		max_us = MAX(int64_t, max_us, lat);
		prev = samples[i];
	}
	if(!ok1(max_us <= readprec_us)) {
		diag("max_us=%u µs", (unsigned)max_us);
	}
}
END_TEST


/* ReadPrecision is the _minimal_ difference that SystemClock can indicate.
 * ensure that this is the case.
 */
START_TEST(kip_sysclock_step)
{
	plan_tests(1);

	const int test_ms = 5;		/* spins for this long. */
	L4_KernelInterfacePage_t *kip = L4_GetKernelInterface();
	const L4_Time_t readprec = { .raw = kip->ClockInfo.X.ReadPrecision };
	unsigned readprec_us = L4_PeriodUs_NP(readprec);
	diag("readprec=%u µs", readprec_us);

	/* save suspiciously low differences in "diffs" for reporting. */
	darray(int64_t) diffs = darray_new();
	darray_make_room(diffs, 256);
	L4_Clock_t prev = L4_SystemClock(), start = prev;
	do {
		L4_Clock_t sample = L4_SystemClock();
		int64_t d = sample.raw - prev.raw;
		if(d > 0) {
			if(d < readprec_us) darray_push(diffs, d);
			prev = sample;
		}
	} while(prev.raw - start.raw < test_ms * 1000);

	int64_t min_diff = L4_SystemClock().raw - start.raw + 1, *d;
	darray_foreach(d, diffs) {
		assert(*d < readprec_us);
		diag("d=%u µs < readprec", (unsigned)*d);
		min_diff = MIN(int64_t, *d, min_diff);
	}
	if(!ok1(min_diff >= readprec_us)) {
		diag("min_diff=%u µs", (unsigned)min_diff);
	}

	darray_free(diffs);
}
END_TEST


/* test that returning from a L4_Sleep() (i.e. no send phase, receive from
 * self with timeout) happens within +-SchedulePrecision of the specified
 * time, regardless of where the sleep starts.
 *
 * TODO: there should be another loop in this test besides the one that sleeps
 * for a constant time; that loop should vary the length of the sleep period.
 * TODO: should also measure where between end..end+readprecision the wait
 * ended using usleep(). (that mechanism seems broken right now though.)
 *
 * the loop's time step is 1/60th of SchedPrecision, or 1 µs if it's less than
 * 60 µs.
 */
START_LOOP_TEST(kip_schedprec_wait, iter, 0, 1)
{
	plan_tests(1);
	const int n_slices = 60;
	const bool use_timept = CHECK_FLAG(iter, 1);
	diag("n_slices=%d, use_timept=%s", n_slices, btos(use_timept));

	L4_KernelInterfacePage_t *kip = L4_GetKernelInterface();
	const L4_Time_t schedprec = { .raw = kip->ClockInfo.X.SchedulePrecision };
	unsigned schedprec_us = L4_PeriodUs_NP(schedprec),
		slice_us = schedprec_us >= n_slices ? schedprec_us / n_slices : 1;
	diag("schedprec=%u µs, slice=%u µs", schedprec_us, slice_us);

	/* part 1: varying the point where the sleep starts, with respect to
	 * start-of-tick, where available, or just any random spot if it's not a
	 * ticky clock.
	 */
	int64_t jitters[n_slices];
	for(int s=0; s < n_slices; s++) {
		next_tick();
		usleep(s * slice_us);

		/* measure */
		L4_Clock_t start = L4_SystemClock();
		unsigned delay_us = schedprec_us * 3;
		L4_Time_t timeout;
		if(use_timept) {
			L4_Clock_t to_at = { .raw = start.raw + delay_us };
			timeout = L4_TimePoint2_NP(start, to_at);
		} else {
			timeout = L4_TimePeriod(delay_us);
		}
		L4_Sleep(timeout);
		L4_Clock_t end = L4_SystemClock();

		/* store result */
		int64_t diff = end.raw - start.raw;
		if(diff < 0) diff = -diff;
		fail_if(diff <= delay_us - schedprec_us,
			"didn't sleep for at least %u µs", delay_us - schedprec_us);
		int64_t jitter = diff - delay_us;
		if(jitter < 0) jitter = -jitter;

		jitters[s] = jitter;
	}

	/* result */
	int64_t max_jitter_us = 0;
	for(int i=0; i < n_slices; i++) {
		int64_t jitter = jitters[i];
		max_jitter_us = MAX(int64_t, jitter, max_jitter_us);
		if(jitter > schedprec_us && jitter < max_jitter_us) {
			diag("loop %d: jitter=%u µs", i, (unsigned)jitter);
		}
	}
	if(!ok1(max_jitter_us <= schedprec_us)) {
		diag("max_jitter=%u µs", (unsigned)max_jitter_us);
	}
}
END_TEST


/* test whether the "caller in same space as dest's scheduler" condition
 * checks out.
 */
START_TEST(syscall_access)
{
	plan_tests(4);

	/* part 1: allowed to call Schedule on a local thread. */
	L4_ThreadId_t o_tid = start_thread(&starvin_marvin, NULL);
	fail_if(L4_IsNilThread(o_tid));
	if(!ok(try_sched(o_tid), "local sched ok")) {
		diag("ec=%#lx", L4_ErrorCode());
	}
	fail_unless(send_quit(o_tid), "ec=%#lx", L4_ErrorCode());
	join_thread(o_tid);

	/* part 2: should be allowed to call Schedule on a remote thread when
	 * scheduler is set to the local space; and not when the Scheduler TCR is
	 * set to the same thread.
	 */
	int child = fork_tid(&o_tid);
	if(child == 0) {
		starvin_marvin(NULL);
		exit(0);
	} else {
		L4_Word_t res = L4_ThreadControl(o_tid, o_tid, L4_Myself(),
			L4_Pager(), (void *)-1);
		fail_if(res == 0, "ec=%#lx", L4_ErrorCode());
		ok(try_sched(o_tid), "remote sched ok");

		res = L4_ThreadControl(o_tid, o_tid, o_tid, L4_Pager(), (void *)-1);
		fail_if(res == 0, "ec=%#lx", L4_ErrorCode());
		ok(!try_sched(o_tid), "remote sched fail");
		ok(L4_ErrorCode() == 1, "fail is ``no privilege''");

		fail_unless(send_quit(o_tid), "ec=%#lx", L4_ErrorCode());
		int st = 0, dead = wait(&st);
		fail_if(dead != child, "dead=%d, st=%d, child=%d", dead, st, child);
	}
}
END_TEST


struct spinner_param
{
	L4_ThreadId_t parent;
	L4_Word_t spin_ms;
	bool signal_preempt, is_polite, delay_pe;
};


static void spinner_fn(void *param_ptr)
{
	struct spinner_param *param = param_ptr;

	if(param->signal_preempt) {
		L4_EnablePreemptionFaultException();
	}
	if(param->delay_pe) L4_DisablePreemption();

#if 0
	diag("spinner spins for %lu ms from %llu...", param->spin_ms,
		L4_SystemClock().raw);
#endif

	L4_Clock_t start = L4_SystemClock();
	do {
		delay_loop(iters_per_tick / 4);
		if(param->is_polite && L4_PreemptionPending()) {
			assert(param->delay_pe);
			L4_EnablePreemption();
			L4_ThreadSwitch(L4_nilthread);
			L4_DisablePreemption();
		}
	} while(start.raw + param->spin_ms * 1000 > L4_SystemClock().raw);

#if 0
	diag("spinner thread exiting at %llu.", L4_SystemClock().raw);
#endif

	if(!L4_IsNilThread(param->parent)) {
		L4_LoadMR(0, 0);
		L4_Send_Timeout(param->parent, L4_TimePeriod(5 * 1000));
	}

	free(param);
}


static L4_ThreadId_t start_spinner(
	int priority, int spin_ms,
	L4_Time_t timeslice, L4_Time_t total_quantum,
	bool signal_preempt, bool is_polite, bool delay_pe)
{
	assert(spin_ms > 0);
	assert(time_in_us(timeslice) >= 1000);
	assert(priority >= 0 && priority <= 0xff);
	assert(total_quantum.raw != L4_ZeroTime.raw);
	assert(L4_IsTimePeriod_NP(total_quantum));

	struct spinner_param *p = malloc(sizeof(*p));
	if(p == NULL) return L4_nilthread;

	*p = (struct spinner_param){
		.parent = L4_Myself(),
		.spin_ms = spin_ms,
		.signal_preempt = signal_preempt,
		.is_polite = is_polite, .delay_pe = delay_pe,
	};
	L4_ThreadId_t spinner = start_thread_long(&spinner_fn, p,
		priority, timeslice, total_quantum);
	if(L4_IsNilThread(spinner)) free(p);
	return spinner;
}


/* TODO: move this into a misc.c or some such! */
int find_own_priority(void)
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
		log("loop result was zero (supplanting with 100)\n");
		pri = 100;		/* make shit up! */
	}
	return pri;
}


static void preempt_fn(void *param_ptr)
{
	unsigned int sleep_ms = (L4_Word_t)param_ptr;
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


/* test preemption exceptions. records just their time of occurrence, which
 * should coincide with preempt_fn's sleep or the thread's timeslice ending.
 */
static void preempt_exn_case(
	struct preempt_exn_result *r,
	L4_Time_t spinner_ts,
	bool signal_preempt,
	int preempt_delay,
	int spin_time_ms,
	int receive_wait_ms)
{
	const int loop_time_ms = 150;

	int my_pri = find_own_priority();
	L4_ThreadId_t spinner = start_spinner(my_pri - 2, spin_time_ms,
		spinner_ts, L4_Never, signal_preempt, false, false);
	fail_unless(!L4_IsNilThread(spinner));

	L4_ThreadId_t preempt;
	if(preempt_delay > 0) {
		preempt = start_preempt(preempt_delay);
		fail_unless(!L4_IsNilThread(preempt));
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
			diag("malloc fail, wakeup at %lu not recorded",
				(unsigned long)now.raw);
		}

		if(L4_IpcFailed(tag)) {
			fail_unless(L4_ErrorCode() == 3, "unexpected ipc error (ec=%#lx)",
				L4_ErrorCode());
		} else if(tag.X.label >> 4 == (-4u & 0xfff)) {
			const L4_Word_t *words = &mrs[1];

			/* should reply or the thread will stop. */
			tag.X.label = 0;
			L4_LoadMR(0, tag.raw);
			L4_LoadMRs(1, num_words, words);
			tag = L4_Reply(spinner);
			IPC_FAIL(tag);
			if(r->num_exn == 0) r->first_preempt = L4_SystemClock();
			r->num_exn++;
		} else if(tag.X.u == 0) {
			/* ordinary regular spinner exit */
			break;
		} else {
			diag("got unexpected message (label=%#lx, u=%#lx, t=%#lx)",
				(L4_Word_t)tag.X.label, (L4_Word_t)tag.X.u,
				(L4_Word_t)tag.X.t);
		}
	} while(r->loop_start.raw + loop_time_ms * 1000 > now.raw);

	join_thread(spinner);
	join_thread(preempt);
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
	plan_tests(2);

	struct preempt_exn_result *res = malloc(sizeof(*res));
	list_head_init(&res->wakeups);
	preempt_exn_case(res, L4_TimePeriod(120 * 1000), false, 0, 25, 4);

	/* part #1: result should be at least five wakeups */
	if(!ok1(res->num_wake >= 5)) {
		diag("num_wake=%d", (int)res->num_wake);
	}

	/* part #2: they should have at least three ms in between. */
	uint64_t prev = 0;
	int count = 0;	/* # of intervals seen */
	struct preempt_wakeup *w;
	bool time_ok = true;
	list_for_each(&res->wakeups, w, result_link) {
		diag("  wkup@=%lu, was_exn=%s", (unsigned long)w->clock.raw,
			btos(w->was_exn));
		if(prev == 0) {
			fail_unless(w == list_top(&res->wakeups, struct preempt_wakeup,
				result_link));
			prev = w->clock.raw;
		} else {
			int diff = w->clock.raw / 1000 - prev / 1000;
			if(diff < 3) {
				diag("preemption at %d ms (expected _ < 3)", diff);
				time_ok = false;
			}
			if(++count == 4) break;
		}
	}
	if(!ok1(time_ok && count == 4)) diag("count=%d", count);

	free_preempt_exn_result(res);
}
END_TEST


START_LOOP_TEST(preempt_exn_test, t, 0, 7)
{
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
	preempt_exn_case(res, L4_TimePeriod((big_ts ? 120 : 4) * 1000),
		sig_pe, has_pe ? 10 : 0, 25, 22);

	fail_unless(res->num_exn <= res->num_wake);
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
		uint32_t diff = res->first_preempt.raw / 1000 - res->loop_start.raw / 1000;
		diag("started at %llu, first preempt at %llu (diff %u), preempted %d time(s)",
			res->loop_start.raw, res->first_preempt.raw, diff, res->num_exn);
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
			fail_unless(!big_ts && !has_pe);
			ok1(res->num_exn == 6);		/* see above */
			ok1(diff < 8);
		}
	}

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
 * observe times when preemptions occur. these should cover:
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
 *
 * FIXME: preemption IPC should only be sent once the preempted thread
 * resumes. thus the test cannot rely on this.
 */
START_LOOP_TEST(delay_preempt, iter, 0, 15)
{
	const bool delay_pe = CHECK_FLAG(iter, 1),
		high_sens_pri = CHECK_FLAG(iter, 2),
		polite = CHECK_FLAG(iter, 4),
		small_ts = CHECK_FLAG(iter, 8);
	diag("delay_pe=%s, high_sens_pri=%s, polite=%s, small_ts=%s",
		btos(delay_pe), btos(high_sens_pri), btos(polite), btos(small_ts));

	struct list_head preempts = LIST_HEAD_INIT(preempts);
	L4_Clock_t start_time;
	bool ok = delay_preempt_case(&preempts, &start_time,
		delay_pe, high_sens_pri, polite, small_ts);
	fail_unless(ok, "experiment failed");

	struct preempt_wakeup *wu = list_top(&preempts, struct preempt_wakeup,
		result_link);
	uint64_t wake_ms, start_ms = start_time.raw / 1000;
	int at;
	if(wu != NULL) {
		wake_ms = wu->clock.raw / 1000;
		at = wake_ms - start_ms;
	} else {
		wake_ms = 0;
		at = 0;
	}

	if(!delay_pe || !high_sens_pri) {
		/* polite won't matter due to !delay_pe; or, delay_pe won't matter due
		 * to !high_sens_pri.
		 *
		 * FIXME: plan the number of tests as there are ok() primitives, and
		 * add the inverse condition to their clause.
		 */

		if(high_sens_pri) {
			plan_tests(small_ts ? 2 : 1);
			fail_unless(wu != NULL, "need wakeup");
			ok(at >= 4 && at <= 6, "first preemption at preempt wakeup");

			/* consume the wakeup. (could be a macro if used twice.) */
			if(wu->result_link.next != &preempts.n) {
				wu = container_of(wu->result_link.next, struct preempt_wakeup,
					result_link);
				wake_ms = wu->clock.raw / 1000;
				at = wake_ms - start_ms;
			} else {
				wu = NULL;
			}
		} else {
			plan_tests(1);
		}

		if(small_ts) {
			/* and (again?) by timeslice running out. */
			fail_unless(wu != NULL, "need wakeup");
			if(high_sens_pri) {
				ok(at >= 11 && at <= 13,
					"preemption by short timeslice");
			} else {
				ok(at >= 11 && at <= 13 && wu == list_top(&preempts,
						struct preempt_wakeup, result_link),
					"first preemption by short timeslice");
			}
		} else if(!high_sens_pri) {
			ok(list_empty(&preempts),
				"no preemption with long ts, low sens_pri");
		}
	} else if(!polite && !small_ts) {
		plan_tests(2);
		if(wu == NULL) {
			pass("preemption was delayed");
			skip(1, "no wakeup");
		} else {
			ok(at > 5, "preemption was delayed");
			ok(wu->was_exn && at >= 14 && at <= 16,
				"preemption by max delay");
		}
	} else if(!polite && small_ts) {
		plan_tests(2);
		skip_start(wu == NULL, 2, "no wakeup");
			ok(at > 5, "preemption was delayed");
			ok(wu->was_exn && at >= 11 && at <= 13,
				"preemption by quantum during delay");
		skip_end;
	} else if(polite && !small_ts) {
		plan_tests(1);
		ok(list_empty(&preempts),
			"no preemption with polite spinner");
	} else if(polite && small_ts) {
		plan_tests(1);
		skip_start(wu == NULL, 1, "no wakeup");
			/* politeness gets the spinner first 5 ms of runtime, and then
			 * another 12 ms due to the ThreadSwitch.
			 */
			ok(at >= 16 && at <= 18,
				"preemption by quantum while polite");
		skip_end;
	} else {
		fail_if(true, "unhandled iter=%d", iter);
	}

	/* FIXME: free wakeup structs in "preempts" */
}
END_TEST


static bool delay_preempt_case(
	struct list_head *preempt_times_list,
	L4_Clock_t *start_p,
	bool delay_pe,
	bool high_sens_pri,
	bool polite,
	bool small_ts)
{
	int my_pri = find_own_priority();

	L4_ThreadId_t spinner = start_spinner(my_pri - 2, 25,
		small_ts ? L4_TimePeriod(12 * 1000) : L4_TimePeriod(50 * 1000),
		L4_Never, true, polite, delay_pe);
	if(L4_IsNilThread(spinner)) return false;
	/* TODO: could split delay_pe into two flags: one controlling whether the
	 * delay_preemption flag is set in the spinner's TCR, and the other
	 * controlling the max_delay variable. this tests whether dropping
	 * max_delay will prevent preemption delay regardless of the delay flag's
	 * setting.
	 *
	 * which could likely be a test on its own.
	 */
	L4_Set_PreemptionDelay(spinner,
		high_sens_pri ? my_pri - 1 : my_pri - 2,
		delay_pe ? 10 * 1000 : 0);

	L4_ThreadId_t preempt = start_thread_long(&preempt_fn, (void *)5,
		my_pri - 1, L4_TimePeriod(2 * 1000), L4_Never);
	if(L4_IsNilThread(preempt)) return false;

	/* TODO: turn this loop into a function. it shows up in a couple of tests
	 * now.
	 */
	bool ok = true;
	*start_p = L4_SystemClock();
	do {
		L4_MsgTag_t tag = L4_Receive_Timeout(spinner,
			L4_TimePeriod(50 * 1000));
		L4_Word_t mrs[64];
		int num_words = tag.X.u + tag.X.t;
		if(num_words > 63) num_words = 63;
		if(L4_IpcSucceeded(tag)) L4_StoreMRs(0, num_words + 1, mrs);

		struct preempt_wakeup *wu = malloc(sizeof(*wu));
		wu->clock = L4_SystemClock();
		wu->was_exn = false;
		if(tag.X.label >> 4 == (-4u & 0xfff)) {
			wu->was_exn = true;

			/* restart the thread */
			tag.X.label = 0;
			L4_LoadMR(0, tag.raw);
			L4_LoadMRs(1, num_words, &mrs[1]);
			tag = L4_Reply(spinner);
			if(L4_IpcFailed(tag)) {
				log("spinner preempt reply failed: ec %#lx\n",
					L4_ErrorCode());
				ok = false;
				break;
			}
		} else if(tag.X.u == 0) {
			/* ordinary regular spinner exit */
			break;
		} else {
			log("got unexpected message (label %#lx, u %#lx, t %#lx)\n",
				(L4_Word_t)tag.X.label, (L4_Word_t)tag.X.u,
				(L4_Word_t)tag.X.t);
		}
		list_add_tail(preempt_times_list, &wu->result_link);
	} while(start_p->raw + 100 * 1000 > L4_SystemClock().raw);

	join_thread(spinner);
	join_thread(preempt);

	return ok;
}


/* start a thread that spins for longer than its total quantum. this should
 * cause a preemption message.
 */
START_TEST(total_quantum_exhaust_rpc)
{
	int my_pri = find_own_priority();
	const L4_Time_t ts_len = L4_TimePeriod(6 * 1000),
		tq_len = L4_TimePeriod(13 * 1000);

	diag("my_pri=%d", my_pri);
	plan_tests(5);
	todo_start("no implementation");

	L4_ThreadId_t spinner = start_spinner(my_pri - 2, 20,
		ts_len, tq_len, false, false, false);
	/* wait for the message. or not. */
	bool msg_got = false, sync_got = false;
	L4_Clock_t msg_clock = { .raw = 0 }, ipc_at_clock = { .raw = 0 },
		start_clock = L4_SystemClock();
	L4_ThreadId_t msg_from = L4_nilthread;
	for(;;) {
		L4_ThreadId_t sender;
		L4_MsgTag_t tag = L4_Wait_Timeout(TEST_IPC_DELAY, &sender);
		if(L4_IpcFailed(tag)) {
			diag("ipc failed: ec=%#lx", L4_ErrorCode());
			break;
		} else if(L4_Label(tag) == 0xffd0) {
			L4_Word_t mr[2]; L4_StoreMRs(1, 2, mr);
			ipc_at_clock = L4_SystemClock();
			msg_clock.raw = (L4_Word64_t)mr[0] | (L4_Word64_t)mr[1];
			msg_got = true;
			msg_from = sender;
			diag("resetting total_quantum for %lu:%lu",
				L4_ThreadNo(sender), L4_Version(sender));
			L4_Word_t ret = L4_Set_Timeslice(sender, ts_len, tq_len);
			if(ret == 0) {
				diag("  Schedule failed, ec=%lu", L4_ErrorCode());
			}
		} else if(tag.raw == 0 && L4_SameThreads(sender, spinner)) {
			/* spinner's exit signal. */
			sync_got = true;
			break;
		} else {
			diag("unrecognized tag=%#lx from %lu:%lu",
				tag.raw, L4_ThreadNo(sender), L4_Version(sender));
		}
	}
	L4_Clock_t end_clock = L4_SystemClock();

	L4_Word64_t t = end_clock.raw - start_clock.raw,
		msg_t = msg_clock.raw - start_clock.raw,
		ipc_t = ipc_at_clock.raw - start_clock.raw;

	ok(msg_got, "got total_quantum message");
	if(msg_got) {
		diag("t=%lu, msg_t=%lu, ipc_t=%lu",
			(unsigned long)t, (unsigned long)msg_t, (unsigned long)ipc_t);
	}
	ok(L4_SameThreads(msg_from, spinner), "came from spinner");

	todo_start("fajskfdjaslkfdjsal");
	ok1(msg_t >= L4_PeriodUs_NP(tq_len));
	ok1(ipc_t >= L4_PeriodUs_NP(tq_len));

	ok(sync_got, "got clean exit message");

	xjoin_thread(spinner);
}
END_TEST


/* returns the difference between spinner switch and return therefrom. */
static int yield_timeslice_case(bool preempt_spinner)
{
	int my_pri = find_own_priority();

	L4_ThreadId_t spinner = start_spinner(my_pri - 2, 15,
		L4_TimePeriod(2 * 1000), L4_Never, false, false, false);

	L4_ThreadId_t preempt = L4_nilthread;
	if(preempt_spinner) {
		preempt = start_thread_long(&preempt_fn, (void *)4,
			my_pri - 1, L4_TimePeriod(2 * 1000), L4_Never);
		L4_ThreadSwitch(preempt);
	}

	L4_Clock_t start = L4_SystemClock();
	L4_ThreadSwitch(spinner);
	L4_Clock_t end = L4_SystemClock();

	L4_Receive_Timeout(spinner, L4_TimePeriod(45 * 1000));
	join_thread(spinner);

	if(preempt_spinner) join_thread(preempt);

	return (end.raw - start.raw + 500) / 1000;
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



struct chain_param {
	L4_ThreadId_t parent, next;
	L4_Word_t last_ec;
	L4_Time_t rcv_timeout, reply_delay;
	bool sleep_in_recv;
};

static void chain_thread_fn(void *param_ptr)
{
	struct chain_param *p = param_ptr;

	bool running = true;
	do {
		L4_ThreadId_t sender;
		L4_Accept(L4_UntypedWordsAcceptor);
		L4_MsgTag_t tag = L4_Wait(&sender);
		for(;;) {
			if(L4_IpcFailed(tag)) {
				diag("%s: ipc failed, ec=%#lx", __func__, L4_ErrorCode());
				break;
			} else if(L4_Label(tag) == QUIT_LABEL) {
				running = false;
				break;	/* and don't reply */
			}

			if(!L4_IsNilThread(p->next)) {
				/* anything else we pass to the next-in-line if one exists,
				 * and record an error for the parent's interest.
				 */
				L4_LoadMR(0, tag.raw);
				L4_ThreadId_t dummy;
				/* (akin to a L4_Lcall_Timeouts().) */
				tag = L4_Lipc(p->next, p->next,
					L4_Timeouts(L4_Never, p->rcv_timeout), &dummy);
				p->last_ec = L4_IpcSucceeded(tag) ? 0 : L4_ErrorCode();
			}

			/* the tip does what? */
			if(p->reply_delay.raw != L4_ZeroTime.raw) {
				if(p->sleep_in_recv) {
					L4_Receive_Timeout(L4_MyGlobalId(), p->reply_delay);
				} else {
					L4_Send_Timeout(L4_MyGlobalId(), p->reply_delay);
				}
			}

#if 0
			diag("doing LreplyWait from %lu:%lu to %#lx",
				L4_ThreadNo(L4_MyGlobalId()), L4_Version(L4_MyGlobalId()),
				sender.raw);
#endif
			L4_LoadMR(0, 0);
			tag = L4_LreplyWait(sender, &sender);
		}
	} while(running);
}


/* test the effect of various RcvTimeout settings in Lipc.
 *
 * note that the L4.X2 spec prescribes that Lipc should always have a
 * RcvTimeout=Never, but as the other values are cheap to implement as well
 * (and useful for regular Ipc, later), we'll test them here.
 *
 * variables:
 *   - do_timeout: whether the partner-chain tip tries to cause a timeout
 *   - have_timeout: whether base specifies a receive timeout
 *   - zero_timeout: whether base's timeout is ZeroTime or tens of ms
 *   - long_chain: number of links in the chain; low or high
 *   - sleep_in_recv: tip sleep mode (send phase, receive phase)
 *   - TODO:
 *     - tip sleep length (less/more than chain timeout minimum, n/a when
 *       !do_timeout || zero_timeout)
 *     - position of minimum timeout in chain (base, midpoint)
 *     - undo the chain a bit before sleeping (no, 3/4 down)
 *     - repeat the chain from having been replied to (no, 1/4 down)
 */
START_LOOP_TEST(lipc_chain_timeout, iter, 0, 31)
{
	const bool do_timeout = CHECK_FLAG(iter, 1),
		have_timeout = CHECK_FLAG(iter, 2),
		zero_timeout = CHECK_FLAG(iter, 4),
		long_chain = CHECK_FLAG(iter, 8),
		sleep_in_recv = CHECK_FLAG(iter, 16);
	const size_t chain_length = !long_chain ? 4 : 45,
		timeout_ms = 30;
	diag("do_timeout=%s, have_timeout=%s, zero_timeout=%s, long_chain=%s",
		btos(do_timeout), btos(have_timeout), btos(zero_timeout),
		btos(long_chain));
	diag("sleep_in_recv=%s", btos(sleep_in_recv));
	diag("parent tid is %lu:%lu",
		L4_ThreadNo(L4_Myself()), L4_Version(L4_Myself()));
	plan_tests(2);

	const L4_Time_t recvto = !have_timeout ? L4_Never
		: (zero_timeout ? L4_ZeroTime : L4_TimePeriod(timeout_ms * 1000));

	/* setup */
	void *tctx = talloc_new(NULL);
	L4_ThreadId_t chain_tids[chain_length];
	struct chain_param *ps[chain_length];
	for(int i = chain_length - 1; i >= 0; --i) {
		const bool last = i + 1 == chain_length;
		ps[i] = talloc(tctx, struct chain_param);
		*ps[i] = (struct chain_param){
			.parent = L4_MyGlobalId(),
			.next = !last ? chain_tids[i + 1] : L4_nilthread,
			.last_ec = ~0ul,
			.reply_delay = !last || !do_timeout ? L4_ZeroTime
				: L4_TimePeriod((timeout_ms + 40) * 1000),
			.rcv_timeout = L4_Never,
			.sleep_in_recv = !last || sleep_in_recv,
		};
		chain_tids[i] = L4_LocalIdOf(xstart_thread(&chain_thread_fn, ps[i]));
	}

	/* experiment */
	L4_LoadMR(0, (L4_MsgTag_t){ .X.u = 2, .X.label = 0xb007 }.raw);
	L4_LoadMR(1, 0x12345678);
	L4_LoadMR(2, 0xdefeca7e);
	L4_ThreadId_t dummy;
	L4_MsgTag_t tag = L4_Lipc(chain_tids[0], chain_tids[0],
		L4_Timeouts(TEST_IPC_DELAY, recvto), &dummy);
	L4_Word_t ec = L4_ErrorCode();

	/* sync & analysis */
	for(size_t i=0; i < chain_length; i++) send_quit(chain_tids[i]);
	for(size_t i=0; i < chain_length; i++) xjoin_thread(chain_tids[i]);
	if(!iff_ok1(have_timeout && do_timeout, L4_IpcFailed(tag) && ec == 0x3)) {
		diag("tag=%#lx, ec=%#lx", tag.raw, ec);
	}
	int first_fail = -1;
	for(size_t i=0; i < chain_length - 1; i++) {
		if(ps[i]->last_ec != 0 && first_fail < 0) first_fail = i;
	}
	if(!ok1(first_fail < 0)) {
		diag("first_fail=%d (ec=%#lx)", first_fail, ps[first_fail]->last_ec);
	}

	/* cleanup */
	talloc_free(tctx);
}
END_TEST


Suite *sched_suite(void)
{
	Suite *s = suite_create("sched");

	{
		TCase *tc = tcase_create("api");
		tcase_set_fork(tc, false);
		tcase_add_test(tc, single_as_errors);
		tcase_add_test(tc, syscall_access);
		tcase_add_test(tc, range_errors);
		suite_add_tcase(s, tc);
	}

	{
		TCase *tc = tcase_create("kip");
		tcase_add_test(tc, kip_sysclock_latency);
		tcase_add_test(tc, kip_sysclock_step);
		/* TODO: add more schedprec tests:
		 *   - one for preemption by a higher-priority thread's wait ending
		 *   - another for timeslice exhaustion
		 */
		tcase_add_test(tc, kip_schedprec_wait);
		suite_add_tcase(s, tc);
	}

	/* tests regarding preemption. currently only ones that're due to thread
	 * quantums, but should be expanded with ones from ipc:preempt.
	 *
	 * TODO: fix forkserv's new_thread() call to support all of the timeslice
	 * etc. bits from start_thread_long(), then remove the no-fork attribute
	 */
	{
		TCase *tc = tcase_create("preempt");
		tcase_set_fork(tc, false);
		tcase_add_test(tc, simple_preempt_test);
		tcase_add_test(tc, preempt_exn_test);
		tcase_add_test(tc, delay_preempt);
		tcase_add_test(tc, total_quantum_exhaust_rpc);
		suite_add_tcase(s, tc);
	}

	/* TODO: see above */
	{
		TCase *tc = tcase_create("yield");
		tcase_set_fork(tc, false);
		tcase_add_test(tc, yield_timeslice_test);
		suite_add_tcase(s, tc);
	}

	/* tests about Ipc/Lipc interactions with scheduling. */
	{
		TCase *tc = tcase_create("ipc");
		tcase_add_test(tc, lipc_chain_timeout);
		suite_add_tcase(s, tc);
	}

	return s;
}
