
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
#include "threadmgr-defs.h"


#define log(fmt, ...) log_f("%s: " fmt, __func__, ##__VA_ARGS__)


struct preempt_wakeup
{
	struct list_node link;
	L4_Clock_t clock;
	bool was_exn;
	L4_MsgTag_t tag;
	L4_Word_t mrs[];	/* excl. tag */
};


/* receives preemptions and preemption exceptions from @source. logs them
 * earliest-first in @wkups. exits early if @max_wait is hit, if the wait
 * returned more than 500ms after *@start_time_p, or if @source sends a u=0
 * message to indicate end of experiment.
 *
 * returns negative on error, positive #wakeups on success.
 */
static int pump_wakeups(
	struct list_head *wkups,	/* adds talloc'd <struct preempt_wakeup> */
	L4_Clock_t *start_time_p,	/* SystemClock before first wait */
	L4_ThreadId_t source,
	L4_Time_t max_wait);


/* fuzzy compare for time measurements. */
static bool fuzz_eq(uint64_t a, uint64_t b, uint32_t slop) {
	return a >= b - slop && a <= b + slop;
}


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


/* basic test of Ipc used as L4_Sleep(). */
START_TEST(sleeping_ipc)
{
	plan_tests(1);

	L4_Word64_t sleep_start = L4_SystemClock().raw;
	L4_Sleep(L4_TimePeriod(500000));
	L4_Word64_t wake = L4_SystemClock().raw;
	diag("woke up at %llu; slept for %llu µs\n", wake, wake - sleep_start);
	ok(wake - sleep_start >= 500000, "long enough");
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

	if(!L4_IsNilThread(param->parent)) {
		L4_Set_ExceptionHandler(param->parent);
	}
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
		L4_Send_Timeout(param->parent, TEST_IPC_DELAY);
	}

	free(param);
}


/* starts a thread that spins for @spin_ms, setting its ExceptionHandler to
 * the current thread to allow measurement using pump_wakeups().
 */
L4_ThreadId_t start_spinner(
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
	/* FIXME: in theory this should be 254, because that's where the roottask
	 * starts out at. in practice anything different than 100 causes the
	 * test-running thread not to start at all. so let's leave it here for the
	 * time being.
	 */
	int pri = 100;
	L4_ThreadId_t self = L4_Myself();
	while(pri > 0
		&& (L4_Set_Priority(self, pri) & 0xff) == L4_SCHEDRESULT_ERROR
		&& L4_ErrorCode() == 5)
	{
		/* invalid parameter means the priority was too high. this loop stops
		 * when "pri" is the actual priority of this thread, without altering
		 * the kernel setting.
		 */
		pri--;
	}
	if(pri == 0) {
		diag("loop result was zero (supplanting with 100)\n");
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


/* talloc'd. members of @wakeups talloc'd somewhere under this. */
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

	struct list_head *wakeups = talloc(r, struct list_head);
	list_head_init(wakeups);
	int n = pump_wakeups(wakeups, &r->loop_start, spinner,
		L4_TimePeriod(receive_wait_ms * 1000));
	list_append_list(&r->wakeups, wakeups);
	if(n < 0) diag("pump_wakeups returned n=%d", n);

	r->num_exn = 0;
	r->num_wake = 0;
	r->first_preempt.raw = 0;
	struct preempt_wakeup *cur;
	list_for_each(&r->wakeups, cur, link) {
		if(r->num_exn + r->num_wake == 0) r->first_preempt = cur->clock;
		if(cur->was_exn) r->num_exn++;
		r->num_wake++;
	}

	xjoin_thread(spinner);
	xjoin_thread(preempt);
}


START_LOOP_TEST(simple_preempt_test, iter, 0, 1)
{
	const bool get_signal = CHECK_FLAG(iter, 1);
	diag("get_signal=%s", btos(get_signal));
	plan_tests(4);

	struct preempt_exn_result *res = talloc(NULL, struct preempt_exn_result);
	list_head_init(&res->wakeups);
	preempt_exn_case(res, L4_TimePeriod(5 * 1000), get_signal, 0, 50, 150);

	/* # of wakeups depends on the spinner's "s" flag. */
	diag("num_wake=%d", res->num_wake);
	imply_ok1(!get_signal, res->num_wake == 0);
	imply_ok1(get_signal, res->num_wake >= 5);

	/* where wakeups came in, they should have at least 4..6 ms in between. */
	skip_start(res->num_wake < 2, 1, "not enough wakeups to measure") {
		uint64_t prev = 0;
		uint64_t min_interval_us = ~0ull;
		struct preempt_wakeup *w;
		list_for_each(&res->wakeups, w, link) {
			diag("  wkup@=%lu, was_exn=%s", (unsigned long)w->clock.raw,
				btos(w->was_exn));
			if(prev > 0) {
				if(w->clock.raw < prev) diag("    (too close!)");
				else {
					min_interval_us = MIN(uint64_t, min_interval_us,
						w->clock.raw - prev);
				}
			}
			prev = w->clock.raw;
		}
		if(!ok1(fuzz_eq(min_interval_us, 5000, 1000))) {
			diag("min_interval_us=%lu", (unsigned long)min_interval_us);
		}
	} skip_end;

	/* no wakeup should be an exception. */
	skip_start(res->num_wake < 1, 1, "no wakeups at all") {
		struct preempt_wakeup *w;
		bool no_exns = true;
		list_for_each(&res->wakeups, w, link) {
			if(w->was_exn) {
				no_exns = false;
				break;
			}
		}
		ok1(no_exns);	/* NO EXCEPTIONS. */
	} skip_end;

	talloc_free(res);
}
END_TEST


/* test signaling of preemption exceptions.
 *
 * variables:
 *   - timeslice length (big/small)
 *   - preemption signaling switch (on/off)
 *   - whether there'll be a higher-priority wakeup 10 ms in (yes/no)
 */
START_LOOP_TEST(preempt_exn_test, iter, 0, 7)
{
	const bool big_ts = CHECK_FLAG(iter, 1),
		sig_pe = CHECK_FLAG(iter, 2),
		has_pe = CHECK_FLAG(iter, 4);
	diag("big_ts=%s, sig_pe=%s, has_pe=%s",
		btos(big_ts), btos(sig_pe), btos(has_pe));

	plan_tests(7);

	struct preempt_exn_result *res = talloc_zero(NULL,
		struct preempt_exn_result);
	list_head_init(&res->wakeups);
	preempt_exn_case(res, L4_TimePeriod((big_ts ? 120 : 4) * 1000),
		sig_pe, has_pe ? 10 : 0, 25, 200);

	int64_t diff = res->first_preempt.raw / 1000 - res->loop_start.raw / 1000;
	diag("started at %llu, first preempt at %llu (diff %ld)",
		res->loop_start.raw, res->first_preempt.raw, (long)diff);
	diag("res->num_exn=%d, res->num_wake=%d", res->num_exn, res->num_wake);

	ok1(res->num_exn == 0);
	iff_ok1(res->num_wake == 0, !sig_pe || (big_ts && !has_pe));
	imply_ok1(sig_pe && big_ts && has_pe, res->num_wake == 1);
	imply_ok1(sig_pe && !big_ts, res->num_wake > 1);

	imply_ok1(sig_pe && !big_ts, diff >= 0 && diff < 8);
	imply_ok1(sig_pe && big_ts && has_pe, diff >= 10 && diff <= 13);
	imply_ok1(sig_pe && big_ts && !has_pe, diff <= 0);

	talloc_free(res);
}
END_TEST


static int pump_wakeups(
	struct list_head *wkups,
	L4_Clock_t *start_p,
	L4_ThreadId_t source,
	L4_Time_t max_wait)
{
	int n = 0;
	*start_p = L4_SystemClock();
	L4_Clock_t limit = L4_ClockAddUsec(*start_p, 500 * 1000);
	do {
		L4_MsgTag_t tag = L4_Receive_Timeout(source, max_wait);
		if(L4_IpcFailed(tag)) return -(int)L4_ErrorCode();

		L4_Word_t mrs[64];
		int num_words = tag.X.u + tag.X.t;
		if(num_words > 63) num_words = 63;
		L4_StoreMRs(0, num_words + 1, mrs);

		struct preempt_wakeup *wu = talloc_zero_size(wkups,
			sizeof(*wu) + sizeof(L4_Word_t) * num_words);
		wu->clock = L4_SystemClock();
		wu->was_exn = false;
		wu->tag = tag;
		memcpy(wu->mrs, mrs, sizeof(L4_Word_t) * num_words);

		if(L4_Label(tag) == 0xffc0) {
			/* preempt exception. */
			wu->was_exn = true;

			/* restart the thread */
			tag.X.label = 0;
			L4_LoadMR(0, tag.raw);
			L4_LoadMRs(1, num_words, &mrs[1]);
			tag = L4_Reply(source);
			if(L4_IpcFailed(tag)) {
				n = -(int)L4_ErrorCode();
				diag("%s: preempt reply failed: ec=%#lx", __func__,
					L4_ErrorCode());
			}
			if(n >= 0) n++;
		} else if(L4_Label(tag) == 0xffd0 && L4_UntypedWords(tag) == 2) {
			/* preempt fault. */
			wu->was_exn = false;
			if(n >= 0) n++;
		} else if(L4_UntypedWords(tag) == 0 || L4_Label(tag) == QUIT_LABEL) {
			/* exit message. */
			diag("%s: exiting, n=%d", __func__, n);
			break;
		} else {
			diag("%s: unexpected message at %lu (label=%#lx, u=%lu, t=%lu)",
				__func__, L4_SystemClock().raw,
				L4_Label(tag), L4_UntypedWords(tag), L4_TypedWords(tag));
		}
		list_add_tail(wkups, &wu->link);
	} while(L4_IsClockEarlier(L4_SystemClock(), limit));

	return n;
}


/* test on basic preemption delay functionality. disproves that preemption
 * delay happens iff the d bit is set && sens_pri >= preemptor.pri &&
 * max_delay > 0.
 *
 * setup: spinner thread at priority P-2 for 25 ms; preempt thread at P-1 to
 * interrupt at 5ms. spinner thread set to signal preemptions.
 *
 * variables:
 *   - [delay_pe] spinner thread set to delay preemptions, or not
 *   - [high_sens_pri] spinner thread's sens_pri set to level of preempt
 *     thread, or not
 *   - [max_delay_zero] spinner's max_delay set to 0, or 10ms.
 *
 * measurement is the time at which the spinner was preempted. if delay was in
 * effect, preemption should happen at 15ms; if it wasn't, it happens at 5ms.
 */
START_LOOP_TEST(delay_basics, iter, 0, 7)
{
	const bool delay_pe = CHECK_FLAG(iter, 1),
		high_sens_pri = CHECK_FLAG(iter, 2),
		max_delay_zero = CHECK_FLAG(iter, 4);
	diag("delay_pe=%s, high_sens_pri=%s, max_delay_zero=%s", 
		btos(delay_pe), btos(high_sens_pri), btos(max_delay_zero));
	plan_tests(4);

	/* setup */
	int my_pri = find_own_priority();
	L4_ThreadId_t preempt = start_thread_long(&preempt_fn, (void *)5,
		my_pri - 1, L4_TimePeriod(2 * 1000), L4_Never);
	fail_if(L4_IsNilThread(preempt));
	preempt = L4_GlobalIdOf(preempt);

	L4_ThreadId_t spinner = start_spinner(my_pri - 2, 25,
		L4_TimePeriod(50 * 1000), L4_Never, true, false, delay_pe);
	fail_if(L4_IsNilThread(spinner));
	spinner = L4_GlobalIdOf(spinner);

	L4_Set_PreemptionDelay(spinner,
		high_sens_pri ? my_pri - 1 : my_pri - 2,
		max_delay_zero ? 0 : 10000);

	diag("spinner=%lu:%lu, preempt=%lu:%lu",
		L4_ThreadNo(spinner), L4_Version(spinner),
		L4_ThreadNo(preempt), L4_Version(preempt));

	/* measurement */
	struct list_head *preempts = talloc(NULL, struct list_head);
	list_head_init(preempts);

	L4_Clock_t start_time;
	int n = pump_wakeups(preempts, &start_time, spinner, TEST_IPC_DELAY);
	if(!ok1(n >= 0)) diag("error n=%d from pump_wakeups()", n);

	struct preempt_wakeup *wu = list_pop(preempts,
		struct preempt_wakeup, link);
	ok(wu != NULL, "got at least one wakeup");
	skip_start(wu == NULL, 2, "no wakeups were recorded!") {
		int at = (wu->clock.raw - start_time.raw + 500) / 1000;

		imply_ok1(delay_pe && high_sens_pri && !max_delay_zero,
			at >= 14 && at <= 16);
		imply_ok1(!delay_pe || !high_sens_pri || max_delay_zero,
			at >= 4 && at <= 6);

		diag("at=%d", at);
	} skip_end;

	/* cleanup */
	xjoin_thread(spinner);
	xjoin_thread(preempt);
	talloc_free(preempts);
}
END_TEST


/* test on preemption delay and yielding. disproves that a thread that yields
 * when preemption is delayed will raise an exception at the end of its
 * max_delay.
 *
 * setup: same as delay_basics, but the spinner will be given a timeslice
 * short enough to pop at least one preemption message.
 *
 * variables:
 *   - [yield] whether spinner will yield or not.
 *
 * measurement is the type of preemption that's seen. it should be an
 * exception iff !yield.
 */
START_LOOP_TEST(delay_yield, iter, 0, 1)
{
	const bool yield = CHECK_FLAG(iter, 1);
	diag("yield=%s", btos(yield));
	plan_tests(5);

	/* setup */
	int my_pri = find_own_priority();
	L4_ThreadId_t preempt = start_thread_long(&preempt_fn, (void *)5,
		my_pri - 1, L4_TimePeriod(2 * 1000), L4_Never);
	fail_if(L4_IsNilThread(preempt));
	preempt = L4_GlobalIdOf(preempt);

	L4_ThreadId_t spinner = start_spinner(my_pri - 2, 25,
		L4_TimePeriod(15 * 1000), L4_Never, true, yield, true);
	fail_if(L4_IsNilThread(spinner));
	spinner = L4_GlobalIdOf(spinner);
	L4_Set_PreemptionDelay(spinner, my_pri - 1, 5000);

	diag("spinner=%lu:%lu, preempt=%lu:%lu",
		L4_ThreadNo(spinner), L4_Version(spinner),
		L4_ThreadNo(preempt), L4_Version(preempt));

	/* measurement */
	struct list_head *preempts = talloc(NULL, struct list_head);
	list_head_init(preempts);

	L4_Clock_t start_time;
	int n = pump_wakeups(preempts, &start_time, spinner, TEST_IPC_DELAY);
	if(!ok1(n >= 0)) diag("error n=%d from pump_wakeups()", n);

	struct preempt_wakeup *wu = list_pop(preempts,
		struct preempt_wakeup, link);
	ok(wu != NULL, "got at least one wakeup");
	skip_start(wu == NULL, 3, "no wakeups were recorded!") {
		int msg_at = (wu->clock.raw - start_time.raw + 500) / 1000;
		diag("msg_at=%d, wu->was_exn=%s", msg_at, btos(wu->was_exn));

		imply_ok1(yield, msg_at >= 19 && msg_at <= 21);
		imply_ok1(!yield, msg_at >= 9 && msg_at <= 11);
		iff_ok1(wu->was_exn, !yield);
	} skip_end;

	/* cleanup */
	xjoin_thread(spinner);
	xjoin_thread(preempt);
	talloc_free(preempts);
}
END_TEST


/* test on the exception message sent when a thread exceeds its preemption
 * delay. disproves that a thread that exceeds the delay will receive an
 * exception message, and won't if it switches off before the delay is up.
 *
 * setup: same as delay_basics. max_delay set to 5ms.
 *
 * variables:
 *   - [delay] whether spinner will delay preemption or not.
 *   - [preempt_close] how long preemptor waits, 23ms/5ms.
 *
 * measurement is the type of preemption that's seen. it should be an
 * exception iff delay & preempt_close.
 */
START_LOOP_TEST(delay_exception, iter, 0, 3)
{
	const bool delay = CHECK_FLAG(iter, 1),
		preempt_close = CHECK_FLAG(iter, 2);
	const int preempt_at_ms = preempt_close ? 5 : 23;
	diag("delay=%s, preempt_close=%s", btos(delay), btos(preempt_close));
	diag("preempt_at_ms=%d", preempt_at_ms);
	plan_tests(3);

	/* setup */
	int my_pri = find_own_priority();
	L4_ThreadId_t preempt = start_thread_long(&preempt_fn,
		(void *)preempt_at_ms, my_pri - 1, L4_TimePeriod(2 * 1000),
		L4_Never);
	fail_if(L4_IsNilThread(preempt));
	preempt = L4_GlobalIdOf(preempt);

	L4_ThreadId_t spinner = start_spinner(my_pri - 2, 25,
		L4_TimePeriod(50 * 1000), L4_Never, true, false, delay);
	fail_if(L4_IsNilThread(spinner));
	spinner = L4_GlobalIdOf(spinner);
	L4_Set_PreemptionDelay(spinner, my_pri - 1, 5000);

	diag("spinner=%lu:%lu, preempt=%lu:%lu",
		L4_ThreadNo(spinner), L4_Version(spinner),
		L4_ThreadNo(preempt), L4_Version(preempt));

	/* measurement */
	struct list_head *preempts = talloc(NULL, struct list_head);
	list_head_init(preempts);

	L4_Clock_t start_time;
	int n = pump_wakeups(preempts, &start_time, spinner, TEST_IPC_DELAY);
	if(!ok1(n >= 0)) diag("error n=%d from pump_wakeups()", n);

	struct preempt_wakeup *wu = list_pop(preempts,
		struct preempt_wakeup, link);
	iff_ok1(wu == NULL, delay && !preempt_close);
	skip_start(wu == NULL, 1, "no wakeups were recorded") {
		int msg_at = (wu->clock.raw - start_time.raw + 500) / 1000;
		diag("msg_at=%d, wu->was_exn=%s", msg_at, btos(wu->was_exn));
		iff_ok1(wu->was_exn, delay && preempt_close);
	} skip_end;

	/* cleanup */
	xjoin_thread(spinner);
	xjoin_thread(preempt);
	talloc_free(preempts);
}
END_TEST


/* test on the effect of max_delay on preemption. disproves that max_delay
 * doesn't affect the spinner's preemption time, and that a quantum that runs
 * out during the delay period interrupts the delay.
 *
 * setup: same as delay_basics. preempt at 5ms.
 *
 * variables:
 *   - [is_long] max_delay short (10ms) or long (15ms)
 *   - [short_ts] quantum runs out during preempt delay, or not
 *
 * measurement is the time at which preemption occurs. should be 15ms for
 * short, 20ms for long.
 */
START_LOOP_TEST(delay_max_duration, iter, 0, 3)
{
	const bool is_long = CHECK_FLAG(iter, 1), short_ts = CHECK_FLAG(iter, 2);
	diag("is_long=%s, short_ts=%s", btos(is_long), btos(short_ts));
	plan_tests(5);

	/* setup */
	int my_pri = find_own_priority();
	L4_ThreadId_t preempt = start_thread_long(&preempt_fn, (void *)5,
		my_pri - 1, L4_TimePeriod(2 * 1000), L4_Never);
	fail_if(L4_IsNilThread(preempt));
	preempt = L4_GlobalIdOf(preempt);

	L4_ThreadId_t spinner = start_spinner(my_pri - 2, 25,
		L4_TimePeriod((short_ts ? 10 : 50) * 1000), L4_Never,
		true, false, true);
	fail_if(L4_IsNilThread(spinner));
	spinner = L4_GlobalIdOf(spinner);
	L4_Set_PreemptionDelay(spinner, my_pri - 1,
		is_long ? 15000 : 10000);

	diag("spinner=%lu:%lu, preempt=%lu:%lu",
		L4_ThreadNo(spinner), L4_Version(spinner),
		L4_ThreadNo(preempt), L4_Version(preempt));

	/* measurement */
	struct list_head *preempts = talloc(NULL, struct list_head);
	list_head_init(preempts);

	L4_Clock_t start_time;
	int n = pump_wakeups(preempts, &start_time, spinner, TEST_IPC_DELAY);
	if(!ok1(n >= 0)) diag("error n=%d from pump_wakeups()", n);

	struct preempt_wakeup *wu = list_pop(preempts,
		struct preempt_wakeup, link);
	ok(wu != NULL, "got at least one preempt");
	skip_start(wu == NULL, 2, "no wakeups were recorded") {
		fail_if(wu->clock.raw - start_time.raw > ULONG_MAX);
		L4_Word_t msg_at = wu->clock.raw - start_time.raw;
		diag("msg_at=%lu", msg_at);

		iff_ok1(fuzz_eq(msg_at, 10000, 1000), short_ts);

		imply_ok1(!is_long && !short_ts, fuzz_eq(msg_at, 15000, 1000));
		imply_ok1(is_long && !short_ts, fuzz_eq(msg_at, 20000, 1000));
	} skip_end;

	/* cleanup */
	xjoin_thread(spinner);
	xjoin_thread(preempt);
	talloc_free(preempts);
}
END_TEST


struct preempt {
	bool was_exn;
	L4_Clock_t clock, msg_clock;
};


static bool get_preempt(struct preempt *p)
{
	L4_Word_t hi, lo;
	bool ret, was_exn;
	int64_t c_diff;
	int n = __tmgr_get_preempt_record(get_mgr_tid(), &ret,
		L4_Myself().raw, &hi, &lo, &c_diff, &was_exn);
	fail_if(n != 0, "n=%d", n);
	if(!ret) return false;
	else {
		if(p != NULL) {
			*p = (struct preempt){
				.clock = { .raw = (L4_Word64_t)hi << 32 | lo },
				.was_exn = was_exn,
			};
			p->msg_clock.raw = (int64_t)p->clock.raw + c_diff;
		}
		return true;
	}
}


/* ¬s ∧ d.
 *
 * experiment: spin for 10 ms, preempt at 5 ms, spin for another 10 ms.
 * measurement: when the I flag is set.
 * variables: max_delay = 0 | 10ms.
 */
START_LOOP_TEST(delay_without_signal, iter, 0, 1)
{
	const bool max_delay_zero = CHECK_FLAG(iter, 1);
	diag("max_delay_zero=%s", btos(max_delay_zero));
	int my_pri = find_own_priority();
	diag("my_pri=%d", my_pri);
	plan_tests(7);

	L4_DisablePreemptionFaultException();

	L4_ThreadId_t pre = start_preempt(5);
	L4_Word_t rc = L4_Set_Priority(pre, my_pri - 1);
	fail_if(rc == L4_SCHEDRESULT_ERROR);
	rc = L4_Set_PreemptionDelay(L4_Myself(), my_pri,
		max_delay_zero ? 0 : 10 * 1000);
	fail_if(rc == L4_SCHEDRESULT_ERROR);
	rc = L4_Set_Priority(L4_Myself(), my_pri - 2);
	fail_if(rc == L4_SCHEDRESULT_ERROR);
	L4_ThreadSwitch(pre);

	bool sig_before_start = get_preempt(NULL);
	L4_Clock_t c_start = L4_SystemClock();
	bool pending_start = L4_PreemptionPending();
	L4_DisablePreemption();
	usleep(10 * 1000);
	bool pending_mid = L4_PreemptionPending();
	L4_Clock_t c_mid = L4_SystemClock();
	usleep(10 * 1000);
	bool pending_end = L4_PreemptionPending();
	L4_Clock_t c_end = L4_SystemClock();
	L4_EnablePreemption();
	bool sig_after_end = get_preempt(NULL);

	xjoin_thread(pre);
	diag("c_start=%lu, c_mid=%lu, c_end=%lu",
		(unsigned long)c_start.raw, (unsigned long)c_mid.raw,
		(unsigned long)c_end.raw);

	/* validation tests. */
	ok1(fuzz_eq(c_mid.raw - c_start.raw, 10000, 2000));
	ok1(fuzz_eq(c_end.raw - c_start.raw, 20000, 2000));

	ok1(!sig_before_start);
	ok1(!sig_after_end);
	ok1(!pending_start);
	iff_ok1(pending_mid, !max_delay_zero);
	ok1(!pending_end);
}
END_TEST


/* s ∧ d? ∧ max_delay=0. should pop a message immediately; it'll be an
 * exception iff d ∧ cur.sens_pri >= pre.pri.
 */
START_LOOP_TEST(exception_without_delay, iter, 0, 3)
{
	const bool d_set = CHECK_FLAG(iter, 1),
		low_sens_pri = CHECK_FLAG(iter, 2);
	diag("d_set=%s, low_sens_pri=%s", btos(d_set), btos(low_sens_pri));
	int my_pri = find_own_priority();
	diag("my_pri=%d", my_pri);
	plan_tests(6);

	L4_DisablePreemptionFaultException();
	L4_EnablePreemption();
	L4_ThreadId_t pre = start_preempt(5);
	L4_Set_Priority(pre, my_pri - 1);
	L4_Set_PreemptionDelay(L4_Myself(),
		low_sens_pri ? my_pri - 2 : my_pri, 0);
	L4_Set_Priority(L4_Myself(), my_pri - 2);
	L4_ThreadSwitch(pre);

	L4_Clock_t start = L4_SystemClock();
	L4_PreemptionPending();		/* clear "I" */
	bool sig_before_start = get_preempt(NULL);
	L4_EnablePreemptionFaultException();
	if(d_set) L4_DisablePreemption();
	usleep(10 * 1000);
	bool pending = L4_PreemptionPending();
	if(d_set) L4_EnablePreemption();
	L4_DisablePreemptionFaultException();
	L4_Clock_t end = L4_SystemClock();
	struct preempt p;
	bool sig_before_end = get_preempt(&p);

	/* validation */
	ok1(fuzz_eq(end.raw - start.raw, 10000, 2000));
	ok1(!sig_before_start);

	/* measurement */
	ok1(!pending);
	skip_start(!ok1(sig_before_end), 2, "no preemption") {
		iff_ok1(p.was_exn, d_set && !low_sens_pri);
		ok1(fuzz_eq(p.clock.raw - start.raw, 5000, 2000));
	} skip_end;

	xjoin_thread(pre);
}
END_TEST


/* start a thread that spins for longer than its total quantum. this should
 * cause a preemption message.
 *
 * TODO:
 *   - add test points for when the tqe rpc happens, and the time value it
 *     carries.
 */
START_LOOP_TEST(total_quantum_exhaust_rpc, iter, 0, 3)
{
	const bool short_tq = CHECK_FLAG(iter, 1),
		rcv_active = CHECK_FLAG(iter, 2);
	const int spin_ms = 20, my_pri = find_own_priority();
	const L4_Time_t ts_len = L4_TimePeriod(spin_ms * 1000 / 2),
		totq_len = L4_TimePeriod((short_tq ? spin_ms * 2 / 3 : spin_ms * 2) * 1000),
		ipc_len = L4_TimePeriod(spin_ms * 3 * 1000);
	diag("short_tq=%s, rcv_active=%s", btos(short_tq), btos(rcv_active));
	diag("spin_ms=%d, my_pri=%d, ts_len=%luµs, totq_len=%luµs",
		spin_ms, my_pri, (unsigned long)time_in_us(ts_len),
		(unsigned long)time_in_us(totq_len));
	plan_tests(5);

	L4_ThreadId_t spinner = start_spinner(my_pri - 2, spin_ms,
		ts_len, totq_len, false, false, false);
	spinner = L4_GlobalIdOf(spinner);
	diag("spinner=%lu:%lu", L4_ThreadNo(spinner), L4_Version(spinner));
	/* wait for the message. or not. */
	bool msg_got = false, sync_got = false;
	L4_Clock_t msg_clock = { .raw = 0 }, ipc_at_clock = { .raw = 0 },
		start_clock = L4_SystemClock();
	L4_ThreadId_t msg_from = L4_nilthread;
	for(;;) {
		if(rcv_active) {
			/* force passive send. */
			L4_Sleep(L4_TimePeriod(time_in_us(totq_len) + 2000));
		}
		L4_ThreadId_t sender;
		L4_MsgTag_t tag = L4_Wait_Timeout(ipc_len, &sender);
		if(L4_IpcFailed(tag)) {
			if(L4_ErrorCode() == 3) break;
			diag("ipc failed: ec=%#lx", L4_ErrorCode());
		} else if(L4_Label(tag) == 0xffd0) {
			L4_Word_t mr[2]; L4_StoreMRs(1, 2, mr);
			ipc_at_clock = L4_SystemClock();
			msg_clock.raw = (L4_Word64_t)mr[0] | (L4_Word64_t)mr[1];
			msg_got = true;
			msg_from = sender;
			diag("resetting total_quantum");
			L4_Word_t ret = L4_Set_Timeslice(sender, ts_len, L4_Never);
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

	iff_ok1(short_tq, msg_got);
	skip_start(!msg_got, 3, "tqe rpc didn't happen") {
		diag("msg: t=%lu, msg_t=%lu, ipc_t=%lu",
			(unsigned long)t, (unsigned long)msg_t, (unsigned long)ipc_t);
		ok1(L4_SameThreads(msg_from, spinner));
		ok1(msg_t >= L4_PeriodUs_NP(totq_len));
		ok1(ipc_t >= L4_PeriodUs_NP(totq_len));
	} skip_end;

	ok(sync_got, "got clean exit message");

	xjoin_thread(spinner);
}
END_TEST


/* if a thread in preëmpt delay clears "d" or "s" before an entered delay is
 * exceeded, either a preëmpt fault or nothing at all is sent, but never a
 * max-delay exceeded exception.
 *
 * variables:
 *   - [clear_s] whether to clear "s" after observing the "I" flag
 *   - [clear_d] the same for "d"
 */
START_LOOP_TEST(clear_preempt_flags_while_pending, iter, 0, 3)
{
	const bool clear_s = CHECK_FLAG(iter, 1), clear_d = CHECK_FLAG(iter, 2);
	diag("clear_s=%s, clear_d=%s", btos(clear_s), btos(clear_d));
	plan_tests(7);
	int my_pri = find_own_priority();
	diag("my_pri=%d, myself=%lu:%lu", my_pri,
		L4_ThreadNo(L4_MyGlobalId()), L4_Version(L4_MyGlobalId()));

	L4_ThreadId_t oth = start_preempt(5);
	L4_Set_Priority(oth, my_pri - 1);
	L4_Set_PreemptionDelay(L4_Myself(), my_pri, 10000);
	L4_Set_Priority(L4_Myself(), my_pri - 2);

	/* TODO: while the kernel doesn't properly handle Schedule that changes
	 * the current thread's preëmption status, we'll yield here to bring
	 * things to a correct status regardless.
	 */
	L4_ThreadSwitch(L4_nilthread);

	L4_Clock_t start = L4_SystemClock();
	L4_EnablePreemptionFaultException();
	L4_DisablePreemption();
	L4_PreemptionPending();	/* clear "I" */
	usleep(10 * 1000);
	bool pending = L4_PreemptionPending();
	if(clear_s) L4_DisablePreemptionFaultException();
	if(clear_d) L4_EnablePreemption();
	usleep(15 * 1000);

	bool s_after = L4_DisablePreemptionFaultException();
	bool nd_after = L4_EnablePreemption();

	/* validation */
	ok1(pending);
	iff_ok1(s_after, !clear_s);
	ok1(!nd_after);

	/* measurement */
	struct preempt p;
	bool msg = get_preempt(&p);
	diag("start=%llu", start.raw);
	if(msg) diag("p.msg_clock=%llu", p.msg_clock.raw);
	iff_ok1(!msg, clear_s);
	iff_ok1(msg && p.was_exn, !clear_s && !clear_d);
	iff_ok1(msg && !p.was_exn, !clear_s && clear_d);
	imply_ok1(msg, fuzz_eq(p.msg_clock.raw, start.raw + 15000, 2000));

	xjoin_thread(oth);
}
END_TEST


/* a test on extraordinary scheduling (ThreadSwitch), and its interaction with
 * preemption.
 *
 * TODO: needs better docs. and more variables. and a name that doesn't
 * mention yielding; that's strictly for switching to who-knows-which thread.
 */
START_LOOP_TEST(yield_timeslice, iter, 0, 1)
{
	const bool preempt_spinner = CHECK_FLAG(iter, 1);
	diag("preempt_spinner=%s", btos(preempt_spinner));
	plan_tests(2);

	int my_pri = find_own_priority();
	diag("my_pri=%d", my_pri);

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

	L4_MsgTag_t tag = L4_Receive_Timeout(spinner, L4_TimePeriod(45 * 1000));
	if(L4_IpcFailed(tag)) {
		diag("receive failed, ec=%#lx", L4_ErrorCode());
	}

	xjoin_thread(spinner);
	if(preempt_spinner) xjoin_thread(preempt);

	int diff_ms = (end.raw - start.raw + 500) / 1000;

	/* yielding should schedule out for at least 10 ms. */
	imply_ok1(!preempt_spinner, diff_ms >= 10);
	/* extraordinary scheduling should be preempted within 5 ms. */
	imply_ok1(preempt_spinner, diff_ms <= 5);
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
				diag("%s: ipc failed in %lu:%lu, ec=%#lx", __func__,
					L4_ThreadNo(L4_MyGlobalId()), L4_Version(L4_MyGlobalId()),
					L4_ErrorCode());
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
		tcase_add_test(tc, sleeping_ipc);
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
		tcase_add_test(tc, delay_basics);
		tcase_add_test(tc, delay_yield);
		tcase_add_test(tc, delay_exception);
		tcase_add_test(tc, delay_max_duration);
		tcase_add_test(tc, delay_without_signal);
		tcase_add_test(tc, exception_without_delay);
		tcase_add_test(tc, total_quantum_exhaust_rpc);
		tcase_add_test(tc, clear_preempt_flags_while_pending);
		suite_add_tcase(s, tc);
	}

	/* TODO: see above */
	{
		TCase *tc = tcase_create("yield");
		tcase_set_fork(tc, false);
		tcase_add_test(tc, yield_timeslice);
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
