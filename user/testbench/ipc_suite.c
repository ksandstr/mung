
/* tests concerning the Ipc system call, with the exception of anything
 * related to string transfers (which is tested in string_suite.c instead).
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>

#include <ccan/compiler/compiler.h>
#include <ccan/hash/hash.h>
#include <ccan/crc/crc.h>

#include <l4/types.h>
#include <l4/ipc.h>
#include <l4/kip.h>
#include <l4/space.h>
#include <l4/schedule.h>
#include <l4/syscall.h>

#include <ukernel/util.h>
#include <muidl.h>

#include "defs.h"
#include "test.h"

#define IPCHELPER_IMPL_SOURCE
#include "ipc-suite-defs.h"


#define IS_LOCAL_TID(tid) (!L4_IsNilThread(L4_LocalIdOf((tid))))


struct sender_param {
	L4_ThreadId_t parent;
	L4_Word_t payload;
	L4_Time_t delay;	/* ZeroTime or TimePeriod */
};


struct helper_ctx
{
	int fault_delay_us;
	bool running;
};


static struct helper_ctx *helper_ctx(void)
{
	static int helper_ctx_key = -1;

	if(helper_ctx_key == -1) {
		tsd_key_create(&helper_ctx_key, &free);
	}
	struct helper_ctx *ctx = tsd_get(helper_ctx_key);
	if(ctx == NULL) {
		ctx = malloc(sizeof *ctx);
		*ctx = (struct helper_ctx){
			.running = true,
			.fault_delay_us = 0,
		};
		tsd_set(helper_ctx_key, ctx);
		assert(tsd_get(helper_ctx_key) == ctx);
	}

	return ctx;
}


static void helper_quit_impl(void) {
	helper_ctx()->running = false;
}


static void helper_yield_impl(void) {
	L4_ThreadSwitch(L4_nilthread);
}


static void helper_sleep_impl(
	int32_t us,
	bool spin,
	int32_t sleep_after_us)
{
#if 0
	diag("%s: us=%d, spin=%s, sleep_after_us=%d", __func__,
		us, btos(spin), sleep_after_us);
#endif
	if(us > 0) {
		if(spin) usleep(us); else L4_Sleep(L4_TimePeriod(us));
	}

	if(sleep_after_us > 0) {
		L4_Sleep(L4_TimePeriod(sleep_after_us));
	}

	helper_ctx()->fault_delay_us = MAX(int, us, 0);
}


static void helper_handle_fault_impl(
	L4_Word_t faddr,
	L4_Word_t fip,
	L4_MapItem_t *map)
{
	int sleep_us = helper_ctx()->fault_delay_us;
	// diag("faddr=%#lx, fip=%#lx, sleep_us=%d", faddr, fip, sleep_us);
	if(sleep_us > 0) {
		L4_Sleep(L4_TimePeriod(sleep_us));
	}

	/* pass it up. */
	L4_MsgTag_t tag = muidl_get_tag();
	L4_LoadBR(0, L4_CompleteAddressSpace.raw);
	L4_LoadMR(0, tag.raw);
	L4_LoadMR(1, faddr);
	L4_LoadMR(2, fip);
	tag = L4_Call(L4_Pager());
	if(!L4_IpcSucceeded(tag)) {
		diag("%s: ipc failed, ec %#lx", __func__, L4_ErrorCode());
	}

	L4_StoreMRs(1, 2, map->raw);
}


static void munge_string_local(
	const char *input,
	char *output,
	int32_t rnd_seed)
{
	uint32_t str_seed = rnd_seed ^ hash_string(input);
	int length = strlen(input);
	char *buffer = malloc(length + 1);
	random_string(buffer, length, &str_seed);
	for(int i=0; i < length; i++) {
		output[i] = buffer[i] ^ input[i];
	}
	output[length] = '\0';
}


static const struct ipc_helper_vtable helper_vtab = {
	.quit = &helper_quit_impl,
	.yield = &helper_yield_impl,
	.sleep = &helper_sleep_impl,
	.handle_fault = &helper_handle_fault_impl,
	.munge_string = &munge_string_local,
};

IDL_FIXTURE(helper, ipc_helper, &helper_vtab, !helper_ctx()->running);
IDL_FIXTURE(other_helper, ipc_helper, &helper_vtab,
	!helper_ctx()->running);


/* sender thread utilities. these attempt to do an IPC with the caller thread
 * either from another thread in this process, or a thread in a forked address
 * space.
 */
static void sender_thread_fn(void *param_ptr)
{
	struct sender_param *p = param_ptr;

	if(p->delay.raw != L4_ZeroTime.raw) {
		assert(p->delay.raw != L4_Never.raw);
		L4_Sleep(p->delay);
	}

	L4_LoadMR(0, (L4_MsgTag_t){ .X.u = 1, .X.label = 0xd00d }.raw);
	L4_LoadMR(1, p->payload);
	L4_MsgTag_t tag = L4_Send(p->parent);
	if(L4_IpcFailed(tag)) {
		diag("%s: send failed, ec %#lx", __func__, L4_ErrorCode());
	}

	free(p);
}


static L4_ThreadId_t send_from_thread(L4_Word_t payload, L4_Time_t delay)
{
	struct sender_param *param = malloc(sizeof(*param));
	fail_if(param == NULL);
	*param = (struct sender_param){
		.parent = L4_MyGlobalId(), .payload = payload, .delay = delay,
	};
	L4_ThreadId_t tid = start_thread(&sender_thread_fn, param);
	return tid;
}


static L4_ThreadId_t send_from_fork(L4_Word_t payload, L4_Time_t delay)
{
	L4_ThreadId_t child_tid, parent_tid = L4_MyGlobalId();
	int pid = fork_tid(&child_tid);
	if(pid == 0) {
		struct sender_param *param = malloc(sizeof(*param));
		fail_if(param == NULL);
		*param = (struct sender_param){
			.parent = parent_tid, .payload = payload, .delay = delay,
		};
		sender_thread_fn(param);
		exit(0);
	}

	return child_tid;
}


static void close_sender(L4_ThreadId_t sender)
{
#if 0
	diag("%lu:%lu is %slocal", L4_ThreadNo(sender), L4_Version(sender),
		IS_LOCAL_TID(sender) ? "" : "not ");
#endif

	if(IS_LOCAL_TID(sender)) {
		L4_Word_t ec = 0;
		join_thread_long(sender, TEST_IPC_DELAY, &ec);
		if(ec != 0) {
			diag("%s: join_thread_long(): ec %#lx", __func__, ec);
		}
	} else {
		int status = 0, id = wait(&status);
		if(id < 0) diag("wait failed");
		else if(status != 0) diag("status from wait = %d", status);
	}
}


/* test four things about the L4_anylocalthread FromSpecifier:
 *   1) that when no thread at all is sending, L4_anylocalthread should cause
 *      a timeout (base case);
 *   2a) when only one thread is sending, but it is not local, should timeout;
 *   2b) same but one thread, local, should not timeout;
 *   3) two threads, one local & one foreign, should receive from local, and
 *      then timeout.
 *
 * due to the test harness being somewhat less than featureful wrt looped
 * tests, there's a loop around parts 2 and 3 to introduce delays, hopefully
 * triggering both active send and receive.
 */
START_TEST(receive_from_anylocalthread)
{
	plan_tests(1 + 2 * 4);

	const int delay_ms = 3;
	const L4_Time_t delay = L4_TimePeriod(delay_ms * 1000);

	/* flush immediate senders first with vanilla IPC. */
	bool timed_out = false;
	for(int i=0; i < 20; i++) {
		L4_ThreadId_t from;
		L4_MsgTag_t tag = L4_Wait_Timeout(L4_ZeroTime, &from);
		fail_unless(L4_IpcSucceeded(tag) || (L4_ErrorCode() & 0xf) == 3,
			"expected receive timeout or success, got ec %#lx",
			L4_ErrorCode());
		if(L4_IpcFailed(tag)) timed_out = true;
	}
	fail_unless(timed_out, "palate-cleansing didn't take");


	/* part 1 */
	L4_ThreadId_t from;
	L4_MsgTag_t tag = L4_WaitLocal_Timeout(TEST_IPC_DELAY, &from);
	ok(L4_IpcFailed(tag) && (L4_ErrorCode() & 0xf) == 3,
		"recv timeout in no-sender");

	for(int i=0; i <= 1; i++) {
		const bool d_self = !CHECK_FLAG(i, 1);
		const L4_Time_t d = d_self ? L4_ZeroTime : delay;
		diag("delay_ms=%d applies to %s", delay_ms,
			d_self ? "receiver" : "sender");

		/* part 2a */
		L4_ThreadId_t sender = send_from_fork(0xdeadbeef, d);
		if(d_self) L4_Sleep(delay);
		tag = L4_WaitLocal_Timeout(TEST_IPC_DELAY, &from);
		/* (no diag(), receive phase timeout is expected) */
		ok(L4_IpcFailed(tag) && (L4_ErrorCode() & 0xf) == 3,
			"recv timeout in foreign sender");
		/* (clear it, though.) */
		do {
			tag = L4_Wait(&from);
			fail_if(L4_IpcFailed(tag));
		} while(from.raw != sender.raw);
		close_sender(sender);

		/* part 2b */
		L4_ThreadId_t fork_sender = send_from_thread(0xf00bdead, d);
		if(d_self) L4_Sleep(delay);
		tag = L4_WaitLocal_Timeout(TEST_IPC_DELAY, &from);
		if(L4_IpcFailed(tag)) diag("ec %#lx", L4_ErrorCode());
		L4_Word_t payload;
		L4_StoreMR(1, &payload);
		ok(L4_IpcSucceeded(tag) && payload == 0xf00bdead,
			"recv success in local sender");
		close_sender(fork_sender);

		/* part 3 */
		fork_sender = send_from_fork(0xbaddcafe, d);
		sender = send_from_thread(0xb0a7face, d);
		if(d_self) L4_Sleep(delay);
		tag = L4_WaitLocal_Timeout(TEST_IPC_DELAY, &from);
		if(L4_IpcFailed(tag)) diag("ec %#lx", L4_ErrorCode());
		L4_StoreMR(1, &payload);
		ok(L4_IpcSucceeded(tag) && payload == 0xb0a7face,
			"received from thread, first");
		close_sender(sender);	/* must catch its death gurgle first */

		tag = L4_Wait_Timeout(TEST_IPC_DELAY, &from);
		if(L4_IpcFailed(tag)) diag("ec %#lx", L4_ErrorCode());
		L4_StoreMR(1, &payload);
		ok(L4_IpcSucceeded(tag) && payload == 0xbaddcafe,
			"received from fork, after");
		close_sender(fork_sender);
	}
}
END_TEST


/* IPC sleep with a TimePoint value. passes when it not only doesn't panic the
 * microkernel, but also when it drops out of the sleep at the correct time.
 */
START_LOOP_TEST(point_ipc_timeouts, iter, 0, 15)
{
	const unsigned pre_us = 800 * iter, sleep_us = 20 * 1000;
	assert(pre_us < sleep_us);

	plan_tests(2);

	L4_Clock_t pt = { .raw = L4_SystemClock().raw + sleep_us };
	L4_Time_t t = L4_TimePoint(pt);
	fail_unless(L4_IsTimePoint_NP(t));

	/* part 1: a timeout on a time point should happen at the specified time
	 * regardless of interim sleeps or the like.
	 */
	if(pre_us > 0) L4_Sleep(L4_TimePeriod(pre_us));
	L4_Sleep(t);
	L4_Clock_t wake_at = L4_SystemClock();
	ok(wake_at.raw - pt.raw < 200, "woke up at t + %u µs", sleep_us);

	/* part 2: an invalid (expired) timepoit should be handled like
	 * L4_ZeroTime.
	 */
	L4_Sleep(L4_TimePeriod(1000));	/* put it definitely in the swamp */

	L4_Clock_t pre_sleep = L4_SystemClock();
	fail_if(pre_sleep.raw <= pt.raw);
	L4_Sleep(t);
	wake_at = L4_SystemClock();
	ok(wake_at.raw - pre_sleep.raw < 200,
		"didn't sleep with expired point");
}
END_TEST


/* call IpcHelper::munge_string() with buffers that're known not to be mapped.
 * compare result with locally-computed equivalent via crc32c().
 */
static int munge_case(
	L4_ThreadId_t partner_tid,
	size_t munge_size,
	uint32_t munge_seed)
{
	char *munge_in = malloc(munge_size + 1),
		*munge_out = malloc(munge_size + 1);
	uint32_t rnd_seed = 0xcabb1e23;
	random_string(munge_in, munge_size, &rnd_seed);
	munge_string_local(munge_in, munge_out, munge_seed);
	uint32_t local_crc = crc32c(0, munge_out, munge_size);

	flush_byte_range((uintptr_t)munge_in, munge_size, L4_FullyAccessible);
	flush_byte_range((uintptr_t)munge_out, munge_size, L4_FullyAccessible);

	int n = __ipchelper_munge_string(other_helper_tid,
		munge_in, munge_out, munge_seed);
	if(n != 0) goto end;
	uint32_t remote_crc = crc32c(0, munge_out, munge_size);
	if(remote_crc != local_crc) {
		diag("remote_crc=%#x, local_crc=%#x", __func__,
			(unsigned)remote_crc, (unsigned)local_crc);
	}

end:
	free(munge_in);
	free(munge_out);
	return n;
}


/* same, but for xfer timeouts. this involves a string transfer helper thread,
 * and a delaying pager.
 */
START_TEST(point_xfer_timeouts)
{
	const size_t munge_size = 6 * 1024 + 77;
	const uint32_t munge_seed = 0x715517da;
	const int to_us = 25 * 1000, pg_delay_us = 5 * 1000;

	plan_tests(4);

	L4_Set_Pager(helper_tid);
	int n = __ipchelper_sleep(helper_tid, 0, false, 0);
	fail_unless(n == 0, "init sleep failed, n %#x", (unsigned)n);

	/* base case: no timeout when none is given. */
	L4_Set_XferTimeouts(L4_Timeouts(L4_Never, L4_Never));
	n = munge_case(other_helper_tid, munge_size, munge_seed);
	if(!ok1(n == 0)) diag("n=%d", n);

	/* part #1: no timeout when timeout is given, but no delay. */
	L4_Clock_t base = L4_SystemClock();
	L4_Time_t to_pt = L4_TimePoint2_NP(base,
		(L4_Clock_t){ .raw = base.raw + to_us });
	L4_Set_XferTimeouts(L4_Timeouts(to_pt, to_pt));
	n = munge_case(other_helper_tid, munge_size, munge_seed);
	if(!ok1(n == 0)) diag("n=%d", n);

	/* part #2: timeout should occur when delay is set. */
	L4_Set_XferTimeouts(L4_Timeouts(L4_Never, L4_Never));
	n = __ipchelper_sleep(helper_tid, pg_delay_us, false, 0);
	fail_unless(n == 0, "delay-setting sleep failed, n %#x", (unsigned)n);

	base = L4_SystemClock();
	to_pt = L4_TimePoint2_NP(base, (L4_Clock_t){ .raw = base.raw + to_us });
	diag("base=%#lx, to_pt={e=%d, m=%#x, c=%d} -> @%#lx",
		(L4_Word_t)base.raw, to_pt.point.e, to_pt.point.m, to_pt.point.c,
		(L4_Word_t)L4_PointClock_NP(base, to_pt).raw);
	L4_Sleep(L4_TimePeriod(to_us - pg_delay_us));
	fail_unless(pt_is_valid(L4_SystemClock(), to_pt));

	L4_Set_XferTimeouts(L4_Timeouts(to_pt, to_pt));
	n = munge_case(other_helper_tid, munge_size, munge_seed);
	L4_Clock_t after = L4_SystemClock();
	diag("after=%#lx", (L4_Word_t)after.raw);
	int code = (n >> 1) & 0x7;
	if(!ok(code == 5 || code == 6, "hit xfer timeout")) diag("n=%d", n);
	int64_t end_diff = (int64_t)after.raw - base.raw - to_us;
	if(!ok1(end_diff > 0)) diag("end_diff=%d", (int)end_diff);
}
END_TEST


static void recv_spin_fn(void *param_ptr)
{
	const L4_Word_t *ps = param_ptr;
	int sleep_us = ps[0];

	L4_ThreadId_t from;
	L4_MsgTag_t tag = L4_Wait(&from);
	if(L4_IpcFailed(tag)) {
		printf("%s: ipc failed, ec %#lx\n", __func__, L4_ErrorCode());
		goto end;
	}

	usleep(sleep_us);

end:
	free(param_ptr);
}


/* tests that a send from a lower to higher priority thread causes a
 * scheduling preemption. also tests that a send to a same-priority thread
 * causes no preemption.
 *
 * FIXME: see comment at r_recv_timeout_case; same applies here
 */
START_LOOP_TEST(send_preempt, iter, 0, 1)
{
	plan_tests(2);

	const bool p_preempt = CHECK_FLAG(iter, 1);
	diag("p_preempt=%s", btos(p_preempt));
	const int start_pri = find_own_priority(),
		spin_us = 5000;	/* 5 ms */
	fail_unless(start_pri >= 12,
		"need start_pri at least 12, got %d", start_pri);

	L4_Word_t *param = malloc(sizeof(L4_Word_t));
	param[0] = spin_us;
	L4_ThreadId_t other = start_thread_long(&recv_spin_fn, param,
		-1, L4_TimePeriod(50 * 1000), L4_Never);
	fail_if(L4_IsNilThread(other));
	L4_ThreadSwitch(other);

	if(p_preempt) {
		L4_Word_t ret = L4_Set_Priority(L4_Myself(), start_pri - 11);
		fail_if(ret == 0, "ret %lu, ec %#lx", ret, L4_ErrorCode());
	}

	L4_Clock_t start = L4_SystemClock();
	L4_LoadMR(0, 0);
	L4_MsgTag_t tag = L4_Send(other);
	fail_if(L4_IpcFailed(tag), "ec %#lx", L4_ErrorCode());
	L4_Clock_t end = L4_SystemClock();
	uint64_t diff_us = end.raw - start.raw;
	ok1(!p_preempt || diff_us >= spin_us);
	ok1(p_preempt || diff_us < 2000);

	diag("diff_us=%lu", (unsigned long)diff_us);

	join_thread(other);
}
END_TEST


START_LOOP_TEST(recv_timeout_from_send, iter, 0, 3)
{
	plan_tests(6);

	const bool p_spin = CHECK_FLAG(iter, 1),
		p_preempt = CHECK_FLAG(iter, 2);
	const unsigned int sleep_ms = 70, timeo_ms = sleep_ms / 2;
	diag("p_spin=%s, p_preempt=%s", btos(p_spin), btos(p_preempt));
	diag("sleep_ms=%u, timeo_ms=%u", sleep_ms, timeo_ms);

	if(p_preempt) {
		/* variant: with the helper at a higher priority, receive timeout
		 * should start from send-phase completion, and still work properly.
		 */
		L4_Word_t ret = L4_Set_Priority(L4_Myself(),
			find_own_priority() - 10);
		fail_if(ret == 0, "set_priority failed: ret=%lu", ret);
	}

	/* part 1: shouldn't timeout without a timeout. */
	int n = __ipchelper_yield(helper_tid);
	fail_unless(n == 0, "n=%d", n);
	n = __ipchelper_sleep_timeout(helper_tid, sleep_ms * 1000, p_spin,
		0, L4_Never, L4_Never);
	if(n != 0) diag("n=%d", n);
	ok(n == 0, "base (no timeout)");

	/* part 2: should timeout with a timeout.
	 *
	 * when the partner is IPC-sleeping, the timeout should happen on
	 * schedule. when it's spinning, timeout shouldn't happen sooner than when
	 * the helper's quantum is exhausted (!p_preempt), or at sleep completion
	 * (otherwise).
	 */
	n = __ipchelper_yield(helper_tid);
	fail_unless(n == 0, "n=%d", n);
	L4_Clock_t start = L4_SystemClock();
	n = __ipchelper_sleep_timeout(helper_tid, sleep_ms * 1000, p_spin,
		0, L4_Never, L4_TimePeriod(timeo_ms * 1000));
	L4_Clock_t end = L4_SystemClock();
	const int code = (n >> 1) & 0x7;
	const bool send_phase = !CHECK_FLAG(n, 1);
	diag("n=%d, code=%d, send_phase=%s", n, code, btos(send_phase));
	ok1(!send_phase);
	ok(code == 1, "is timeout");
	int diff_us = end.raw - start.raw,
		diff_ms = diff_us / 1000;
	diag("diff_us=%d, diff_ms=%d", diff_us, diff_ms);
	ok1(p_spin || diff_ms <= timeo_ms + 2);

	/* p_spin && !p_preempt ==> diff_ms = 50 (within error).
	 * (50 ms comes from start_thread()'s default quantum.)
	 */
	ok1(!p_spin || p_preempt || (diff_ms >= 50 && diff_ms < 55));

	/* p_spin && p_preempt ==> diff_ms >= sleep_ms. */
	ok1(!p_spin || !p_preempt || diff_ms >= sleep_ms);
}
END_TEST


/* synopsis: test that receive timeout is applied from send-phase completion
 * even under instant preemption. this reflects the (potentially late) R_RECV
 * -> RECV_WAIT transition by the scheduler, which should retain the timeout
 * set in the previous transition to R_RECV.
 *
 * plan: call IpcHelper::sleep() from a lower-priority thread. spin for a
 * shorter time than the timeout, but sleep for long enough after to pop the
 * timeout -- or not, if it wasn't retained from R_RECV.
 */
START_TEST(recv_timeout_from_preempt)
{
	plan_tests(2);
	const unsigned timeo_ms = 50, spin_ms = 35, sleep_ms = 25;

	L4_Word_t ret = L4_Set_Priority(L4_Myself(), find_own_priority() - 10);
	fail_if(ret == 0, "setpri failed, ret=%lu", ret);

	int n = __ipchelper_yield(helper_tid);
	fail_unless(n == 0, "n=%d", n);
	n = __ipchelper_sleep_timeout(helper_tid, spin_ms * 1000, true,
		sleep_ms * 1000, L4_Never, L4_TimePeriod(timeo_ms * 1000));
	const int code = (n >> 1) & 0x7;
	const bool send_phase = !CHECK_FLAG(n, 1);
	diag("n=%d, code=%d, send_phase=%s", n, code, btos(send_phase));
	ok1(!send_phase);
	ok(code == 1, "is timeout");
}
END_TEST


/* API tests wrt the Ipc system call. */

#define MUTATE_NUM_WAYS 2

static L4_ThreadId_t mutate_tid(L4_ThreadId_t tid, int way)
{
	assert(way < MUTATE_NUM_WAYS && way >= 0);
	L4_ThreadId_t wrong_tid = tid;
	if(L4_IsLocalId(wrong_tid)) {
		switch(way) {
			default: abort();
			case 0:
				/* local TIDs have the lowest six bits, i.e. 0x3f, set to
				 * zero. so flipping the seventh bit ought to produce an
				 * invalid local ID rather than refer to a neighbouring
				 * thread.
				 */
				wrong_tid.raw ^= 0x40;
				break;
			case 1:
				/* this puts the local TID entirely outside the space's UTCB
				 * segment.
				 */
				wrong_tid.raw ^= 0x94123000;
				break;
		}
		assert(L4_IsLocalId(wrong_tid));
	} else {
		switch(way) {
			default: abort();
			case 0:
				/* perturb the version somewhat. */
				wrong_tid.global.X.version ^= 0x29;
				wrong_tid.global.X.version |= 2;
				break;
			case 1:
				/* WHEEEEEEE! */
				wrong_tid.raw ^= 0xf4a28007;
				break;
		}
		assert(L4_IsGlobalId(wrong_tid));
	}
	assert(wrong_tid.raw != tid.raw);
	return wrong_tid;
}


/* test that L4_anythread, L4_anylocalthread, and the kernel ThreadNo range
 * aren't valid in Ipc's @to.
 */
START_TEST(tid_spec_to_fail)
{
	L4_KernelInterfacePage_t *kip = L4_GetKernelInterface();
	int // last_int = kip->ThreadInfo.X.SystemBase - 1,
		last_kern = kip->ThreadInfo.X.UserBase - 1;

	const struct {
		L4_ThreadId_t tid;
		const char *name;
	} invalid_cases[] = {
		{ L4_anythread, "anythread" },
		{ L4_anylocalthread, "anylocalthread" },
		{ L4_GlobalId(last_kern, 11), "tno=last_kern" },
		/* the interrupt range is tested in interrupt_suite.c, (TODO: or
		 * should be, anyway)
		 */
	};
	plan_tests(2 * NUM_ELEMENTS(invalid_cases));

	/* part 1: invalid @to. */
	for(int i=0; i < NUM_ELEMENTS(invalid_cases); i++) {
		L4_ThreadId_t tid = invalid_cases[i].tid;
		const char *name = invalid_cases[i].name;
		diag("tid=%#lx", tid.raw);
		L4_LoadMR(0, 0);
		L4_MsgTag_t tag = L4_Reply(tid);
		L4_Word_t ec = L4_ErrorCode(), code = ec >> 1;
		if(!ok(L4_IpcFailed(tag), "%s shouldn't be valid Ipc.to", name)) {
			diag("tag.raw=%#lx", tag.raw);
		}
		ok1(code == 2);
	}
}
END_TEST


static void receive_once(void *param UNUSED) {
	L4_ThreadId_t sender;
	L4_Wait(&sender);
}


/* local and global ID validity as Ipc.to .
 * 
 * i.e. that global and local thread IDs are valid if and only if the peer
 * exists, and that wrong thread version numbers and mangled local TIDs are
 * rejected.
 */
START_LOOP_TEST(tid_spec_to_ok, iter, 0, MUTATE_NUM_WAYS * 2 - 1)
{
	plan_tests(7);

	const bool local = CHECK_FLAG(iter, 1);
	const char *kind = local ? "local" : "global";
	const int mut_way = iter >> 1;

	L4_ThreadId_t gtid = start_thread(&receive_once, NULL),
		test_tid = local ? L4_LocalIdOf(gtid) : gtid,
		wrong_tid = mutate_tid(test_tid, mut_way);
	fail_if(L4_IsNilThread(gtid));

	L4_LoadMR(0, 0);
	L4_MsgTag_t tag = L4_Send_Timeout(test_tid, TEST_IPC_DELAY);
	if(!ok(L4_IpcSucceeded(tag), "send to extant %s TID", kind)) {
		 diag("ec=%#lx", L4_ErrorCode());
	}

	L4_LoadMR(0, 0);
	tag = L4_Send_Timeout(wrong_tid, TEST_IPC_DELAY);
	L4_Word_t ec = L4_ErrorCode();
	ok(L4_IpcFailed(tag), "send to wrong %s TID", kind);
	if(!ok1(ec == 4)) diag("ec=%#lx", ec);

	join_thread(gtid);

	L4_LoadMR(0, 0);
	tag = L4_Send_Timeout(test_tid, TEST_IPC_DELAY);
	ec = L4_ErrorCode();
	ok(L4_IpcFailed(tag), "send to joined %s TID", kind);
	ok1(ec == 4);

	L4_LoadMR(0, 0);
	tag = L4_Send_Timeout(wrong_tid, TEST_IPC_DELAY);
	ec = L4_ErrorCode();
	ok(L4_IpcFailed(tag), "send to wrong %s TID post-join", kind);
	ok1(ec == 4);
}
END_TEST


/* test that the kernel ThreadNo range isn't valid in Ipc's @from.
 * (NOTE: this is not specified in the L4.X2 API document.)
 *
 * the interrupt range is tested in interrupt_suite.c, (TODO: or should be,
 * anyway)
 */
START_TEST(tid_spec_from_fail)
{
	plan_tests(2);

	L4_KernelInterfacePage_t *kip = L4_GetKernelInterface();
	int last_kern = kip->ThreadInfo.X.UserBase - 1;

	L4_LoadMR(0, 0);
	L4_MsgTag_t tag = L4_Receive_Timeout(L4_GlobalId(last_kern, 17),
		L4_ZeroTime);
	L4_Word_t ec = L4_ErrorCode();
	if(!ok(L4_IpcFailed(tag), "last_kern shouldn't be valid Ipc.from")) {
		diag("tag.raw=%#lx", tag.raw);
	}
	if(!ok1(ec == 5)) diag("ec=%#lx", ec);
}
END_TEST


static void send_once_fn(void *param)
{
	L4_ThreadId_t to = { .raw = (uintptr_t)param };
	L4_LoadMR(0, 0);
	L4_Send_Timeout(to, TEST_IPC_DELAY);
}


/* test that global and local thread IDs are valid if and only if the peer
 * exists, and that wrong thread version numbers and mangled local TIDs are
 * rejected.
 */
START_LOOP_TEST(tid_spec_from_ok, iter, 0, MUTATE_NUM_WAYS * 4 - 1)
{
	plan_tests(7);
	const bool local = CHECK_FLAG(iter, 1),
		pass_local = CHECK_FLAG(iter, 2);
	const int mut_way = iter >> 2;
	assert(mut_way < MUTATE_NUM_WAYS);
	diag("local=%s, pass_local=%s, mut_way=%d", btos(local),
		btos(pass_local), mut_way);

	const char *kind = local ? "local" : "global";

	L4_ThreadId_t parent_id = pass_local ? L4_MyLocalId() : L4_MyGlobalId(),
		gtid = start_thread(&send_once_fn, (void *)(uintptr_t)parent_id.raw),
		test_tid = local ? L4_LocalIdOf(gtid) : gtid,
		wrong_tid = mutate_tid(test_tid, mut_way);
	fail_if(L4_IsNilThread(gtid));
	diag("parent_id=%#lx, test_tid=%#lx, wrong_tid=%#lx",
		parent_id.raw, test_tid.raw, wrong_tid.raw);

	L4_MsgTag_t tag = L4_Receive_Timeout(test_tid, TEST_IPC_DELAY);
	if(!ok(L4_IpcSucceeded(tag), "recv from extant %s TID", kind)) {
		diag("ec=%#lx", L4_ErrorCode());
	}

	tag = L4_Receive_Timeout(wrong_tid, TEST_IPC_DELAY);
	L4_Word_t ec = L4_ErrorCode();
	ok(L4_IpcFailed(tag), "recv from wrong %s TID", kind);
	if(!ok1(ec == 5)) diag("ec=%#lx", ec);

	join_thread(gtid);

	tag = L4_Receive_Timeout(test_tid, TEST_IPC_DELAY);
	ec = L4_ErrorCode();
	ok(L4_IpcFailed(tag), "recv from joined %s TID", kind);
	ok1(ec == 5);

	tag = L4_Receive_Timeout(wrong_tid, TEST_IPC_DELAY);
	ec = L4_ErrorCode();
	ok(L4_IpcFailed(tag), "recv from wrong %s TID post-join", kind);
	ok1(ec == 5);
}
END_TEST


struct prop_param {
	L4_ThreadId_t dest;
	bool p_bit, use_ltid;
};


static void prop_peer_fn(void *param)
{
	struct prop_param *p = param;
	L4_ThreadId_t orig = start_thread(&exit_thread, NULL);
	fail_if(L4_IsNilThread(orig));
	if(p->use_ltid) orig = L4_LocalIdOf(orig);
	else orig = L4_GlobalIdOf(orig);

	L4_MsgTag_t tag = { .X.u = 1, .X.label = 0x1234 };
	if(p->p_bit) {
		L4_Set_Propagation(&tag);
		L4_Set_VirtualSender(orig);
	} else {
		L4_Set_VirtualSender(L4_nilthread);
	}
	L4_LoadMR(0, tag.raw);
	L4_LoadMR(1, orig.raw);
	tag = L4_Call(p->dest);
	p->dest.raw = L4_IpcFailed(tag) ? L4_ErrorCode() : 0;

	join_thread(orig);
}


START_LOOP_TEST(propagation, iter, 0, 15)
{
	plan_tests(7);
	const bool same_as = CHECK_FLAG(iter, 1), p_bit = CHECK_FLAG(iter, 2),
		use_ltid = CHECK_FLAG(iter, 4), passive = CHECK_FLAG(iter, 8);
	diag("same_as=%s, p_bit=%s, use_ltid=%s, passive=%s",
		btos(same_as), btos(p_bit), btos(use_ltid), btos(passive));

	/* start peer. */
	struct prop_param *param = malloc(sizeof(*param));
	param->p_bit = p_bit;
	param->use_ltid = use_ltid;
	param->dest = L4_MyGlobalId();
	int child = -1;
	L4_ThreadId_t peer_tid;
	if(same_as) {
		peer_tid = start_thread(&prop_peer_fn, param);
	} else {
		child = fork_tid(&peer_tid);
		if(child == 0) {
			prop_peer_fn(param);
			exit(0);
		}
	}
	if(!passive) {
		/* hackiest thing ever.
		 * TODO: come up with a better way to force active/passive receive.
		 */
		L4_Sleep(L4_TimePeriod(2 * 1000));
	}

	/* receive and examine the entrails. */
	L4_ThreadId_t sender;
	L4_MsgTag_t tag = L4_Wait_Timeout(TEST_IPC_DELAY, &sender);
	fail_unless(L4_IpcSucceeded(tag), "ec=%#lx", L4_ErrorCode());
	L4_ThreadId_t orig_tid; L4_StoreMR(1, &orig_tid.raw);
	L4_ThreadId_t as_tid = L4_ActualSender();
	diag("sender=%lu:%lu, orig_tid=%lu:%lu, as_tid=%lu:%lu",
		L4_ThreadNo(sender), L4_Version(sender),
		L4_ThreadNo(orig_tid), L4_Version(orig_tid),
		L4_ThreadNo(as_tid), L4_Version(as_tid));
	fail_unless(use_ltid == L4_IsLocalId(orig_tid));

	iff_ok1(p_bit, L4_IpcPropagated(tag));
	imply_ok1(L4_SameThreads(sender, orig_tid), p_bit);
	imply_ok1(p_bit, L4_SameThreads(as_tid, peer_tid));
	imply_ok1(p_bit && same_as, L4_IsLocalId(as_tid));
	imply_ok1(p_bit && !same_as, L4_IsGlobalId(as_tid));

	iff_ok1(!p_bit, L4_SameThreads(sender, peer_tid));
	iff_ok1(same_as, L4_IsLocalId(sender));

	/* cleanup. */
	L4_LoadMR(0, 0);
	tag = L4_Reply(peer_tid);
	if(L4_IpcFailed(tag)) {
		diag("peer reply failed, ec=%#lx", L4_ErrorCode());
	}

	if(same_as) {
		join_thread(peer_tid);
	} else {
		assert(child >= 0);
		int st, dead = wait(&st);
		fail_unless(dead == child, "dead=%d, child=%d", dead, child);
	}
	free(param);
}
END_TEST


Suite *ipc_suite(void)
{
	Suite *s = suite_create("ipc");

	{
		TCase *tc = tcase_create("api");
		tcase_add_test(tc, tid_spec_to_fail);
		tcase_add_test(tc, tid_spec_to_ok);
		tcase_add_test(tc, tid_spec_from_fail);
		tcase_add_test(tc, tid_spec_from_ok);
		tcase_add_test(tc, propagation);
		suite_add_tcase(s, tc);
	}

	/* tests written to hit a panic() in ipc.c, which haven't been sorted
	 * elsewhere yet
	 */
	TCase *panic_case = tcase_create("panic");
	tcase_add_test(panic_case, receive_from_anylocalthread);
	suite_add_tcase(s, panic_case);

	TCase *preempt_case = tcase_create("preempt");
	tcase_set_fork(preempt_case, false);
	tcase_add_test(preempt_case, send_preempt);
	/* TODO: also one for receive */
	suite_add_tcase(s, preempt_case);

	TCase *timeout_case = tcase_create("timeout");
	tcase_set_fork(timeout_case, false);
	ADD_IDL_FIXTURE_U(timeout_case, helper);
	ADD_IDL_FIXTURE_U(timeout_case, other_helper);
	tcase_add_test(timeout_case, recv_timeout_from_send);
	tcase_add_test(timeout_case, recv_timeout_from_preempt);
	tcase_add_test(timeout_case, point_ipc_timeouts);
	tcase_add_test(timeout_case, point_xfer_timeouts);
	suite_add_tcase(s, timeout_case);

	return s;
}
