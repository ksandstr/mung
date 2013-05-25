
/* tests concerning the Ipc system call, with the exception of anything
 * related to string transfers (which is tested in string_suite.c instead).
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>

#include <ccan/compiler/compiler.h>

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


static L4_ThreadId_t helper_tid;
static bool helper_running;


/* TODO: move this into util.c or some such */
static int fork_tid(L4_ThreadId_t *tid_p)
{
	L4_MsgTag_t tag;
	L4_ThreadId_t parent = L4_MyGlobalId();
	int pid = fork();
	if(pid != 0) {
		tag = L4_Wait(tid_p);
	} else {
		*tid_p = L4_nilthread;
		L4_LoadMR(0, 0);
		tag = L4_Send(parent);
	}
	fail_if(L4_IpcFailed(tag), "%s: ec %#lx", __func__, L4_ErrorCode());

	return pid;
}


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


/* TODO: move into util.h or some such. this isn't found in the Pistachio
 * <l4/ipc.h> API declaration, but maybe should be.
 */
static inline L4_MsgTag_t L4_WaitLocal_Timeout(
	L4_Time_t rcv_timeout,
	L4_ThreadId_t *from_p)
{
	return L4_Ipc(L4_nilthread, L4_anylocalthread,
		L4_Timeouts(L4_Never, rcv_timeout), from_p);
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

	const unsigned int sleep_ms = 70, timeo_ms = sleep_ms / 2;
	const bool p_spin = CHECK_FLAG(iter, 1),
		p_preempt = CHECK_FLAG(iter, 2);
	diag("sleep_ms=%d, timeo_ms=%d, p_spin=%s, p_preempt=%s",
		sleep_ms, timeo_ms, btos(p_spin), btos(p_preempt));

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

	/* 50 ms comes from start_thread()'s default quantum. this is true
	 * regardless of p_preempt.
	 */
	ok1(!p_spin || diff_ms >= 50);

	/* however, p_spin && p_preempt ==> diff_ms >= sleep_ms. */
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


static void helper_quit_impl(void) {
	helper_running = false;
}


static void helper_yield_impl(void) {
	L4_ThreadSwitch(L4_nilthread);
}


static void helper_sleep_impl(
	int32_t us,
	bool spin,
	int32_t sleep_after_us)
{
	if(spin) usleep(us); else L4_Sleep(L4_TimePeriod(us));

	if(sleep_after_us > 0) {
		L4_Sleep(L4_TimePeriod(sleep_after_us));
	}
}


static void helper_thread_fn(void *param UNUSED)
{
	static const struct ipc_helper_vtable vtab = {
		.quit = &helper_quit_impl,
		.yield = &helper_yield_impl,
		.sleep = &helper_sleep_impl,
	};

	helper_running = true;
	while(helper_running) {
		L4_Word_t status = _muidl_ipc_helper_dispatch(&vtab);
		if(status == MUIDL_UNKNOWN_LABEL
			&& muidl_get_tag().X.label == 0xcbad)
		{
			/* ignore this; it's just a "pop back to the loop" thing that
			 * makes us re-check helper_running.
			 */
		} else if(status != 0 && !MUIDL_IS_L4_ERROR(status)) {
			printf("helper: dispatch status %#lx\n", status);
		}
	}
}


static void helper_setup(void)
{
	fail_unless(L4_IsNilThread(helper_tid));
	helper_tid = start_thread(&helper_thread_fn, NULL);
	fail_unless(!L4_IsNilThread(helper_tid));
}


static void helper_teardown(void)
{
	bool quit_ok = send_quit(helper_tid);
	fail_unless(quit_ok, "send_quit() failed, ec %#lx", L4_ErrorCode());
	/* hacky hacky. provoke an unknown ipc status, and quit of the helper
	 * thread.
	 */
	L4_LoadMR(0, (L4_MsgTag_t){ .X.label = 0xcbad }.raw);
	L4_MsgTag_t tag = L4_Send_Timeout(helper_tid, TEST_IPC_DELAY);
	fail_if(L4_IpcFailed(tag), "ec %#lx", L4_ErrorCode());

	void *value = join_thread(helper_tid);
	fail_unless(value == NULL,
		"unexpected return from string test thread: `%s'", (char *)value);
	helper_tid = L4_nilthread;
}


Suite *ipc_suite(void)
{
	Suite *s = suite_create("ipc");

	/* tests written to hit a panic() in ipc.c, which haven't been sorted
	 * elsewhere yet
	 */
	TCase *panic_case = tcase_create("panic");
	tcase_add_test(panic_case, receive_from_anylocalthread);
	suite_add_tcase(s, panic_case);

	TCase *preempt_case = tcase_create("preempt");
	tcase_add_test(preempt_case, send_preempt);
	/* TODO: also one for receive */
	suite_add_tcase(s, preempt_case);

	TCase *timeout_case = tcase_create("timeout");
	tcase_add_checked_fixture(timeout_case,
		&helper_setup, &helper_teardown);
	tcase_add_test(timeout_case, recv_timeout_from_send);
	tcase_add_test(timeout_case, recv_timeout_from_preempt);
	suite_add_tcase(s, timeout_case);

	return s;
}
