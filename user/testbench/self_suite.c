
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include <l4/types.h>
#include <l4/ipc.h>
#include <l4/thread.h>

#include "defs.h"
#include "test.h"


static struct pager_stats *stats;
static L4_ThreadId_t stats_tid;


/* tcase fork: tests of forkserv's own process management features. */

START_TEST(basic_fork_and_wait)
{
	plan_tests(2);

	int spid = fork();
	if(spid == 0) {
		/* child side */
		exit(0);
		assert(false);
	}
	ok(spid > 0, "fork succeeded");
	int status = 0, dead = wait(&status);
	ok(dead > 0 && dead == spid, "child exited");
	/* TODO: test the status, too */
}
END_TEST


START_TEST(copy_on_write)
{
	plan_tests(2);
	const int buffer_size = 256;

	const char *teststr = "the quick brown fox jumps over the lazy dog";
	uint8_t *buffer = malloc(buffer_size);
	for(int i=0; i < 256; i++) buffer[i] = i;
	strlcpy((void *)buffer, teststr, buffer_size);

	int spid = fork();
	if(spid != 0) {
		fail_if(spid < 0, "fork failed");
		int status = 0, dead = wait(&status);
		fail_if(dead < 0, "wait failed");
		fail_if(dead != spid, "expected %d from wait, got %d", spid, dead);
		ok(status == (int)buffer[0] + 1 + (int)(buffer[255] + 1) % 256,
			"exit status is correct");

		bool bad = false;
		int testlen = strlen(teststr);
		for(int i = 0; i < 256 && !bad; i++) {
			uint8_t expect;
			if(i < testlen) expect = teststr[i];
			else if(i == testlen) expect = 0;
			else expect = i;
			if(buffer[i] != expect) {
				diag("at position %d: expected %#x, found %#x", i,
					(unsigned)expect, (unsigned)buffer[i]);
				bad = true;
			}
		}
		ok(!bad, "parent's buffer was not modified");

		free(buffer);
	} else {
		/* alter the buffer's contents. */
		for(int i=0; i < 256; i++) buffer[i]++;
		exit((int)buffer[0] + (int)buffer[255]);
	}
}
END_TEST


static void wait_and_exit(int rc, L4_Word_t wait_ms)
{
	L4_Sleep(L4_TimePeriod(wait_ms * 1000));
	exit(rc);
}


START_LOOP_TEST(multi_fork_and_wait, iter, 0, 3)
{
	bool do_wait = (iter & 1) != 0, many = (iter & 2) != 0;

	plan_tests(1);

	const int num_subs = many ? 6 : 1;
	int subs[num_subs];
	for(int i=0; i < num_subs; i++) {
		int child = fork();
		fail_if(child == -1, "fork() failed");
		if(child != 0) {
			subs[i] = child;
			if(!do_wait) L4_Sleep(L4_TimePeriod(5 * 1000));
		} else {
			/* child side */
			if(do_wait) wait_and_exit(0, 10); else exit(0);
		}
	}

	int got = 0;
	for(int i=0; i < num_subs; i++) {
		int status = 0, id = wait(&status);
		fail_if(id < 0, "wait() failed: %d", id);

		for(int j=0; j < num_subs; j++) {
			if(subs[j] == id) {
				got++;
				subs[j] = -1;
				break;
			}
		}
	}
	ok(got == num_subs, "waited for all child processes");
}
END_TEST


/* test that the exit status is communicated back correctly. note that this
 * hits the waiting and non-waiting cases equally.
 */
START_TEST(return_exit_status)
{
	plan_tests(1);

	int expect_sum = 0, observe_sum = 0;
	for(int i=0; i < 16; i++) {
		expect_sum += (i + 1);
		int child = fork();
		if(child != 0) {
			fail_if(child == -1);
			int status = 0, dead = wait(&status);
			fail_unless(dead == child);
			observe_sum += status;
		} else {
			int rc = i + 1;
			if((i & 1) != 0) wait_and_exit(rc, 2); else exit(rc);
		}
	}
	ok1(expect_sum == observe_sum);
}
END_TEST


/* fork a child process, and fork another child process from inside that. wait
 * on one, and then the other.
 */
START_TEST(deep_fork)
{
	plan_tests(1);
	const int exit_magic = 23;

	int child = fork();
	if(child != 0) {
		fail_if(child < 0);
		int status = 0, dead = wait(&status);
		fail_if(dead < 0);
		ok1(dead == child && status == 0);
	} else {
		child = fork();
		if(child != 0) {
			int status = 0, dead = wait(&status);
			exit(dead == child && status == exit_magic ? 0 : 1);
		} else {
			exit(exit_magic);
		}
		assert(false);
	}
}
END_TEST


L4_Word_t __attribute__((noinline)) get_utcb_noinline(void) {
	return (L4_Word_t)__L4_Get_UtcbAddress();
}


START_TEST(ipc_with_child)
{
	plan_tests(2);

	L4_ThreadId_t parent_tid = L4_Myself();
#if 0
	diag("parent (%lu:%lu) UTCB base is %p",
		L4_ThreadNo(parent_tid), L4_Version(parent_tid),
		__L4_Get_UtcbAddress());
#endif
	int spid = fork();
	if(spid == 0) {
#if 0
		diag("child UTCB looks like %p", __L4_Get_UtcbAddress());
		diag("real UTCB base is %#lx", get_utcb_noinline());

		L4_ThreadId_t ctid = L4_Myself();
		diag("child %#lx sending to parent %#lx", ctid.raw, parent_tid.raw);
#endif

		L4_LoadMR(0, (L4_MsgTag_t){ .X.label = 0x2323, .X.u = 1 }.raw);
		L4_LoadMR(1, L4_Myself().raw);
		L4_MsgTag_t tag = L4_Call(parent_tid);
		if(L4_IpcFailed(tag)) exit(L4_ErrorCode());

		L4_Word_t value;
		L4_StoreMR(1, &value);
		value++;
		L4_LoadMR(0, (L4_MsgTag_t){ .X.u = 1, .X.label = 0x4242 }.raw);
		L4_LoadMR(1, value);
		tag = L4_Send(parent_tid);
		if(L4_IpcFailed(tag)) exit(L4_ErrorCode());

		exit(0);
	}
	fail_if(spid <= 0, "fork failed: return value %d", spid);

	L4_ThreadId_t child_tid;
	L4_MsgTag_t tag = L4_Wait_Timeout(TEST_IPC_DELAY, &child_tid);
	fail_if(L4_IpcFailed(tag), "child IPC fail, ec %#lx", L4_ErrorCode());

	L4_ThreadId_t reported_tid;
	L4_StoreMR(1, &reported_tid.raw);
	ok(reported_tid.raw == child_tid.raw, "child knows its own TID");

	/* reply with an incrementable value. */
	const L4_Word_t in_val = 666;
	L4_LoadMR(0, (L4_MsgTag_t){ .X.u = 1 }.raw);
	L4_LoadMR(1, in_val);
	L4_Reply(child_tid);

	tag = L4_Receive_Timeout(child_tid, TEST_IPC_DELAY);
	fail_if(L4_IpcFailed(tag), "child IPC receive fail, ec %#lx",
		L4_ErrorCode());

	L4_Word_t out_val;
	L4_StoreMR(1, &out_val);
	ok(out_val == in_val + 1, "child incremented given value");

	int status = 0, dead_pid = wait(&status);
	fail_unless(dead_pid == spid, "wait() failed (status %d, retval %d)",
		status, dead_pid);
}
END_TEST


/* tcase pagers: tests related to custom pagers used by testbench (except for
 * forkserv, which is tested in the "process" case).
 */

START_TEST(stats_delay_test)
{
	const int delay_ms = 25, repeat_ct = 4;
	fail_if(delay_ms < 15);

	plan_tests(1 + (repeat_ct + 4) * 2);

	/* part 1: without delay, the reset call should return immediately. */
	fail_if(!send_reset(stats_tid), "ec %#lx", L4_ErrorCode());
	fail_if(!send_delay(stats_tid, L4_ZeroTime, 0), "ec %#lx", L4_ErrorCode());
	L4_Clock_t start = L4_SystemClock();
	fail_if(!send_reset(stats_tid), "ec %#lx", L4_ErrorCode());
	L4_Clock_t end = L4_SystemClock();
	fail_if(end.raw < start.raw);
	int elapsed_ms = (end.raw - start.raw) / 1000;
	ok1(elapsed_ms <= 1);

	/* part 2: with delay, it should apply a measurable delay that's close
	 * enough to delay_ms (FIXME: adjust this using the clock precision).
	 *
	 * and part 3: it should stop applying the delay after repeat_ct calls.
	 */
	fail_if(!send_delay(stats_tid, L4_TimePeriod(delay_ms * 1000), repeat_ct),
		"ec %#lx", L4_ErrorCode());
	for(int i=0; i < repeat_ct + 4; i++) {
		start = L4_SystemClock();
		fail_if(!send_reset(stats_tid), "ec %#lx", L4_ErrorCode());
		end = L4_SystemClock();
		fail_if(end.raw < start.raw);
		int elapsed_ms = (end.raw - start.raw) / 1000;

		ok1(i >= repeat_ct || elapsed_ms > delay_ms - 10);
		ok1(i < repeat_ct || elapsed_ms <= 1);
	}
}
END_TEST


/* start the stats-collecting pager thread. */
static void stats_setup(void)
{
	stats = malloc(sizeof(*stats));
	fail_unless(stats != NULL);
	stats_tid = start_stats_pager(stats);
}


static void stats_teardown(void)
{
	L4_Word_t ec = stop_stats_pager(stats_tid);
	fail_if(ec != 0, "stop_stats_pager() failed, ec %#lx", ec);
}


Suite *self_suite(void)
{
	Suite *s = suite_create("self");

	TCase *fork_case = tcase_create("fork");
	tcase_add_test(fork_case, basic_fork_and_wait);
	tcase_add_test(fork_case, copy_on_write);
	tcase_add_test(fork_case, return_exit_status);
	tcase_add_test(fork_case, ipc_with_child);
	tcase_add_test(fork_case, deep_fork);
	tcase_add_test(fork_case, multi_fork_and_wait);
	suite_add_tcase(s, fork_case);

	TCase *pagers = tcase_create("pg");
	tcase_add_checked_fixture(pagers, &stats_setup, &stats_teardown);
	tcase_add_test(pagers, stats_delay_test);
	suite_add_tcase(s, pagers);

	return s;
}
