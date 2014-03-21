
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include <l4/types.h>
#include <l4/ipc.h>
#include <l4/thread.h>
#include <l4/kip.h>

#include <ukernel/util.h>

#include "defs.h"
#include "test.h"
#include "forkserv-defs.h"


#define now_ms() (L4_SystemClock().raw / 1000)


static struct pager_stats *stats;
static L4_ThreadId_t stats_tid;


static void fresh_tick(void)
{
	L4_Clock_t start = L4_SystemClock();
	while(start.raw >= L4_SystemClock().raw) {
		/* spin! */
	}
}


/* tcase util: tests of whatever's in defs.h */

START_TEST(basic_delay_test)
{
	plan_tests(2);
	L4_KernelInterfacePage_t *kip = L4_GetKernelInterface();
	const uint64_t tick_us = time_in_us((L4_Time_t){
		.raw = kip->ClockInfo.X.ReadPrecision });
	diag("tick_us=%lu", (unsigned long)tick_us);

	/* base case: a sleep of 0 µs causes advance of 0 clock ticks. */
	fresh_tick();
	uint64_t start_ms = now_ms();
	usleep(0);
	uint64_t end_ms = now_ms();
	ok1(start_ms == end_ms);

	/* sleeping for 2 ticks' worth should advance the clock by 2 ticks. */
	fresh_tick();
	L4_Clock_t start = L4_SystemClock();
	/* well... plus a bit of jitter to cross the edge properly */
	usleep(tick_us * 2 + (tick_us / 30));
	L4_Clock_t end = L4_SystemClock();
	diag("end %lu, start %lu",
		(unsigned long)end.raw, (unsigned long)start.raw);
	ok1(end.raw - start.raw == 2 * tick_us);
}
END_TEST


/* tcase fork: tests of forkserv's own process management features. */

START_TEST(basic_fork_and_wait)
{
	plan_tests(2);

	int spid = fork();
	if(spid == 0) {
		/* child side */
		diag("child running, sees pid=%d", getpid());
		exit(0);
		assert(false);
	}
	diag("spid=%d", spid);
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


/* provoke two kinds of segfault in as many children. the first is when the
 * ordinary process dies, and the second causes the process manager thread to
 * segfault. both segfaults will occur as writes on the kernel interface page.
 */
START_LOOP_TEST(report_child_segfault, iter, 0, 1)
{
	L4_KernelInterfacePage_t *kip = L4_GetKernelInterface();

	plan_tests(2);

	int child = fork();
	if(child == 0) {
		if(iter == 0) {
			memset(kip, 0, 100);	/* boomqvist! */
		} else {
			/* coax the manager thread into a snafu: cancel its IPC and send
			 * it to a function that attempts to write the KIP. and just to be
			 * extra effective, set SP to the middle of the KIP.
			 * (control = 0x1e = {isSR})
			 */
			L4_Word_t dummy;
			L4_ThreadId_t dummy_tid, out_tid;
			diag("setting mgr to sp=%#lx, ip=%#lx",
				(L4_Word_t)kip + PAGE_SIZE / 2, (L4_Word_t)&exit_thread);
			out_tid = L4_ExchangeRegisters(get_mgr_tid(), 0x1e,
				(L4_Word_t)kip + PAGE_SIZE / 2, (L4_Word_t)&exit_thread,
				0, 0, L4_nilthread,
				&dummy, &dummy, &dummy, &dummy, &dummy, &dummy_tid);
			if(L4_IsNilThread(out_tid)) {
				diag("exregs failed, ec=%#lx", L4_ErrorCode());
			}
			/* let the manager crash. */
			L4_Sleep(A_SHORT_NAP);
		}
		diag("child exiting normally when it shouldn't");
		exit(0);
	}

	int st = 0, dead = wait(&st);
	fail_if(dead != child, "different dead child: expected=%d, got=%d",
		child, dead);
	diag("child=%d, dead=%d, st=%#lx", child, dead, (unsigned long)st);
	ok((st & 0xf) == 7, "segfault was indicated");
	ok((st & ~PAGE_MASK) == ((L4_Word_t)kip & ~PAGE_MASK),
		"segfault was at correct address");
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


/* yo dawg! */
START_TEST(deeper_fork)
{
	plan_tests(2);
	const int depth = 5;
	const int test_pid = getpid();

	/* warning: this control flow will do your head in. */
	int my_depth = -1, children[depth], not_ok = 0, nok_child = 0;
	for(int i=0; i < depth; i++) {
		children[i] = fork();
		if(children[i] == 0) my_depth = i;
		else if(children[i] == -1) {
			diag("d%d p%d: fork() failed (i=%d)",
				my_depth, (int)getpid(), i);
		}
	}
	for(int i = my_depth + 1; i < depth; i++) {
		int st, dead = wait(&st);
		if(dead <= 0) {
			diag("d%d p%d: wait() failed, dead=%d",
				my_depth, (int)getpid(), dead);
			continue;
		}
		bool found = false, waited = false;
		for(int j = my_depth + 1; j < depth; j++) {
			if(children[j] == -dead) {
				waited = true;
				break;
			} else if(children[j] == dead) {
				children[j] = -dead;
				found = true;
				break;
			}
		}
		if(!found) {
			diag("d%d p%d waited on %s pid=%d, st=%d",
				my_depth, (int)getpid(),
				waited ? "previously-waited" : "non-child", dead, st);
			not_ok++;
			continue;
		}
		if(st > 0) nok_child += st;
	}
	if(my_depth >= 0) {
#if 0
		if(my_depth == depth - 1) {
			diag("d%d p%d exiting!", my_depth, (int)getpid());
		}
#endif
		exit(nok_child + not_ok);
	}

	fail_unless(getpid() == test_pid);
	ok1(not_ok == 0);
	ok1(nok_child == 0);
}
END_TEST


/* do fork_tid() three times. have each child call to the parent.
 *
 * this teases out the bug where fork_tid() discovers the child process TID by
 * a wildcard receive, breaking other IPC flows.
 */
START_TEST(multi_fork_tid)
{
	plan_tests(3);

	int childs[3];
	L4_ThreadId_t tids[3], parent = L4_MyGlobalId();
	for(int i=0; i < 3; i++) {
		childs[i] = fork_tid(&tids[i]);
		if(childs[i] == 0) {
			L4_LoadBR(0, 0);
			L4_LoadMR(0, 0);
			L4_Clock_t start = L4_SystemClock();
			L4_MsgTag_t tag = L4_Call_Timeouts(parent,
				TEST_IPC_DELAY, TEST_IPC_DELAY);
			L4_Clock_t end = L4_SystemClock();
			if(L4_IpcFailed(tag)) {
				diag("i=%d: child-to-parent call failed, ec=%#lx", i, L4_ErrorCode());
				diag("... start=%lu, end=%lu, diff=%lu",
					(unsigned long)start.raw, (unsigned long)end.raw,
					(unsigned long)(end.raw - start.raw));
			}
			exit(L4_IpcFailed(tag) ? 1 : 0);
		}
	}

	int n_replied = 0;
	for(int i=0; i < 3; i++) {
		L4_LoadBR(0, 0);
		L4_ThreadId_t sender;
		L4_MsgTag_t tag = L4_Wait_Timeout(TEST_IPC_DELAY, &sender);
		if(L4_IpcFailed(tag)) {
			diag("wait failed on i=%d, ec=%#lx", i, L4_ErrorCode());
			break;
		}
		L4_LoadMR(0, 0);
		tag = L4_Reply(sender);
		if(L4_IpcFailed(tag)) {
			diag("reply to %lu:%lu failed",
				L4_ThreadNo(sender), L4_Version(sender));
		} else {
			n_replied++;
		}
	}

	ok(childs[0] != childs[1] && childs[0] != childs[2]
		&& childs[1] != childs[2],
		"PIDs are different");
	ok(!L4_SameThreads(tids[0], tids[1])
		&& !L4_SameThreads(tids[0], tids[2])
		&& !L4_SameThreads(tids[1], tids[2]),
		"TIDs are different");
	if(!ok(n_replied == 3, "replied to all children")) {
		diag("n_replied=%d", n_replied);
	}

	if(exit_status() != 0) {
		for(int i=0; i < 3; i++) {
			diag("childs[%d]=%d, tids[%d]=%lu:%lu", i, childs[i],
				i, L4_ThreadNo(tids[i]), L4_Version(tids[i]));
		}
	}

	for(int i=0; i < 3; i++) {
		int st = 0, dead = wait(&st);
		if(dead < 0) diag("wait i=%d: dead=%d, st=%d", i, dead, st);
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
	int spid = fork();
	if(spid == 0) {
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
	fail_if(!send_delay(stats_tid, L4_ZeroTime, 0, false),
		"ec %#lx", L4_ErrorCode());
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
	fail_if(!send_delay(stats_tid, L4_TimePeriod(delay_ms * 1000),
		repeat_ct, false), "ec %#lx", L4_ErrorCode());
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


/* "thread" suite */

static void taking_a_nap_fn(void *param) {
	L4_Sleep(A_SHORT_NAP);
	exit_thread(param);
}


/* tests "thread exits first" and "join called first" modes of join_thread(). */
START_TEST(basic_thread_test)
{
	plan_tests(2);

	/* first case. */
	diag("creating thread");
	L4_ThreadId_t tid = xstart_thread(&exit_thread, "death has appeared");
	diag("sleeping");
	L4_Sleep(A_SHORT_NAP);
	L4_Word_t ec = 0;
	diag("joining %lu:%lu", L4_ThreadNo(tid), L4_Version(tid));
	void *ptr = join_thread_long(tid, L4_TimePeriod(5000), &ec);
	if(!ok(ptr != NULL, "join of exited thread")) {
		diag("ec=%#lx", ec);
	}

	/* second case. */
	tid = xstart_thread(&taking_a_nap_fn, "in ur punani");
	ec = 0;
	ptr = join_thread_long(tid, L4_TimePeriod(50000), &ec);
	if(!ok(ptr != NULL, "join of active thread")) {
		diag("ec=%#lx", ec);
	}
}
END_TEST


/* this eats the join_thread() initiated pre-join rendezvous. */
static void eat_prejoin_label_fn(void *param UNUSED)
{
	L4_Clock_t now = L4_SystemClock();
	L4_Time_t after = L4_TimePoint2_NP(now,
		(L4_Clock_t){ .raw = now.raw + 10000 });
	L4_ThreadId_t sender;
	L4_MsgTag_t tag;
	do {
		tag = L4_Wait_Timeout(after, &sender);
	} while(L4_IpcSucceeded(tag) && L4_Label(tag) != PREJOIN_LABEL);

	L4_Word_t ec = L4_ErrorCode();
	if(L4_IpcFailed(tag) && ec != 3) {
		diag("%s: wait failed, ec=%#lx", __func__, ec);
	}

	exit_thread("who goes there?");
}


/* tests whether it's possible to put the thread joining part out of whack
 * with an incomplete IPC flow.
 */
START_TEST(join_thread_test)
{
	plan_tests(1);

	L4_ThreadId_t tid = xstart_thread(&eat_prejoin_label_fn, NULL);
	L4_Word_t ec = 0;
	void *ptr = join_thread_long(tid, TEST_IPC_DELAY, &ec);
	if(!ok(ptr != NULL && ec == 0, "out-of-whack join ok")) {
		diag("join ec=%#lx, ret=%p", ec, ptr);
		xjoin_thread(tid);
	}
}
END_TEST


static void wait_for_child_fn(void *param UNUSED)
{
	int st = 0, dead = wait(&st);
	diag("waited for %d; st=%d", dead, st);
	exit_thread("success!");
}


/* tests subprocess exit when its last thread terminates. */
START_LOOP_TEST(exit_with_thread_test, iter, 0, 1)
{
	plan_tests(1);
	const bool from_thread = CHECK_FLAG(iter, 1);
	diag("from_thread=%s", btos(from_thread));

	L4_ThreadId_t child_tid;
	int child = fork_tid(&child_tid);
	if(child == 0) {
		L4_Sleep(L4_TimePeriod(5000));
		if(from_thread) {
			diag("child thread exiting");
			exit_thread(NULL);
		} else {
			diag("child process exiting");
			exit(0);
		}
		diag("child returned from exit_thread(), aborting");
		abort();
	}

	diag("child_tid=%lu:%lu",
		L4_ThreadNo(child_tid), L4_Version(child_tid));
	L4_Accept(L4_UntypedWordsAcceptor);
	L4_MsgTag_t tag = L4_Receive_Timeout(child_tid,
		L4_TimePeriod(10000));
	L4_Word_t ec = L4_ErrorCode();
	fail_unless(L4_IpcFailed(tag));
	fail_unless(ec == 3 || ec == 5, "ec=%#lx", ec);

	L4_ThreadId_t w_tid = xstart_thread(&wait_for_child_fn, NULL);
	ec = 0;
	void *ret = join_thread_long(w_tid, L4_TimePeriod(15000), &ec);
	if(!ok(ret != NULL && ec == 0, "wait ok")) {
		diag("ec=%#lx", ec);
		kill_thread(w_tid);
	}
}
END_TEST


void access_memory_fn(void *ptr)
{
	diag("accessing %p", ptr);
	uint8_t *p = ptr;
	(*p)++;
	diag("didn't die");
	exit_thread(NULL);
}


/* test that a segmentation fault (illegal access) is signaled to the joiner.
 * this deviates rather hard from POSIX semantics.
 */
START_TEST(segv_test)
{
	plan_tests(3);

	/* test that access to valid memory doesn't fail. */
	void *foo = malloc(128);
	fail_if(foo == NULL);
	memset(foo, 0x7d, 128);
	L4_ThreadId_t tid = xstart_thread(&access_memory_fn, foo);
	L4_Word_t ec = 0;
	void *ret = join_thread_long(tid, L4_TimePeriod(15000), &ec);
	if(!ok(ret == NULL && ec == 0, "valid access joined without fault")) {
		diag("ret=%p, ec=%#lx", ret, ec);
	}

	/* and that an illegal access does fail. */
	uintptr_t ill = (uintptr_t)foo ^ 0x40000000;
	tid = xstart_thread(&access_memory_fn, (void *)ill);
	ec = 0;
	ret = join_thread_long(tid, L4_TimePeriod(15000), &ec);
	if(!ok(ret != NULL || ec == 0, "illegal access joined ok")) {
		diag("ret=%p, ec=%#lx", ret, ec);
	}
	if(!ok1(ret == (void *)ill)) {
		diag("ret=%p", ret);
	}

	free(foo);
}
END_TEST


/* check for process termination on an unhandled segv. also tests access from
 * another thread to confirm that the exit happens because of unhandled segv
 * and not merely because the last thread croaked.
 */
START_LOOP_TEST(uncaught_segv_test, iter, 0, 3)
{
	plan_tests(3);
	const bool handle_segv = !CHECK_FLAG(iter, 1),
		indirect = CHECK_FLAG(iter, 2);
	diag("handle_segv=%s, indirect=%s", btos(handle_segv), btos(indirect));

	void *foo = malloc(128);
	memset(foo, 0x7e, 128);
	uint8_t *ill = (uint8_t *)((uintptr_t)foo ^ 0x40000000);
	int child = fork();
	if(child == 0) {
		if(!handle_segv) {
			int n = forkserv_set_mgr_tid(L4_Pager(), L4_nilthread.raw);
			fail_if(n != 0, "clear mgr_tid failed, n=%d", n);
		}
		diag("doing %sdirect illegal access at %p",
			indirect ? "in" : "", ill);
		if(indirect) {
			L4_ThreadId_t tid = xstart_thread(&access_memory_fn, ill);
			L4_Sleep(A_SHORT_NAP);
			xjoin_thread(tid);
		} else {
			access_memory_fn(ill);
		}
		exit(666);
	} else {
		const bool should_fault = !handle_segv || !indirect;
		int st, dead = wait(&st);
		fail_if(dead != child, "wait returned pid=%d, wanted %d",
			dead, child);
		diag("st=%#lx", (unsigned long)st);
		iff_ok1(should_fault, (st & 15) == 7);

		uintptr_t f_addr = st & ~15,
			trunc_ill = (uintptr_t)ill & ~15;
		if(!imply_ok1(should_fault, f_addr == trunc_ill)) {
			diag("f_addr=%#lx, trunc_ill=%#lx", f_addr, trunc_ill);
		}

		iff_ok1(!should_fault, st == 666);
	}
}
END_TEST


static L4_MsgTag_t send_fault(
	L4_ThreadId_t pager,
	L4_Word_t addr,
	L4_Word_t eip,
	L4_Word8_t access)
{
	L4_LoadBR(0, L4_CompleteAddressSpace.raw);
	L4_LoadMR(0, (L4_MsgTag_t){ .X.u = 2,
		.X.label = ((-2) & 0xfff) << 4 | (access & 0xf) }.raw);
	L4_LoadMR(1, addr);
	L4_LoadMR(2, eip);
	return L4_Call(pager);
}


static void send_fault_at_fn(void *param_ptr)
{
	L4_Word_t addr = (L4_Word_t)param_ptr;
	diag("%s: addr=%#lx", __func__, addr);
	L4_MsgTag_t tag = send_fault(L4_Pager(), addr,
		(L4_Word_t)&send_fault_at_fn, L4_Readable | L4_Writable);
	diag("%s: returned, tag=%#lx, ec=%#lx", __func__, tag.raw,
		L4_ErrorCode());
	exit_thread(NULL);
}


/* test that a segfault is generated when a child process faults to forkserv
 * in the UTCB or KIP range. this is used to detect the microkernel unwisely
 * permitting a client process to unmap its UTCB or KIP range.
 *
 * this test only checks the positive case, i.e. that faults on UTCB and KIP
 * ranges cause segv signaling, because segv_test validates nonfaulting on
 * valid memory and faulting on invalid memory.
 */
START_TEST(segv_on_special_fault)
{
	const L4_Word_t special_addrs[] = {
		(L4_Word_t)L4_GetKernelInterface() + 255,
		(L4_Word_t)L4_MyLocalId().raw + 31,
	};
	plan_tests(2 * NUM_ELEMENTS(special_addrs));

	for(int i=0; i < NUM_ELEMENTS(special_addrs); i++) {
		const L4_Word_t addr = special_addrs[i];
		L4_ThreadId_t tid = xstart_thread(&send_fault_at_fn, (void *)addr);

		L4_Word_t ec = 0;
		void *ret = join_thread_long(tid, TEST_IPC_DELAY, &ec);
		if(!ok(ret != NULL || ec == 0, "faulted at addr=%#lx", addr)) {
			diag("ret=%p, ec=%#lx", ret, ec);
		}
		if(!ok1(ret == (void *)addr)) {
			diag("ret=%p", ret);
		}
	}
}
END_TEST


static void ping_fn(void *param_ptr UNUSED)
{
	L4_ThreadId_t sender;
	L4_MsgTag_t tag = L4_Wait_Timeout(TEST_IPC_DELAY, &sender);
	if(L4_IpcSucceeded(tag)) {
		L4_LoadMR(0, 0);
		L4_Reply(sender);
	}
}


/* check that it's possible to start at least MAX_THREADS - 4 threads, and
 * that the show can go on even when no more can be started.
 */
START_TEST(many_threads_test)
{
	plan_tests(2);
	const int goal = MAX_THREADS - 4;

	int count_ok = 0;
	L4_ThreadId_t threads[MAX_THREADS];
	for(int i=0; i < MAX_THREADS; i++) threads[i] = L4_nilthread;
	for(int i=0; i < MAX_THREADS; i++) {
		threads[i] = start_thread(&ping_fn, NULL);
		if(L4_IsNilThread(threads[i])) break; else count_ok++;
	}
	ok(count_ok < MAX_THREADS, "ran out of slots as expected");
	if(!ok1(count_ok >= goal)) {
		diag("count_ok=%d, goal=%d", count_ok, goal);
	}

	for(int i=0; i < MAX_THREADS && count_ok > 0; i++, count_ok--) {
		L4_LoadMR(0, 0);
		L4_MsgTag_t tag = L4_Call(threads[i]);
		IPC_FAIL(tag);
		xjoin_thread(threads[i]);
	}
}
END_TEST


static void add_thread_tests(TCase *tc)
{
	tcase_add_test(tc, basic_thread_test);
	tcase_add_test(tc, join_thread_test);
	tcase_add_test(tc, exit_with_thread_test);
	tcase_add_test(tc, segv_test);
	tcase_add_test(tc, uncaught_segv_test);
	tcase_add_test(tc, segv_on_special_fault);
	tcase_add_test(tc, many_threads_test);
}


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

	TCase *util_case = tcase_create("util");
	tcase_set_fork(util_case, false);
	tcase_add_test(util_case, basic_delay_test);
	suite_add_tcase(s, util_case);

	{
		TCase *tc = tcase_create("thread");
		add_thread_tests(tc);
		suite_add_tcase(s, tc);
	}

	{
		TCase *tc = tcase_create("threadnf");
		add_thread_tests(tc);
		suite_add_tcase(s, tc);
	}

	{
		TCase *tc = tcase_create("fork");
		tcase_set_fork(tc, false);
		tcase_add_test(tc, basic_fork_and_wait);
		tcase_add_test(tc, copy_on_write);
		tcase_add_test(tc, return_exit_status);
		tcase_add_test(tc, report_child_segfault);
		tcase_add_test(tc, ipc_with_child);
		tcase_add_test(tc, deep_fork);
		tcase_add_test(tc, multi_fork_and_wait);
		tcase_add_test(tc, deeper_fork);
		tcase_add_test(tc, multi_fork_tid);
		suite_add_tcase(s, tc);
	}

	TCase *pagers = tcase_create("pg");
	tcase_add_checked_fixture(pagers, &stats_setup, &stats_teardown);
	tcase_add_test(pagers, stats_delay_test);
	suite_add_tcase(s, pagers);

	return s;
}
