
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <errno.h>
#include <threads.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>

#include <ccan/str/str.h>
#include <ccan/crc32c/crc32c.h>
#include <ccan/talloc/talloc.h>

#include <l4/types.h>
#include <l4/ipc.h>
#include <l4/thread.h>
#include <l4/kip.h>

#include <ukernel/util.h>

#include "defs.h"
#include "test.h"
#include "forkserv-defs.h"
#include "threadmgr-defs.h"


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


/* tcase str: <string.h> exports */

static const char test_copypasta[] __attribute__((aligned(16))) =
	"What the fuck did you just fucking say about me, you little bitch? "
	"I’ll have you know I graduated top of my class in the Navy Seals, and "
	"I’ve been involved in numerous secret raids on Al-Quaeda, and I have "
	"over 300 confirmed kills. I am trained in gorilla warfare and I’m the "
	"top sniper in the entire US armed forces. You are nothing to me but "
	"just another target. I will wipe you the fuck out with precision the "
	"likes of which has never been seen before on this Earth, mark my "
	"fucking words.";


static size_t ref_strlen(const char *s)
{
	size_t n = 0;
	while(s[n] != '\0') n++;
	return n;
}


START_TEST(strlen_basic)
{
	const int ref_len = ref_strlen(test_copypasta);
	diag("ref_len=%d", ref_len);
	plan_tests(4);

	const size_t arena_size = PAGE_SIZE * 3;
	char *arena = aligned_alloc(PAGE_SIZE, arena_size);
	diag("arena=%p", arena);

	/* align(0..3) x step(0..3) */
	for(int align = 0; align <= 3; align++) {
		memset(arena, 0, arena_size);
		char *buf = &arena[align];
		memcpy(buf, test_copypasta, ref_len + 1);
		bool steps_ok = true;
		for(int step = 0; step <= 3; step++) {
			int test_len = strlen(&buf[step]);
			if(test_len != ref_len - step) {
				diag("align=%d, step=%d: test_len=%d", align, step, test_len);
				steps_ok = false;
			}
		}
		ok(steps_ok, "steps_ok for align=%d", align);
	}

	free(arena);
}
END_TEST


static char *ref_strchr(const char *s, int c)
{
	size_t n = 0;
	while(s[n] != '\0' && s[n] != c) n++;
	return s[n] == '\0' ? NULL : (char *)&s[n];
}


START_LOOP_TEST(strchr_basic, iter, 0, 51)
{
	const char *alphabet = "abcdefghijklmnopqrstuvwxyz";
	size_t ref_len = ref_strlen(test_copypasta);
	bool upcase = CHECK_FLAG(iter, 1);
	char c = alphabet[iter >> 1];
	if(upcase) c = toupper(c);
	diag("upcase=%s, c=`%c'", btos(upcase), c);
	plan_tests(4);

	const size_t arena_size = PAGE_SIZE * 3;
	char *arena = aligned_alloc(PAGE_SIZE, arena_size);
	diag("arena=%p", arena);

	/* align(0..3) x step(0..3) */
	for(int align = 0; align <= 3; align++) {
		memset(arena, 0, arena_size);
		char *buf = &arena[align];
		memcpy(buf, test_copypasta, ref_len + 1);
		assert(buf[ref_len] == '\0');
		buf[ref_len + 1] = c;		/* sneaky, sneaky */
		bool steps_ok = true;
		for(int step = 0; step <= 3; step++) {
			char *test_out = strchr(&buf[step], c),
				*ref_out = ref_strchr(&buf[step], c);
			if(test_out != ref_out) {
				diag("align=%d, step=%d: test_out=%p, ref_out=%p",
					align, step, test_out, ref_out);
				steps_ok = false;
			}
		}
		ok(steps_ok, "steps_ok for align=%d", align);
	}

	free(arena);
}
END_TEST


/* yeah, 128 iterations. so what? at least this one doesn't sleep. */
START_LOOP_TEST(strscpy_basic, iter, 0, 127)
{
	const bool page_step = CHECK_FLAG(iter, 1);
	iter >>= 1;
	const int src_offset = (iter & 7), dst_offset = ((iter >> 3) & 7);
	diag("page_step=%s, src_offset=%d, dst_offset=%d",
		btos(page_step), src_offset, dst_offset);
	const char *teststr = &test_copypasta[src_offset];
	assert(((uintptr_t)teststr & 7) == src_offset);
	const int test_size = strlen(teststr);
	diag("test_size=%d", test_size);
	if(page_step) {
		plan_skip_all("page-stepping not implemented");
		goto end;
	}
	plan_tests(8);

	void *tal = talloc_new(NULL);
	char *tst = talloc_array(tal, char, test_size * 3) + dst_offset,
		*cmp = talloc_array(tal, char, test_size * 3);
	assert(((uintptr_t)tst & 7) == dst_offset);

	memset(tst, 'a', test_size * 2);
	memset(cmp, 'a', test_size * 2);
	/* when count is larger than test size, should equal memcpy(). */
	memcpy(cmp, teststr, test_size + 1);
	int n = strscpy(tst, teststr, test_size * 2);
	if(!ok1(n == test_size)) diag("n=%d", n);
	ok1(streq(tst, cmp));

	memset(tst, 'a', test_size * 2);
	memset(cmp, 'a', test_size * 2);
	/* same for count being equal to test size plus terminator. */
	memcpy(cmp, teststr, test_size + 1);
	n = strscpy(tst, teststr, test_size + 1);
	if(!ok1(n == test_size)) diag("n=%d", n);
	ok1(streq(tst, cmp));

	memset(tst, 'a', test_size * 2);
	memset(cmp, 'a', test_size * 2);
	/* when it's smaller, should return -E2BIG and output up to test size less
	 * one for terminator.
	 */
	memcpy(cmp, teststr, test_size - 7); cmp[test_size - 7] = '\0';
	n = strscpy(tst, teststr, test_size - 6);
	ok1(n == -E2BIG);
	ok1(tst[test_size - 7] == '\0');
	ok1(strlen(tst) == test_size - 7);
	ok1(streq(tst, cmp));

	talloc_free(tal);
end: ;;
}
END_TEST


/* cases where destination and source parameters to strscpy() overlap. this
 * has less fancy environment setup than strscpy_basic due to implementation
 * knowledge: either the start and end of the output will be correct, or it'll
 * be wrong for sure.
 */
START_LOOP_TEST(strscpy_overlap, iter, 0, 31)
{
	const char *test_str = test_copypasta;
	const unsigned test_size = strlen(test_str);
	const int step = iter;
	diag("test_size=%u, step=%d", test_size, step);
	plan_tests(4);

	void *tal = talloc_new(NULL);
	char *dest = talloc_array(tal, char, test_size * 3);
	memset(dest, 'a', test_size * 3); dest[test_size * 3 - 1] = '\0';
	memcpy(dest, test_str, test_size + 1);
	assert(dest[test_size] == '\0');
	const char *src = dest + step;
	dest += 16;
	diag("src=%p, dest=%p", src, dest);
	assert(strlen(src) <= test_size);
	int n = strscpy(dest, src, test_size + 1);

	/* when src < dest, should produce a repeating sequence of characters
	 * until size - 1 and terminate.
	 */
	int cycle = labs(dest - src);
	assert(test_size > cycle * 7);
	assert(src >= dest || cycle > 0);
	diag("n=%d, cycle=%d", n, cycle);
	imply_ok1(src < dest, n == -E2BIG);
	imply_ok(src < dest,
		memcmp(dest, test_str + step, cycle) == 0
		&& memcmp(dest + cycle, test_str + step, cycle) == 0
		&& memcmp(dest + cycle * 7, test_str + step, cycle) == 0
		&& dest[test_size] == '\0',
		"output is a repeating prefix when src < dest");

	/* forward cases, where src >= dest, should function normally. */
	imply_ok1(src >= dest, n >= 0 && n < test_size + 1);
	imply_ok1(src >= dest, streq(dest, test_str + step));

	talloc_free(tal);
}
END_TEST


/* null cases of strscpy(). these should happen regardless of alignment and
 * page crossing, so these can stay outside strscpy_basic.
 */
START_TEST(strscpy_null)
{
	plan_tests(9);

	/* max=0 with a valid dst, valid src. (base case.) */
	char dst[16];
	memset(dst, 'a', sizeof dst); dst[sizeof dst - 1] = '\0';
	int n = strscpy(dst, "hello", 0);
	ok1(n == -E2BIG);
	ok1(dst[0] == 'a');

	/* same but valid dst, NULL src. */
	n = strscpy(dst, NULL, 0);
	pass("didn't crash on src=NULL");
	ok1(n == -E2BIG);
	ok1(dst[0] == 'a');

	/* same but NULL dst, valid src. */
	n = strscpy(NULL, "hello", 0);
	pass("didn't crash on dst=NULL");
	ok1(n == -E2BIG);

	/* and finally both NULL. */
	n = strscpy(NULL, NULL, 0);
	pass("didn't crash on both NULL");
	ok1(n == -E2BIG);
}
END_TEST


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

/* variables:
 *   - [sleeping_child] child should sleep to let parent get to wait() first.
 *     if false, parent sleeps to let child get to exit() first.
 */
START_LOOP_TEST(basic_fork_and_wait, iter, 0, 1)
{
	bool sleeping_child = CHECK_FLAG(iter, 1);
	diag("sleeping_child=%s", btos(sleeping_child));
	plan_tests(4);

	int parent_pid = getpid();
	int spid = fork();
	if(spid == 0) {
		/* child side */
		diag("child running, sees pid=%d (parent=%d)", getpid(), parent_pid);
		if(sleeping_child) L4_Sleep(A_SHORT_NAP);
		exit(abs(parent_pid - getpid()));
		assert(false);
	}
	diag("spid=%d", spid);
	ok(spid > 0, "fork succeeded");
	if(!sleeping_child) L4_Sleep(A_SHORT_NAP);
	int status = 0, dead = wait(&status);
	ok(dead > 0 && dead == spid, "child exited");
	skip_start(!ok1(WIFEXITED(status)), 1, "didn't exit normally") {
		ok(WEXITSTATUS(status) == abs(getpid() - spid),
			"exit status was delivered");
	} skip_end;
}
END_TEST


static void wait_for_some_child_fn(void *param_ptr)
{
	int *result_p = param_ptr;
	int st, n = wait(&st);
	diag("%s: n=%d, st=%d", __func__, n, st);
	if(result_p != NULL) {
		*result_p = n > 0 ? n : -errno;
		exit_thread(result_p);
	}
}


/* arrange for a child process to call wait(2) while there are no children,
 * store the wait(2) result, then exit.
 *
 * variables:
 *   - [many] whether there's five waiting threads, or just one.
 *   - [do_join] whether the test threads will be joined and reported back
 *     before subprocess exit.
 */
START_LOOP_TEST(wait_without_child, iter, 0, 3)
{
	int n_waits = CHECK_FLAG(iter, 1) ? 5 : 1;
	bool do_join = CHECK_FLAG(iter, 2);
	diag("n_waits=%d, do_join=%s", n_waits, btos(do_join));
	plan_tests(4);

	L4_ThreadId_t parent_tid = do_join ? L4_Myself() : L4_nilthread,
		child_tid;
	int waiter_pid = fork_tid(&child_tid);
	if(waiter_pid == 0) {
		diag("starting waiter threads");
		int *ctx = talloc_new(NULL);
		L4_ThreadId_t tids[n_waits];
		for(int i=0; i < n_waits; i++) {
			tids[i] = xstart_thread(&wait_for_some_child_fn,
				talloc(ctx, int));
		}
		diag("waiter threads started");
		L4_Sleep(A_SHORT_NAP);
		if(do_join) {
			/* collect results & report back. */
			L4_Word_t results[n_waits];
			for(int i=0; i < n_waits; i++) {
				L4_Word_t ec = 0;
				int *p = join_thread_long(tids[i], A_SHORT_NAP, &ec);
				results[i] = p != NULL ? *p : -ENOSYS;
			}
			L4_MsgTag_t tag = (L4_MsgTag_t){ .X.u = n_waits };
			L4_LoadMR(0, tag.raw);
			L4_LoadMRs(1, n_waits, results);
			tag = L4_Send_Timeout(parent_tid, TEST_IPC_DELAY);
			if(L4_IpcFailed(tag)) {
				diag("child send failed, ec=%#lx", L4_ErrorCode());
			}
		} else {
			/* don't. we don't join those threads; they're expected to get
			 * killed at process exit. this tests for a particular assert()
			 * which used to exist in forkserv.c:handle_exit().
			 */
		}
		diag("waiter child exiting");
		exit(0);
	}

	diag("waiter_pid=%d", waiter_pid);

	int num_echild = 0;
	skip_start(!do_join, 2, "!do_join") {
		L4_Word_t results[n_waits];
		L4_MsgTag_t tag = L4_Receive_Timeout(child_tid,
			L4_TimePeriod(200 * 1000));
		if(!ok(L4_IpcSucceeded(tag), "child reported back")) {
			diag("ec=%#lx", L4_ErrorCode());
		}
		int n_got = MIN(int, n_waits, L4_UntypedWords(tag));
		L4_StoreMRs(1, n_got, results);
		ok1(n_got > 0);
		for(int i=0; i < n_waits; i++) {
			if(results[i] == (L4_Word_t)-ECHILD) num_echild++;
			// else diag("non-ECHILD results[%d]=%#lx", i, results[i]);
		}
	} skip_end;
	if(!imply_ok1(do_join, num_echild == n_waits)) {
		diag("num_echild=%d", num_echild);
	}

	int st, pid = wait(&st);
	if(!ok1(pid == waiter_pid)) {
		diag("st=%d, pid=%d", st, pid);
	}
}
END_TEST


/* arrange for a child process to call wait() from more threads than it has
 * children, then exit the children it does have, then exit itself.
 *
 * variables:
 *   - [many] whether there's three actual children, or just one.
 *   - [do_join] whether the test threads will be joined and reported back
 *     before subprocess exit.
 *
 * this test shares a lot of code with wait_without_child. it could be merged
 * in, with a flag to set whether it'll actually start children or not. for
 * now, however...
 */
START_LOOP_TEST(wait_more_children, iter, 0, 3)
{
	int n_children = CHECK_FLAG(iter, 1) ? 3 : 1;
	bool do_join = CHECK_FLAG(iter, 2);
	diag("n_children=%d, do_join=%s", n_children, btos(do_join));
	plan_tests(4);

	const int n_waits = n_children + 2;
	L4_ThreadId_t parent_tid = do_join ? L4_Myself() : L4_nilthread,
		child_tid;
	int waiter_pid = fork_tid(&child_tid);
	if(waiter_pid == 0) {
		int children[n_children];
		diag("starting children");
		for(int i=0; i < n_children; i++) {
			children[i] = fork();
			if(children[i] == 0) {
				L4_Sleep(A_SHORT_NAP);
				exit(0);
			} else {
				diag("children[%d]=%d", i, children[i]);
			}
		}
		diag("starting waiters");
		void *ctx = talloc_new(NULL);
		L4_ThreadId_t tids[n_waits];
		for(int i=0; i < n_waits; i++) {
			tids[i] = xstart_thread(&wait_for_some_child_fn,
				talloc(ctx, int));
		}
		diag("waiters started");
		L4_Sleep(A_SHORT_NAP);
		L4_Sleep(A_SHORT_NAP);
		if(do_join) {
			/* collect results & report back. */
			L4_Word_t results[n_waits];
			for(int i=0; i < n_waits; i++) {
				L4_Word_t ec = 0;
				int *p = join_thread_long(tids[i], A_SHORT_NAP, &ec);
				results[i] = p != NULL ? *p : -ENOSYS;
			}
			L4_MsgTag_t tag = (L4_MsgTag_t){ .X.u = n_waits };
			L4_LoadMR(0, tag.raw);
			L4_LoadMRs(1, n_waits, results);
			tag = L4_Send_Timeout(parent_tid, TEST_IPC_DELAY);
			if(L4_IpcFailed(tag)) {
				diag("child send failed, ec=%#lx", L4_ErrorCode());
			}
		} else {
			/* see comment in same position of wait_without_child */
		}
		diag("waiter child exiting");
		exit(0);
	}

	diag("waiter_pid=%d", waiter_pid);

	int num_echild = 0;
	skip_start(!do_join, 2, "!do_join") {
		L4_Word_t results[n_waits];
		L4_MsgTag_t tag = L4_Receive_Timeout(child_tid,
			L4_TimePeriod(200 * 1000));
		if(!ok(L4_IpcSucceeded(tag), "child reported back")) {
			diag("ec=%#lx", L4_ErrorCode());
		}
		int n_got = MIN(int, n_waits, L4_UntypedWords(tag));
		L4_StoreMRs(1, n_got, results);
		ok1(n_got > 0);
		for(int i=0; i < n_waits; i++) {
			if(results[i] == (L4_Word_t)-ECHILD) num_echild++;
			// else diag("non-ECHILD results[%d]=%#lx", i, results[i]);
		}
	} skip_end;
	if(!imply_ok1(do_join, num_echild == 2)) {
		diag("num_echild=%d", num_echild);
	}

	int st, pid = wait(&st);
	if(!ok1(pid == waiter_pid)) {
		diag("st=%d, pid=%d", st, pid);
	}
}
END_TEST


/* fork a bunch of child processes and then waitpid() on them in sequence.
 *
 * variables:
 *   - [reverse] wait in reverse order
 *   - [nohang] sleep until all have exited, then use WNOHANG
 */
START_LOOP_TEST(multi_child_waitpid, iter, 0, 3)
{
	const int num_children = 16;
	const bool reverse = !!(iter & 1), nohang = !!(iter & 2);
	diag("num_children=%d, reverse=%s, nohang=%s",
		num_children, btos(reverse), btos(nohang));
	plan_tests(num_children);

	pid_t pids[num_children];
	for(int i=0; i < num_children; i++) {
		pid_t c = fork();
		if(c == 0) {
			usleep(5 * 1000);
			exit(i);
		}
		pids[i] = c;
	}
	if(nohang) usleep(15 * 1000);
	for(int i=0; i < num_children; i++) {
		subtest_start("wait for i=%d", i);
		plan_tests(2);
		int st, offset = reverse ? num_children - 1 - i : i;
		pid_t c = pids[offset], n = waitpid(c, &st, nohang ? WNOHANG : 0);
		if(!ok(n == c, "waited for right PID")) {
			diag("c=%d, n=%d, errno=%d", c, n, errno);
		}
		ok1(WIFEXITED(st) && WEXITSTATUS(st) == offset);
		subtest_end();
	}

	/* clean up stragglers */
	int st;
	pid_t c;
	while(c = wait(&st), c > 0 || errno != ECHILD) {
		if(c >= 0) diag("cleaned up child=%d", c);
		else {
			diag("wait(2) failed, errno=%d", errno);
			break;
		}
	}
}
END_TEST


/* a call to waitpid(2) should return -1 (ECHILD) when there are no children.
 * (copypasta'd from sneks user/test/process/wait.c, then altered to cover
 * WNOHANG.)
 */
START_LOOP_TEST(empty_wait, iter, 0, 1)
{
	const bool nohang = !!(iter & 1);
	diag("nohang=%s", btos(nohang));
	plan_tests(2);

	int st, pid = waitpid(-1, &st, nohang ? WNOHANG : 0);
	ok1(pid == -1);
	if(!ok1(errno == ECHILD)) diag("errno=%d", errno);
}
END_TEST


/* a call to waitpid(... WHOHANG) should return 0 when there are children but
 * they've not entered a waitable state.
 *
 * (similar to the test in sneks user/test/process/wait.c .)
 */
START_LOOP_TEST(busy_wait, iter, 0, 1)
{
	const bool wild = !!(iter & 1);
	diag("wild=%s", btos(wild));
	plan_tests(2);

	pid_t c = fork();
	if(c == 0) {
		L4_Sleep(L4_TimePeriod(10 * 1000));
		exit(0);
	}

	int st;
	pid_t dead = waitpid(wild ? -1 : c, &st, WNOHANG);
	if(!ok1(dead == 0)) {
		diag("dead=%d, errno=%d", dead, errno);
	}
	L4_Sleep(L4_TimePeriod(20 * 1000));
	dead = waitpid(wild ? -1 : c, &st, WNOHANG);
	if(!ok1(dead == c)) diag("dead=%d, errno=%d", dead, errno);

	wait(&st);	/* clean up */
}
END_TEST


/* waitpid(..., 0) should return correctly whether the child exits before the
 * call or after. (active/passive sleeping test.)
 */
START_LOOP_TEST(sleeping_wait, iter, 0, 3)
{
	const bool active_wait = !!(iter & 1), wild = !!(iter & 2);
	diag("active_wait=%s, wild=%s", btos(active_wait), btos(wild));
	plan_tests(2);

	pid_t c = fork();
	if(c == 0) {
		usleep(10 * 1000);
		exit(0);
	}
	if(active_wait) usleep(20 * 1000);

	int st;
	pid_t n = waitpid(wild ? -1 : c, &st, 0);
	if(!ok1(n == c)) diag("n=%d, errno=%d", n, errno);
	ok1(WIFEXITED(st) && WEXITSTATUS(st) == 0);

	wait(&st);	/* clean up on failure */
}
END_TEST


START_TEST(copy_on_write)
{
	plan_tests(3);
	const int buffer_size = 256;

	const char *teststr = "the quick brown fox jumps over the lazy dog";
	uint8_t *buffer = malloc(buffer_size);
	for(int i=0; i < 256; i++) buffer[i] = i;
	strscpy((void *)buffer, teststr, buffer_size);

	int spid = fork();
	if(spid != 0) {
		fail_if(spid < 0, "fork failed");
		int status = 0, dead = wait(&status);
		fail_if(dead < 0, "wait failed");
		fail_if(dead != spid, "expected %d from wait, got %d", spid, dead);
		ok1(WIFEXITED(status));
		ok(WEXITSTATUS(status) == (int)buffer[0] + 1 + (int)(buffer[255] + 1) % 256,
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
 * hits both the waiting and non-waiting cases.
 */
START_LOOP_TEST(return_exit_status, iter, 0, 1)
{
	plan_tests(3);
	const bool do_wait = CHECK_FLAG(iter, 1);
	diag("do_wait=%s", btos(do_wait));

	pid_t child = fork();
	if(child == 0) {
		int rc = getpid() ^ 0x7f;
		if(do_wait) wait_and_exit(rc, 2); else exit(rc);
		fail_if(true, "child shouldn't get here");
	}
	fail_if(child == -1, "fork failed");

	int status = 0;
	pid_t dead = wait(&status);
	ok1(dead == child);
	ok1(WIFEXITED(status));
	ok1(WEXITSTATUS(status) == (child ^ 0x7f));
	diag("dead=%d, status=%#x (%d)", dead, (unsigned)status, status);
}
END_TEST


/* start an outer child, and an inner child. have the outer child exit without
 * waiting. see if shit breaks.
 */
START_TEST(reparent_orphans)
{
	plan_tests(1);

	int child = fork();
	if(child == 0) {
		int second = fork();
		if(second == 0) {
			L4_Sleep(TEST_IPC_DELAY);
			diag("second exiting");
			exit(0);
		}
		diag("second=%d (child exiting)", second);
		exit(0);
	}
	diag("child=%d", child);
	L4_Sleep(A_SHORT_NAP);

	int st, dead = wait(&st);
	fail_unless(dead == child, "dead=%d, child=%d", dead, child);

	pass("didn't crash");

	/* let the grandchild exit. */
	L4_Sleep(TEST_IPC_DELAY);
}
END_TEST


/* provoke two kinds of segfault: first is vanilla from the child's main
 * thread, the second by causing the process manager thread to segfault. both
 * are done as writes on the kernel interface page.
 */
START_LOOP_TEST(report_child_segfault, iter, 0, 1)
{
	L4_KernelInterfacePage_t *kip = L4_GetKernelInterface();

	plan_tests(3);
	const bool mgr_fault = CHECK_FLAG(iter, 1);
	diag("mgr_fault=%s", btos(mgr_fault));

	int child = fork();
	if(child == 0) {
		if(!mgr_fault) {
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
	fail_if(dead != child,
		"different dead child: expected=%d, got=%d", child, dead);
	diag("st=%#x", st);
	skip_start(!ok1(WIFSIGNALED(st)), 2, "didn't exit by signal") {
		ok(WTERMSIG(st) == 11, "signal was SEGV");
		ok((WSEGVADDR(st) & ~PAGE_MASK) == ((L4_Word_t)kip & ~PAGE_MASK),
			"segfault was at correct address");
	} skip_end;
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
		fail_if(dead != child);
		if(!ok1(WEXITSTATUS(status) == 0)) {
			diag("WIFEXITED=%s, WEXITSTATUS=%d",
				btos(WIFEXITED(status)), WEXITSTATUS(status));
		}
	} else {
		child = fork();
		if(child != 0) {
			int status = 0, dead = wait(&status);
			exit((dead == child && WIFEXITED(status)
				&& WEXITSTATUS(status) == exit_magic) ? 0 : 1);
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


START_TEST(many_fork_sequence)
{
	/* keep this low enough to not time out on a slower core2, plz */
	const size_t n_forks = 256;

	plan_tests(1);

	for(int i=0; i < n_forks; i++) {
		int child = fork();
		if(child == 0) {
			exit(i);
		} else {
			int st, dead = wait(&st);
			fail_if(dead != child, "expected dead=%d, got %d", child, dead);
			fail_if(!WIFEXITED(st) || WEXITSTATUS(st) != i,
				"expected st=%d, got %d", i, st);
		}
	}
	ok(true, "didn't die");
}
END_TEST


/* test that TSDs are retained across fork(). */
START_TEST(retain_tsd_on_fork)
{
	plan_tests(2);

	const char *teststr = "i am just a humble test string";

	int key;
	tsd_key_create(&key, &free);
	tsd_set(key, strdup(teststr));
	ok(streq(tsd_get(key), teststr), "match before fork");
	int child = fork();
	if(child == 0) {
		const char *gotstr = tsd_get(key);
		bool fail = gotstr == NULL || !streq(gotstr, teststr);
		if(fail) diag("gotstr=`%s'", gotstr);
		exit(fail ? 1 : 0);
	} else {
		int st, dead = wait(&st);
		fail_if(dead != child);
		ok(st == 0, "match after fork");
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
	 * enough to delay_ms (TODO: adjust this using the clock precision).
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
	plan_tests(2);
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
	L4_MsgTag_t tag = L4_Receive_Timeout(child_tid, L4_TimePeriod(10000));
	L4_Word_t ec = L4_ErrorCode();
	if(!ok1(L4_IpcFailed(tag) && (ec == 3 || ec == 5))) {
		diag("tag=%#lx, ec=%#lx", tag.raw, ec);
	}

	L4_ThreadId_t w_tid = xstart_thread(&wait_for_child_fn, NULL);
	ec = 0;
	void *ret = join_thread_long(w_tid, L4_TimePeriod(15000), &ec);
	if(!ok(ret != NULL && ec == 0, "waiter joined")) {
		diag("ec=%#lx", ec);
		kill_thread(w_tid);
	}
}
END_TEST


struct exiter_param {
	bool daemon_flag;
	int exit_value;
};


static void daemon_exiter_fn(void *param_ptr)
{
	const struct exiter_param *p = param_ptr;

	int n = thrd_set_daemon_NP(thrd_current(), p->daemon_flag);
	if(n < 0) {
		diag("%s: setdaemon failed, errno=%d", __func__, errno);
		exit_thread("this string is not an ok value");
	}

	L4_Accept(L4_UntypedWordsAcceptor);
	L4_ThreadId_t sender;
	L4_MsgTag_t tag = L4_Wait(&sender);
	if(L4_IpcFailed(tag)) {
		diag("%s: wait failed, ec=%#lx", __func__, L4_ErrorCode());
		exit_thread("this string is not an ok value");
	}
	sender = L4_GlobalIdOf(sender);
	L4_LoadMR(0, 0);
	L4_Reply(sender);

	/* wait until the main thread disappears. */
	tag = L4_Receive_Timeout(sender, L4_TimePeriod(
		L4_PeriodUs_NP(A_SHORT_NAP) * 3));
	L4_Word_t ec = L4_ErrorCode();
	if(L4_IpcSucceeded(tag) || ec != 3) {
		diag("ipc %s, ec=%#lx",
			L4_IpcSucceeded(tag) ? "succeeded" : "failed", ec);
		exit_thread("this string is not an ok value");
	}

	exit(p->exit_value);
}


static L4_ThreadId_t launch_exiter(bool daemon_flag, int ev)
{
	struct exiter_param *p = talloc(NULL, struct exiter_param);
	p->daemon_flag = daemon_flag;
	p->exit_value = ev;
	L4_ThreadId_t oth = xstart_thread(&daemon_exiter_fn, p);
	L4_Accept(L4_UntypedWordsAcceptor);
	L4_LoadMR(0, 0);
	L4_MsgTag_t tag = L4_Call_Timeouts(oth, TEST_IPC_DELAY, TEST_IPC_DELAY);
	if(L4_IpcFailed(tag)) {
		diag("child's sync call failed, ec=%#lx", L4_ErrorCode());
		kill_thread(oth);
		return L4_nilthread;
	} else {
		return oth;
	}
}


START_LOOP_TEST(daemon_allows_exit, iter, 0, 1)
{
	const bool daemon_flag = CHECK_FLAG(iter, 1);
	diag("daemon_flag=%s", btos(daemon_flag));
	plan_tests(3);

	int child = fork();
	if(child == 0) {
		launch_exiter(daemon_flag, 2);
		L4_Sleep(A_SHORT_NAP);
		exit_thread(NULL);
	}

	int st, pid = wait(&st);
	fail_if(pid != child, "pid=%d, errno=%d", pid, errno);

	diag("st=%d", st);
	skip_start(!ok1(WIFEXITED(st)), 2, "didn't exit normally") {
		imply_ok1(!daemon_flag, WEXITSTATUS(st) == 2);
		imply_ok1(daemon_flag, WEXITSTATUS(st) == 0);
	} skip_end;
}
END_TEST


START_LOOP_TEST(daemon_set_triggers_exit, iter, 0, 3)
{
	const bool other_thread = CHECK_FLAG(iter, 1),
		daemonize_self = CHECK_FLAG(iter, 2);
	diag("other_thread=%s, daemonize_self=%s",
		btos(other_thread), btos(daemonize_self));
	if(!other_thread && daemonize_self) {
		plan_skip_all("redundant combination");
		goto end;
	}
	plan_tests(3);

	int child = fork();
	if(child == 0) {
		if(other_thread) {
			launch_exiter(true, 2);
			if(!daemonize_self) exit_thread(NULL);
			else {
				int n = thrd_set_daemon_NP(thrd_current(), true);
				if(n < 0) {
					printf("main child's setdaemon failed, errno=%d", errno);
				}
			}
			exit(666);
		} else {
			int n = thrd_set_daemon_NP(thrd_current(), true);
			if(n < 0) exit(10000 + errno); else exit(2);
		}
	}

	int st, pid = wait(&st);
	fail_if(pid != child, "pid=%d, st=%d", pid, st);

	diag("st=%d", st);
	skip_start(!ok1(WIFEXITED(st)), 2, "didn't exit normally") {
		ok(WEXITSTATUS(st) == 0 || WEXITSTATUS(st) == 2,
			"exit WEXITSTATUS(st)atus is valid");
		ok(WEXITSTATUS(st) == 0, "child didn't exit(3)");
	} skip_end;

end:
	;;
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
	uintptr_t ill = (uintptr_t)foo | 0xc0000000;
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
	uint8_t *ill = (uint8_t *)((uintptr_t)foo | 0xc0000000);
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
		iff_ok1(should_fault, WIFSIGNALED(st) && WTERMSIG(st) == 11);

		uintptr_t f_addr = WSEGVADDR(st) & ~PAGE_MASK,
			trunc_ill = (uintptr_t)ill & ~PAGE_MASK;
		if(!imply_ok1(should_fault, f_addr == trunc_ill)) {
			diag("f_addr=%#lx, trunc_ill=%#lx", f_addr, trunc_ill);
		}

		iff_ok1(!should_fault, WIFEXITED(st) && WEXITSTATUS(st) == 666);
	}
}
END_TEST


static L4_MsgTag_t send_fault(
	L4_ThreadId_t pager,
	L4_Word_t addr, L4_Word_t eip, int access)
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


/* check that the manager thread will log preempt faults as it should. as the
 * microkernel can't be relied on to produce those fault messages (that test
 * is in sched_suite.c which runs later), we'll send some manually.
 */
START_TEST(preempt_fault_logging)
{
	plan_tests(7);

	L4_ThreadId_t child = xstart_thread(&taking_a_nap_fn, NULL),
		mgr_tid = get_mgr_tid();
	diag("child=%lu:%lu, mgr=%lu:%lu", L4_ThreadNo(child), L4_Version(child),
		L4_ThreadNo(mgr_tid), L4_Version(mgr_tid));
	L4_ThreadSwitch(child);

	L4_Word_t hi, lo;
	bool ret, was_exn;
	int64_t msg_diff = 0;
	int n = __tmgr_get_preempt_record(mgr_tid, &ret, child.raw,
		&hi, &lo, &msg_diff, &was_exn);
	fail_if(n != 0, "n=%d", n);
	ok(!ret, "no faults before experiment");

	L4_Clock_t funny = { .raw = 0xbadcafe }, sent_at = L4_SystemClock();
	L4_MsgTag_t tag = { .X.u = 2, .X.label = 0xffd0 };
	L4_Set_Propagation(&tag);
	L4_Set_VirtualSender(child);
	L4_LoadMR(0, tag.raw);
	L4_LoadMR(1, funny.raw >> 32);
	L4_LoadMR(2, funny.raw & 0xffffffff);
	tag = L4_Send_Timeout(mgr_tid, TEST_IPC_DELAY);
	ok(L4_IpcSucceeded(tag), "send to mgr didn't fail");

	ret = false;
	msg_diff = 0;
	n = __tmgr_get_preempt_record(mgr_tid, &ret, child.raw,
		&hi, &lo, &msg_diff, &was_exn);
	fail_if(n != 0, "n=%d", n);
	ok(ret, "fault was recorded");
	ok(hi == funny.raw >> 32 && lo == (funny.raw & 0xffffffff),
		"returned clock value was correct");
	if(!ok1((int64_t)sent_at.raw - msg_diff == funny.raw)) {
		diag("sent_at=%llu, msg_diff=%lld, funny=%llu",
			sent_at.raw, msg_diff, funny.raw);
	}
	ok(!was_exn, "didn't indicate an exception");

	ret = true;
	n = __tmgr_get_preempt_record(mgr_tid, &ret, child.raw,
		&hi, &lo, &msg_diff, &was_exn);
	fail_if(n != 0, "n=%d", n);
	ok(!ret, "no further faults were recorded");

	xjoin_thread(child);
}
END_TEST


static void add_thread_tests(TCase *tc)
{
	tcase_add_test(tc, basic_thread_test);
	tcase_add_test(tc, join_thread_test);
	tcase_add_test(tc, exit_with_thread_test);
	tcase_add_test(tc, daemon_allows_exit);
	tcase_add_test(tc, daemon_set_triggers_exit);
	tcase_add_test(tc, segv_test);
	tcase_add_test(tc, uncaught_segv_test);
	tcase_add_test(tc, segv_on_special_fault);
	tcase_add_test(tc, many_threads_test);
	tcase_add_test(tc, preempt_fault_logging);
}


/* tcase "mutex" */

START_TEST(init_plain_mutex)
{
	plan_tests(1);

	mtx_t *mtx = malloc(sizeof(mtx_t));
	fail_if(mtx == NULL);
	int n = mtx_init(mtx, mtx_plain);
	ok(n == thrd_success, "mtx_init returned success");

	mtx_destroy(mtx);
	free((void *)mtx);
}
END_TEST


START_TEST(trylock_plain_mutex)
{
	plan_tests(3);

	mtx_t *mtx = malloc(sizeof(mtx_t));
	fail_if(mtx == NULL);
	int n = mtx_init(mtx, mtx_plain);
	fail_if(n != thrd_success);

	n = mtx_trylock(mtx);
	ok(n == thrd_success, "mtx_trylock returned success");
	n = mtx_trylock(mtx);
	ok(n == thrd_busy, "mtx_trylock returned busy");
	mtx_destroy(mtx);
	pass("mtx_destroy didn't abort");

	free((void *)mtx);
}
END_TEST


START_TEST(lock_plain_mutex)
{
	plan_tests(2);

	mtx_t *mtx = malloc(sizeof(mtx_t));
	fail_if(mtx == NULL);
	int n = mtx_init(mtx, mtx_plain);
	fail_if(n != thrd_success);

	n = mtx_lock(mtx);
	if(!ok(n == thrd_success, "mtx_lock returned success")) {
		diag("n=%d", n);
	}
	n = mtx_unlock(mtx);
	if(!ok(n == thrd_success, "mtx_unlock returned success")) {
		diag("n=%d", n);
	}

	mtx_destroy(mtx);
	free((void *)mtx);
}
END_TEST


static void lock_and_hold_fn(void *param_ptr)
{
	mtx_t *mtx = param_ptr;
	int n = mtx_lock(mtx);
	fail_unless(n == thrd_success);

	L4_Accept(L4_UntypedWordsAcceptor);
	L4_ThreadId_t sender;
	L4_MsgTag_t tag = L4_Wait(&sender);
	if(L4_IpcFailed(tag)) {
		diag("%s: ipc failed, ec=%#lx", __func__, L4_ErrorCode());
	} else if(L4_Label(tag) != QUIT_LABEL) {
		diag("%s: weird tag=%#lx", __func__, tag.raw);
	}

	n = mtx_unlock(mtx);
	int *status = malloc(sizeof(int));
	*status = n;
	exit_thread(status);
}


/* variables:
 *   - [do_trylock] whether the main thread locks the mutex using
 *     mtx_trylock(), or mtx_lock()
 */
START_LOOP_TEST(conflict_plain_mutex, iter, 0, 1)
{
	bool do_trylock = CHECK_FLAG(iter, 1);
	diag("do_trylock=%s", btos(do_trylock));
	plan_tests(7);

	mtx_t *mtx = malloc(sizeof(mtx_t));
	mtx_init(mtx, mtx_plain);
	int n;
	if(do_trylock) n = mtx_trylock(mtx); else n = mtx_lock(mtx);
	if(!ok(n == thrd_success, "own lock succeeds")) {
		diag("n=%d", (int)n);
	}

	L4_ThreadId_t oth = xstart_thread(&lock_and_hold_fn, (void *)mtx);
	bool quit_ok = send_quit(oth);
	ok(!quit_ok, "other thread blocks");

	n = mtx_unlock(mtx);
	if(!ok(n == thrd_success, "own unlock succeeds")) {
		diag("n=%d", (int)n);
	}
	L4_ThreadSwitch(oth);
	n = mtx_trylock(mtx);
	ok(n == thrd_busy, "other thread owns lock");
	send_quit(oth);
	L4_ThreadSwitch(oth);
	n = mtx_trylock(mtx);
	ok(n == thrd_success, "trylock succeeded after quit");

	n = mtx_unlock(mtx);
	if(!ok(n == thrd_success, "own unlock succeeds (after trylock)")) {
		diag("n=%d", (int)n);
	}

	int *retp = xjoin_thread(oth);
	ok(retp != NULL && *retp == thrd_success,
		"other thread unlock succeeded");
	free(retp);

	mtx_destroy(mtx);
	free((void *)mtx);
}
END_TEST


/* tcase "subtest" */

START_TEST(basic_fork_subtest)
{
	plan_tests(2);

	pid_t sub = fork_subtest_start("hello subtest!") {
		plan_tests(1);
		ok(true, "happy");
	} fork_subtest_end;

	ok(true, "something else");
	fork_subtest_ok1(sub);
}
END_TEST


START_TEST(failing_fork_subtest)
{
	plan_tests(2);

	pid_t sub = fork_subtest_start("goodbye subtest") {
		plan_tests(1);
		ok(false, "sad!");
	} fork_subtest_end;

	ok(true, "whatever man");
	int st = fork_subtest_join(sub);
	ok(WIFEXITED(st) && WEXITSTATUS(st) == 1, "%s exited with status=1",
		fetch_subtest_msg(sub, "unknown subtest"));
}
END_TEST


/* tcase "env" */

/* note: this'll examine at most DROP_PAGER_LOG_SIZE pages, which is typically
 * less than 100. so it's inappropriate for readings over more than low tens
 * of faults.
 *
 * TODO: move this to an adjoining position with the stats pager, and export
 * it for the benefit of various other tests.
 */
static size_t count_nontext_faults(struct pager_stats *stats, int max_faults)
{
	extern const char _start, _end;
	assert(max_faults >= 0);
	size_t num = 0;
	max_faults = MIN(int, max_faults, DROP_PAGER_LOG_SIZE);
	for(int i = 0, l = MIN(int, max_faults, stats->n_faults); i < l; i++) {
		int ix = stats->log_top - i;
		if(ix < 0) ix += sizeof(stats->log) / sizeof(stats->log[0]);
		assert(ix >= 0);
		L4_Fpage_t p = stats->log[ix];
		if(!BETWEEN((L4_Word_t)&_start, (L4_Word_t)&_end, L4_Address(p))) {
			num++;
		}
	}
	return num;
}


/* check that Forkserv::discontiguate() preserves existing data in the same
 * process, and unmaps the memory region it's being applied to.
 *
 * variables:
 *   - length of region (2 bits)
 *   - size of grain (2 bits)
 */
START_LOOP_TEST(discontiguate_basic, iter, 0, 15)
{
	static const int region_shift[4] = { 14, 17, 21, 23 },
		grain_vals[4] = { 0, 12, 16, 20 };
	const size_t region_size = 1u << region_shift[iter & 3];
	const int grain = grain_vals[(iter >> 2) & 3];

	diag("region_size=%#lx, grain=%d", (L4_Word_t)region_size, grain);
	plan_tests(3);

	void *regptr = aligned_alloc(region_size, region_size);
	fail_unless(regptr != NULL);
	L4_Fpage_t region = L4_Fpage((L4_Word_t)regptr, region_size);
	fail_unless(L4_Size(region) == region_size);
	/* fill it with stuff. */
	memset(regptr, '\0', region_size);
	uint32_t rand_seed = 0xb00bface;
	random_string(regptr, region_size, &rand_seed);

	/* measurement #1: that there are no faults from accessing the memory
	 * region once it's been written with memset() and random_string().
	 * compute refcrc while we're here. (unmap property, precondition.)
	 */
	/* fault crc32() in so it doesn't interfere with readings. */
	L4_Word_t dummy = crc32c(0, "what what", 10);
	diag("dummy=%#lx", dummy);	/* must print this out */
	L4_ThreadId_t old_pager = L4_Pager();
	int old_faults = stats->n_faults;
	L4_Set_Pager(stats_tid);
	asm volatile ("" ::: "memory");
	L4_Word_t refcrc = crc32c(0, regptr, region_size);
	L4_Set_Pager(old_pager);
	asm volatile ("" ::: "memory");
	int n_faults = count_nontext_faults(stats, stats->n_faults - old_faults);
	diag("refcrc=%#lx", refcrc);
	if(!ok1(n_faults == 0)) {
		diag("n_faults=%d", n_faults);
	}

	int n = forkserv_discontiguate(L4_Pager(), region, grain);
	fail_if(n != 0, "n=%d", n);

	/* measurement #2: that there are as many faults from accessing that
	 * region after discontiguate as there are minimal pages. (unmap property,
	 * proper.)
	 */
	old_faults = stats->n_faults;
	L4_Set_Pager(stats_tid);
	asm volatile ("" ::: "memory");
	L4_Word_t aftercrc = crc32c(0, regptr, region_size);
	L4_Set_Pager(old_pager);
	asm volatile ("" ::: "memory");
	n_faults = stats->n_faults - old_faults;
	if(!ok1(n_faults == region_size / PAGE_SIZE)) {
		diag("n_faults=%d", n_faults);
	}

	/* measurement #3: that the CRCs match. (preservation.) */
	if(!ok1(refcrc == aftercrc)) {
		diag("refcrc=%#lx, aftercrc=%#lx", refcrc, aftercrc);
	}

	free(regptr);
}
END_TEST


/* check that Forkserv::discontiguate() doesn't impact data in the other side
 * of a fork().
 *
 * variables:
 *   - length of region (32k, 2M)
 *   - size of grain (0, 14)
 *   - invoking role (parent / child)
 *
 * TODO: choose whether child computes its own crc first, or after the parent.
 */
START_LOOP_TEST(discontiguate_fork, iter, 0, 7)
{
	const size_t region_size = 1ul << (!CHECK_FLAG(iter, 1) ? 15 : 21);
	const int grain = !CHECK_FLAG(iter, 2) ? 0 : 14;
	const bool child_calls = CHECK_FLAG(iter, 4);

	diag("region_size=%#lx, grain=%d, child_calls=%s",
		(L4_Word_t)region_size, grain, btos(child_calls));
	plan_tests(3);

	void *regptr = aligned_alloc(region_size, region_size);
	fail_unless(regptr != NULL);
	L4_Fpage_t region = L4_Fpage((L4_Word_t)regptr, region_size);
	fail_unless(L4_Size(region) == region_size);
	/* fill it with whatever. */
	uint32_t rand_seed = 0xdadab00b; /* solid diamond w/ a cardboard nipple */
	random_string(regptr, region_size, &rand_seed);
	L4_Word_t refcrc = crc32c(0, regptr, region_size);
	diag("refcrc=%#lx", refcrc);

	L4_ThreadId_t parent_tid = L4_Myself(), child_tid = L4_nilthread;
	int child = fork_tid(&child_tid);
	if(child == 0) {
		if(child_calls) {
			int n = forkserv_discontiguate(L4_Pager(), region, grain);
			fail_if(n != 0, "n=%d (in child)", n);
		} else {
			L4_Sleep(A_SHORT_NAP);
		}
		L4_Word_t crc = crc32c(0, regptr, region_size);
		L4_LoadMR(0, (L4_MsgTag_t){ .X.u = 1 }.raw);
		L4_LoadMR(1, crc);
		L4_MsgTag_t tag = L4_Call_Timeouts(parent_tid,
			TEST_IPC_DELAY, TEST_IPC_DELAY);
		exit(L4_IpcSucceeded(tag) ? 0 : 1);
	}

	if(!child_calls) {
		int n = forkserv_discontiguate(L4_Pager(), region, grain);
		fail_if(n != 0, "n=%d", n);
	} else {
		L4_Sleep(A_SHORT_NAP);
	}
	L4_Word_t aftercrc = crc32c(0, regptr, region_size);
	L4_MsgTag_t tag = L4_Receive_Timeout(child_tid, TEST_IPC_DELAY);
	L4_Word_t ec = L4_ErrorCode(), child_crc = 0;
	L4_StoreMR(1, &child_crc);
	if(!ok(L4_IpcSucceeded(tag) && L4_UntypedWords(tag) == 1,
		"got child msg"))
	{
		diag("ec=%#lx", ec);
	}
	if(!ok1(child_crc == refcrc)) diag("child_crc=%#lx", child_crc);
	if(!ok1(aftercrc == refcrc)) diag("aftercrc=%#lx", aftercrc);
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


static Suite *self_suite(void)
{
	Suite *s = suite_create("self");

	/* tests on lib/string.c.
	 *
	 * generally the "_basic" ones test short strings and buffers on various
	 * alignments and combinations thereof, and "_page" test the same but with
	 * a page-crossing in the middle.
	 */
	{
		TCase *tc = tcase_create("str");
		tcase_add_checked_fixture(tc, &stats_setup, &stats_teardown);
		tcase_add_test(tc, strlen_basic);
		tcase_add_test(tc, strchr_basic);
		tcase_add_test(tc, strscpy_basic);
		tcase_add_test(tc, strscpy_null);
		tcase_add_test(tc, strscpy_overlap);
		suite_add_tcase(s, tc);
	}

	{
		TCase *tc = tcase_create("util");
		tcase_set_fork(tc, false);
		tcase_add_test(tc, basic_delay_test);
		suite_add_tcase(s, tc);
	}

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

	/* TODO: this should be split into one for forking (e.g. copy-on-write
	 * semantics), and another for the nontrivial cases of waitpid(2).
	 */
	{
		TCase *tc = tcase_create("fork");
		tcase_set_fork(tc, false);
		tcase_add_test(tc, basic_fork_and_wait);
		tcase_add_test(tc, wait_without_child);
		tcase_add_test(tc, wait_more_children);
		tcase_add_test(tc, copy_on_write);
		tcase_add_test(tc, return_exit_status);
		tcase_add_test(tc, reparent_orphans);
		tcase_add_test(tc, report_child_segfault);
		tcase_add_test(tc, ipc_with_child);
		tcase_add_test(tc, deep_fork);
		tcase_add_test(tc, multi_fork_and_wait);
		tcase_add_test(tc, deeper_fork);
		tcase_add_test(tc, multi_fork_tid);
		tcase_add_test(tc, many_fork_sequence);
		tcase_add_test(tc, multi_child_waitpid);
		tcase_add_test(tc, empty_wait);
		tcase_add_test(tc, busy_wait);
		tcase_add_test(tc, sleeping_wait);
		tcase_add_test(tc, retain_tsd_on_fork);
		suite_add_tcase(s, tc);
	}

	/* tests about the C11-like mutexes in <threads.h>. */
	{
		TCase *tc = tcase_create("mutex");
		tcase_add_test(tc, init_plain_mutex);
		tcase_add_test(tc, trylock_plain_mutex);
		tcase_add_test(tc, lock_plain_mutex);
		tcase_add_test(tc, conflict_plain_mutex);
		suite_add_tcase(s, tc);
	}

	/* tests concerning the testing environment, i.e. anything not having to
	 * do with non-microkernel functions (bogomips delay loops), threading, or
	 * POSIXy fork()ing; but also not implemented within testbench (such as
	 * pg, below).
	 */
	{
		TCase *tc = tcase_create("env");
		tcase_add_checked_fixture(tc, &stats_setup, &stats_teardown);
		tcase_add_test(tc, discontiguate_basic);
		tcase_add_test(tc, discontiguate_fork);
		suite_add_tcase(s, tc);
	}

	{
		TCase *tc = tcase_create("pg");
		tcase_add_checked_fixture(tc, &stats_setup, &stats_teardown);
		tcase_add_test(tc, stats_delay_test);
		suite_add_tcase(s, tc);
	}

	/* tests about the forked subtest syntax. */
	{
		TCase *tc = tcase_create("subtest");
		tcase_add_test(tc, basic_fork_subtest);
		tcase_add_test(tc, failing_fork_subtest);
		suite_add_tcase(s, tc);
	}

	return s;
}


static const struct suite_spec s = { &self_suite, 1000 };
AUTODATA(testsuites, &s);
