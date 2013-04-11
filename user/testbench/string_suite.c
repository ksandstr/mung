
/* tests of L4.X2 string transfer features. */

#include <stdlib.h>
#include <string.h>
#include <ccan/compiler/compiler.h>
#include <ccan/str/str.h>

#include <l4/types.h>
#include <l4/space.h>
#include <l4/ipc.h>
#include <l4/thread.h>
#include <l4/message.h>

#include <ukernel/util.h>

#include "defs.h"
#include "test.h"


#define ECHO_LABEL	0x6857		/* "hW" */
#define DELAY_LABEL	0x7a5a		/* "zZ" */
#define PING_LABEL	0x6849		/* "hI" */


static L4_ThreadId_t test_tid, stats_tid, drop_tid;
static struct pager_stats *stats;
static struct drop_param *drop_param;


static const uint32_t seed_bins[4] = {
	0xdeadbeef, 0xf0adcafe, 0xb00b1e5, 0x71849a3f,
};


static bool read_fault(L4_Word_t addr) {
	return CHECK_FLAG(L4_Rights(get_fault(stats, addr)), L4_Readable);
}


static bool write_fault(L4_Word_t addr) {
	return CHECK_FLAG(L4_Rights(get_fault(stats, addr)), L4_Writable);
}


static void diag_faults(struct pager_stats *st)
{
	diag("%d faults, %d read, %d write:",
		st->n_faults, st->n_read, st->n_write);
	for(int i=0; i <= st->log_top; i++) {
		diag("  %#lx:%#lx, %#x", L4_Address(st->log[i]),
			L4_Size(st->log[i]), L4_Rights(st->log[i]));
	}
}


static void send_delay(L4_ThreadId_t tid, L4_Time_t time)
{
	L4_LoadMR(0, (L4_MsgTag_t){ .X.label = DELAY_LABEL, .X.u = 1 }.raw);
	L4_LoadMR(1, time.raw);
	L4_MsgTag_t tag = L4_Call_Timeouts(tid, TEST_IPC_DELAY,
		TEST_IPC_DELAY);
	fail_if(L4_IpcFailed(tag), "ec %#lx", __func__, L4_ErrorCode());
}


static void string_test_thread(void *param UNUSED)
{
	const int rbuf_len = 64 * 1024;
	char *recvbuf = valloc(rbuf_len);
	memset(recvbuf, 0, rbuf_len);
	L4_StringItem_t recv_si = L4_StringItem(rbuf_len, recvbuf);

	bool running = true;
	while(running) {
		L4_ThreadId_t from;
		/* simple acceptor over the entire recvbuf. */
		L4_LoadBR(0, 1);		/* only stringitems */
		L4_LoadBRs(1, 2, recv_si.raw);
		L4_MsgTag_t tag = L4_Wait(&from);

		L4_Time_t delay = L4_ZeroTime;
		while(running) {
			if(L4_IpcFailed(tag)) {
				diag("helper got ipc failure, ec %#lx", L4_ErrorCode());
				break;
			}

			switch(tag.X.label) {
				case QUIT_LABEL: running = false; break;
				case ECHO_LABEL: {
					if(tag.X.t != 2 || tag.X.u != 0) {
						diag("invalid echo message");
						L4_LoadMR(0, 0);
						break;
					}
					L4_StringItem_t si;
					L4_StoreMRs(1, 2, si.raw);
					if(!L4_IsStringItem(&si)) {
						diag("is not a string item");
						L4_LoadMR(0, 0);
						break;
					}
					recvbuf[MIN(int, rbuf_len - 1, si.X.string_length)] = '\0';
					int rec_len = strlen(recvbuf), tmplen = rec_len + 64;
					char *tmp = malloc(tmplen);
					if(tmp == NULL) {
						strlcpy(recvbuf, "malloc failed", rbuf_len);
					} else {
						snprintf(tmp, tmplen, "echo, echo! %s", recvbuf);
						strlcpy(recvbuf, tmp, tmplen);
						free(tmp);
					}
					si = L4_StringItem(strlen(recvbuf) + 1, recvbuf);
					L4_LoadMR(0, (L4_MsgTag_t){ .X.t = 2 }.raw);
					L4_LoadMRs(1, 2, si.raw);
					break;
				}

				case DELAY_LABEL: {
					if(tag.X.u != 1 || tag.X.t > 0) {
						diag("invalid delay message");
						L4_LoadMR(0, 0);
						break;
					}
					L4_Word_t timeword;
					L4_StoreMR(1, &timeword);
					delay.raw = timeword;
					break;
				}

				case PING_LABEL: {
					/* reply with the same regs. */
					L4_Word_t regs[64];
					L4_StoreMRs(0, tag.X.u + tag.X.t + 1, regs);
					L4_LoadMRs(0, tag.X.u + tag.X.t + 1, regs);
					break;
				}

				default:
					diag("unknown label %#lx (tag %#lx)", (L4_Word_t)tag.X.label,
						tag.raw);
					L4_LoadMR(0, 0);
			}

			if(running) {
				/* simple acceptor over the entire recvbuf. */
				L4_LoadBR(0, 1);		/* only stringitems */
				L4_LoadBRs(1, 2, recv_si.raw);
				if(delay.raw == L4_ZeroTime.raw) {
					tag = L4_ReplyWait(from, &from);
				} else {
					tag = L4_Reply(from);
					L4_Sleep(delay);
					delay = L4_ZeroTime;
					if(L4_IpcSucceeded(tag)) {
						tag = L4_Wait(&from);
					}
				}
			}
		}
	}

	free(recvbuf);
}


static void stt_setup(void)
{
	fail_unless(L4_IsNilThread(test_tid));
	test_tid = start_thread(&string_test_thread, NULL);
	fail_unless(!L4_IsNilThread(test_tid));
}


static void stt_teardown(void)
{
	bool quit_ok = send_quit(test_tid);
	fail_unless(quit_ok, "send_quit() failed, ec %#lx", L4_ErrorCode());

	void *value = join_thread(test_tid);
	fail_unless(value == NULL,
		"unexpected return from string test thread: `%s'", (char *)value);
	test_tid = L4_nilthread;
}


/* simple echo. send string, receev bacon. */
static void echo(
	L4_ThreadId_t serv_tid,
	char *replybuf,
	size_t reply_size,
	L4_StringItem_t *got_si,
	const char *echostr,
	size_t echo_len)
{
	L4_StringItem_t rep_si = L4_StringItem(reply_size, replybuf);
	L4_LoadBR(0, 1);
	L4_LoadBRs(1, 2, rep_si.raw);
	if(echo_len == 0) echo_len = strlen(echostr);
	L4_StringItem_t si = L4_StringItem(echo_len + 1, (void *)echostr);
	L4_LoadMR(0, (L4_MsgTag_t){ .X.label = ECHO_LABEL, .X.t = 2 }.raw);
	L4_LoadMRs(1, 2, si.raw);
	L4_MsgTag_t tag = L4_Call_Timeouts(serv_tid, TEST_IPC_DELAY,
		TEST_IPC_DELAY);
	fail_unless(L4_IpcSucceeded(tag), "ipc failed: ec %#lx", L4_ErrorCode());
	fail_unless(tag.X.t == 2, "reply tag is weird (%#lx)", tag.raw);

	if(got_si != NULL) {
		L4_StoreMRs(1, 2, got_si->raw);
		fail_unless(L4_IsStringItem(got_si));
	}
}


/* main test suite */


START_TEST(echo_simple)
{
	plan_tests(2);
	const char *echostr = "does a polar bear crap in the woods?";

	char replybuf[1024];
	L4_StringItem_t got_si;
	echo(test_tid, replybuf, sizeof(replybuf), &got_si, echostr, 0);

	fail_unless(L4_IsStringItem(&got_si));
	replybuf[MIN(int, sizeof(replybuf) - 1, got_si.X.string_length)] = '\0';
	int rlen = strlen(replybuf);
	ok(rlen >= strlen(echostr), "reply length >= input length");
	ok(streq(&replybuf[rlen - strlen(echostr)], echostr),
		"echo output ends with input");
}
END_TEST


/* iter should be 0..15. it'll be used to select a seed value for the string
 * parameter.
 */
START_LOOP_TEST(echo_long, test_iter)
{
	fail_unless(test_iter >= 0 && test_iter < 16);
	uint32_t seed = seed_bins[test_iter & 0x3] ^ seed_bins[test_iter >> 2];

	const size_t test_len = 24 * 1024 + 1;
	char *echostr = malloc(test_len);
	fail_if(echostr == NULL);
	random_string(echostr, test_len, &seed);
	fail_unless(strlen(echostr) == test_len - 1);

	plan_tests(2);

	char *replybuf = malloc(test_len * 2);
	fail_if(replybuf == NULL);
	L4_StringItem_t got_si;
	echo(test_tid, replybuf, test_len * 2, &got_si, echostr, 0);

	fail_unless(L4_IsStringItem(&got_si));
	replybuf[MIN(int, test_len * 2 - 1, got_si.X.string_length)] = '\0';
	int rlen = strlen(replybuf);
	ok(rlen >= strlen(echostr), "reply length >= input length");
	ok(streq(&replybuf[rlen - strlen(echostr)], echostr),
		"echo output ends with input");

	free(echostr);
	free(replybuf);
}
END_TEST


/* iter should be 0..15. it'll be used to select a seed value for the string
 * parameter.
 *
 * TODO: use the stats pager, too
 */
START_LOOP_TEST(echo_long_xferfault, test_iter)
{
	fail_unless(test_iter >= 0 && test_iter < 16);
	uint32_t seed = seed_bins[test_iter & 0x3] ^ seed_bins[test_iter >> 2];

	const size_t test_len = 24 * 1024 + 1;
	char *echostr = valloc(test_len);
	fail_if(echostr == NULL);
	random_string(echostr, test_len, &seed);
	fail_unless(strlen(echostr) == test_len - 1);

	plan_tests(2);

	L4_ThreadId_t old_pager = L4_Pager();
	L4_Set_Pager(drop_tid);

	/* provoke send-side faults also */
	L4_Fpage_t echo_page = L4_Fpage((L4_Word_t)echostr, test_len * 2);
	L4_Set_Rights(&echo_page, L4_FullyAccessible);
	L4_FlushFpage(echo_page);

	char *replybuf = valloc(test_len * 2);
	fail_if(replybuf == NULL);
	L4_StringItem_t got_si;
	diag("calling echo");
	echo(test_tid, replybuf, test_len * 2, &got_si, echostr, test_len - 1);
	L4_Set_Pager(old_pager);
	diag("echo returned");

	fail_unless(L4_IsStringItem(&got_si));
	replybuf[MIN(int, test_len * 2 - 1, got_si.X.string_length)] = '\0';
	int rlen = strlen(replybuf);
	ok(rlen >= strlen(echostr), "reply length >= input length");
	ok(streq(&replybuf[rlen - strlen(echostr)], echostr),
		"echo output ends with input");

	free(echostr);
	free(replybuf);
}
END_TEST


/* like echo_simple, but flushes mappings from some of the memory where
 * strings are sent and/or received.
 */
START_TEST(echo_with_hole)
{
	plan_tests(6);

	const size_t buf_size = 16 * 1024;
	const char *echostr = "what did the pope say to the bear?";

	char *replybuf = valloc(buf_size), *sendbuf = valloc(buf_size);
	memset(replybuf, 0, buf_size);
	memset(sendbuf, 0, buf_size);
	char *sendstr = &sendbuf[19];
	memcpy(sendstr, echostr, strlen(echostr) + 1);

	/* unmap the largish-enough page around both, ensuring a pagefault on both
	 * sides
	 */
	L4_Word_t sndpage = (L4_Word_t)sendstr & ~PAGE_MASK,
		rpypage = (L4_Word_t)replybuf & ~PAGE_MASK;
	L4_Fpage_t flush[2] = {
		L4_FpageLog2(sndpage, PAGE_BITS),
		L4_FpageLog2(rpypage, PAGE_BITS),
	};
	for(int i=0; i < NUM_ELEMENTS(flush); i++) {
		L4_Set_Rights(&flush[i], L4_FullyAccessible);
	}
	L4_FlushFpages(2, flush);
	L4_ThreadId_t old_pager = L4_Pager();
	L4_Set_Pager(stats_tid);

	L4_StringItem_t got_si;
	fail_unless(stats->n_faults == 0,
		"saw %d faults before echo", stats->n_faults);
	echo(test_tid, replybuf, buf_size - (sendstr - sendbuf),
		&got_si, sendstr, strlen(echostr));
	L4_Set_Pager(old_pager);

	/* echo result */
	replybuf[MIN(int, buf_size - 1, got_si.X.string_length)] = '\0';
	int rlen = strlen(replybuf);
	ok(rlen >= strlen(sendstr), "reply length >= input length");
	ok(streq(&replybuf[MAX(int, 0, rlen - strlen(echostr))], echostr),
		"echo output ends with input");

	/* fault entrails */
	/* TODO: ignore rx faults, use "==" instead */
	ok1(stats->n_faults >= 2);
	ok1(stats->n_write >= 1);
	ok1(read_fault(sndpage));
	ok1(write_fault(rpypage));

	free(replybuf);
	free(sendbuf);

	if(exit_status() > 0) {
		diag_faults(stats);
		diag("replybuf %p, sendstr %p", replybuf, sendstr);
		diag("rpypage %#lx, sndpage %#lx", rpypage, sndpage);
	}
}
END_TEST


static void *bump_and_align(void *ptr, size_t bump, size_t align)
{
	assert(((align - 1) & align) == 0);		/* has 0 or 1 bits set */
	uintptr_t p = (uintptr_t)ptr + bump;
	return (void *)((p + align - 1) & ~(align - 1)) - bump;
}


/* like echo_with_hole, but makes two pages' worth of holes and sends/receives
 * at the border.
 */
START_TEST(echo_with_long_hole)
{
	plan_tests(8);

	const size_t buf_size = 16 * 1024;
	const char *echostr = "what did the pope say to the bear?";
	const int echo_len = strlen(echostr);

	diag("test tid %lu:%lu, self %lu:%lu",
		L4_ThreadNo(test_tid), L4_Version(test_tid),
		L4_ThreadNo(L4_Myself()), L4_Version(L4_Myself()));

	char *replybuf = calloc(1, buf_size), *sendbuf = calloc(1, buf_size);
	// diag("replybuf %p, sendbuf %p", replybuf, sendbuf);
	char *replyptr = bump_and_align(replybuf + PAGE_SIZE,
			echo_len / 2 - 1, PAGE_SIZE),
		*sendptr = bump_and_align(sendbuf + PAGE_SIZE,
			echo_len / 2, PAGE_SIZE);
	// diag("replyptr %p, sendptr %p", replyptr, sendptr);
	fail_unless(((uintptr_t)replyptr & PAGE_MASK) != 0);
	fail_unless(((uintptr_t)sendptr & PAGE_MASK) != 0);
	memcpy(sendptr, echostr, echo_len + 1);

	/* unmap two primitive pages' worth, starting from the first page. */
	L4_Word_t sndpage = (L4_Word_t)sendptr & ~PAGE_MASK,
		rpypage = (L4_Word_t)replyptr & ~PAGE_MASK;
	L4_Fpage_t flush[] = {
		L4_FpageLog2(sndpage, PAGE_BITS),
		L4_FpageLog2(sndpage + PAGE_SIZE, PAGE_BITS),
		L4_FpageLog2(rpypage, PAGE_BITS),
		L4_FpageLog2(rpypage + PAGE_SIZE, PAGE_BITS),
	};
	L4_ThreadId_t old_pager = L4_Pager();
	L4_Set_Pager(stats_tid);
	for(int i=0; i < NUM_ELEMENTS(flush); i++) {
		L4_Set_Rights(&flush[i], L4_FullyAccessible);
#if 1
		diag("flushing %#lx:%#lx", L4_Address(flush[i]), L4_Size(flush[i]));
#endif
	}
	stats->n_faults = 0;
	stats->n_write = 0;
	L4_FlushFpages(NUM_ELEMENTS(flush), flush);

	L4_StringItem_t got_si;
	fail_unless(stats->n_faults == 0, "had %d faults before test",
		stats->n_faults);
	echo(test_tid, replyptr, buf_size - (sendptr - sendbuf),
		&got_si, sendptr, strlen(echostr));
	L4_Set_Pager(old_pager);

	/* echo result */
	replyptr[MIN(int, buf_size - 1, got_si.X.string_length)] = '\0';
	int rlen = strlen(replyptr);
	ok(rlen >= strlen(sendptr), "reply length >= input length");
	ok(streq(&replyptr[MAX(int, 0, rlen - strlen(echostr))], echostr),
		"echo output ends with input");

	/* fault entrails */
	diag("%d faults, %d write", stats->n_faults, stats->n_write);
	ok1(stats->n_faults >= 4);	/* only mildly useful */
	ok1(stats->n_write >= 2);
	diag_faults(stats);
	ok1(read_fault(sndpage));
	ok1(read_fault(sndpage + PAGE_SIZE));
	ok1(write_fault(rpypage));
	ok1(write_fault(rpypage + PAGE_SIZE));

	free(replybuf);
	free(sendbuf);
}
END_TEST


/* TODO: use this from non-xferto tests also
 * TODO: add an option to flush the receive buffer independently
 */
static L4_Word_t faulting_echo(
	L4_ThreadId_t serv_tid,
	int test_iter,
	bool do_unmap)
{
	uint32_t seed = seed_bins[test_iter & 0x3] ^ seed_bins[test_iter >> 2];

	const size_t test_len = 11 * 1024 + 1;
	char *echostr = malloc(test_len);
	fail_if(echostr == NULL);
	random_string(echostr, test_len, &seed);
	fail_unless(strlen(echostr) == test_len - 1);

	char *replybuf = malloc(test_len * 2);
	fail_if(replybuf == NULL);

	if(do_unmap) {
		L4_Fpage_t unmap_page = L4_Fpage((uintptr_t)echostr, test_len * 4);
		L4_Set_Rights(&unmap_page, L4_FullyAccessible);
		diag("buffer %p:%#x", echostr, (unsigned)test_len);
		diag("flushing %#lx:%#lx", L4_Address(unmap_page),
			L4_Size(unmap_page));
		L4_FlushFpage(unmap_page);
	}

	L4_StringItem_t got_si;
	L4_StringItem_t rep_si = L4_StringItem(test_len * 2, replybuf);
	L4_LoadBR(0, 1);
	L4_LoadBRs(1, 2, rep_si.raw);
	int echo_len = strlen(echostr);
	L4_StringItem_t si = L4_StringItem(echo_len + 1, (void *)echostr);
	L4_LoadMR(0, (L4_MsgTag_t){ .X.label = ECHO_LABEL, .X.t = 2 }.raw);
	L4_LoadMRs(1, 2, si.raw);
	L4_MsgTag_t tag = L4_Call_Timeouts(test_tid, TEST_IPC_DELAY,
		TEST_IPC_DELAY);
	L4_Word_t ec = L4_ErrorCode();
	fail_unless(L4_IpcFailed(tag) || tag.X.t == 2,
		"reply tag is weird (%#lx)", tag.raw);
	if(L4_IpcSucceeded(tag)) {
		L4_StoreMRs(1, 2, got_si.raw);
		fail_unless(L4_IsStringItem(&got_si));

		replybuf[MIN(int, test_len * 2 - 1, got_si.X.string_length)] = '\0';
		int rlen = strlen(replybuf);
		fail_unless(rlen >= strlen(echostr), "reply length >= input length");
		fail_unless(streq(&replybuf[rlen - strlen(echostr)], echostr),
			"echo output ends with input");
	}

	free(echostr);
	free(replybuf);

	return ec;
}


/* xferto testcase: transfer timeouts (and lack thereof). */

START_TEST(no_xfer_timeout)
{
	plan_tests(2);

	L4_Set_XferTimeouts(L4_Timeouts(L4_Never, L4_Never));
	L4_Word_t ec = faulting_echo(test_tid, 0, false);
	ok(ec == 0, "no-fault n/n case");

	L4_Set_XferTimeouts(L4_Timeouts(L4_Never, L4_Never));
	ec = faulting_echo(test_tid, 1, true);
	ok(ec == 0, "faulting n/n case");
}
END_TEST


START_TEST(immediate_xfer_timeout)
{
	plan_tests(2);

	todo_start("not implemented");

	L4_Word_t tos = L4_Timeouts(L4_ZeroTime, L4_ZeroTime);
	L4_Set_XferTimeouts(tos);
	L4_Word_t ec = faulting_echo(test_tid, 0, false);
	ok(ec == 0, "no-fault z/z case");

	L4_Set_XferTimeouts(tos);
	ec = faulting_echo(test_tid, 0, true);
	/* expecting 5 or 6 in send phase, indicating xfer timeout in invoker or
	 * partner's address space (which are the same thing)
	 */
	int code = (ec >> 1) & 0x7;
	diag("ec %#lx, code %d", ec, code);
	ok((code == 5 || code == 6) && (ec & 1) == 0,
		"timeout in no-fault z/z send phase");

	todo_end();
}
END_TEST


/* meta tests
 *
 * TODO: test of the "ping" function
 */

START_TEST(delay_test)
{
	plan_tests(5);

	/* synchronize. */
	L4_LoadMR(0, (L4_MsgTag_t){ .X.label = PING_LABEL, .X.u = 1 }.raw);
	L4_LoadMR(1, ECHO_LABEL ^ DELAY_LABEL);
	L4_MsgTag_t tag = L4_Call(test_tid);
	fail_unless(L4_IpcSucceeded(tag),
		"sync ping failed, ec %#lx", L4_ErrorCode());

	/* without delay, reply should be immediate. */
	L4_Clock_t before = L4_SystemClock();
	L4_LoadMR(0, (L4_MsgTag_t){ .X.label = PING_LABEL, .X.u = 1 }.raw);
	L4_LoadMR(1, ECHO_LABEL ^ DELAY_LABEL);
	tag = L4_Call_Timeouts(test_tid, L4_ZeroTime, L4_Never);
	L4_Clock_t after = L4_SystemClock();
	if(L4_IpcFailed(tag)) diag("error code %#lx", L4_ErrorCode());
	ok(L4_IpcSucceeded(tag), "immediate call succeeded");
	uint64_t diff_us = after.raw - before.raw;
	diag("ipc took %lu µs", (unsigned long)diff_us);
	ok(diff_us < 1000 * 10, "immediate call was immediate");

	/* with delay, there should be a send-side timeout between 16 and 19 ms,
	 * inclusive, rounding down.
	 */
	send_delay(test_tid, L4_TimePeriod(20 * 1000));
	before = L4_SystemClock();
	L4_LoadMR(0, (L4_MsgTag_t){ .X.label = PING_LABEL, .X.u = 1 }.raw);
	L4_LoadMR(1, ECHO_LABEL ^ DELAY_LABEL);
	tag = L4_Call_Timeouts(test_tid, L4_TimePeriod(1000 * 17), L4_Never);
	after = L4_SystemClock();
	if(L4_IpcSucceeded(tag) || L4_ErrorCode() != 0x2) {
		diag("unexpected ec %#lx", L4_ErrorCode());
	}
	ok(L4_IpcFailed(tag) && L4_ErrorCode() == 0x2,
		"delayed call had send-side timeout");
	diff_us = after.raw - before.raw;
	diag("ipc took %lu µs", (unsigned long)diff_us);
	int diff_ms = diff_us / 1000;
	ok1(diff_ms > 16 && diff_ms < 20);

	/* after delay, ping should complete properly. */
	L4_LoadMR(0, (L4_MsgTag_t){ .X.label = PING_LABEL, .X.u = 1 }.raw);
	L4_LoadMR(1, ECHO_LABEL ^ DELAY_LABEL);
	tag = L4_Call_Timeouts(test_tid, TEST_IPC_DELAY, TEST_IPC_DELAY);
	if(L4_IpcFailed(tag)) diag("after ec %#lx", L4_ErrorCode());
	ok(L4_IpcSucceeded(tag), "after ipc ok");
}
END_TEST


START_TEST(faulting_echo_test)
{
	plan_tests(2);

	L4_Set_XferTimeouts(L4_Timeouts(L4_Never, L4_Never));

	/* part 1: faulting_echo() should produce no faults on second go without
	 * the unmap option.
	 */
	L4_Word_t ec = faulting_echo(test_tid, 0, false);
	fail_unless(ec == 0, "warmup failed: ec %#lx", ec);

	stats->n_faults = 0; stats->n_write = 0;
	L4_ThreadId_t old_pager = L4_Pager();
	L4_Set_Pager(stats_tid);
	ec = faulting_echo(test_tid, 1, false);
	L4_Set_Pager(old_pager);
	fail_unless(ec == 0, "no-fault call failed: ec %#lx", ec);
	ok1(stats->n_faults == 0);

	/* part 2: subsequently, faulting_echo() with do_unmap should produce at
	 * least one page fault.
	 */
	stats->n_faults = 0; stats->n_write = 0;
	L4_Set_Pager(stats_tid);
	ec = faulting_echo(test_tid, 2, true);
	L4_Set_Pager(old_pager);
	fail_unless(ec == 0, "fault call failed: ec %#lx", ec);
	ok(stats->n_faults > 0, "n_faults=%d with do_unmap", stats->n_faults);
	diag_faults(stats);
}
END_TEST


/* start string_test_thread in a forked space. */
static void fork_stt_setup(void)
{
	assert(L4_IsNilThread(test_tid));

	L4_ThreadId_t parent_tid = L4_Myself();
	int child = fork();
	if(child == 0) {
		L4_LoadMR(0, (L4_MsgTag_t){ .X.u = 1 }.raw);
		L4_LoadMR(1, L4_Myself().raw);
		L4_MsgTag_t tag = L4_Send_Timeout(parent_tid, TEST_IPC_DELAY);
		if(L4_IpcFailed(tag)) exit(L4_ErrorCode());
		else {
			string_test_thread(NULL);
			exit(0);
		}
	}
	fail_if(child < 0, "fork() failed: child = %d", child);

	L4_Word_t got_ctid;
	do {
		L4_MsgTag_t tag = L4_Wait_Timeout(TEST_IPC_DELAY, &test_tid);
		fail_if(L4_IpcFailed(tag));
		L4_StoreMR(1, &got_ctid);
	} while(test_tid.raw != got_ctid);
	fail_unless(!L4_IsNilThread(test_tid));
}


static void fork_stt_teardown(void)
{
	bool quit_ok = send_quit(test_tid);
	fail_unless(quit_ok, "send_quit() failed, ec %#lx", L4_ErrorCode());

	int status, pid = wait(&status);
	fail_unless(pid > 0);
	fail_unless(status == 0);

	test_tid = L4_nilthread;
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


/* fixture for the page-dropping pager thread. set to keep at most two
 * previous maps around.
 */
static void drop_setup(void)
{
	drop_param = malloc(sizeof(*drop_param));
	drop_param->keep = 2;
	fail_unless(drop_param != NULL);
	drop_tid = start_drop_pager(drop_param);
}


static void drop_teardown(void)
{
	L4_Word_t ec = stop_drop_pager(drop_tid);
	fail_if(ec != 0, "stop_drop_pager() failed, ec %#lx", ec);
}


Suite *string_suite(void)
{
	Suite *s = suite_create("string");

	TCase *meta = tcase_create("meta");
	tcase_add_checked_fixture(meta, &stt_setup, &stt_teardown);
	tcase_add_checked_fixture(meta, &stats_setup, &stats_teardown);
	tcase_add_test(meta, delay_test);
	tcase_add_test(meta, faulting_echo_test);
	suite_add_tcase(s, meta);

	TCase *basic = tcase_create("basic");
	tcase_add_checked_fixture(basic, &stt_setup, &stt_teardown);
	tcase_add_checked_fixture(basic, &stats_setup, &stats_teardown);
	tcase_add_checked_fixture(basic, &drop_setup, &drop_teardown);
	tcase_add_test(basic, echo_simple);
	tcase_add_loop_test(basic, echo_long, 0, 15);
	tcase_add_loop_test(basic, echo_long_xferfault, 0, 15);
	tcase_add_test(basic, echo_with_hole);
	tcase_add_test(basic, echo_with_long_hole);
	suite_add_tcase(s, basic);

	/* inter-space cases, i.e. mapdb interactions and so forth. */
	TCase *space = tcase_create("space");
	tcase_add_checked_fixture(space, &fork_stt_setup, &fork_stt_teardown);
	tcase_add_checked_fixture(space, &stats_setup, &stats_teardown);
	tcase_add_checked_fixture(space, &drop_setup, &drop_teardown);
	tcase_add_test(space, echo_simple);
	tcase_add_loop_test(space, echo_long, 0, 15);
	tcase_add_loop_test(space, echo_long_xferfault, 0, 15);
	tcase_add_test(space, echo_with_hole);
	tcase_add_test(space, echo_with_long_hole);
	suite_add_tcase(s, space);

	/* transfer timeout tests */
	TCase *xferto = tcase_create("xferto");
	tcase_add_checked_fixture(xferto, &stt_setup, &stt_teardown);
	tcase_add_test(xferto, no_xfer_timeout);
	tcase_add_test(xferto, immediate_xfer_timeout);
	suite_add_tcase(s, xferto);

	return s;
}
