
/* tests of L4.X2 string transfer features. */

#include <stdlib.h>
#include <string.h>
#include <assert.h>
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


#define QUIT_LABEL	0x7151		/* "qQ", implying suicide */
#define ECHO_LABEL	0x6857		/* "hW" */


static L4_ThreadId_t test_tid;


static void string_test_thread(void *param UNUSED)
{
	const int rbuf_len = 2048;
	char *recvbuf = calloc(rbuf_len, 1);
	L4_StringItem_t recv_si = L4_StringItem(rbuf_len, recvbuf);
	char tmp[512];		/* sendbuf */

	bool running = true;
	while(running) {
		L4_ThreadId_t from;
		/* simple acceptor over the entire recvbuf. */
		L4_LoadBR(0, 1);		/* only stringitems */
		L4_LoadBRs(1, 2, recv_si.raw);
		L4_MsgTag_t tag = L4_Wait(&from);

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
					snprintf(tmp, sizeof(tmp), "echo, echo! %s", recvbuf);
					strlcpy(recvbuf, tmp, rbuf_len);
					si = L4_StringItem(strlen(recvbuf) + 1, recvbuf);
					L4_LoadMR(0, (L4_MsgTag_t){ .X.t = 2 }.raw);
					L4_LoadMRs(1, 2, si.raw);
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
				tag = L4_ReplyWait(from, &from);
			}
		}
	}

	free(recvbuf);
}


static void stt_setup(void)
{
	assert(L4_IsNilThread(test_tid));
	test_tid = start_thread(&string_test_thread, NULL);
	fail_unless(!L4_IsNilThread(test_tid));
}


static void stt_teardown(void)
{
	L4_LoadMR(0, (L4_MsgTag_t){ .X.label = QUIT_LABEL }.raw);
	L4_MsgTag_t tag = L4_Send_Timeout(test_tid, TEST_IPC_DELAY);
	fail_unless(L4_IpcSucceeded(tag),
		"QUIT_LABEL failed, ec %#lx", L4_ErrorCode());

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


/* like echo_simple, but flushes mappings from some of the memory where
 * strings are sent and/or received.
 *
 * TODO: check that faults occurred
 */
START_TEST(echo_with_hole)
{
	plan_tests(2);

	const size_t buf_size = 16 * 1024;
	const char *echostr = "what did the pope say to the bear?";

	char *replybuf = calloc(1, buf_size), *sendbuf = calloc(1, buf_size);
	assert(buf_size > 4200);
	char *sendstr = &sendbuf[4100];
	memcpy(sendstr, echostr, strlen(echostr) + 1);

	/* unmap the largish-enough page around both, ensuring a pagefault on both
	 * sides
	 */
	L4_Fpage_t flush[2] = {
		L4_FpageLog2((L4_Word_t)sendstr & ~PAGE_MASK, PAGE_BITS),
		L4_FpageLog2((L4_Word_t)replybuf & ~PAGE_MASK, PAGE_BITS),
	};
	for(int i=0; i < NUM_ELEMENTS(flush); i++) {
		L4_Set_Rights(&flush[i], L4_FullyAccessible);
	}
	L4_FlushFpages(2, flush);

	L4_StringItem_t got_si;
	echo(test_tid, replybuf, buf_size - (sendstr - sendbuf),
		&got_si, sendstr, strlen(echostr));
	replybuf[MIN(int, buf_size - 1, got_si.X.string_length)] = '\0';
	int rlen = strlen(replybuf);
	ok(rlen >= strlen(sendstr), "reply length >= input length");
	ok(streq(&replybuf[MAX(int, 0, rlen - strlen(echostr))], echostr),
		"echo output ends with input");

	free(replybuf);
	free(sendbuf);
}
END_TEST


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
	/* FIXME: move this into a shared IPC stub */
	L4_LoadMR(0, (L4_MsgTag_t){ .X.label = QUIT_LABEL }.raw);
	L4_MsgTag_t tag = L4_Send_Timeout(test_tid, TEST_IPC_DELAY);
	fail_unless(L4_IpcSucceeded(tag),
		"QUIT_LABEL failed, ec %#lx", L4_ErrorCode());

	int status, pid = wait(&status);
	fail_unless(pid > 0);
	fail_unless(status == 0);

	test_tid = L4_nilthread;
}


Suite *string_suite(void)
{
	Suite *s = suite_create("string");

	TCase *basic = tcase_create("basic");
	tcase_add_checked_fixture(basic, &stt_setup, &stt_teardown);
	tcase_add_test(basic, echo_simple);
	tcase_add_test(basic, echo_with_hole);
	suite_add_tcase(s, basic);

	/* inter-space cases, i.e. mapdb interactions and so forth. */
	TCase *space = tcase_create("space");
	tcase_add_checked_fixture(space, &fork_stt_setup, &fork_stt_teardown);
	tcase_add_test(space, echo_simple);
	tcase_add_test(space, echo_with_hole);
	suite_add_tcase(s, space);

	return s;
}
