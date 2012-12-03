
/* tests of L4.X2 string transfer features. */

#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <ccan/compiler/compiler.h>
#include <ccan/str/str.h>

#include <l4/types.h>
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
	char *recvbuf = malloc(rbuf_len);
	/* simple acceptor over the entire recvbuf. */
	L4_LoadBR(0, 1);		/* only stringitems */
	L4_StringItem_t recv_si = L4_StringItem(rbuf_len, recvbuf);
	L4_LoadBRs(1, 2, recv_si.raw);

	bool running = true;
	while(running) {
		L4_ThreadId_t from;
		L4_MsgTag_t tag = L4_Wait(&from);
		if(L4_IpcFailed(tag)) continue;

		while(running) {
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
					char tmp[512];
					snprintf(tmp, sizeof(tmp), "echo, echo! %s", recvbuf);
					strlcpy(recvbuf, tmp, rbuf_len);
					si = L4_StringItem(strlen(recvbuf) + 1, recvbuf);
					L4_LoadMR(0, (L4_MsgTag_t){ .X.t = 2 }.raw);
					L4_LoadMRs(1, 2, si.raw);
					break;
				}

				default:
					diag("unknown label %#lx", (L4_Word_t)tag.X.label);
					L4_LoadMR(0, 0);
			}

			if(running) tag = L4_ReplyWait(from, &from);
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
	fail_unless(L4_IpcSucceeded(tag));

	void *value = join_thread(test_tid);
	fail_unless(value == NULL,
		"unexpected return from string test thread: `%s'", (char *)value);
}


START_TEST(simple_intraspace)
{
	plan_tests(2);

	/* simple echo. send string, receev bacon. */
	char replybuf[1024];
	L4_StringItem_t rep_si = L4_StringItem(sizeof(replybuf), replybuf);
	L4_LoadBR(0, 1);
	L4_LoadBRs(1, 2, rep_si.raw);
	const char *echostr = "does a polar bear crap in the woods?";
	L4_StringItem_t si = L4_StringItem(strlen(echostr) + 1, (void *)echostr);
	L4_LoadMR(0, (L4_MsgTag_t){ .X.label = ECHO_LABEL, .X.t = 2 }.raw);
	L4_LoadMRs(1, 2, si.raw);
	L4_MsgTag_t tag = L4_Call_Timeouts(test_tid, TEST_IPC_DELAY,
		TEST_IPC_DELAY);
	fail_unless(L4_IpcSucceeded(tag), "ipc failed: ec %#lx", L4_ErrorCode());
	fail_unless(tag.X.t == 2, "reply tag is weird (%#lx)", tag.raw);

	L4_StringItem_t got_si;
	L4_StoreMRs(1, 2, got_si.raw);
	replybuf[MIN(int, sizeof(replybuf) - 1, got_si.X.string_length)] = '\0';
	int rlen = strlen(replybuf);
	ok(rlen >= strlen(echostr), "reply length >= input length");
	ok(streq(&replybuf[rlen - strlen(echostr)], echostr),
		"echo output ends with input");
}
END_TEST


Suite *string_suite(void)
{
	Suite *s = suite_create("string");

	TCase *basic = tcase_create("basic");
	tcase_add_checked_fixture(basic, &stt_setup, &stt_teardown);
	tcase_add_test(basic, simple_intraspace);
	suite_add_tcase(s, basic);

	return s;
}
