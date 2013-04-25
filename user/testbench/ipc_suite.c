
/* tests concerning the Ipc system call, with the exception of anything
 * related to string transfers (which is tested in string_suite.c instead).
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

#include <l4/types.h>
#include <l4/ipc.h>
#include <l4/kip.h>
#include <l4/space.h>
#include <l4/syscall.h>

#include <ukernel/util.h>

#include "defs.h"
#include "test.h"


#define IS_LOCAL_TID(tid) (!L4_IsNilThread(L4_LocalIdOf((tid))))


struct sender_param {
	L4_ThreadId_t parent;
	L4_Word_t payload;
};


/* TODO: move this into util.c or some such */
static int fork_tid(L4_ThreadId_t *tid_p)
{
	L4_ThreadId_t parent = L4_MyGlobalId();
	int pid = fork();
	if(pid != 0) {
		L4_MsgTag_t tag = L4_Wait(tid_p);
		fail_if(L4_IpcFailed(tag),
			"%s: ec %#lx", __func__, L4_ErrorCode());
	} else {
		L4_LoadMR(0, 0);
		L4_Send(parent);
		*tid_p = L4_nilthread;
	}

	return pid;
}


/* sender thread utilities. these attempt to do an IPC with the caller thread
 * either from another thread in this process, or a thread in a forked address
 * space.
 */
static void sender_thread_fn(void *param_ptr)
{
	struct sender_param *p = param_ptr;

	L4_LoadMR(0, (L4_MsgTag_t){ .X.u = 1, .X.label = 0xd00d }.raw);
	L4_LoadMR(1, p->payload);
	L4_MsgTag_t tag = L4_Send_Timeout(p->parent, TEST_IPC_DELAY);
	if(L4_IpcFailed(tag) && (L4_ErrorCode() & 0xf) != 2) {
		diag("%s: send failed, ec %#lx", __func__, L4_ErrorCode());
	}

	free(p);
}


static L4_ThreadId_t send_from_thread(L4_Word_t payload)
{
	struct sender_param *param = malloc(sizeof(*param));
	fail_if(param == NULL);
	*param = (struct sender_param){
		.parent = L4_MyGlobalId(), .payload = payload,
	};
	return start_thread(&sender_thread_fn, param);
}


static L4_ThreadId_t send_from_fork(L4_Word_t payload)
{
	L4_ThreadId_t child_tid, parent_tid = L4_MyGlobalId();
	int pid = fork_tid(&child_tid);
	if(pid == 0) {
		struct sender_param *param = malloc(sizeof(*param));
		fail_if(param == NULL);
		*param = (struct sender_param){
			.parent = parent_tid, .payload = payload,
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
 */
START_TEST(receive_from_anylocalthread)
{
	diag("test in %lu:%lu", L4_ThreadNo(L4_Myself()),
		L4_Version(L4_Myself()));
	plan_tests(5);

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

	todo_start("kernel not ready");

	/* part 1 */
	L4_ThreadId_t from;
	L4_MsgTag_t tag = L4_WaitLocal_Timeout(TEST_IPC_DELAY, &from);
	ok(L4_IpcFailed(tag) && (L4_ErrorCode() & 0xf) == 3,
		"recv timeout in no-sender");

	/* part 2a */
	L4_ThreadId_t sender = send_from_fork(0xdeadbeef);
	tag = L4_WaitLocal_Timeout(TEST_IPC_DELAY, &from);
	ok(L4_IpcFailed(tag) && (L4_ErrorCode() & 0xf) == 3,
		"recv timeout in foreign sender");
	close_sender(sender);

	/* part 2b */
	L4_ThreadId_t fork_sender = send_from_thread(0xf00bdead);
	tag = L4_WaitLocal_Timeout(TEST_IPC_DELAY, &from);
	if(L4_IpcFailed(tag)) diag("ec %#lx", L4_ErrorCode());
	L4_Word_t payload;
	L4_StoreMR(1, &payload);
	ok(L4_IpcSucceeded(tag) && payload == 0xf00bdead,
		"recv success in local sender");
	close_sender(fork_sender);

	/* part 3 */
	fork_sender = send_from_fork(0xbaddcafe);
	sender = send_from_thread(0xb0a7face);
	tag = L4_WaitLocal_Timeout(TEST_IPC_DELAY, &from);
	if(L4_IpcFailed(tag)) diag("ec %#lx", L4_ErrorCode());
	L4_StoreMR(1, &payload);
	ok(L4_IpcSucceeded(tag) && payload == 0xb0a7face,
		"received from thread, first");

	tag = L4_Wait_Timeout(TEST_IPC_DELAY, &from);
	if(L4_IpcFailed(tag)) diag("ec %#lx", L4_ErrorCode());
	L4_StoreMR(1, &payload);
	ok(L4_IpcSucceeded(tag) && payload == 0xbaddcafe,
		"received from fork, after");

	close_sender(fork_sender);
	close_sender(sender);

	todo_end();
}
END_TEST


Suite *ipc_suite(void)
{
	Suite *s = suite_create("ipc");

	/* tests written to hit a panic() in ipc.c, which haven't been sorted
	 * elsewhere yet
	 */
	TCase *panic_case = tcase_create("panic");
	tcase_add_test(panic_case, receive_from_anylocalthread);
	suite_add_tcase(s, panic_case);

	return s;
}
