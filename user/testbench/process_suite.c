
/* tests of forkserv's own process management features. these are of limited
 * utility when those mechanisms are broken, but at least that'll break the
 * whole test sequence rather than going unnoticed.
 */

#include <assert.h>

#include <l4/types.h>
#include <l4/ipc.h>
#include <l4/thread.h>

#include "defs.h"
#include "test.h"


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


L4_Word_t __attribute__((noinline)) get_utcb_noinline(void) {
	return (L4_Word_t)__L4_Get_UtcbAddress();
}


START_TEST(ipc_with_child)
{
	plan_tests(2);

	L4_ThreadId_t parent_tid = L4_Myself();
	diag("parent (%lu:%lu) UTCB base is %p",
		L4_ThreadNo(parent_tid), L4_Version(parent_tid),
		__L4_Get_UtcbAddress());
	int spid = fork();
	if(spid == 0) {
		L4_ThreadId_t ctid = L4_Myself();
		diag("child UTCB looks like %p", __L4_Get_UtcbAddress());
		diag("real UTCB base is %#lx", get_utcb_noinline());
		diag("child %#lx sending to parent %#lx", ctid.raw, parent_tid.raw);

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
	diag("IPC child TID is %#lx (%lu:%lu)", child_tid.raw,
		L4_ThreadNo(child_tid), L4_Version(child_tid));
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
	if(dead_pid != spid) {
		diag("wait() failed: status %d, retval %d", status, dead_pid);
	}
}
END_TEST


Suite *process_suite(void)
{
	Suite *s = suite_create("process");

	TCase *fork_case = tcase_create("fork");
	tcase_add_test(fork_case, basic_fork_and_wait);
	tcase_add_test(fork_case, ipc_with_child);
	suite_add_tcase(s, fork_case);

	return s;
}
