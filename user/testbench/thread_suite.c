
/* unit tests concerning the ThreadControl and ExchangeRegister system calls,
 * and TCR access.
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

#include <l4/types.h>
#include <l4/kip.h>
#include <l4/ipc.h>
#include <l4/syscall.h>

#include <ukernel/util.h>

#include "defs.h"
#include "test.h"


START_TEST(threadctl_basic)
{
	L4_Fpage_t kip_area = L4_FpageLog2(0x100000, 12),
		utcb_area = L4_FpageLog2(0x200000, 12);

	L4_KernelInterfacePage_t *kip = L4_GetKernelInterface();
	const L4_Word_t utcb_size = L4_UtcbSize(kip);

	int n_tests;
	plan_tests(n_tests = 4 + L4_Size(utcb_area) / utcb_size);
	diag("utcb_size=%lu", utcb_size);

	L4_ThreadId_t tid = L4_GlobalId(2369, 199), self = L4_Myself();
	L4_Word_t res = L4_ThreadControl(tid, tid, self, L4_nilthread,
		(void *)-1);
	fail_unless(res == 1, "creating ThreadControl failed, ec %#lx",
		L4_ErrorCode());

	L4_Word_t ctl_out;

	res = L4_SpaceControl(tid, 0, kip_area, utcb_area, L4_anythread,
		&ctl_out);
	fail_unless(res == 1, "SpaceControl failed, ec %#lx", L4_ErrorCode());

	/* configure valid threads within the UTCB area. */
	for(L4_Word_t addr = L4_Address(utcb_area);
		addr < L4_Address(utcb_area) + L4_Size(utcb_area);
		addr += utcb_size)
	{
		res = L4_ThreadControl(tid, tid, L4_nilthread, L4_nilthread,
			(void *)addr);
		ok(res == 1, "can set UTCB at %#lx", addr);
	}

	/* and outside the UTCB area. */
	L4_Word_t out_posns[] = {
		L4_Address(utcb_area) - utcb_size,
		L4_Address(utcb_area) + L4_Size(utcb_area),
	};
	for(int i=0; i < NUM_ELEMENTS(out_posns); i++) {
		L4_Word_t addr = out_posns[i];
		res = L4_ThreadControl(tid, tid, L4_nilthread, L4_nilthread,
			(void *)addr);
		ok(res == 0 && L4_ErrorCode() == 6,
			"addr %#lx is outside UTCB range", addr);
	}

	/* TODO: test UTCB position change while thread is activated. */

	res = L4_ThreadControl(tid, L4_nilthread, L4_nilthread,
		L4_nilthread, (void *)-1);
	ok(res == 1, "thread/space delete ok");

	/* test that further threads cannot be created since the space is gone. */
	L4_ThreadId_t non_tid = L4_GlobalId(2300, 123);
	res = L4_ThreadControl(non_tid, tid, L4_MyGlobalId(),
		L4_nilthread, (void *)-1);
	if(!ok(res == 0 && L4_ErrorCode() == 3,
		"post-delete thread creation fails properly"))
	{
		diag("res=%lu, ec=%#lx", res, L4_ErrorCode());

		/* clean up if it was created anyway. */
		res = L4_ThreadControl(non_tid, L4_nilthread, L4_nilthread,
			L4_nilthread, (void *)-1);
		fail_if(res != 1, "cleanup ThreadControl failed, ec=%#lx",
			L4_ErrorCode());
	}
}
END_TEST


static L4_Word_t privilege_case(L4_Word_t *ec_p)
{
	/* ox cat, ruler of wildebeest */
	L4_ThreadId_t tid = L4_GlobalId(0xca7, 111);
	L4_Word_t res = L4_ThreadControl(tid, L4_Myself(), L4_Myself(),
		L4_nilthread, (void *)-1);
	*ec_p = L4_ErrorCode();
	if(res == 1) {
		/* clean up on success. */
		L4_Word_t r2 = L4_ThreadControl(tid, L4_nilthread, L4_nilthread,
			L4_nilthread, (void *)-1);
		fail_if(r2 != 1, "on delete, ec=%#lx", L4_ErrorCode());
	}

	return res;
}


START_TEST(privilege)
{
	plan_tests(2);

	/* point 1: should succeed from a privileged space. */
	L4_Word_t ec, res = privilege_case(&ec);
	if(!ok1(res == 1)) diag("ec=%#lx", ec);

	/* point 2: should fail from a non-privileged (forked) space. */
	int child = fork();
	if(child == 0) {
		res = privilege_case(&ec);
		if(!ok1(res == 0 && ec == 1)) diag("res=%lu, ec=%#lx", res, ec);
		exit(0);
	} else {
		int status, dead = wait(&status);
		fail_unless(dead == child, "reaped %d (expected %d)",
			dead, child);
	}
}
END_TEST


/* returns 0 on success, ErrorCode on failure */
static int try_create_thread(L4_ThreadId_t tid)
{
	L4_ThreadId_t space = L4_Myself();
	L4_Word_t res = L4_ThreadControl(tid, space, L4_Myself(),
		L4_nilthread, (void *)-1);
	if(res == 0) return L4_ErrorCode();
	else {
		/* delete it right away.
		 * "i liked the part where he said L4_nilthread"
		 */
		res = L4_ThreadControl(tid, L4_nilthread, L4_nilthread,
			L4_nilthread, (void *)-1);
		fail_if(res == 0, "deleting TC failed, ec=%#lx", L4_ErrorCode());
		return 0;
	}
}


/* there are three ranges of thread number: interrupts, kernel threads, and
 * user threads. the former two should not fly for creating new threads in
 * this same address space.
 *
 * similarly there's two kinds of version field: those that have the last bits
 * cleared (i.e. local TIDs), and those that don't. the former is not valid
 * for creation, and the latter is.
 *
 * so that's six test points.
 *
 * TODO: add a similar test for space IDs, also.
 */
START_TEST(thread_id_validity)
{
	plan_tests(7);

	L4_KernelInterfacePage_t *kip = L4_GetKernelInterface();
	int last_int = kip->ThreadInfo.X.SystemBase - 1,
		last_kern = kip->ThreadInfo.X.UserBase - 1;
	fail_if(last_int == 0);
	fail_if(last_kern <= last_int,
		"last_kern=%d, last_int=%d", last_kern, last_int);

	const struct {
		L4_ThreadId_t tid;
		bool pass;
	} cases[5] = {
		{ L4_GlobalId(last_int, 1), false },
		{ L4_GlobalId(last_kern, 1), false },
		{ L4_GlobalId(last_kern + 9999, 1), true },
		{ L4_GlobalId(0x3ffff, 2), true },
		{ L4_GlobalId(0x12345, 256), false },	/* local TID, still */
	};
	for(int version=1; version >= 0; --version) {
		for(int i=0; i < sizeof(cases) / sizeof(cases[0]); i++) {
			if(!version && !cases[i].pass) {
				/* no point in testing when there's no pass/fail line */
				continue;
			}
			L4_ThreadId_t test_tid = L4_GlobalId(L4_ThreadNo(cases[i].tid),
				version ? L4_Version(cases[i].tid) : 0);
			int n = try_create_thread(test_tid);
			bool expect = cases[i].pass && L4_IsGlobalId(test_tid);
			if(!ok((n == 0) == expect, "%s creation of %lu:%lu",
				expect ? "succeed" : "fail",
				L4_ThreadNo(test_tid), L4_Version(test_tid)))
			{
				diag("n=%d, expect=%s", n, btos(expect));
			}
		}
	}
}
END_TEST


/* create a non-activated thread and overwrite its version bits.
 *
 * (the real API test would start an actual thread, overwrite version, restart
 * it on another stack, overwrite that again with the previous bits, then
 * clean up with join_thread(). TODO: implement this.)
 */
START_TEST(tid_stomp)
{
	plan_tests(1);
	const int tno = 1234;

	L4_ThreadId_t first_tid = L4_GlobalId(tno, 1234);
	assert((L4_Version(first_tid) & 0x3f) > 1);
	L4_Word_t res = L4_ThreadControl(first_tid, first_tid, L4_Myself(),
		L4_nilthread, (void *)-1);
	fail_unless(res == 1, "ec=%#lx", L4_ErrorCode());

	L4_ThreadId_t after_tid = L4_GlobalId(tno, L4_Version(first_tid) ^ 0x3f);
	res = L4_ThreadControl(after_tid, first_tid, L4_nilthread,
		L4_nilthread, (void *)-1);
	if(!ok1(res == 1)) diag("ec=%#lx", L4_ErrorCode());

	/* cleanup. */
	res = L4_ThreadControl(after_tid, L4_nilthread, L4_nilthread,
		L4_nilthread, (void *)-1);
	fail_unless(res == 1, "ec=%#lx", L4_ErrorCode());
}
END_TEST


Suite *thread_suite(void)
{
	Suite *s = suite_create("thread");

	{
		TCase *tc = tcase_create("api");
		tcase_set_fork(tc, false);	/* must be run in privileged space */
		tcase_add_test(tc, threadctl_basic);
		tcase_add_test(tc, privilege);
		tcase_add_test(tc, thread_id_validity);
		suite_add_tcase(s, tc);
	}

	{
		TCase *tc = tcase_create("panic");
		tcase_set_fork(tc, false);
		tcase_add_test(tc, tid_stomp);
		suite_add_tcase(s, tc);
	}

	return s;
}
