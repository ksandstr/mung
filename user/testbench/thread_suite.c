
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
		tcase_add_test(tc, threadctl_basic);
		tcase_add_test(tc, privilege);
		suite_add_tcase(s, tc);
	}

	{
		TCase *tc = tcase_create("panic");
		tcase_add_test(tc, tid_stomp);
		suite_add_tcase(s, tc);
	}

	return s;
}
