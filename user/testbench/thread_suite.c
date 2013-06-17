
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
	res = L4_ThreadControl(L4_GlobalId(2300, 123), tid, L4_MyGlobalId(),
		L4_nilthread, (void *)-1);
	if(!ok(res == 0 && L4_ErrorCode() == 3,
		"post-delete thread creation fails properly"))
	{
		diag("res=%lu, ec=%#lx", res, L4_ErrorCode());
	}
}
END_TEST


Suite *thread_suite(void)
{
	Suite *s = suite_create("thread");

	{
		TCase *tc = tcase_create("api");
		tcase_add_test(tc, threadctl_basic);
		suite_add_tcase(s, tc);
	}

	return s;
}
