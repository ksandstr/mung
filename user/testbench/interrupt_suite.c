
/* tests about interrupt handling per the L4.X2 spec. heavily tied to PC-style
 * x86 due to serial port use.
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

#include <ccan/list/list.h>

#include <l4/types.h>
#include <l4/ipc.h>
#include <l4/kip.h>
#include <l4/syscall.h>

#include <ukernel/16550.h>
#include <ukernel/ioport.h>

#include "test.h"
#include "defs.h"


START_TEST(basic_api_test)
{
	plan_tests(2);

	const L4_KernelInterfacePage_t *kip = L4_GetKernelInterface();
	bool assoc_ok = true, deassoc_ok = true;
	for(int i=1; i < kip->ThreadInfo.X.SystemBase; i++) {
		/* these ones don't count. */
		switch(i) {
			case 2:		/* PIC cascade */
			case 8:		/* CMOS clock (microkernel's thing) */
			case 13:	/* FPU/coproc/etc, microkernel zone */
				continue;
		}

		L4_ThreadId_t tid = L4_GlobalId(i, 1);
		L4_Word_t res = L4_ThreadControl(tid, tid, L4_nilthread,
			L4_MyGlobalId(), (void *)-1);
		if(res == 0) {
			diag("failed assoc, i=%d, ec=%#lx", i, L4_ErrorCode());
			assoc_ok = false;
			continue;
		}

		res = L4_ThreadControl(tid, tid, L4_nilthread, tid, (void *)-1);
		if(res == 0) {
			diag("failed deassoc, i=%d, ec=%#lx", i, L4_ErrorCode());
			deassoc_ok = false;
			continue;
		}
	}

	ok1(assoc_ok);
	ok1(deassoc_ok);
}
END_TEST


Suite *interrupt_suite(void)
{
	Suite *s = suite_create("interrupt");

	{
		TCase *tc = tcase_create("api");
		tcase_add_test(tc, basic_api_test);
		suite_add_tcase(s, tc);
	}

	return s;
}
