
/* tests about interrupt handling per the L4.X2 spec. heavily tied to PC-style
 * x86 due to serial port use.
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

#include <ccan/list/list.h>

#include <l4/types.h>
#include <l4/ipc.h>
#include <l4/kip.h>
#include <l4/syscall.h>

#include <ukernel/16550.h>
#include <ukernel/ioport.h>

#include "test.h"
#include "defs.h"


static int com_base;


static bool com_is_fast(int base)
{
	int lcr = inb(base + UART_LCR);
	outb(base + UART_LCR, lcr | 0x80);
	bool slow = inb(base + UART_DLA_DLLO) != 1
		|| inb(base + UART_DLA_DLHI) != 0;
	outb(base + UART_LCR, lcr);
	return !slow;
}


static void com_setup(void)
{
	com_base = 0x3f8;

	/* initialize the serial port to the highest speed, interrupts off.
	 * but only if the microkernel hasn't already done that.
	 */
	if(!com_is_fast(com_base)) {
		outb(com_base + UART_IER, 0);		/* no irqs, no sleep state */
		outb(com_base + UART_LCR, 0x80);	/* set divisor */
		outb(com_base + UART_DLA_DLLO, 1);
		outb(com_base + UART_DLA_DLHI, 0);
		outb(com_base + UART_LCR, 0x03);	/* 8N1 */
		/* enable & clear FIFO, set interrupt at 14 bytes */
		outb(com_base + UART_IIR, 0xc7);
		outb(com_base + UART_MCR, 0x0b);	/* auxout2, rts, dtr */
	}
}


static void com_teardown(void)
{
	/* disable interrupts in case this is the same serial port through which
	 * the tap output goes
	 */
	outb(com_base + UART_IER, 0);
	com_base = -0x7fff;
}


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


START_TEST(interrupt_test)
{
	plan_tests(5);
	diag("com_base=%#x", com_base);

	L4_ThreadId_t i_tid = L4_GlobalId(4, 1);	/* IRQ 4 */
	L4_Word_t res = L4_ThreadControl(i_tid, i_tid, L4_nilthread,
		L4_MyGlobalId(), (void *)-1);
	fail_if(res == 0, "interrupt assoc failed, ec %#lx", L4_ErrorCode());

	outb(com_base + UART_IER, UART_IER_THE_ENABLE);

	/* pass something through the serial port w/ an interrupt every 16 bytes.
	 * five test points:
	 *   - the number of interrupts
	 *   - message labels (should be label = 0xfff0, everything else 0)
	 *   - that the reply was received properly
	 *   - that the interrupt is not delivered again before the reply
	 *   - that the interrupt cannot be cleared before it is delivered
	 */
	int num_ints = 0;
	bool label_ok = true, reply_ok = true, double_ok = true, clear_ok = true;
	const char *message = "# this message is part of the test\n";
	for(int pos=0; pos < strlen(message); pos += 16) {
		for(int i=0; i < 16 && pos + i < strlen(message); i++) {
			outb(com_base + UART_RDWR, message[pos + i]);
		}

		/* point #5: can't clear. */
		L4_LoadMR(0, 0);
		L4_MsgTag_t tag = L4_Send_Timeout(i_tid, TEST_IPC_DELAY);
		if(L4_IpcSucceeded(tag) || L4_ErrorCode() != 2) {
			diag("wrong pre-delivery clear, ec=%#lx", L4_ErrorCode());
			clear_ok = false;
		}

		tag = L4_Receive_Timeout(i_tid, TEST_IPC_DELAY);
		if(L4_IpcSucceeded(tag)) {
			num_ints++;
			if(tag.X.label != 0xfff0 || tag.X.u != 0 || tag.X.t != 0) {
				diag("weird interrupt tag %#08lx", tag.raw);
				label_ok = false;
			}

			/* before the reply, there shouldn't be another interrupt IPC. */
			tag = L4_Receive_Timeout(i_tid, TEST_IPC_DELAY);
			if(L4_IpcSucceeded(tag) || L4_ErrorCode() != 3) {
				diag("double interrupt, ec=%#lx",
					L4_IpcSucceeded(tag) ? 0 : L4_ErrorCode());
				double_ok = false;
			}

			L4_LoadMR(0, 0);
			tag = L4_Reply(i_tid);
			if(L4_IpcFailed(tag)) {
				diag("interrupt reply failed, ec %#lx", L4_ErrorCode());
				reply_ok = false;
			}
		} else {
			diag("interrupt ipc failed, ec %#lx", L4_ErrorCode());
		}
	}
	outb(com_base + UART_RDWR, '\r');

	int n_expect = (strlen(message) + 15) / 16;
	ok(num_ints == n_expect, "num_ints == %d", n_expect);
	ok1(label_ok);
	ok1(reply_ok);
	ok1(double_ok);
	ok1(clear_ok);

	/* thread death deassociates the interrupt. */
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

	{
		TCase *tc = tcase_create("serial");
		tcase_add_checked_fixture(tc, &com_setup, &com_teardown);
		tcase_add_test(tc, interrupt_test);
		suite_add_tcase(s, tc);
	}

	return s;
}
