
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
#include <ukernel/util.h>

#include "test.h"
#include "defs.h"


static int com_base, com_irq;


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
	com_irq = 4;

	/* initialize the serial port to the highest speed (115200 b/s),
	 * interrupts off. but only if the microkernel hasn't already done that.
	 */
	if(!com_is_fast(com_base)) {
		outb(com_base + UART_IER, 0);		/* no irqs, no sleep state */
		outb(com_base + UART_LCR, 0x80);	/* set divisor */
		outb(com_base + UART_DLA_DLLO, 1);
		outb(com_base + UART_DLA_DLHI, 0);
		outb(com_base + UART_LCR, 0x03);	/* 8N1 */
		/* enable & clear FIFO, set interrupt at 14 bytes */
		outb(com_base + UART_FCR, 0xc7);
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
	com_irq = -1;
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


START_TEST(serial_interrupt_test)
{
	plan_tests(5);
	diag("com_base=%#x, com_irq=%d", com_base, com_irq);

	L4_ThreadId_t i_tid = L4_GlobalId(com_irq, 1);
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
		outb(com_base + UART_FCR, 0xc7);	/* clear FIFOs */
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
			int iir = inb(com_base + UART_IIR);
			if((iir & UART_IIR_INT_MASK) != UART_IIR_INT_THEI) {
				diag("unexpected iir=%#02x at int", iir);
			}

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


/* NOTE: callers shouldn't use any diag(), ok() or other TAP function before
 * receiving the interrupt (or failing to do so, anyway).
 */
static L4_ThreadId_t cause_interrupt(void)
{
	const L4_ThreadId_t i_tid = L4_GlobalId(com_irq, 1);

	const char *message = "# (hello.)\n";
	assert(strlen(message) <= 16);

	L4_Word_t res = L4_ThreadControl(i_tid, i_tid, L4_nilthread,
		L4_MyGlobalId(), (void *)-1);
	fail_if(res == 0, "interrupt assoc %d:1 failed, ec=%#lx", com_irq,
		L4_ErrorCode());

	uint8_t iir = inb(com_base + UART_IIR);
	fail_unless(CHECK_FLAG(iir, UART_IIR_FIFO_USABLE),
		"no usable FIFO in this UART!");
	fail_unless(CHECK_FLAG(iir, UART_IIR_FIFO_ENABLE),
		"FIFO not enabled!");

#if 0
	while((iir & 0xf) != 1) {
		/* handle the remaining interrupt. */
		L4_MsgTag_t tag = L4_Receive_Timeout(i_tid, L4_ZeroTime);
		if(L4_IpcSucceeded(tag)) {
			L4_LoadMR(0, 0);
			tag = L4_Reply(i_tid);
			fail_if(L4_IpcFailed(tag), "ec=%#lx", L4_ErrorCode());
			iir = inb(com_base + UART_IIR);
		} else {
			fail_unless(L4_ErrorCode() == 3,
				"should timeout on last interrupt");
			break;
		}
	}
#endif

#if 0
	/* at 28.8 kb/s (how nostalgic!) the UART would clear a 16 byte FIFO in
	 * 4.444... milliseconds. make that room now.
	 */
	nsleep(4444444);
#endif

	outb(com_base + UART_IER, UART_IER_THE_ENABLE);
	outb(com_base + UART_FCR, 0xc7);	/* clear FIFOs */
	inb(com_base + UART_IIR);	/* clears the transmit interrupt */
	L4_Clock_t start = L4_SystemClock();
	for(int i=0; message[i] != '\0'; i++) {
		outb(com_base + UART_RDWR, message[i]);
	}
	L4_Clock_t end = L4_SystemClock();
	iir = inb(com_base + UART_IIR);	/* clear it again */
	iir = inb(com_base + UART_IIR);
	fail_unless(CHECK_FLAG(iir, UART_IIR_NOT_PENDING), "iir=%#02x", iir);
	fail_unless(end.raw - start.raw <= 1000,
		"transmit waited on FIFO for %lu µs (sysclock)",
		(unsigned long)(end.raw - start.raw));

	return i_tid;
}


/* tests out interrupt handling when the system is idle, i.e. in the scheduler
 * loop's hlt instruction.
 */
START_TEST(int_from_wait)
{
	plan_tests(2);

	L4_ThreadId_t i_tid = cause_interrupt();
	L4_MsgTag_t tag = L4_Receive_Timeout(i_tid, TEST_IPC_DELAY);
	if(!ok1(L4_IpcSucceeded(tag))) {
		diag("i_tid=%lu:%lu", L4_ThreadNo(i_tid), L4_Version(i_tid));
	}
	ok1(tag.X.label == 0xfff0);
}
END_TEST


/* tests out interrupt signaling while doing something else (e.g. the bogomips
 * spin).
 */
START_TEST(int_from_user)
{
	const uintptr_t spin_us = 10000;

	plan_tests(1);
	diag("spin_us=%u", (unsigned)spin_us);

	L4_Clock_t start = L4_SystemClock();
	L4_ThreadId_t i_tid = cause_interrupt();
	nsleep(5 * 1000 * 1000);
	L4_Clock_t end = L4_SystemClock();
	L4_MsgTag_t tag = L4_Receive_Timeout(i_tid, TEST_IPC_DELAY);
	if(!ok(L4_IpcSucceeded(tag) && tag.X.label == 0xfff0, "got interrupt")) {
		diag("tag=%#08x, ec=%#lx", tag.raw, L4_ErrorCode());
	}

	fail_unless(end.raw - start.raw > 4000,
		"nsleep(5ms) should have happened");
}
END_TEST


static void delay_spinner(void *parameter)
{
	uintptr_t us = (uintptr_t)parameter;
	nsleep(us * 1000);
}


/* tests out interrupt signaling when a lower-priority thread runs.
 * points:
 *   - interrupt delivery
 *   - time at IPC reception, which must be sooner than other thread's quantum
 */
START_TEST(int_under_other)
{
	const uintptr_t spin_us = 10000;

	plan_tests(3);
	diag("spin_us=%u", (unsigned)spin_us);

	L4_ThreadId_t other = start_thread_long(&delay_spinner,
		(void *)spin_us, find_own_priority() - 1, L4_TimePeriod(20 * 1000),
		L4_Never);
	fail_if(L4_IsNilThread(other));

	L4_ThreadId_t i_tid = cause_interrupt();
	uint8_t iir = inb(com_base + UART_IIR);
	L4_Clock_t start = L4_SystemClock();
	L4_MsgTag_t tag = L4_Receive_Timeout(i_tid, TEST_IPC_DELAY);
	L4_Clock_t end = L4_SystemClock();
	diag("post-cause iir=%#02x", iir);
	iir = inb(com_base + UART_IIR);
	diag("post-receive iir=%#02x", iir);
	if(!ok(L4_IpcSucceeded(tag) && tag.X.label == 0xfff0, "got interrupt")) {
		diag("tag=%#08x, ec=%#lx", tag.raw, L4_ErrorCode());
	}

	uint64_t diff_us = end.raw - start.raw;
	/* TODO: investigate this further. */
	skip_start(true, 1, "can't succeed under emulation");
		ok(diff_us > 1000, "there should be a delay");
	skip_end;
	if(!ok1(diff_us < spin_us) || true) diag("diff_us=%u", (unsigned)diff_us);

	join_thread(other);
}
END_TEST


Suite *interrupt_suite(void)
{
	Suite *s = suite_create("interrupt");

	{
		TCase *tc = tcase_create("api");
		tcase_set_fork(tc, false);
		tcase_add_unchecked_fixture(tc, &com_setup, &com_teardown);
		tcase_add_test(tc, basic_api_test);
		tcase_add_test(tc, serial_interrupt_test);
		suite_add_tcase(s, tc);
	}

	{
		TCase *tc = tcase_create("context");
		tcase_set_fork(tc, false);
		tcase_add_unchecked_fixture(tc, &com_setup, &com_teardown);
		tcase_add_test(tc, int_from_wait);
		tcase_add_test(tc, int_from_user);
		tcase_add_test(tc, int_under_other);
		suite_add_tcase(s, tc);
	}

	return s;
}
