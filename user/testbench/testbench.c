
/* TODO: extend this roottask to support enough of the Check unit testing
 * framework to be useful in running proper unit tests. output via serial port
 * and so forth.
 */

#include <stdio.h>
#include <assert.h>

#include <l4/types.h>
#include <l4/thread.h>
#include <l4/ipc.h>
#include <l4/syscall.h>

#include <ukernel/16550.h>
#include <ukernel/ioport.h>
#include <ukernel/util.h>


void abort(void)
{
	printf("testbench abort() called!\n");
	L4_ThreadId_t dummy;
	for(;;) {
		L4_Ipc(L4_nilthread, L4_nilthread, L4_Timeouts(L4_Never, L4_Never),
			&dummy);
	}
}


/* XXX this code is copypasta'd from kmain.c, and should be moved into a
 * common "ser_io.c" or some such.
 */
/* rudimentary serial port output from µiX */
#define COM_PORT 0x3f8

static void computchar(unsigned char ch)
{
//	unsigned int iter = 1;

	/* we'll poll the LSR until the transmit register is empty. */
	while((inb(COM_PORT + UART_LSR) & UART_LSR_ETHR) == 0) {
#if 0
		/* yield every 128k iterations. that's far more than the time required
		 * to transmit one byte even over a 2400bps line.
		 */
		if((++iter & (128*1024-1)) == 0) L4_Yield();
#endif
	}
	outb(COM_PORT + UART_RDWR, ch);
	/* and then poll again until the holding register is empty, i.e. until
	 * the character has really been transmitted.
	 *
	 * (yeah, a proper serial driver would use the FIFO. no, this is quite
	 * enough for now thank you.)
	 */
//	iter = 1;
	while((inb(COM_PORT + UART_LSR) & UART_LSR_EDHR) == 0) {
//		if((++iter & (128*1024-1)) == 0) L4_Yield();
	}
	if(ch == '\n') computchar('\r');
}


void con_putstr(const char *str) {
	while(*str != '\0') computchar(*(str++));
}


void *malloc(size_t size)
{
	return NULL;
}


void __assert_failure(
	const char *condition,
	const char *file,
	unsigned int line,
	const char *function)
{
	printf("testbench %s(`%s', `%s', %u, `%s')\n", __func__,
		condition, file, line, function);
	abort();
	for(;;) { }
}


static void tid_test(void)
{
	printf("threadid test start.\n");

	printf("\tL4_Myself() == %#x\n", (unsigned)L4_Myself().raw);
	printf("\tL4_MyLocalId() == %#x\n", (unsigned)L4_MyLocalId().raw);
	printf("\tL4_LocalIdOf(L4_MyGlobalId()) == %#x\n",
		(unsigned)L4_LocalIdOf(L4_MyGlobalId()).raw);
	printf("\tL4_GlobalIdOf(L4_MyLocalId()) == %#x\n",
		(unsigned)L4_GlobalIdOf(L4_MyLocalId()).raw);

	printf("threadid test ends.\n");
}


/* NOTE: this test is useless in the absence of a serious sys_unmap() in the
 * kernel.
 */
void unmap_test(void)
{
	printf("unmap test start.\n");

	L4_Fpage_t pages[] = {
		L4_FpageLog2(0xdeadbeef, 12),
		L4_FpageLog2(0xcafebabe, 12),
	};
	const int n_pages = sizeof(pages) / sizeof(pages[0]);
	for(int i=0; i < n_pages; i++) {
		L4_Set_Rights(&pages[i], L4_FullyAccessible);
		L4_LoadMR(i, pages[i].raw);
	}
	L4_Fpage_t out_pages[n_pages];
	L4_Unmap(1);
	for(int i=0; i < n_pages; i++) L4_StoreMR(i, &out_pages[i].raw);

	for(int i=0; i < n_pages; i++) {
		L4_Fpage_t fp = out_pages[i];
		printf("page %d: %#x:%#x, was %c%c%c\n", i,
			(unsigned)L4_Address(fp), (unsigned)L4_Size(fp),
			CHECK_FLAG(L4_Rights(fp), L4_Readable) ? 'r' : '-',
			CHECK_FLAG(L4_Rights(fp), L4_Writable) ? 'w' : '-',
			CHECK_FLAG(L4_Rights(fp), L4_eXecutable) ? 'x' : '-');
	}

	printf("unmap test ends.\n");
}


static void test_thread_fn(void)
{
	L4_ThreadId_t self = L4_Myself();
	printf("test thread fn called! our ID is %u:%u\n",
		L4_ThreadNo(self), L4_Version(self));

	asm volatile ("int $1");
}


void threadctl_test(void)
{
	printf("threadcontrol test start.\n");

	/* thread creation. */
	L4_ThreadId_t self = L4_Myself(),
		dest = L4_GlobalId(L4_ThreadNo(self) + 1, 1);
	L4_Word_t utcb_base = L4_MyLocalId().raw & ~0x1ffUL;
	printf("utcb_base is %#x\n", utcb_base);
	L4_Word_t result = L4_ThreadControl(dest, self, self,
		L4_Pager(), (void *)(utcb_base + 512));
	printf("creating threadcontrol result %u\n", (unsigned)result);
	if(result == 0) {
		printf("error code %#x\n", L4_ErrorCode());
	} else {
		/* statically allocated stack, because who gives a shit. */
		static uint8_t stack[8192];
		L4_Start_SpIp(dest, (L4_Word_t)(stack + sizeof(stack) - 16),
			(L4_Word_t)&test_thread_fn);
		printf("test thread started.\n");
		L4_ThreadSwitch(dest);
	}

	printf("threadcontrol test ends.\n");
}


void spacectl_test(void)
{
	printf("spacecontrol test start.\n");

	L4_Word_t old_ctl, result = L4_SpaceControl(L4_Myself(),
		0xcafeb00b, L4_FpageLog2(0x10000, 12), L4_FpageLog2(0x11000, 15),
		L4_nilthread, &old_ctl);
	printf("spacecontrol result %#x, old_ctl %#x\n", (unsigned)result,
		(unsigned)old_ctl);

	printf("spacecontrol test ends.\n");
}


static void threadswitch_test(void)
{
	printf("threadswitch test start.\n");
	L4_ThreadSwitch(L4_nilthread);
	printf("threadswitch test end.\n");
}


static void schedule_test(void)
{
	printf("schedule test start.\n");
	L4_Word_t old_tc;
	L4_Word_t res = L4_Schedule(L4_Myself(), 1, 2, 3, 4, &old_tc);
	printf("L4_Schedule() returned %#x (old_timectl %#x)\n",
		(unsigned)res, (unsigned)old_tc);
	printf("schedule test end.\n");
}


int main(void)
{
	printf("hello, world!\n");

	tid_test();
	unmap_test();
	threadctl_test();
	threadswitch_test();
	schedule_test();
	spacectl_test();

	printf("testbench completed.\n");

	return 0;
}
