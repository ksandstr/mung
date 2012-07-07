
/* serial I/O functions for debug and test output. these walk all over the
 * first PC COM port, which has the effect of setting it up properly for the
 * userspace testbench.
 */

#include <stdio.h>
#include <ccan/likely/likely.h>

#include <ukernel/16550.h>
#include <ukernel/ioport.h>
#include <ukernel/util.h>


static bool com_is_fast(int base)
{
	int lcr = inb(base + UART_LCR);
	outb(base + UART_LCR, lcr | 0x80);
	bool slow = inb(base + UART_DLA_DLLO) != 1
		|| inb(base + UART_DLA_DLHI) != 0;
	outb(base + UART_LCR, lcr);
	return !slow;
}


/* rudimentary serial port output via µiX */
#define COM_PORT 0x3f8

static void computchar(unsigned char ch)
{
	static bool first = true;
	if(unlikely(first)) {
		first = false;
		/* initialize the serial port to the highest speed, interrupts off.
		 * but only if the microkernel hasn't already done that.
		 */
		if(!com_is_fast(COM_PORT)) {
			outb(COM_PORT + UART_IER, 0);
			outb(COM_PORT + UART_LCR, 0x80);	/* set divisor */
			outb(COM_PORT + UART_DLA_DLLO, 1);
			outb(COM_PORT + UART_DLA_DLHI, 0);
			outb(COM_PORT + UART_LCR, 0x03);	/* 8N1 */
			/* enable & clear FIFO, set interrupt at 14 bytes */
			outb(COM_PORT + UART_IIR, 0xc7);
			outb(COM_PORT + UART_MCR, 0x0b);	/* auxout2, rts, dtr */
		}
	}

	/* could poll things, but the FIFO will do all the blocking nonsense for
	 * us. there's no point given that callers aren't supposed to yield to
	 * another process.
	 */
#if 0
	/* we'll poll the LSR until the transmit register is empty. */
	while((inb(COM_PORT + UART_LSR) & UART_LSR_ETHR) == 0) {
		/* whee */
	}
#endif

	outb(COM_PORT + UART_RDWR, ch);

#if 0
	/* and then poll again until the holding register is empty, i.e. until
	 * the FIFO is no longer stuffed.
	 */
	while((inb(COM_PORT + UART_LSR) & UART_LSR_EDHR) == 0) {
		/* spin spin */
	}
#endif

	if(ch == '\n') computchar('\r');
}


void con_putstr(const char *str) {
	while(*str != '\0') computchar(*(str++));
}
