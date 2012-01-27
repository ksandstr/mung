
#include "ioport.h"
#include "16550.h"


extern void printf(const char *fmt, ...)
	__attribute__((format(printf, 1, 2)));


/* rudimentary serial port output from µiX */
#define COM_PORT 0x3f8

void computchar(unsigned char ch)
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


void putstr(const char *str)
{
	while(*str != '\0') computchar(*(str++));
}


void kmain(void *mbd, unsigned int magic)
{
	if(magic != 0x2BADB002) {
		/* hang! */
		return;
	}

	/* Print a letter to screen to see everything is working: */
	unsigned char *videoram = (unsigned char *)0xb8000;
	videoram[0] = 'A';
	videoram[1] = 0x07;		/* light grey (7) on black (0). */

	/* also, output some stuff to the serial port. */
	printf("hello, world! mbd is at %d\n", (int)mbd);
}
