/*
 * include/kernel/16550.h
 * Copyright 2008  Kalle A. Sandström <ksandstr@iki.fi>
 *
 * This file is part of µiX.
 *
 * µiX is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * µiX is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with µiX.  If not, see <http://www.gnu.org/licenses/>.
 */

/* registers of "PC-style" serial ports, i.e. those based on the
 * 16450/16550/16650/16750 UARTs.
 *
 * based on http://www.beyondlogic.org/serial/serial.htm .
 */

#ifndef SEEN_KERNEL_16550_H
#define SEEN_KERNEL_16550_H


/* port names, offset from the port base address */
#define UART_RDWR	0	/* w: transmitter read buffer, r: receive buffer */
#define UART_IER	1	/* r/w: interrupt enable register */
#define UART_IIR	2	/* r: interrupt identification register */
#define UART_FCR	2	/* w: FIFO control register */
#define UART_LCR	3	/* rw: line control register */
#define UART_MCR	4	/* rw: modem control register */
#define UART_LSR	5	/* r: line status register */
#define UART_MSR	6	/* r: modem status register */
#define UART_MISC	7	/* rw: scratch register (?) */
/* (and ones for when the divisor latch is on) */
#define UART_DLA_DLLO	0	/* rw: divisor latch low byte */
#define UART_DLA_DLHI	1	/* rw: divisor latch high byte */

/* common baud rate divisors */
#define UART_DIV_50		0x0900
#define UART_DIV_300	0x0180
#define UART_DIV_600	0x00c0
#define UART_DIV_2400	0x0030
#define UART_DIV_4800	0x0018
#define UART_DIV_9600	0x000c
#define UART_DIV_19200	0x0006
#define UART_DIV_28800	0x0004		/* (assumed) */
#define UART_DIV_38400	0x0003
#define UART_DIV_57600	0x0002
#define UART_DIV_115200	0x0001

/* IER bits. 7 and 6 are reserved. */
#define UART_IER_LOWPOWER	0x20	/* (16750 only) */
#define UART_IER_SLEEP		0x10	/* (16750 only) */
#define UART_IER_MSI_ENABLE	0x08	/* enable modem status interrupt */
#define UART_IER_RSI_ENABLE	0x04	/* ... receiver line status */
#define UART_IER_THE_ENABLE	0x02	/* ... transmitter holding buffer empty */
#define UART_IER_RDA_ENABLE	0x01	/* ... received data available */

/* IIR bits. (read only.) */
#define UART_IIR_FIFO_ENABLE	0x80
#define UART_IIR_FIFO_USABLE	0x40	/* disabled = unusable, or not present */
#define UART_IIR_FIFO_64BYTE	0x20	/* (16750 only) */
#define UART_IIR_RESERVED_4		0x10
#define UART_IIR_TIMEOUT_INT	0x08	/* 16550: timeout interrupt pending */
#define UART_IIR_INT_MASK		0x06	/* 2-bit mask, of: */
#define UART_IIR_INT_MSI		0x00	/* modem status */
#define UART_IIR_INT_THEI		0x02	/* transmitter holding register empty */
#define UART_IIR_INT_RDAI		0x04	/* received data available */
#define UART_IIR_INT_RLSI		0x06	/* receiver line status */
#define UART_IIR_PENDING		0x01	/* interrupt pending for a bending */

/* FCR bits. (write only.) */
/* bits 7 and 6 are the fifo depth select. */
#define UART_FCR_ITG_14		0xc0	/* interrupt trigger level at 14 bytes */
#define UART_FCR_ITG_8		0x80	/* 8 bytes */
#define UART_FCR_ITG_4		0x40	/* 4 bytes */
#define UART_FCR_ITG_1		0x00	/* 1 byte */
#define UART_FCR_FIFO64_ENA	0x20	/* enable 64-byte FIFO (16750 only) */
#define UART_FCR_RESERVED4	0x10
#define UART_FCR_DMA_SELECT	0x08	/* DMA mode select */
#define UART_FCR_CT_FIFO	0x04	/* clear transmit FIFO */
#define UART_FCR_CR_FIFO	0x02	/* clear receive FIFO */
#define UART_FCR_FIFO_ENA	0x01	/* enable FIFOs */

/* LCR bits. */
#define UART_LCR_DLAB		0x80	/* divisor latch enable */
#define UART_LCR_SBRKE		0x40	/* set break enable */
/* 5 and 4 are the parity select bits, used only if bit 3 is 1. */
#define UART_LCR_PAR_MASK	0x30
#define UART_LCR_PAR_ODD	0x00
#define UART_LCR_PAR_EVEN	0x10
#define UART_LCR_PAR_HI		0x20	/* high parity (sticky) (?) */
#define UART_LCR_PAR_LO		0x30	/* low parity (sticky) (?) */
#define UART_LCR_PAR_ENABLE	0x08	/* parity enable (0 = no parity, 1 = see 5|4) */
#define UART_LCR_STOPBIT	0x04	/* 0 = one, 1 = 2/1.5 */
#define UART_LCR_LEN_MASK	0x03	/* word length mask */
#define UART_LCR_LEN_5BIT	0x00
#define UART_LCR_LEN_6BIT	0x01
#define UART_LCR_LEN_7BIT	0x02
#define UART_LCR_LEN_8BIT	0x03

/* MCR bits. */
#define UART_MCR_AUTOFLOW	0x20	/* autoflow enable (16750 only) */
#define UART_MCR_LOOPBACK	0x10	/* loopback mode */
#define UART_MCR_AUXOUT2	0x08	/* aux output 2 */
#define UART_MCR_AUXOUT1	0x04	/* aux output 1 */
#define UART_MCR_FRTS		0x02	/* force request to send */
#define UART_MCR_FDTR		0x01	/* force data terminal ready */

/* LSR bits. (read only.) */
#define UART_LSR_ERFIFO		0x80	/* error in received FIFO */
#define UART_LSR_EDHR		0x40	/* empty data holding registers */
#define UART_LSR_ETHR		0x20	/* empty data transmit registers */
#define UART_LSR_BREAK		0x10	/* break interrupt */
#define UART_LSR_EFRAME		0x08	/* framing error */
#define UART_LSR_EPARITY	0x04	/* parity error */
#define UART_LSR_EOVERRUN	0x02	/* overrun error */
#define UART_LSR_DRDY		0x01	/* data ready */

/* MSR bits. (read only.) */
#define UART_MSR_CD			0x80	/* carrier detect */
#define UART_MSR_RING		0x40	/* ring indicator */
#define UART_MSR_DSR		0x20	/* data set ready */
#define UART_MSR_CTS		0x10	/* clear to send */
#define UART_MSR_DDCD		0x08	/* delta data carrier detect */
#define UART_MSR_TRING		0x04	/* trailing edge ring indicator */
#define UART_MSR_DDSR		0x02	/* delta data set ready */
#define UART_MSR_DCTS		0x01	/* delta clear to send */


#endif

