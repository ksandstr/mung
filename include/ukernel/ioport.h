/*
 * include/kernel/ioport.h -- x86-style port access primitives
 * Copyright 2008, 2009, 2010  Kalle A. Sandström <ksandstr@iki.fi>
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

#ifndef SEEN_INCLUDE_KERNEL_IOPORT_H
#define SEEN_INCLUDE_KERNEL_IOPORT_H


#include <l4/types.h>

typedef L4_Word16_t ioport_t;

/* this might actually work on amd64, too. but we're not properly
 * multiarchitecture as of yet.
 */

static inline L4_Word8_t inb(const ioport_t port)
{
	L4_Word8_t tmp;
	__asm__ __volatile__ ("inb %w1, %0" :"=a"(tmp) :"dN"(port));
	return tmp;
}


static inline void outb(const ioport_t port, const L4_Word8_t val)
{
	__asm__ __volatile__ ("outb %0, %w1" : :"a"(val), "dN"(port));
}


static inline L4_Word16_t inw(const ioport_t port)
{
	L4_Word16_t tmp;
	__asm__ __volatile__ ("inw %w1, %0" :"=a"(tmp) :"dN"(port));
	return tmp;
}


static inline void outw(const ioport_t port, const L4_Word16_t value)
{
	__asm__ __volatile__ ("outw %0, %w1" : :"a"(value), "dN"(port));
}


static inline L4_Word32_t inl(const ioport_t port)
{
	L4_Word32_t tmp;
	__asm__ __volatile__ ("inl %w1, %0" :"=a"(tmp) :"dN"(port));
	return tmp;
}


static inline void outl(const ioport_t port, const L4_Word32_t value)
{
	__asm__ __volatile__ ("outl %0, %w1" : :"a"(value), "dN"(port));
}


static inline void insb(const ioport_t port, void * addr, L4_Word_t count)
{
	__asm__ __volatile__ ("rep ; ins" "b" : "=D" (addr),
		"=c" (count) : "d" (port),
		"0" (addr),"1" (count));
}

static inline void insw(const ioport_t port, void * addr, L4_Word_t count)
{
	__asm__ __volatile__ ("rep ; ins" "w" : "=D" (addr),
		"=c" (count) : "d" (port),
		"0" (addr),"1" (count));
}


static inline void insl(const ioport_t port, void * addr, L4_Word_t count)
{
	__asm__ __volatile__ ("rep ; ins" "l" : "=D" (addr),
		"=c" (count) : "d" (port),
		"0" (addr),"1" (count));
}
   
static inline void outsb(const ioport_t port, const void * addr, L4_Word_t count)
{
	__asm__ __volatile__ ("rep ; outs" "b" : "=S" (addr),
		"=c" (count) : "d" (port),
		"0" (addr),"1" (count));
}

static inline void outsw(const ioport_t port, const void * addr, L4_Word_t count)
{
	__asm__ __volatile__ ("rep ; outs" "w" : "=S" (addr),
		"=c" (count) : "d" (port),
		"0" (addr),"1" (count));
}

static inline void outsl(const ioport_t port, const void * addr, L4_Word_t count)
{
	__asm__ __volatile__ ("rep ; outs" "l" : "=S" (addr),
		"=c" (count) : "d" (port),
		"0" (addr),"1" (count));
}

#endif
