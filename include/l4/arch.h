
/* architecture-specific things, i.e. I/O fpages for ia32 and amd64, and so
 * forth.
 */

#ifndef __L4__ARCH_H__
#define __L4__ARCH_H__

#include <l4/types.h>


typedef union {
	L4_Word_t raw;
	struct {
		unsigned rwx:4;
		unsigned __two:6;
		unsigned s:6;
		unsigned p:16;
	} X;
} L4_IoFpage_t;


static inline L4_Fpage_t L4_IoFpage(L4_Word_t port, int size)
{
	int msb = sizeof(L4_Word_t) * 8 - __builtin_clzl(size) - 1,
		shift = (1ul << msb) <= size ? msb : msb + 1;
	L4_IoFpage_t fp = {
		.X.p = port, .X.s = shift,
		.X.__two = 2, .X.rwx = L4_ReadWriteOnly,
	};
	return (L4_Fpage_t){ .raw = fp.raw };
}


static inline L4_Fpage_t L4_IoFpageLog2(L4_Word_t port, int sizelog2)
{
	L4_IoFpage_t fp = {
		.X.p = port, .X.s = sizelog2,
		.X.__two = 2, .X.rwx = L4_ReadWriteOnly,
	};
	return (L4_Fpage_t){ .raw = fp.raw };
}


static inline L4_Word_t L4_IoFpagePort(L4_Fpage_t f) {
	L4_IoFpage_t iofp = { .raw = f.raw };
	return iofp.X.p;
}


static inline L4_Word_t L4_IoFpageSize(L4_Fpage_t f) {
	L4_IoFpage_t iofp = { .raw = f.raw };
	return 1 << iofp.X.s;
}


static inline L4_Word_t L4_IoFpageSizeLog2(L4_Fpage_t f) {
	L4_IoFpage_t iofp = { .raw = f.raw };
	return iofp.X.s;
}


static inline L4_Word_t L4_IsIoFpage(L4_Fpage_t f) {
	L4_IoFpage_t iofp = { .raw = f.raw };
	return iofp.X.__two == 2;
}

#endif
