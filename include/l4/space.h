
#ifndef __L4__SPACE_H__
#define __L4__SPACE_H__

#include <stdbool.h>

#include <l4/types.h>
#include <l4/syscall.h>


static inline L4_Fpage_t L4_UnmapFpage(L4_Fpage_t f)
{
	L4_LoadMR(0, f.raw);
	L4_Unmap(0);
	L4_StoreMR(0, &f.raw);
	return f;
}


static inline void L4_UnmapFpages(L4_Word_t n, L4_Fpage_t *fpages)
{
	L4_LoadMRs(0, n, (L4_Word_t *)fpages);
	L4_Unmap(n - 1);
	L4_StoreMRs(0, n, (L4_Word_t *)fpages);
}


static inline L4_Fpage_t L4_FlushFpage(L4_Fpage_t f)
{
	L4_LoadMR(0, f.raw);
	L4_Unmap(0x40);
	L4_StoreMR(0, &f.raw);
	return f;
}


static inline void L4_FlushFpages(L4_Word_t n, L4_Fpage_t *fpages)
{
	L4_LoadMRs(0, n, (L4_Word_t *)fpages);
	L4_Unmap(0x40 | (n - 1));
	L4_StoreMRs(0, n, (L4_Word_t *)fpages);
}


static inline L4_Fpage_t L4_GetStatus(L4_Fpage_t f)
{
	L4_LoadMR(0, f.raw & ~0xful);
	L4_Unmap(0);
	L4_StoreMR(0, &f.raw);
	return f;
}


static inline bool L4_WasWritten(L4_Fpage_t f) {
	return (f.raw & L4_Writable) != 0;
}


static inline bool L4_WasReferenced(L4_Fpage_t f) {
	return (f.raw & L4_Readable) != 0;
}


static inline bool L4_WaseXecuted(L4_Fpage_t f) {
	return (f.raw & L4_eXecutable) != 0;
}


#endif
