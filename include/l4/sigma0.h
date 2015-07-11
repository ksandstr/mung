
#ifndef __L4__SIGMA0_H__
#define __L4__SIGMA0_H__

#include <l4/types.h>
#include <l4/kip.h>
#include <l4/message.h>
#include <l4/ipc.h>


/* mung puts L4_Sigma0_*() in libl4.a because they're rather large, and
 * there's useful shenanigans to be had wrt the kernel interface page.
 *
 * this does, however, deviate from L4Ka::Pistachio in that if the calling
 * process forks to a space with a different KIP address, subsequent calls to
 * L4_Sigma0_GetPage_RcvWindow_High(s0=nilthread, ...) will produce undefined
 * results. there is no mechanism for clearing cached KIP addresses.
 */


extern L4_Fpage_t L4_Sigma0_GetPage_RcvWindow_High(
	L4_ThreadId_t s0, L4_Fpage_t f, L4_Fpage_t RcvWindow, L4_Word_t high);

static inline L4_Fpage_t L4_Sigma0_GetPage_RcvWindow(
	L4_ThreadId_t s0, L4_Fpage_t f, L4_Fpage_t RcvWindow)
{
	return L4_Sigma0_GetPage_RcvWindow_High(s0, f, RcvWindow, 0);
}

static inline L4_Fpage_t L4_Sigma0_GetPage(L4_ThreadId_t s0, L4_Fpage_t f) {
	return L4_Sigma0_GetPage_RcvWindow(s0, f, L4_CompleteAddressSpace);
}

static inline L4_Fpage_t L4_Sigma0_GetAny(
	L4_ThreadId_t s0, L4_Word_t s, L4_Fpage_t RcvWindow)
{
	return L4_Sigma0_GetPage_RcvWindow(s0, L4_FpageLog2(~0ul, s), RcvWindow);
}

extern void *L4_Sigma0_GetSpecial(
	L4_Word_t type, void *address, L4_Word_t pagesize);


#endif
