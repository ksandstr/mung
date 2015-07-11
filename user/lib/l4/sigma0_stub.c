
#include <stdlib.h>
#include <stdbool.h>
#include <l4/sigma0.h>


L4_Fpage_t L4_Sigma0_GetPage_RcvWindow_High(
	L4_ThreadId_t s0, L4_Fpage_t f, L4_Fpage_t RcvWindow, L4_Word_t high)
{
	if(L4_IsNilThread(s0)) {
		static L4_KernelInterfacePage_t *kip = NULL;
		if(kip == NULL) kip = L4_GetKernelInterface();
		s0 = L4_GlobalId(kip->ThreadInfo.X.UserBase, 1);
	}

	L4_Acceptor_t old = L4_Accepted();
	L4_Accept(L4_MapGrantItems(RcvWindow));
	L4_LoadMR(0, (L4_MsgTag_t){
		.X.label = 0xffa0,
		.X.u = f.X.extended == 1 ? 3 : 2,
	}.raw);
	L4_LoadMR(1, f.raw);
	L4_LoadMR(2, 0);
	L4_LoadMR(3, high);
	L4_MsgTag_t tag = L4_Call(s0);
	L4_Accept(old);
	if(L4_IpcFailed(tag)) return L4_Nilpage;
	else {
		L4_StoreMR(2, &f.raw);
		return f;
	}
}


void *L4_Sigma0_GetSpecial(L4_Word_t type, void *address, L4_Word_t pagesize)
{
	static L4_KernelInterfacePage_t *kip = NULL;
	if(kip == NULL) kip = L4_GetKernelInterface();
	L4_ThreadId_t s0 = L4_GlobalId(kip->ThreadInfo.X.UserBase, 1);
	if(L4_IsThreadEqual(L4_Myself(), s0)) return (void *)0;	/* (wtf?) */

	/* find memdesc for the given type. */
	bool found = false;
	L4_MemoryDesc_t *md = NULL;
	for(int i=0; i < L4_NumMemoryDescriptors(kip); i++) {
		md = L4_MemoryDesc(kip, i);
		if(L4_MemoryDescType(md) == type) {
			found = true;
			break;
		}
	}
	if(!found) return (void *)0;

	L4_Word_t start = L4_MemoryDescLow(md),
		end = L4_MemoryDescHigh(md) + 1,
		rcvstart = (L4_Word_t)address;
	if(rcvstart == 0) {
		rcvstart = start;
		address = (void *)start;
	}

	while(start < end) {
		L4_Fpage_t fpage = L4_Fpage(start, pagesize),
			rcvpage = L4_FpageLog2(rcvstart, L4_SizeLog2(fpage));
		L4_Set_Rights(&fpage, L4_ReadWriteOnly);
		fpage = L4_Sigma0_GetPage_RcvWindow(s0, fpage, rcvpage);
		if(L4_IsNilFpage(fpage)) return address;

		start += L4_Size(fpage);
		rcvstart += L4_Size(fpage);
	}

	return address;
}
