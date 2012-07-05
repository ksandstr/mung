
#include <stdio.h>

#include <l4/types.h>
#include <l4/ipc.h>
#include <l4/kip.h>


/* NOTE: the current L4.X2 spec says there's a third parameter, high_address.
 * it's not implemented by user/sigma0.c right now though, and a u3 message
 * won't be recognized by its IPC loop.
 */
L4_Fpage_t sigma0_get_page(L4_Fpage_t page, L4_Word_t attributes)
{
	L4_KernelInterfacePage_t *kip = L4_GetKernelInterface();
	L4_ThreadId_t s0_tid = L4_GlobalId(kip->ThreadInfo.X.UserBase, 1);
	L4_LoadBR(0, L4_CompleteAddressSpace.raw);
	L4_LoadMR(0, (L4_MsgTag_t){ .X.label = ((-6) & 0xfff) << 4,
		.X.u = 2, .X.t = 0 }.raw);
	L4_LoadMR(1, page.raw);
	L4_LoadMR(2, attributes);
	L4_MsgTag_t tag = L4_Call(s0_tid);
	if(L4_IpcFailed(tag)) {
		printf("%s: IPC to s0 failed, error %#lx\n", __func__,
			L4_ErrorCode());
		return L4_Nilpage;
	}

	L4_MapItem_t m;
	L4_StoreMRs(1, 2, m.raw);
	L4_LoadBR(0, 0);
	if(L4_IsNilFpage(L4_MapItemSndFpage(m))) return L4_Nilpage;
	else {
		return L4_FpageLog2(L4_MapItemSndBase(m),
			L4_SizeLog2(L4_MapItemSndFpage(m)));
	}
}


/* TODO: make this, and the kernel heap, contiguous in address space -- and
 * then enable MORECORE_CONTIGUOUS in dlmalloc.c & leave DEFAULT_GRANULARITY
 * at default to minimize dead kernel memory.
 */
void *sbrk(intptr_t increment)
{
	printf("%s: increment is %ld\n", __func__, (long int)increment);
	if(increment == 0) return NULL;
	else {
		int size = L4_SizeLog2(L4_Fpage(0, increment));
		L4_Fpage_t page = sigma0_get_page(L4_FpageLog2(~0ul, size), 0);
		if(L4_IsNilFpage(page)) return NULL;
		else {
			printf("%s: got memory at %#lx\n", __func__, L4_Address(page));
			return (void *)L4_Address(page);
		}
	}
}
