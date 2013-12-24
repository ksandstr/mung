
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ccan/likely/likely.h>
#include <ccan/compiler/compiler.h>

#include <l4/types.h>
#include <l4/ipc.h>
#include <l4/kip.h>

#include <ukernel/util.h>

#include "defs.h"
#include "forkserv.h"


static L4_Word_t heap_pos = 0, heap_top = 0;
static L4_ThreadId_t heap_tid;	/* forkserv's TID */

bool use_forkserv_sbrk = false;


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


L4_Word_t get_heap_top(void)
{
	if(use_forkserv_sbrk) {
		assert(!L4_IsNilThread(heap_tid));
		L4_Word_t ret;
		int n = forkserv_sbrk(heap_tid, &ret, 0);
		if(n != 0) {
			printf("%s: forkserv_sbrk() failed, n=%d\n", __func__, n);
			abort();
		}
		return ret;
	} else {
		return heap_top;
	}
}


void *sbrk(intptr_t increment)
{
	if(unlikely(heap_pos == 0)) {
		heap_init(0);
		assert(heap_pos > 0);
		heap_top = heap_pos;
	}

	if(increment > 0) {
		/* FIXME: get smallest physical page size from KIP */
		increment = (increment + 0xfff) & ~0xfff;
		if(use_forkserv_sbrk) {
			if(L4_IsNilThread(heap_tid)) {
				heap_tid = L4_Pager();
				assert(!L4_IsNilThread(heap_tid));
			}
			L4_Word_t new_pos;
			int n = forkserv_sbrk(heap_tid, &new_pos, increment);
			if(n != 0) {
				printf("forkserv_sbrk() failed, n=%d\n", n);
				abort();		/* whut */
			}
			heap_pos = new_pos;
		} else {
			L4_Fpage_t page = sigma0_get_page(
				L4_Fpage(heap_pos - increment, increment), 0);
			if(L4_IsNilFpage(page)) return NULL;
			heap_pos = L4_Address(page);
		}
	} else if(increment < 0) {
		/* TODO: move the allocated heap backward, so that pages aren't
		 * re-requested from sigma0 once the heap grows again.
		 */
	}

	return (void *)heap_pos;
}


COLD L4_Word_t find_phys_mem_top(void)
{
	L4_KernelInterfacePage_t *kip = L4_GetKernelInterface();
	int n_descs = kip->MemoryInfo.n;
	L4_Word_t high = 0;
	for(int i=0; i < n_descs; i++) {
		const L4_MemoryDesc_t *desc = L4_MemoryDesc(kip, i);
		if(L4_IsMemoryDescVirtual(desc)) {
			if(high == 0) high = L4_MemoryDescHigh(desc);
		} else if(L4_MemoryDescType(desc) == L4_ConventionalMemoryType
			&& L4_MemoryDescHigh(desc) > 0x200000)
		{
			high = MIN(L4_Word_t, L4_MemoryDescHigh(desc), high);
		}
	}

	/* return value is "last valid address", i.e. offset bits hardwired to
	 * all-ones.
	 */
	return high;
}


COLD void heap_init(int adjustment)
{
	assert(adjustment >= 0);
	adjustment = (adjustment + PAGE_SIZE - 1) & ~PAGE_MASK;

	/* for the testbench root task, find the highest address where there's
	 * regular memory. use that as heap top.
	 */
	heap_pos = find_phys_mem_top() + 1 - adjustment;
}
