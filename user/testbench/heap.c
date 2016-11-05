
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <ccan/list/list.h>
#include <ccan/likely/likely.h>
#include <ccan/compiler/compiler.h>

#include <l4/types.h>
#include <l4/ipc.h>
#include <l4/kip.h>
#include <l4/sigma0.h>

#include <ukernel/memdesc.h>
#include <ukernel/util.h>

#include "defs.h"
#include "forkserv.h"


static L4_Word_t heap_pos = 0, heap_top = 0;
static L4_ThreadId_t heap_tid;	/* forkserv's TID */

bool use_forkserv_sbrk = false;


COLD void heap_init(int adjustment)
{
	assert(adjustment >= 0);
	adjustment = (adjustment + PAGE_SIZE - 1) & ~PAGE_MASK;

	/* for the testbench root task, find the highest address where there's
	 * regular memory. use that as heap top.
	 */
	heap_pos = find_phys_mem_top() + 1 - adjustment;
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
			L4_Fpage_t page = L4_Sigma0_GetPage(L4_nilthread,
				L4_Fpage(heap_pos - increment, increment));
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
	struct memdescbuf mds = {
		.ptr = (void *)kip + kip->MemoryInfo.MemDescPtr,
		.len = kip->MemoryInfo.n, .size = kip->MemoryInfo.n,
	};
	L4_Word_t q_pos = 0, high = 0;
	for(;;) {
		L4_Fpage_t part = mdb_query(&mds, q_pos, ~0ul,
			false, false, L4_ConventionalMemoryType);
		if(L4_IsNilFpage(part)) break;
		q_pos = FPAGE_HIGH(part) + 1;
		if(L4_SizeLog2(part) < PAGE_BITS) continue;
		high = MAX(L4_Word_t, FPAGE_HIGH(part), high);
	}

	return high;
}


int getpagesize(void) {
	return PAGE_SIZE;
}


/* interface for lib/slab.c */

/* FIXME: make this threadsafe (and sbrk() as well). slab allocation is used
 * in the mutex serializer thread, so a custom spinlock implementation is
 * indicated.
 */
static struct list_head kmem_list = LIST_HEAD_INIT(kmem_list);

void *kmem_alloc_new_page(void) {
	if(list_empty(&kmem_list)) return sbrk(PAGE_SIZE);
	else {
		struct list_node *link = kmem_list.n.next;
		list_del_from(&kmem_list, link);
		return link;
	}
}

void kmem_free_page(void *ptr) {
	struct list_node *link = ptr;
	list_add(&kmem_list, link);
}
