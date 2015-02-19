
/* service that takes over the root task's memory, letting it fork
 * subprocesses (and subprocesses of those). this is useful for test cases
 * involving map operations in IPC, or the Unmap system call.
 */
#define FORKSERV_IMPL_SOURCE

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <ccan/list/list.h>
#include <ccan/htable/htable.h>
#include <ccan/likely/likely.h>
#include <ccan/container_of/container_of.h>

#include <ukernel/slab.h>
#include <ukernel/guard.h>
#include <ukernel/util.h>

#include <l4/types.h>
#include <l4/ipc.h>
#include <l4/syscall.h>
#include <l4/space.h>
#include <l4/schedule.h>
#include <l4/kip.h>

/* for int_hash() */
#include <ukernel/util.h>

#include "defs.h"
#include "forkserv.h"
#include "muidl.h"

/* IDL bits */
#include "forkserv-defs.h"
#include "threadmgr-defs.h"


#define TPS_SHIFT 6
#define THREADS_PER_SPACE (1 << TPS_SHIFT)


#define FSHELPER_CHOPCHOP	0x6343	/* "cC", moar liek a wakeup IPC */

/* from helper to forkserv */
#define FSHELPER_CHOPCHOP_DONE 0x4364	/* "Cd" */
#define FSHELPER_CHOPCHOP_REPLY 0x4372	/* "Cr" */


struct fs_thread;


struct fs_space
{
	GUARD_MEMBER(g_0);
	L4_Word_t id, parent_id, prog_brk;
	L4_ThreadId_t mgr_tid;
	struct htable pages;
	GUARD_MEMBER(g_1);
	struct fs_thread *threads[THREADS_PER_SPACE];
	GUARD_MEMBER(g_2);
	L4_Fpage_t utcb_area, kip_area;
	struct list_head dead_children;
	struct list_head waiting_threads;
	struct list_node dead_link;		/* in parent's dead_children */
	int exit_status;
	GUARD_MEMBER(g_3);

	L4_ThreadId_t child_redir_tid;
};


struct fs_phys_page
{
	struct list_node link;	/* in free_page_list */
	L4_Word_t local_addr;	/* address in forkserv */
	int refcount;			/* 1 for exclusive, 0 for dead */
};


struct fs_vpage
{
	L4_Word_t address;		/* key in "pages" */
	int access;
	struct fs_phys_page *page;
};


struct fs_thread
{
	L4_ThreadId_t tid;
	struct fs_space *space;
	struct list_node wait_link;	/* in space's waiting_threads */
};


/* queued to helper thread */
struct helper_work {
	struct helper_work *next;
	L4_ThreadId_t from;
	L4_Word_t aid;	/* async ID */

	/* new_thread parameters */
	L4_Word_t ip, sp;
	int32_t space_id, req_tnum, max_delay;
	L4_Time_t ts_len, total_quantum;
	uint8_t priority, sens_pri;
};


/* used by forkserv to track async operation id to sender thread. */
struct async_op {
	L4_Word_t id;
	L4_ThreadId_t from;
};


static size_t hash_word(const void *, void *);
static size_t hash_threadno(const void *, void *);
static struct fs_space *get_space(L4_Word_t id);
static void handle_pf(L4_Word_t addr, L4_Word_t ip, L4_MapItem_t *page_ptr);
static void handle_exit(int32_t status);


/* thread_hash is hashed by threadno, but compared for equality with full TID
 * (i.e. word_cmp().) this is for handle_add_tid()'s overwrite function.
 */
static struct htable
	thread_hash = HTABLE_INITIALIZER(thread_hash, &hash_threadno, NULL),
	space_hash = HTABLE_INITIALIZER(space_hash, &hash_word, NULL),
	async_hash = HTABLE_INITIALIZER(async_hash, &hash_word, NULL);
static LIST_HEAD(free_page_list);
static struct kmem_cache *vpage_slab = NULL;		/* for fs_vpage */

static L4_ThreadId_t console_tid, fpager_tid, helper_tid, forkserv_tid;
static L4_Word_t map_range_pos = 0, next_space_id = 100, next_async_id = 1;
static struct helper_work *helper_queue = NULL;
static L4_Word_t max_vaddr;
static int32_t root_space_id = -1;


static size_t hash_word(const void *elem, void *priv) {
	L4_Word_t val = *(const L4_Word_t *)elem;
	return int_hash(val);
}


static bool word_cmp(const void *a, void *b) {
	return *(const L4_Word_t *)a == *(L4_Word_t *)b;
}


static size_t hash_threadno(const void *elem, void *priv) {
	L4_ThreadId_t val = { .raw = *(const L4_Word_t *)elem };
	return int_hash(L4_ThreadNo(val));
}


static struct fs_thread *get_thread(L4_ThreadId_t tid) {
	return htable_get(&thread_hash, int_hash(L4_ThreadNo(tid)),
		&word_cmp, &tid);
}


#ifdef DEBUG_ME_HARDER
#include <ukernel/invariant.h>

static bool invariants(const char *context)
{
	INV_CTX;
	inv_push("checking from `%s'", context);

	struct htable_iter it;
	for(void *space_ptr = htable_first(&space_hash, &it);
		space_ptr != NULL;
		space_ptr = htable_next(&space_hash, &it))
	{
		struct fs_space *sp = container_of(space_ptr, struct fs_space, id);
		inv_push("check space %d (%p)", (int)sp->id, sp);

		inv_ok1(GUARD_CHECK(sp, g_0));
		inv_ok1(GUARD_CHECK(sp, g_1));
		inv_ok1(GUARD_CHECK(sp, g_2));
		inv_ok1(GUARD_CHECK(sp, g_3));

		inv_ok(get_space(sp->parent_id) != NULL,
			"parent space (id %d) must exist", (int)sp->parent_id);

		for(int i=0; i < THREADS_PER_SPACE; i++) {
			if(sp->threads[i] == NULL) continue;
			inv_ok1(!L4_IsNilThread(sp->threads[i]->tid));
		}

		inv_ok(list_empty(&sp->dead_children) || list_empty(&sp->waiting_threads),
			"either dead children, or waiting threads");

		struct htable_iter page_iter;
		for(void *page_ptr = htable_first(&sp->pages, &page_iter);
			page_ptr != NULL;
			page_ptr = htable_next(&sp->pages, &page_iter))
		{
			struct fs_vpage *vp = container_of(page_ptr, struct fs_vpage,
				address);
			inv_push("checking page at %#lx, access [%c%c%c]", vp->address,
				(vp->access & L4_Readable) != 0 ? 'r' : '-',
				(vp->access & L4_Writable) != 0 ? 'w' : '-',
				(vp->access & L4_eXecutable) != 0 ? 'x' : '-');
			inv_ok1(vp->page != NULL);
			inv_log("  phys at %#lx, refct %d", vp->page->local_addr,
				vp->page->refcount);
			if(vp->page->refcount > 1) {
				inv_ok((vp->access & L4_Writable) == 0,
					"when refcount > 1, write must be off");
			}
			inv_pop();
		}

		inv_pop();
	}

	inv_pop();

	return true;

inv_fail:
	return false;
}
#else
static bool invariants(const char *where) {
	return true;
}
#endif


/* find the highest virtual address usable to user space. */
COLD L4_Word_t find_high_vaddr(void)
{
	L4_KernelInterfacePage_t *kip = L4_GetKernelInterface();
	int n_descs = kip->MemoryInfo.n;
	L4_Word_t low = 0, high = ~0ul;
	for(int i=0; i < n_descs; i++) {
		const L4_MemoryDesc_t *desc = L4_MemoryDesc(kip, i);
		if(!L4_IsMemoryDescVirtual(desc)) continue;
		switch(L4_MemoryDescType(desc)) {
			case L4_ConventionalMemoryType:
				low = MAX(L4_Word_t, low, L4_MemoryDescLow(desc));
				high = MIN(L4_Word_t, high, L4_MemoryDescHigh(desc));
				break;
			case L4_ReservedMemoryType:
				if(L4_MemoryDescLow(desc) > low) {
					high = MIN(L4_Word_t, high, L4_MemoryDescLow(desc));
				} else {
					low = MAX(L4_Word_t, low, L4_MemoryDescHigh(desc));
				}
				break;
		}
	}
	if(low != 0) {
		printf("%s: low=%#lx, don't know what the fuck\n", __func__, low);
		abort();
	}

	return high;
}


static struct fs_space *make_initial_space(int id)
{
	struct fs_space *sp = malloc(sizeof(*sp));
	*sp = (struct fs_space){
		.id = id, .parent_id = 0, .prog_brk = find_phys_mem_top() + 1,
		.utcb_area = L4_Fpage(0x30000, UTCB_SIZE * THREADS_PER_SPACE),
		.kip_area = L4_FpageLog2(0x2f000, 12),
		.child_redir_tid = L4_anythread,
	};
	GUARD_INIT(sp, g_0);
	GUARD_INIT(sp, g_1);
	GUARD_INIT(sp, g_2);
	GUARD_INIT(sp, g_3);
	list_head_init(&sp->dead_children);
	list_head_init(&sp->waiting_threads);
	htable_init(&sp->pages, &hash_word, NULL);
	htable_add(&space_hash, int_hash(id), &sp->id);

	return sp;
}


static struct fs_space *get_space(L4_Word_t id)
{
	void *ptr = htable_get(&space_hash, int_hash(id), &word_cmp, &id);
	if(ptr != NULL) return container_of(ptr, struct fs_space, id);
	else return NULL;
}


static struct fs_space *get_space_by_tid(L4_ThreadId_t tid)
{
	struct fs_thread *t = get_thread(tid);
	return t == NULL ? NULL : t->space;
}


static struct fs_phys_page *alloc_new_page(void)
{
	struct fs_phys_page *phys = list_pop(&free_page_list,
		struct fs_phys_page, link);
	if(phys == NULL) {
		phys = malloc(sizeof(*phys));
		phys->local_addr = (uintptr_t)sbrk(PAGE_SIZE);
	}
	phys->refcount = 1;
	memset((void *)phys->local_addr, 0, PAGE_SIZE);
	return phys;
}


void __assert_failure(
	const char *condition,
	const char *file,
	unsigned int line,
	const char *function)
{
	printf("assert(%s) failed in `%s' (%s:%u)\n", condition, function,
		file, line);
	for(;;) { asm volatile ("int $1"); }
}


void abort(void)
{
	printf("forkserv: abort() called from %p!\n",
		__builtin_return_address(0));
	for(;;) { asm volatile ("int $1"); }
}


void malloc_panic(void) {
	printf("forkserv: %s called in %lu:%lu, returns %p, %p!\n", __func__,
		L4_ThreadNo(L4_Myself()), L4_Version(L4_Myself()),
		__builtin_return_address(0), __builtin_return_address(1));
	abort();
}


int sched_yield(void)
{
	L4_ThreadSwitch(L4_nilthread);
	return 0;
}


/* this is an oneway operation. the client is expected to complete the
 * transaction with a SEND_PAGE_2 IPC.
 */
static void handle_send_page(L4_Word_t phys_addr, int32_t space_id)
{
	const L4_ThreadId_t from = muidl_get_sender();

	L4_Fpage_t window;
	struct fs_space *sp = get_space(space_id);
	if(sp == NULL) sp = make_initial_space(space_id);
	if(space_id == 0) {
		window = L4_Fpage(phys_addr, PAGE_SIZE);
	} else {
		if(unlikely(map_range_pos == 0)) {
			map_range_pos = find_phys_mem_top() + 1;
		}

		window = L4_Fpage(map_range_pos, PAGE_SIZE);
	}

	L4_MsgTag_t tag;
	do {
		L4_LoadBR(0, window.raw);
		tag = L4_Receive_Timeout(from, L4_TimePeriod(10 * 1000));
		L4_LoadBR(0, 0);
		if(L4_IpcFailed(tag)) {
			printf("%s: IPC failed, ec %#lx\n", __func__, L4_ErrorCode());
			return;
		} else if((tag.X.label & 0xfff0) == 0xffe0
			&& tag.X.u == 2 && tag.X.t == 0)
		{
			/* intervening pagefault. handle it. */
			L4_Word_t faddr, fip;
			L4_StoreMR(1, &faddr);
			L4_StoreMR(2, &fip);
#if 0
			printf("%s: inter-send pf, faddr=%#lx, fip=%#lx\n", __func__,
				faddr, fip);
#endif
			muidl_set_tag(tag);
			L4_MapItem_t map = { };
			handle_pf(faddr, fip, &map);
			L4_LoadMR(0, (L4_MsgTag_t){ .X.t = 2 }.raw);
			L4_LoadMRs(1, 2, map.raw);
			L4_MsgTag_t r = L4_Reply(from);
			if(L4_IpcFailed(r)) {
				printf("%s: reply to intervening pf failed, ec=%#lx\n",
					__func__, L4_ErrorCode());
				return;
			}
		} else if(tag.X.label != FORKSERV_SEND_PAGE_2) {
			printf("%s: wrong SEND_PAGE_2 label=%#x\n", __func__, tag.X.label);
			return;
		}
	} while(tag.X.label != FORKSERV_SEND_PAGE_2);

	L4_MapItem_t mi;
	L4_StoreMRs(1, 2, mi.raw);
#if 0
	printf("got page %#lx:%#lx, offset %#lx, phys_addr %d:%#lx, local %#lx\n",
		L4_Address(L4_MapItemSndFpage(mi)), L4_Size(L4_MapItemSndFpage(mi)),
		L4_MapItemSndBase(mi), space_id, phys_addr, L4_Address(window));
#endif
	if(space_id != 0) map_range_pos += L4_Size(window);

	struct fs_phys_page *phys = malloc(sizeof(*phys));
	*phys = (struct fs_phys_page){
		.local_addr = L4_Address(window),
		.refcount = 1,
	};
	struct fs_vpage *p = kmem_cache_alloc(vpage_slab);
	*p = (struct fs_vpage){
		.address = phys_addr,
		.page = phys,
		.access = L4_FullyAccessible,
	};
	assert(htable_get(&sp->pages, int_hash(p->address),
		&word_cmp, &p->address) == NULL);
	htable_add(&sp->pages, int_hash(p->address), &p->address);

	/* this replies to the SEND_PAGE_2 IPC. */
	L4_LoadMR(0, 0);
	L4_Reply(from);
}


/* it'd be nice if this part could communicate with the outside world without
 * a pager thread in testbench. such as via a serial port.
 */
static void handle_pf(
	L4_Word_t addr,
	L4_Word_t ip,
	L4_MapItem_t *page_ptr)
{
	assert(invariants("pf"));

	L4_MsgTag_t tag = muidl_get_tag();
	L4_Word_t fault_access = tag.X.label & 0xf;
#if 0
	L4_ThreadId_t sender = muidl_get_sender();
	printf("forkserv: pf [%c%c%c] in %lu:%lu (ip %#lx, addr %#lx)\n",
		CHECK_FLAG(fault_access, L4_Readable) ? 'r' : '-',
		CHECK_FLAG(fault_access, L4_Writable) ? 'w' : '-',
		CHECK_FLAG(fault_access, L4_eXecutable) ? 'x' : '-',
		L4_ThreadNo(sender), L4_Version(sender),
		ip, addr);
#endif

	static int rep_count = 0;
	static L4_Word_t last_addr = 0;
	if(last_addr == addr && ++rep_count == 10) {
		printf("*** all work and no play makes jack a dull boy\n");
		rep_count = 0;
		last_addr = 0;
		abort();		/* NOTE: this is useful for debugging. */
	} else if(last_addr != addr) {
		rep_count = 0;
		last_addr = addr;
	}

	L4_ThreadId_t from = muidl_get_sender();
	struct fs_space *sp = get_space_by_tid(from);
	if(sp == NULL) {
		printf("source %#lx isn't known\n", from.raw);
		goto no_reply;
	}

	const bool in_special = ADDR_IN_FPAGE(sp->utcb_area, addr)
		|| ADDR_IN_FPAGE(sp->kip_area, addr);
	L4_Word_t page_addr = addr & ~PAGE_MASK;
	struct fs_vpage *page = htable_get(&sp->pages,
		int_hash(page_addr), &word_cmp, &page_addr);
	if(page == NULL && page_addr > 0xf000 && page_addr < sp->prog_brk
		&& !in_special)
	{
		/* moar ramz pls */
		page = kmem_cache_alloc(vpage_slab);
		page->address = page_addr;
		page->page = alloc_new_page();
		page->access = L4_FullyAccessible;
#if 0
		printf("%s: new page %#lx -> %#lx (brk=%#lx)\n", __func__,
			page->page->local_addr, page->address, sp->prog_brk);
#endif
		htable_add(&sp->pages, int_hash(page_addr), &page->address);
	} else if(page == NULL || page_addr > max_vaddr || in_special) {
		/* illegal address, or some such. the print should be retained for
		 * debugging.
		 */
		if(page_addr > max_vaddr) {
			printf("illegal access to addr=%#lx from ip=%#lx\n", addr, ip);
		} else {
			printf("segfault in thread %lu:%lu, space %lu (brk %#lx)\n",
				L4_ThreadNo(from), L4_Version(from), sp->id, sp->prog_brk);
			printf("  addr %#lx, ip %#lx\n", addr, ip);
		}
		int n;
		if(!L4_IsNilThread(sp->mgr_tid)
			&& !L4_SameThreads(from, sp->mgr_tid))
		{
			/* we'll be somewhat more persistent with the root task, because
			 * it cannot reasonably be killed. (it can end up in a pager loop
			 * though, so until the helper thread starts sending these things
			 * a finite send-timeout will serve as band-aid adequately.)
			 */
			n = __tmgr_segv_timeout(sp->mgr_tid, from.raw, addr,
				sp->id == root_space_id ? L4_TimePeriod(200 * 1000)
					: L4_ZeroTime);
		} else {
			n = -1;
		}
		if(n != 0) {
			/* mgr_tid not found, or cannot be contacted; kill the
			 * process instead.
			 *
			 * FIXME: this may happen more often than necessary when the
			 * manager thread's program or stack isn't mapped to the extent
			 * required for it to return to the ipc receive bit. that can be
			 * alleviated by making the manager thread always have the largest
			 * priority in its process.
			 *
			 * NOTE: for now, low 4 bits of status is 7 for a segfault. its
			 * faulting address is delivered in the remaining bits, sans the
			 * low four.
			 */
			handle_exit((addr & ~15) | 7);
		}
		goto no_reply;
	} else if(page->page->refcount == 1
		&& !CHECK_FLAG_ALL(page->access, fault_access))
	{
		/* always give access. */
#if 0
		printf("%s: expand access at %#lx: old %#x -> new %#x\n", __func__,
			page->address, (unsigned)page->access,
			(unsigned)(page->access | fault_access));
#endif
		page->access |= fault_access;
	} else if(page->page->refcount > 1
		&& CHECK_FLAG(fault_access, L4_Writable))
	{
		/* duplicate, drop reference, and remap on top of the old page. */
#if 0
		printf("%s: duplicating rc %d phys page %#lx -> %#lx\n",
			__func__, page->page->refcount, page->page->local_addr,
			page_addr);
#endif
		assert(!CHECK_FLAG(page->access, L4_Writable));
		struct fs_phys_page *copy = alloc_new_page();
		memcpy((void *)copy->local_addr, (void *)page->page->local_addr,
			PAGE_SIZE);
		page->page->refcount--;
		page->page = copy;
		page->access |= L4_Writable;
	} else {
#if 0
		printf("%s: remap old page %#lx -> %#lx (access %#x)\n", __func__,
			page->page->local_addr, page_addr, (unsigned)page->access);
#endif
	}

	L4_Fpage_t fp = L4_FpageLog2(page->page->local_addr, PAGE_BITS);
	L4_Set_Rights(&fp, page->access & fault_access);
	*page_ptr = L4_MapItem(fp, page->address);
	return;

no_reply:
	muidl_raise_no_reply();
}


/* FIXME: threads added like this have an implicit UTCB allocated for them, but
 * use some other UTCB address configured by their actual creator. this may
 * cause fuckups when forkserv's thread creation function is called from the
 * original testbench space.
 */
static void handle_add_tid(int32_t space_id, L4_Word_t raw_tid)
{
	const L4_ThreadId_t tid = { .raw = raw_tid };
	struct fs_space *sp = get_space(space_id);
	if(sp == NULL) sp = make_initial_space(space_id);

	size_t tno_hash = int_hash(L4_ThreadNo(tid));
	struct fs_thread *t;

	/* just find the matching thread number for overwrite. */
	struct htable_iter it;
	for(t = htable_firstval(&thread_hash, &it, tno_hash);
		t != NULL;
		t = htable_nextval(&thread_hash, &it, tno_hash))
	{
		if(L4_ThreadNo(t->tid) == L4_ThreadNo(tid)) break;
	}

	if(t != NULL) {
		t->tid = tid;
		t->space = sp;
		if(t->tid.raw != tid.raw) {
			htable_delval(&thread_hash, &it);
			t->tid = tid;
			htable_add(&thread_hash, int_hash(tid.raw), &t->tid.raw);
		}
	} else {
		t = malloc(sizeof(struct fs_thread));
		*t = (struct fs_thread){ .tid = tid, .space = sp };
		htable_add(&thread_hash, tno_hash, &t->tid);

		int slot;
		for(slot = THREADS_PER_SPACE - 1; slot >= 0; --slot) {
			if(t->space->threads[slot] == NULL) break;
		}
		if(slot < 0) {
			printf("%s: no slots left in space %u!\n", __func__, space_id);
			abort();	/* breaks unit testing, which is OK */
		}
		t->space->threads[slot] = t;
	}
}


static void handle_sbrk(
	L4_Word_t *start_p,
	int32_t increment)
{
	const L4_ThreadId_t from = muidl_get_sender();
	struct fs_space *sp = get_space_by_tid(from);
	if(sp == NULL) {
		printf("%s: can't find for tid %#lx's fs_space\n", __func__, from.raw);
		*start_p = 0;
		return;
	}

	if(increment >= 0) {
		*start_p = sp->prog_brk;
		if(increment > 0) {
			sp->prog_brk += increment + PAGE_SIZE - 1;
			sp->prog_brk &= ~PAGE_MASK;
		}
	} else {
		sp->prog_brk += increment;
		sp->prog_brk &= ~PAGE_MASK;
		*start_p = sp->prog_brk;
	}
}


static void handle_get_utcb_area(L4_Fpage_t *output)
{
	struct fs_space *sp = get_space_by_tid(muidl_get_sender());
	*output = sp != NULL ? sp->utcb_area : L4_Nilpage;
}


/* returns 1 on success, 0 on syscall failure, and L4_IpcFailed(*tag_out),
 * L4_ErrorCode() on ipc failure.
 */
static L4_Word_t fpager_threadctl(
	L4_MsgTag_t *tag_out,
	L4_Word_t *ec_out,
	L4_ThreadId_t dest,
	L4_ThreadId_t space,
	L4_ThreadId_t scheduler,
	L4_ThreadId_t pager,
	void *utcb_location)
{
	L4_LoadBR(0, 0);
	L4_LoadMR(0, (L4_MsgTag_t){ .X.label = FPAGER_THREADCTL, .X.u = 5 }.raw);
	L4_LoadMR(1, dest.raw);
	L4_LoadMR(2, space.raw);
	L4_LoadMR(3, scheduler.raw);
	L4_LoadMR(4, pager.raw);
	L4_LoadMR(5, (L4_Word_t)utcb_location);
	*tag_out = L4_Call(fpager_tid);
	if(L4_IpcFailed(*tag_out)) return 2;
	if(tag_out->X.u != 2 || tag_out->X.t != 0) {
		printf("%s: weird reply tag; u %d, t %d, label %#lx\n", __func__,
			tag_out->X.u, tag_out->X.t, (L4_Word_t)tag_out->X.label);
	}

	L4_Word_t retval;
	L4_StoreMR(1, &retval);
	L4_StoreMR(2, ec_out);
	return retval;
}


/* see comment above fpager_threadctl() */
static L4_Word_t fpager_spacectl(
	L4_MsgTag_t *tag_out,
	L4_Word_t *ec_out,
	L4_ThreadId_t spacespec,
	L4_Word_t control,
	L4_Fpage_t kip_area,
	L4_Fpage_t utcb_area,
	L4_ThreadId_t redirector,
	L4_Word_t *ctl_out)
{
	L4_LoadBR(0, 0);
	L4_LoadMR(0, (L4_MsgTag_t){ .X.label = FPAGER_SPACECTL, .X.u = 5 }.raw);
	L4_LoadMR(1, spacespec.raw);
	L4_LoadMR(2, control);
	L4_LoadMR(3, kip_area.raw);
	L4_LoadMR(4, utcb_area.raw);
	L4_LoadMR(5, redirector.raw);
	*tag_out = L4_Call(fpager_tid);
	if(L4_IpcFailed(*tag_out)) return 2;
	if(tag_out->X.u != 3 || tag_out->X.t != 0) {
		printf("%s: weird reply tag; u %d, t %d, label %#lx\n", __func__,
			tag_out->X.u, tag_out->X.t, (L4_Word_t)tag_out->X.label);
	}

	L4_Word_t retval;
	L4_StoreMR(1, &retval);
	L4_StoreMR(2, ec_out);
	L4_StoreMR(3, ctl_out);
	return retval;
}


/* (this function just plain doesn't clean up on failure.) */
static bool handle_new_thread(struct helper_work *w)
{
	/* (adaptation from previous function parameters, since moved into
	 * helper_work proper.)
	 */
	L4_ThreadId_t from = w->from;
	L4_Word_t space_id = w->space_id;
	L4_Word_t instptr = w->ip, stkptr = w->sp;
	int req_threadnum = w->req_tnum;

	const char *what;

	struct fs_space *sp;
	if(space_id == ~(L4_Word_t)0) {
		sp = get_space_by_tid(from);
		assert(sp != NULL);
		space_id = sp->id;
	} else {
		sp = get_space(space_id);
		if(sp == NULL) {
			printf("%s: space %lu not found\n", __func__, space_id);
			return false;
		}
	}

	/* forkserv's initial pager will do privileged operations on forkserv's
	 * behalf.
	 */
	int t;
	if(req_threadnum >= 0 && req_threadnum < THREADS_PER_SPACE
		&& sp->threads[req_threadnum] == NULL)
	{
		t = req_threadnum;
	} else {
		for(t = 0; t < THREADS_PER_SPACE; t++) {
			if(sp->threads[t] == NULL) break;
		}
		if(t >= THREADS_PER_SPACE) goto fail;
	}

	L4_ThreadId_t new_tid = L4_GlobalId((space_id << TPS_SHIFT) + t, space_id + 1);
	if(L4_IsLocalId(new_tid)) {
		new_tid.global.X.version++;
		assert(L4_IsGlobalId(new_tid));
	}
	L4_ThreadId_t space_tid = new_tid;
	for(int i=0; i < THREADS_PER_SPACE; i++) {
		if(sp->threads[i] != NULL) {
			assert(!L4_IsNilThread(sp->threads[i]->tid));
			space_tid = sp->threads[i]->tid;
			break;
		}
	}

	sp->threads[t] = malloc(sizeof(struct fs_thread));
	*sp->threads[t] = (struct fs_thread){ .space = sp, .tid = new_tid };
	htable_add(&thread_hash, int_hash(L4_ThreadNo(new_tid)),
		&sp->threads[t]->tid);

	L4_MsgTag_t tag;
	L4_Word_t syscall_ec, retval = fpager_threadctl(&tag, &syscall_ec,
		new_tid, space_tid, L4_Myself(), L4_Myself(), (void *)-1);
	if(retval != 1) goto tc_fail;

	if(space_tid.raw == new_tid.raw) {
		/* also initialize the new address space. */
		struct fs_space *parent_sp = get_space(sp->parent_id);
		assert(parent_sp != NULL);
		L4_ThreadId_t redir_tid = parent_sp->child_redir_tid;

		L4_Word_t ctl_out;
		retval = fpager_spacectl(&tag, &syscall_ec,
			space_tid, 0, sp->kip_area, sp->utcb_area, redir_tid,
			&ctl_out);
		if(retval != 1) goto sc_fail;
	}

	/* set a lower priority so that the thread doesn't start up and cause
	 * pagefaults before the second ThreadControl changes its pager TCR.
	 *
	 * FIXME: is this at all necessary? threads shouldn't become activated
	 * until they have both a non-nil pager, and a valid UTCB address, and
	 * pagefaults only start happening after breath-of-life.
	 */
	L4_Word_t timectl_out;
	retval = L4_Schedule(new_tid, ~0ul, ~0ul, 80, ~0ul, &timectl_out);
	if(retval == L4_SCHEDRESULT_ERROR) {
		printf("%s: L4_Schedule() failed: ec %#lx\n", __func__,
			L4_ErrorCode());
		abort();
	}

	retval = fpager_threadctl(&tag, &syscall_ec,
		new_tid, space_tid, L4_nilthread, L4_nilthread,
		(void *)L4_Address(sp->utcb_area) + t * UTCB_SIZE);
	if(retval != 1) goto tc_fail;

	/* finally, a breath-of-life IPC.
	 *
	 * TODO: strictly speaking this should be sent via forkserv_tid.
	 * FIXME: make it propagate, once propagation starts working. (also set
	 * the correct pager right away.)
	 */
	L4_LoadMR(0, (L4_MsgTag_t){ .X.u = 2 }.raw);
	L4_LoadMR(1, instptr);
	L4_LoadMR(2, stkptr);
	tag = L4_Send(new_tid);
	if(L4_IpcFailed(tag)) {
		printf("%s: breath of life to %lu:%lu failed, ec %#lx\n",
			__func__, L4_ThreadNo(new_tid), L4_Version(new_tid),
			L4_ErrorCode());
		abort();
	}

	/* and a final pager switch. */
	retval = fpager_threadctl(&tag, &syscall_ec,
		new_tid, space_tid, forkserv_tid, forkserv_tid, (void *)-1);
	if(retval != 1) {
		printf("%s: pager reset failed, ec %#lx\n", __func__,
			L4_ErrorCode());
		abort();
	}

	L4_LoadMR(0, (L4_MsgTag_t){ .X.u = 1 }.raw);
	L4_LoadMR(1, new_tid.raw);
	return true;

tc_fail:
	what = "threadctl";
	goto syscall_fail;

sc_fail:
	what = "spacectl";

syscall_fail:
	if(sp->threads[t] != NULL) {
		htable_del(&thread_hash, int_hash(L4_ThreadNo(new_tid)),
			&sp->threads[t]->tid);
		free(sp->threads[t]);
		sp->threads[t] = NULL;
	}
	if(retval == 2) {
		printf("%s: ipc fail in %s: ec %#lx\n", __func__, what,
			L4_ErrorCode());
	} else if(retval == 0) {
		printf("%s: %s fail, ec %#lx\n", __func__, what, syscall_ec);
	} else {
		printf("%s: odd retval %lu\n", __func__, retval);
		assert(false);
	}

fail:
	L4_LoadMR(0, (L4_MsgTag_t){ .X.u = 1 }.raw);
	L4_LoadMR(1, L4_nilthread.raw);
	return true;
}


static int32_t handle_fork(void)
{
	const L4_ThreadId_t from = muidl_get_sender();
	L4_Word_t copy_id;

	struct fs_space *sp = get_space_by_tid(from);
	if(sp == NULL) {
		printf("%s: can't find fs_space of tid %#lx\n", __func__, from.raw);
		copy_id = ~(L4_Word_t)0;
		goto end;
	}

	do {
		copy_id = next_space_id++;
	} while(htable_get(&space_hash, int_hash(copy_id), &word_cmp,
		&copy_id) != NULL);
	struct fs_space *copy_space = malloc(sizeof(*copy_space));
	for(int i=0; i < THREADS_PER_SPACE; i++) copy_space->threads[i] = NULL;
	copy_space->id = copy_id;
	copy_space->parent_id = sp->id;
	copy_space->prog_brk = sp->prog_brk;
	copy_space->utcb_area = sp->utcb_area;
	copy_space->kip_area = sp->kip_area;
	copy_space->mgr_tid = L4_nilthread;
	copy_space->child_redir_tid = sp->child_redir_tid;
	GUARD_INIT(copy_space, g_0);
	GUARD_INIT(copy_space, g_1);
	GUARD_INIT(copy_space, g_2);
	GUARD_INIT(copy_space, g_3);
	list_head_init(&copy_space->dead_children);
	list_head_init(&copy_space->waiting_threads);
	htable_init(&copy_space->pages, &hash_word, NULL);
	htable_add(&space_hash, int_hash(copy_space->id), &copy_space->id);

	/* do the thing. with the pages and so on. */
	L4_Fpage_t unmapbuf[64];
	int num_unmap = 0;
	struct htable_iter it;
	for(struct fs_vpage *ovp = htable_first(&sp->pages, &it);
		ovp != NULL;
		ovp = htable_next(&sp->pages, &it))
	{
		struct fs_vpage *cvp = kmem_cache_alloc(vpage_slab);
		cvp->address = ovp->address;
		cvp->access = L4_Readable;
		cvp->page = ovp->page;
		cvp->page->refcount++;
		ovp->access = L4_Readable;
		htable_add(&copy_space->pages, int_hash(cvp->address),
			&cvp->address);

		unmapbuf[num_unmap] = L4_FpageLog2(
			ovp->page->local_addr, PAGE_BITS);
		/* FIXME: setting this to L4_FullyAccessible (which isn't necessary
		 * here) makes SelfThreUnca:3 blow up with an inconsistent
		 * mapdb/pagetable fail. but not if DEBUG_ME_HARDER is set.
		 */
		L4_Set_Rights(&unmapbuf[num_unmap], L4_Writable);
		num_unmap++;

		if(num_unmap == 64) {
			L4_UnmapFpages(64, unmapbuf);
			num_unmap = 0;
		}
	}
	if(num_unmap > 0) L4_UnmapFpages(num_unmap, unmapbuf);
	assert(!CHECK_FLAG(copy_id, 0x80000000ul));

end:
	return copy_id;
}


static void handle_set_fork_redir(L4_Word_t *prev_tid, L4_Word_t next_tid)
{
	struct fs_thread *t = get_thread(muidl_get_sender());
	if(t == NULL) goto fail;

	L4_ThreadId_t next = { .raw = next_tid };
	*prev_tid = t->space->child_redir_tid.raw;
	if(!L4_IsNilThread(next)) {
		/* NOTE: this allows escape from the redirector hierarchy. that's
		 * fine, testbench isn't supposed to guarantee anything. if such
		 * criteria are needed, add a predicate that examines the redirector
		 * hierarchy in the inner conditional.
		 */
		if(next.raw != L4_anythread.raw) {
			struct fs_thread *rd = get_thread(next);
			if(rd == NULL) goto fail;
		}
		t->space->child_redir_tid = next;
	}

	return;

fail:
	*prev_tid = L4_nilthread.raw;
}


static void handle_set_mgr_tid(L4_Word_t arg_tid)
{
	struct fs_thread *t = get_thread(muidl_get_sender());
	if(t == NULL) return;

	assert(t->space != NULL);
	t->space->mgr_tid.raw = arg_tid;
}


static void handle_as_cfg(
	int32_t space_id,
	L4_Word_t roottask_mgr_tid,
	L4_Fpage_t kip_area,
	L4_Fpage_t utcb_area)
{
	assert(root_space_id == -1 || root_space_id == space_id);
	root_space_id = space_id;

	struct fs_space *sp = get_space(space_id);
	if(sp == NULL) sp = make_initial_space(space_id);
	if(!L4_IsNilFpage(kip_area)) sp->kip_area = kip_area;
	if(!L4_IsNilFpage(utcb_area)) sp->utcb_area = utcb_area;
	L4_ThreadId_t mgr_tid = { .raw = roottask_mgr_tid };
	if(!L4_IsNilThread(mgr_tid)) {
		bool found = false;
		for(int i=0; i < THREADS_PER_SPACE; i++) {
			if(sp->threads[i] == NULL) continue;
			if(L4_SameThreads(sp->threads[i]->tid, mgr_tid)) {
				found = true;
				break;
			}
		}
		if(!found) {
			printf("%s: mgr_tid %lu:%lu not in caller's space\n",
				__func__, L4_ThreadNo(mgr_tid), L4_Version(mgr_tid));
		} else {
			sp->mgr_tid = mgr_tid;
		}
	}

#if 1
	const L4_ThreadId_t from = muidl_get_sender();
	printf("%s: from %#lx, space_id %d, kip_area %#lx:%#lx, utcb_area %#lx:%#lx\n",
		__func__, from.raw, space_id, L4_Address(kip_area), L4_Size(kip_area),
		L4_Address(utcb_area), L4_Size(utcb_area));
	printf("  ... mgr_tid %lu:%lu\n",
		L4_ThreadNo(mgr_tid), L4_Version(mgr_tid));
#endif
}


static void handle_unmap(
	const L4_Fpage_t *pages,
	unsigned pages_len,
	L4_Fpage_t *output,
	unsigned *output_len)
{
	L4_ThreadId_t from = muidl_get_sender();
	struct fs_space *sp = get_space_by_tid(from);
	if(sp == NULL) {
		printf("%s: unknown space for %#lx\n", __func__, from.raw);
		return;
	}

	/* holy nested loops, batman! */
	L4_Fpage_t local[63];
	int outpos = 0;
	for(int i=0, in_len = pages_len; i < in_len && outpos < 63; i++) {
		for(L4_Word_t fp_addr = L4_Address(pages[i]),
					  fp_max = fp_addr + L4_Size(pages[i]);
			fp_addr < fp_max && outpos < 63;
			fp_addr += PAGE_SIZE)
		{
			struct fs_vpage *vp = htable_get(&sp->pages,
				int_hash(fp_addr), &word_cmp, &fp_addr);
			if(vp == NULL) continue;

			local[outpos] = L4_FpageLog2(vp->page->local_addr, 12);
			output[outpos] = L4_FpageLog2(fp_addr, 12);
			L4_Set_Rights(&local[outpos], L4_Rights(pages[i]));
			outpos++;
		}
	}
	L4_UnmapFpages(outpos, local);
	for(int i=0; i < outpos; i++) {
		L4_Set_Rights(&output[outpos], L4_Rights(local[outpos]));
	}
	*output_len = outpos;
}


/* POSIX reparents children to PID1. since there's no PID1 in the testbench
 * personality, any dead children waiting to get reaped by @sp will be
 * recursively destroyed.
 */
static void destroy_space(struct fs_space *sp)
{
	/* find children, reset parent_id. */
	struct htable_iter it;
	for(void *space_ptr = htable_first(&space_hash, &it);
		space_ptr != NULL;
		space_ptr = htable_next(&space_hash, &it))
	{
		struct fs_space *child = container_of(space_ptr, struct fs_space, id);
		if(child->parent_id != sp->id) continue;
		child->parent_id = 0;
	}

	/* destroy dead children. */
	struct fs_space *cur, *next;
	list_for_each_safe(&sp->dead_children, cur, next, dead_link) {
		list_del(&cur->dead_link);
		destroy_space(cur);
	}
	assert(list_empty(&sp->dead_children));
	free(sp);
}


static int32_t handle_wait(int32_t *status_ptr)
{
	L4_ThreadId_t ipc_from = muidl_get_sender();
	struct fs_space *sp = get_space_by_tid(ipc_from);
	if(sp == NULL) {
		printf("forkserv: don't know space for %lu:%lu\n",
			L4_ThreadNo(ipc_from), L4_Version(ipc_from));
		return -1;
	}

	struct fs_space *dead = list_top(&sp->dead_children,
		struct fs_space, dead_link);
	if(dead == NULL) {
		struct fs_thread *t = get_thread(ipc_from);
		list_add_tail(&sp->waiting_threads, &t->wait_link);
		muidl_raise_no_reply();
		return -666;
	} else {
		int id = dead->id;
		*status_ptr = dead->exit_status;
		list_del_from(&sp->dead_children, &dead->dead_link);
		destroy_space(dead);

		return id;
	}
}


static int32_t handle_getpid(void)
{
	struct fs_space *sp = get_space_by_tid(muidl_get_sender());
	return sp == NULL ? -1 : sp->id;
}


static bool end_thread(L4_ThreadId_t tid)
{
	struct fs_thread *t = get_thread(tid);
	if(t == NULL) return false;

	struct fs_space *sp = t->space;
	bool found = false;
	for(int i=0; i < THREADS_PER_SPACE; i++) {
		if(sp->threads[i] == t) {
			assert(!found);		/* must only occur once. */
			sp->threads[i] = NULL;
			found = true;
		}
	}
	if(!found) {
		printf("tid=%lu:%lu wasn't found\n",
			L4_ThreadNo(tid), L4_Version(tid));
		abort();
	}

	L4_MsgTag_t tag;
	L4_Word_t ec = 0, res = fpager_threadctl(&tag, &ec, tid, L4_nilthread,
		L4_nilthread, L4_nilthread, (void *)-1);
	if(res != 1) {
		printf("forkserv/end_thread: threadctl failed, %s ec %#lx\n",
			L4_IpcFailed(tag) ? "ipc" : "syscall", ec);
		/* can't do much else. */
	}
	htable_del(&thread_hash, int_hash(L4_ThreadNo(t->tid)), &t->tid);
	free(t);

	return true;
}


static void handle_exit(int32_t status)
{
	L4_ThreadId_t ipc_from = muidl_get_sender();
	struct fs_space *sp = get_space_by_tid(ipc_from);
	if(sp == NULL) {
		printf("forkserv/exit: unknown TID %lu:%lu\n",
			L4_ThreadNo(ipc_from), L4_Version(ipc_from));
		return;
	}
	for(int i=0; i < THREADS_PER_SPACE; i++) {
		if(sp->threads[i] != NULL) end_thread(sp->threads[i]->tid);
	}

	/* destroy the virtual memory bits and zombify the address space */
	assert(list_empty(&sp->waiting_threads));
	htable_del(&space_hash, int_hash(sp->id), &sp->id);
	struct htable_iter it;
	for(struct fs_vpage *vp = htable_first(&sp->pages, &it);
		vp != NULL;
		vp = htable_next(&sp->pages, &it))
	{
		if(vp->page != NULL) {
			if(--vp->page->refcount == 0) {
				list_add(&free_page_list, &vp->page->link);
				vp->page = NULL;
			}
		}
		kmem_cache_free(vpage_slab, vp);
	}
	htable_clear(&sp->pages);
	sp->utcb_area = L4_Nilpage;
	L4_Word_t dead_id = sp->id;

	/* activate waiting thread, or make it immediately reapable (when there's
	 * no waiter)
	 */
	struct fs_space *parent = get_space(sp->parent_id);
	assert(parent != NULL);
	assert(parent->id == sp->parent_id);
	assert(sp != parent);		/* space 0 cannot die */
	struct fs_thread *wakeup = list_top(&parent->waiting_threads,
		struct fs_thread, wait_link);
	if(wakeup != NULL) {
		list_del_from(&parent->waiting_threads, &wakeup->wait_link);
		/* move own dead children over to parent */
		destroy_space(sp);

		L4_LoadMR(0, (L4_MsgTag_t){ .X.u = 2 }.raw);
		L4_LoadMR(1, dead_id);
		L4_LoadMR(2, status);
		L4_MsgTag_t tag = L4_Reply(wakeup->tid);
		if(L4_IpcFailed(tag)) {
			printf("forkserv: can't send wakeup, ec=%#lx\n",
				L4_ErrorCode());
		}
	} else {
		/* the dead walk */
		list_add_tail(&parent->dead_children, &sp->dead_link);
		sp->exit_status = status;
	}

	muidl_raise_no_reply();
}


static void handle_exit_thread(L4_Word_t dest_raw)
{
	L4_ThreadId_t dest_tid = { .raw = dest_raw },
		from = muidl_get_sender();
	if(dest_tid.raw == L4_nilthread.raw) dest_tid = from;

	struct fs_space *sp = get_space_by_tid(dest_tid);
	if(sp == NULL || !end_thread(dest_tid)) return;

	/* check if this was the last thread. */
	bool is_last = true;
	for(int i=0; i < THREADS_PER_SPACE; i++) {
		if(sp->threads[i] != NULL) {
			is_last = false;
			break;
		}
	}

	if(is_last && dest_tid.raw == from.raw) handle_exit(0);
	else if(is_last) {
		/* would do exit of a process via exit_thread() from a foreign address
		 * space. which is weird.
		 */
		printf("forkserv: weird exit_thread case not handled\n");
	}

	if(dest_tid.raw == from.raw) muidl_raise_no_reply();
}


static L4_Word_t forward_new_thread(
	int32_t space_id,
	L4_Word_t ip,
	L4_Word_t sp,
	int32_t req_tnum,
	L4_Time_t ts_len,
	L4_Time_t total_quantum,
	uint8_t priority,
	uint8_t sens_pri,
	int32_t max_delay)
{
	assert(invariants("new_thread"));
	const L4_ThreadId_t from = muidl_get_sender();
	const L4_MsgTag_t tag = muidl_get_tag();

	/* pass it to the queued handler */
	L4_Word_t aid = next_async_id++;
	struct async_op *op = malloc(sizeof(*op));
	op->id = aid;
	op->from = from;
	htable_add(&async_hash, int_hash(op->id), &op->id);
	struct helper_work *w = malloc(sizeof(struct helper_work));
	*w = (struct helper_work){
		.from = from, .aid = aid,
		.ip = ip, .sp = sp,
		.space_id = space_id, .req_tnum = req_tnum, .max_delay = max_delay,
		.ts_len = ts_len, .total_quantum = total_quantum,
		.priority = priority, .sens_pri = sens_pri,
	};
	L4_LoadMR(0, (L4_MsgTag_t){ .X.u = 1, .X.label = FSHELPER_CHOPCHOP }.raw);
	L4_LoadMR(1, (L4_Word_t)w);
	L4_MsgTag_t pass_tag = L4_Send_Timeout(helper_tid, L4_ZeroTime);
	if(L4_IpcFailed(pass_tag) && L4_ErrorCode() == 2) {
		do {
			w->next = *(struct helper_work *volatile *)&helper_queue;
		} while(!__sync_bool_compare_and_swap(&helper_queue, w->next, w));

		L4_Time_t x, y;
		L4_Word_t sr = L4_Timeslice(helper_tid, &x, &y);
		printf("forkserv: deferred call; helper=%lu:%lu, sr=%lu\n",
			L4_ThreadNo(helper_tid), L4_Version(helper_tid), sr);
	} else if(L4_IpcFailed(tag)) {
		printf("helper_tid call failed: ec %#lx\n",
			L4_ErrorCode());
		free(w);
	} else {
		assert(L4_IpcSucceeded(tag));
	}

	muidl_raise_no_reply();	/* replied by the chopchop thread. */
	return L4_nilthread.raw;
}


static void handle_send_bol(
	L4_Word_t dest_raw,
	L4_Word_t ip,
	L4_Word_t sp)
{
	L4_ThreadId_t dest = { .raw = dest_raw };
#if 0
	printf("forkserv: %s: dest=%lu:%lu, ip=%#lx, sp=%#lx\n", __func__,
		L4_ThreadNo(dest), L4_Version(dest), ip, sp);
#endif
	if(L4_IsLocalId(dest)) {
		printf("forkserv: %s: can't handle a local TID\n", __func__);
		return;
	}

	struct fs_thread *t = get_thread(dest);
	if(t == NULL) {
		printf("forkserv: %s: unknown TID %lu:%lu\n", __func__,
			L4_ThreadNo(dest), L4_Version(dest));
		return;
	}

	L4_LoadMR(0, (L4_MsgTag_t){ .X.u = 2 }.raw);
	L4_LoadMR(1, ip);
	L4_LoadMR(2, sp);
	L4_MsgTag_t tag = L4_Send(dest);
	if(L4_IpcFailed(tag)) {
		printf("forkserv: %s: BOL failed, ec=%#lx\n", __func__,
			L4_ErrorCode());
		return;
	}
}


/* (not quite certain if this'll work with just a single thread. more RAM can
 * be had from sigma0, though, so there's no dependency there.)
 */
static void forkserv_dispatch_loop(void)
{
	static const struct fork_serv_vtable vtable = {
		.handle_fault = &handle_pf,
		.as_cfg = &handle_as_cfg,
		.set_mgr_tid = &handle_set_mgr_tid,
		.send_page = &handle_send_page,
		.add_tid = &handle_add_tid,
		.fork = &handle_fork,
		.set_fork_redir = &handle_set_fork_redir,
		.sbrk = &handle_sbrk,
		.get_utcb_area = &handle_get_utcb_area,
		.exit_thread = &handle_exit_thread,
		.exit = &handle_exit,
		.wait = &handle_wait,
		.unmap = &handle_unmap,
		.getpid = &handle_getpid,
		.new_thread = &forward_new_thread,	/* this one is a bit special. */
		.send_bol = &handle_send_bol,
	};

	for(;;) {
		L4_Word_t status = _muidl_fork_serv_dispatch(&vtable);
		if(status == MUIDL_UNKNOWN_LABEL) {
			L4_ThreadId_t sender = muidl_get_sender();
			L4_MsgTag_t tag = muidl_get_tag();
			switch(tag.X.label) {
				case FSHELPER_CHOPCHOP_DONE:
					/* check the queue for our armless friend. */
					assert(invariants("chopchop_done"));
					if(helper_queue != NULL) {
						L4_LoadMR(0, (L4_MsgTag_t){
								.X.u = 1, .X.label = FSHELPER_CHOPCHOP,
							}.raw);
						L4_LoadMR(1, 0);        /* byoq */
						L4_Reply(helper_tid);
					}
					break;

				case FSHELPER_CHOPCHOP_REPLY: {
					L4_Word_t async_id;
					L4_MsgTag_t reply_tag;
					L4_StoreMR(1, &async_id);
					L4_StoreMR(2, &reply_tag.raw);
					L4_Word_t words[62];
					int n_words = reply_tag.X.u + reply_tag.X.t;
					L4_StoreMRs(3, n_words, words);
					assert(invariants("chopchop_reply"));
					size_t hash = int_hash(async_id);
					struct async_op *op = htable_get(&async_hash, hash,
						&word_cmp, &async_id);
					if(op == NULL) {
						printf("%s: unknown async op %#lx\n", __func__,
							async_id);
						break;
					}
					htable_del(&async_hash, hash, op);
					L4_ThreadId_t from = op->from;
					free(op);
					L4_LoadMR(0, reply_tag.raw);
					L4_LoadMRs(1, n_words, words);
					L4_MsgTag_t tag = L4_Reply(from);
					if(L4_IpcFailed(tag)) {
						printf("%s: failed reply to %lu:%lu\n", __func__,
							L4_ThreadNo(from), L4_Version(from));
					}
					break;
				}

				default:
					printf("forkserv: unknown label %#x from %lu:%lu\n",
						muidl_get_tag().X.label, L4_ThreadNo(sender),
						L4_Version(sender));
			}
		} else if(status != 0 && !MUIDL_IS_L4_ERROR(status)) {
			printf("forkserv: dispatch status %#lx (last tag %#lx)\n",
				status, muidl_get_tag().raw);
		}
	}
}


static struct helper_work *reverse_work(struct helper_work *w)
{
	struct helper_work *prev = NULL;
	while(w != NULL) {
		struct helper_work *next = w->next;
		w->next = prev;
		prev = w;
		w = next;
	}

	return prev;
}


static void helper_chopchop(L4_ThreadId_t reply_tid, struct helper_work *w)
{
	const L4_Word_t async_id = w->aid;
	bool reply = handle_new_thread(w);

	if(reply) {
		/* this looks funny, and it sort of is. it essentially snarfs up the
		 * IPC reply for handle_new_thread(), and resends it via the main
		 * forkserv dispatcher thread.
		 *
		 * TODO: do this with propagation, once mung starts doing that, too.
		 */
		L4_MsgTag_t tag;
		L4_Word_t tmp[64];
		L4_StoreMR(0, &tag.raw);
		int n_words = tag.X.u + tag.X.t;
		if(n_words > 62) {
			n_words = 62;
			printf("%s: reply too large (truncated to %d words)\n", __func__,
				n_words);
		}
		L4_StoreMRs(1, n_words, tmp);
		L4_LoadMR(0, (L4_MsgTag_t){
				.X.label = FSHELPER_CHOPCHOP_REPLY, .X.u = n_words + 2,
			}.raw);
		/* chopchop reply metadata. */
		L4_LoadMR(1, async_id);
		L4_LoadMR(2, tag.raw);
		/* reply body (typed items transferred as untyped) */
		L4_LoadMRs(3, n_words, tmp);
		tag = L4_Send(reply_tid);
		if(L4_IpcFailed(tag)) {
			printf("%s: reply failed; ec %#lx\n", __func__, L4_ErrorCode());
		}
	}

	free(w);
}


static void helper_thread_fn(void)
{
	L4_Set_ExceptionHandler(L4_Pager());

	for(;;) {
		L4_ThreadId_t from;
		L4_MsgTag_t tag;

		/* polling, polling, polling, polling... */
		L4_LoadMR(0, (L4_MsgTag_t){ .X.label = FSHELPER_CHOPCHOP_DONE }.raw);
		tag = L4_Ipc(forkserv_tid, L4_anythread,
			L4_Timeouts(L4_Never, L4_Never), &from);

		for(;;) {
			if(L4_IpcFailed(tag)) {
				printf("forkserv helper IPC failed: ec %#lx\n",
					L4_ErrorCode());
				break;
			}

			bool reply;
			switch(tag.X.label) {
				case FSHELPER_CHOPCHOP: {
					L4_Word_t first;
					L4_StoreMR(1, &first);
					if(first != 0) {
						helper_chopchop(from, (struct helper_work *)first);
					}

					struct helper_work *w;
					do {
						w = *(struct helper_work *volatile *)&helper_queue;
					} while(w != NULL
						&& !__sync_bool_compare_and_swap(&helper_queue,
							w, NULL));

					if(w != NULL) {
						w = reverse_work(w);
						while(w != NULL) {
							struct helper_work *next = w->next;
							helper_chopchop(from, w);
							w = next;
						}
					}

					reply = false;
					break;
				}

				default:
					printf("forkserv helper got weird IPC label %#lx from %#lx\n",
						(L4_Word_t)tag.X.label, from.raw);
					reply = false;
			}

			if(reply) {
				L4_Reply(from);
			}
			/* fold race condition in the producer. */
			L4_LoadMR(0, (L4_MsgTag_t){
					.X.label = FSHELPER_CHOPCHOP_DONE,
				}.raw);
			/* "SendWait" */
			tag = L4_Ipc(forkserv_tid, L4_anythread,
				L4_Timeouts(L4_Never, L4_Never), &from);
		}
	}
}


static void start_helper_thread(void)
{
	L4_ThreadId_t self = L4_Myself();
	helper_tid = L4_GlobalId(L4_ThreadNo(self) + 1, 23);
	L4_MsgTag_t tag = { .raw = 0 };
	L4_Word_t my_utcb = (L4_Word_t)__L4_Get_UtcbAddress();	/* FIXME: nonportable! */
	L4_Word_t syscall_ec = 0,
		retval = fpager_threadctl(&tag, &syscall_ec,
			helper_tid, self, self, L4_Pager(),
			/* FIXME: no idea how this is properly supposed to go. the UTCB
			 * pointer goes to the middle of the memory region, though.
			 */
			(void *)((my_utcb + UTCB_SIZE) & ~(UTCB_SIZE - 1)));
	if(retval != 1) {
		printf("%s: retval %lu, ec %#lx, own ec %#lx\n", __func__,
			retval, syscall_ec, L4_ErrorCode());
		abort();
	}

	void *helper_stack = malloc(16 * 1024);
	L4_Word_t helper_sp = (L4_Word_t)helper_stack + 16 * 1024 - 32;
	L4_Start_SpIp(helper_tid, helper_sp, (L4_Word_t)&helper_thread_fn);
}


int main(void)
{
	L4_Set_ExceptionHandler(L4_Pager());
	console_tid = L4_Pager();
	fpager_tid = L4_Pager();
	forkserv_tid = L4_Myself();

	max_vaddr = find_high_vaddr();
	printf("forkserv: high vaddr=%#lx\n", max_vaddr);
	heap_init(0x80000);		/* leave 512 KiB for testbench */
	vpage_slab = KMEM_CACHE_NEW("fs_vpage slab", struct fs_vpage);
	start_helper_thread();
	forkserv_dispatch_loop();

	return 0;
}
