
/* service that takes over the root task's memory, letting it fork
 * subprocesses (and subprocesses of those). this is useful for test cases
 * involving map operations in IPC, or the Unmap system call.
 *
 * TODO: make this somehow less big and ugly.
 */

#define FORKSERV_IMPL_SOURCE

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <stdatomic.h>
#include <stdbool.h>
#include <stdnoreturn.h>
#include <errno.h>
#include <threads.h>
#include <ccan/list/list.h>
#include <ccan/htable/htable.h>
#include <ccan/likely/likely.h>
#include <ccan/container_of/container_of.h>

#include <ukernel/slab.h>
#include <ukernel/util.h>
#include <ukernel/kip.h>
#include <ukernel/memdesc.h>

#include <l4/types.h>
#include <l4/ipc.h>
#include <l4/syscall.h>
#include <l4/space.h>
#include <l4/schedule.h>
#include <l4/kip.h>
#include <l4/sigma0.h>

#include "defs.h"
#include "forkserv.h"
#include "muidl.h"

#include "forkserv-defs.h"
#include "threadmgr-defs.h"
#include "l4x2-defs.h"


#define TPS_SHIFT 6
#define THREADS_PER_SPACE (1 << TPS_SHIFT)


#define FSHELPER_CHOPCHOP	0x6343	/* "cC", moar liek a wakeup IPC */

/* from helper to forkserv */
#define FSHELPER_CHOPCHOP_DONE 0x4364	/* "Cd" */
#define FSHELPER_CHOPCHOP_REPLY 0x4372	/* "Cr" */

/* debug options */
#define PF_TRACING 0	/* helps debug mapdb */


struct fs_thread;


struct fs_space
{
	L4_Word_t id, parent_id, prog_brk;
	L4_ThreadId_t mgr_tid;
	struct htable pages;
	struct fs_thread *threads[THREADS_PER_SPACE];
	L4_Fpage_t utcb_area, kip_area;
	struct list_head children, dead_children, waiting_threads;
	struct list_node child_link;	/* in parent's children | dead_children */
	int wait_status;
	L4_Word_t last_segv_addr;

	L4_ThreadId_t child_redir_tid;
};


/* linked in free_page_list, quick_alloc_list, resv_page_list, or nothing when
 * refcount > 0. kept in phys_page_hash by local_addr per hash_word().
 */
struct fs_phys_page
{
	L4_Word_t local_addr;	/* address in forkserv */
	struct list_node link;
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
	struct list_node wait_link;	/* in space->waiting_threads | next == NULL */
	pid_t wait_pid;		/* child PID being waited on, -1 for any */
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

static void exit_common(L4_ThreadId_t target, int32_t wait_status);
static void exit_segv(L4_ThreadId_t target, uintptr_t fault_addr);


/* thread_hash is keyed by ThreadNo; the version field is ignored. this
 * supports handle_add_tid()'s overwrite part, and
 * thread:panic:version_switcheroo.
 */
static struct htable
	thread_hash = HTABLE_INITIALIZER(thread_hash, &hash_threadno, NULL),
	space_hash = HTABLE_INITIALIZER(space_hash, &hash_word, NULL),
	async_hash = HTABLE_INITIALIZER(async_hash, &hash_word, NULL),
	phys_page_hash = HTABLE_INITIALIZER(phys_page_hash, &hash_word, NULL);
static LIST_HEAD(free_page_list);		/* forkserv's private stash */
static LIST_HEAD(resv_page_list);		/* slab pages etc. */
static LIST_HEAD(quick_alloc_list);		/* top-down freelist */
static struct kmem_cache *vpage_slab = NULL,		/* for fs_vpage */
	*phys_page_slab = NULL;		/* for fs_phys_page */

static L4_ThreadId_t console_tid, fpager_tid, helper_tid, forkserv_tid;
static L4_Word_t map_range_pos = 0, next_space_id = 100, next_async_id = 1;
static struct helper_work *helper_queue = NULL;
static L4_Word_t max_vaddr, phys_mem_top, brk_pos = 0, phys_alloc_pos = 0;
static bool pump_done = false;
static int32_t root_space_id = -1;

L4_KernelInterfacePage_t *the_kip;


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


static bool threadno_cmp(const void *pa, void *pb) {
	const L4_ThreadId_t *a = pa, *b = pb;
	return L4_ThreadNo(*a) == L4_ThreadNo(*b);
}


static bool list_member(struct list_head *list, struct list_node *link) {
	struct list_node *i = list->n.next;
	while(i != link && i != &list->n) i = i->next;
	return i == link;
}


static struct fs_thread *get_thread(L4_ThreadId_t tid) {
	return htable_get(&thread_hash, int_hash(L4_ThreadNo(tid)),
		&threadno_cmp, &tid);
}


static struct fs_phys_page *get_phys_at(void *address)
{
	L4_Word_t key = (L4_Word_t)address & ~PAGE_MASK;
	return htable_get(&phys_page_hash, hash_word(&key, NULL),
		&word_cmp, &key);
}


static struct fs_vpage *get_vpage_at(struct fs_space *sp, L4_Word_t addr) {
	return htable_get(&sp->pages, int_hash(addr), &word_cmp, &addr);
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
static L4_Word_t find_high_vaddr(void)
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


/* points a memdescbuf at MemoryDescs on the KIP. */
static inline struct memdescbuf kip_mdb(void)
{
	int n = L4_NumMemoryDescriptors(the_kip);
	struct memdescbuf mds = {
		.ptr = L4_MemoryDesc(the_kip, 0), .len = n, .size = n,
	};
	return mds;
}


/* by examining the KIP MemoryDesc array, figure out:
 *   - highest virtual address
 *   - top of physical RAM
 *   - bottom address for sbrk() heap (1M alignment at or above 16M)
 */
static void configure_heap(void)
{
	max_vaddr = find_high_vaddr();
	printf("forkserv: high vaddr=%#lx\n", max_vaddr);

	phys_mem_top = 0;
	struct memdescbuf mds = kip_mdb();
	L4_Word_t q_start = 0, q_end = ~0ul;
	for(;;) {
		L4_Fpage_t part = mdb_query(&mds, q_start, q_end,
			false, false, L4_ConventionalMemoryType);
		if(L4_IsNilFpage(part)) break;
		q_start = L4_Address(part) + L4_Size(part);
		if(L4_SizeLog2(part) < PAGE_BITS) continue;

		phys_mem_top = MAX(L4_Word_t, phys_mem_top, FPAGE_HIGH(part));
	}

	L4_Fpage_t part = mdb_query(&mds, 16 * 1024 * 1024, ~0ul,
		false, false, L4_ConventionalMemoryType);
	brk_pos = L4_Address(part);
	phys_alloc_pos = phys_mem_top & ~PAGE_MASK;

	printf("forkserv: phys_mem_top=%#lx, brk_pos=%#lx, phys_alloc_pos=%#lx\n",
		phys_mem_top, brk_pos, phys_alloc_pos);
}


/* add some seed memory from sigma0, enough to create the slab caches used for
 * roottask transfer and the eventual sigma0 pump.
 */
static void seed_heap(void)
{
	static struct fs_phys_page pp[48];
	for(int i=0; i < NUM_ELEMENTS(pp); i++) {
		L4_Fpage_t got = L4_Sigma0_GetAny(L4_nilthread, PAGE_BITS,
			L4_CompleteAddressSpace);
		if(L4_IsNilFpage(got)) {
			printf("%s: i=%d: result is nilpage!\n", __func__, i);
			abort();
		}
		pp[i] = (struct fs_phys_page){
			.local_addr = L4_Address(got), .refcount = 0,
		};
		list_add_tail(&free_page_list, &pp[i].link);
		bool ok = htable_add(&phys_page_hash,
			hash_word(&pp[i], NULL), &pp[i]);
		if(!ok) {
			printf("forkserv: seed page at %#lx lost to OOM!\n",
				pp[i].local_addr);
		}
	}
}


static void pump_sigma0_mem(void)
{
	L4_Word_t highest_end = 0;
	int n_pages = 0, n_qal = 0;
	for(int s = sizeof(L4_Word_t) * 8; s >= PAGE_BITS; --s) {
		for(;;) {
			L4_Fpage_t got = L4_Sigma0_GetAny(L4_nilthread, s,
				L4_CompleteAddressSpace);
			if(L4_IsNilFpage(got)) break;
			n_pages += L4_Size(got) / PAGE_SIZE;
			highest_end = MAX(L4_Word_t, highest_end,
				L4_Address(got) + (L4_Size(got) - 1));

			if(L4_Address(got) < brk_pos && L4_Address(got) > 0x100000) {
				/* add free pages between 1M and brk_pos to the quick-alloc
				 * list.
				 */
				for(L4_Word_t i=0; i < L4_Size(got); i += PAGE_SIZE) {
					struct fs_phys_page *p = kmem_cache_alloc(phys_page_slab);
					p->local_addr = L4_Address(got) + i;
					p->refcount = 0;
					list_add_tail(&quick_alloc_list, &p->link);
					bool ok = htable_add(&phys_page_hash,
						hash_word(p, NULL), p);
					if(!ok) {
						printf("forkserv: early page at %#lx lost to OOM!\n",
							p->local_addr);
					}
					n_qal++;
				}
			}
		}
	}

	printf("forkserv: got %d pages (%d KiB) of conventional memory\n",
		n_pages, n_pages * PAGE_SIZE / 1024);
	printf("  (%d pages, %d KiB on tap)\n", n_qal, n_qal * PAGE_SIZE / 1024);

	/* not all memory indicated in the top-end memdescs is available. revise
	 * phys_alloc_pos to match this fact.
	 */
	phys_alloc_pos = MIN(L4_Word_t, phys_alloc_pos - 1,
		highest_end) & ~PAGE_MASK;
	printf("  (phys_alloc_pos'=%#lx)\n", phys_alloc_pos);

	pump_done = true;
}


void *sbrk(intptr_t increment)
{
	L4_Word_t prev_break = brk_pos;
	if(increment < 0) {
		/* trim. (or rather, don't.) */
		printf("%s: won't trim (not implemented)\n", __func__);
		abort();
	} else if(increment > 0) {
		/* grow. */
		increment = (increment + PAGE_SIZE - 1) & ~PAGE_MASK;
		if(increment == 0) return (void *)prev_break;

		L4_Word_t start = prev_break;
		static bool first_brk = true;
		if(first_brk) {
			/* memory at brk_pos might not be available from sigma0 despite
			 * there being physical memory at that address. step brk_pos
			 * forward to 16M offsets until there is; but stop at 256M for
			 * safety.
			 */
			while(brk_pos < 256 * 1024 * 1024) {
				L4_Fpage_t got = L4_Sigma0_GetPage(L4_nilthread,
					L4_FpageLog2(brk_pos, PAGE_BITS));
				if(!L4_IsNilFpage(got)) break;
				brk_pos = (brk_pos + 16 * 1024 * 1024 - 1) & ~(16 * 1024 * 1024 - 1);
			}
			if(brk_pos >= 256 * 1024 * 1024) {
				printf("%s: can't locate brk heap below 256M\n", __func__);
				abort();
			}
			prev_break = brk_pos;
			brk_pos += PAGE_SIZE;
			start = brk_pos;
			increment -= PAGE_SIZE;
			first_brk = false;
			if(increment == 0) return (void *)prev_break;
		}
		brk_pos += increment;
		printf("%s: increment=%lu; allocating %#lx..%#lx\n", __func__,
			(unsigned long)increment, prev_break, brk_pos - 1);

		if(brk_pos > phys_alloc_pos) {
			/* TODO: hunt the free-page list for enough pages to satisfy the
			 * request. only abort() on failure. (not properly necessary until
			 * scalability testing comes along.)
			 */
			abort();
		}

		if(!pump_done) {
			/* grab the necessary pages in early (pre-pump) startup. */
			L4_Word_t addr, size;
			for_page_range(start, brk_pos, addr, size) {
				L4_Fpage_t got = L4_Sigma0_GetPage(L4_nilthread,
					L4_FpageLog2(addr, size));
				if(L4_IsNilFpage(got)) {
					printf("%s: got nil page for %#lx:%#lx!\n", __func__,
						addr, 1ul << size);
					abort();
				}
			}
		}
	}

	return (void *)prev_break;
}


static struct fs_space *make_initial_space(int id)
{
	struct fs_space *sp = malloc(sizeof(*sp));
	*sp = (struct fs_space){
		.id = id, .parent_id = 0, .prog_brk = phys_mem_top + 1,
		.utcb_area = L4_Fpage(0x30000, UTCB_SIZE * THREADS_PER_SPACE),
		.kip_area = L4_FpageLog2(0x2f000, 12),
		.child_redir_tid = L4_anythread,
	};
	list_head_init(&sp->children);
	list_head_init(&sp->dead_children);
	list_head_init(&sp->waiting_threads);
	htable_init(&sp->pages, &hash_word, NULL);
	if(sp->id == 0) {
		/* catch all early orphans. generally this only affects pid=1, i.e.
		 * the testbench root task.
		 */
		struct htable_iter it;
		for(struct fs_space *orphan = htable_first(&space_hash, &it);
			orphan != NULL;
			orphan = htable_next(&space_hash, &it))
		{
			if(orphan->parent_id != 0) continue;
			assert(orphan->child_link.next == NULL);
			list_add(&sp->children, &orphan->child_link);
		}
	}

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


/* ctor for fs_phys_page. returns NULL on ENOMEM, or if @addr is a gap.
 * (caller distinguishes where relevant.)
 */
static struct fs_phys_page *make_phys_page(L4_Word_t addr)
{
	struct fs_phys_page *phys = kmem_cache_alloc(phys_page_slab);
	phys->local_addr = addr;
	bool ok = htable_add(&phys_page_hash, hash_word(phys, NULL), phys);
	if(!ok) {
		kmem_cache_free(phys_page_slab, phys);
		return NULL;
	}
	/* FIXME: confirm that the address is valid, i.e. that we didn't hit
	 * a pothole in the great big contiguous memory range.
	 */

	return phys;
}


/* returns # of pages taken on success, -EFAULT when htable_add() fails or
 * there's no more room in Hell. stuffs @output[] with however many pages 1 <<
 * @size_log2 covers. phys pages will be aligned to that size.
 */
static int alloc_phys_pages(struct fs_phys_page **output, int size_log2)
{
	assert(size_log2 >= PAGE_BITS);

	if(size_log2 == PAGE_BITS) {
		output[0] = list_pop(&quick_alloc_list, struct fs_phys_page, link);
		if(output[0] != NULL) {
			output[0]->refcount = 0;
			return 1;
		}
	}

	/* align phys_alloc_pos, and release spare pages to the quick pool. */
	const size_t size = 1u << size_log2;
	while(((phys_alloc_pos + PAGE_SIZE) & (size - 1)) != 0) {
		if(phys_alloc_pos <= brk_pos) return -EFAULT;
		struct fs_phys_page *pp = make_phys_page(phys_alloc_pos);
		if(pp == NULL) return -EFAULT;
		list_add_tail(&quick_alloc_list, &pp->link);
		phys_alloc_pos -= PAGE_SIZE;
	}
	if(phys_alloc_pos - size <= brk_pos) return -EFAULT;

	/* make pages for the parts of the buffalo that we use. */
	phys_alloc_pos -= size;
	for(size_t i=0; i < size / PAGE_SIZE; i++) {
		L4_Word_t addr = phys_alloc_pos + (i + 1) * PAGE_SIZE;
		struct fs_phys_page *pp = make_phys_page(addr);
		if(pp == NULL) {
			/* don't lose the pages, but fail */
			for(size_t j=0; j <= i; j++) {
				list_add_tail(&quick_alloc_list, &output[j]->link);
			}
			return -EFAULT;
		}
		output[i] = pp;
	}

	return size / PAGE_SIZE;
}


/* allocates memory for forkserv clients. */
static struct fs_phys_page *alloc_user_page(void)
{
	struct fs_phys_page *phys = list_pop(&quick_alloc_list,
		struct fs_phys_page, link);
	if(phys == NULL) {
		int n = alloc_phys_pages(&phys, PAGE_BITS);
		if(n < 0) return NULL;
	}
	phys->refcount = 1;
	memset((void *)phys->local_addr, 0, PAGE_SIZE);
	return phys;
}


void *kmem_alloc_new_page(void)
{
	struct fs_phys_page *pp = list_pop(&free_page_list,
		struct fs_phys_page, link);
	assert(pp != NULL);
	if(list_empty(&free_page_list)) {
		/* replenish. */
		for(int i=0; i < 8; i++) {
			struct fs_phys_page *tmp;
			int n = alloc_phys_pages(&tmp, PAGE_BITS);
			if(n >= 0) {
				tmp->refcount = 0;
				list_add(&free_page_list, &tmp->link);
			} else {
				printf("forkserv: failed to replenish; n=%d\n", n);
				break;
			}
		}
	}
	list_add(&resv_page_list, &pp->link);
	pp->refcount = 0;
	return (void *)pp->local_addr;
}


void kmem_free_page(void *ptr)
{
	struct fs_phys_page *pp = get_phys_at(ptr);
	if(pp != NULL) {
		assert(pp->refcount == 0);
		list_del_from(&resv_page_list, &pp->link);
		list_add(&free_page_list, &pp->link);
	} else {
		printf("forkserv:%s: can't find ptr=%p\n", __func__, ptr);
	}
}


noreturn void __assert_failure(
	const char *condition,
	const char *file, int line, const char *func)
{
	printf("assert(%s) failed in `%s' (%s:%u)\n", condition, func,
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


/* degenerate implementation of mtx_init(), mtx_lock(), and mtx_unlock() for
 * dlmalloc.
 */
int mtx_init(mtx_t *mtx, int type) {
	*mtx = 0;
	return thrd_success;
}


int mtx_lock(mtx_t *mtx)
{
	L4_Word_t prev = 0, next = L4_Myself().raw;
	while(!atomic_compare_exchange_strong(mtx, &prev, next)) {
		sched_yield();
		prev = 0;
	}
	return thrd_success;
}


int mtx_unlock(mtx_t *mtx)
{
	L4_Word_t prev = L4_Myself().raw;
	if(!atomic_compare_exchange_strong(mtx, &prev, 0)) {
		printf("%s: expected prev=%#lx, found prev=%#lx\n", __func__,
			L4_Myself().raw, prev);
		abort();
	}
	return thrd_success;
}


/* this is an oneway operation. the client is expected to complete the
 * transaction with a SEND_PAGE_2 IPC.
 */
static void handle_send_page(L4_Word_t virt_addr, int space_id)
{
	const L4_ThreadId_t from = muidl_get_sender();

	L4_Fpage_t window;
	struct fs_space *sp = get_space(space_id);
	if(sp == NULL) sp = make_initial_space(space_id);
	if(space_id == 0) {
		window = L4_Fpage(virt_addr, PAGE_SIZE);
	} else {
		if(unlikely(map_range_pos == 0)) {
			map_range_pos = phys_mem_top + 1;
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
			/* handle intervening pagefaults. */
			L4_Word_t faddr, fip;
			L4_StoreMR(1, &faddr);
			L4_StoreMR(2, &fip);
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

	L4_Word_t phys_addr; L4_StoreMR(1, &phys_addr);
	L4_MapItem_t mi; L4_StoreMRs(2, 2, mi.raw);
#if 0
	printf("got page=%#lx:%#lx sndbase=%#lx phys=%#lx virt=%#lx space=%d\n",
		L4_Address(L4_MapItemSndFpage(mi)), L4_Size(L4_MapItemSndFpage(mi)),
		L4_MapItemSndBase(mi), phys_addr, virt_addr, space_id);
#endif
	if(space_id != 0) map_range_pos += L4_Size(window);

	struct fs_phys_page *phys = kmem_cache_alloc(phys_page_slab);
	*phys = (struct fs_phys_page){
		.local_addr = L4_Address(window), .refcount = 1,
	};
	struct fs_vpage *p = kmem_cache_alloc(vpage_slab);
	*p = (struct fs_vpage){
		.address = phys_addr, .page = phys,
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
static void handle_pf(L4_Word_t addr, L4_Word_t ip, L4_MapItem_t *page_ptr)
{
	assert(invariants("pf"));

	L4_MsgTag_t tag = muidl_get_tag();
	L4_Word_t fault_access = tag.X.label & 0xf;
#if PF_TRACING
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
		from = L4_GlobalIdOf(from);
		printf("forkserv: source %lu:%lu isn't known\n", L4_ThreadNo(from),
			L4_Version(from));
		goto no_reply;
	}

	const bool in_special = ADDR_IN_FPAGE(sp->utcb_area, addr)
		|| ADDR_IN_FPAGE(sp->kip_area, addr);
	L4_Word_t page_addr = addr & ~PAGE_MASK;
	struct fs_vpage *page = get_vpage_at(sp, page_addr);
	if(page == NULL && page_addr > 0xf000 && page_addr < sp->prog_brk
		&& !in_special)
	{
		/* moar ramz pls */
		page = kmem_cache_alloc(vpage_slab);
		page->address = page_addr;
		page->page = alloc_user_page();
		page->access = L4_FullyAccessible;
#if PF_TRACING
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
		}
		printf("segfault in thread %lu:%lu, space %lu (brk %#lx)\n",
			L4_ThreadNo(from), L4_Version(from), sp->id, sp->prog_brk);
		printf("  addr %#lx, ip %#lx\n", addr, ip);
		int n;
		if(!L4_IsNilThread(sp->mgr_tid)
			&& !L4_SameThreads(from, sp->mgr_tid))
		{
			/* we'll be somewhat more persistent with the root task, because
			 * it cannot reasonably be killed. (it can end up in a pager loop
			 * though, so until the helper thread starts sending these things
			 * a finite send-timeout will serve as band-aid adequately.)
			 */
			sp->last_segv_addr = addr;
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
			 */
			exit_segv(muidl_get_sender(), addr);
		}
		goto no_reply;
	} else if(page->page->refcount == 1
		&& !CHECK_FLAG_ALL(page->access, fault_access))
	{
		/* always give access. */
#if PF_TRACING
		printf("%s: expand access at %#lx: old %#x -> new %#x\n", __func__,
			page->address, (unsigned)page->access,
			(unsigned)(page->access | fault_access));
#endif
		page->access |= fault_access;
	} else if(page->page->refcount > 1
		&& CHECK_FLAG(fault_access, L4_Writable))
	{
		/* duplicate, drop reference, and remap on top of the old page. */
#if PF_TRACING
		printf("%s: duplicating rc %d phys page %#lx -> %#lx\n",
			__func__, page->page->refcount, page->page->local_addr,
			page_addr);
#endif
		assert(!CHECK_FLAG(page->access, L4_Writable));
		struct fs_phys_page *copy = alloc_user_page();
		memcpy((void *)copy->local_addr, (void *)page->page->local_addr,
			PAGE_SIZE);
		page->page->refcount--;
		page->page = copy;
		page->access |= L4_Writable;
	} else {
#if PF_TRACING
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


static void handle_iopf(L4_Fpage_t iofp, L4_Word_t eip, L4_MapItem_t *ports_out)
{
	assert(L4_IsIoFpage(iofp));

#if PF_TRACING
	printf("forkserv: iofp=%#lx:%#lx, eip=%#lx\n",
		L4_IoFpagePort(iofp), L4_IoFpageSize(iofp), eip);
#endif

	/* forward the fault to our own pager. */
	int n = __l4x2_handle_x86_io_fault(L4_IoFpageLog2(0, 16),
		L4_Pager(), iofp, 0xdeadbeef, ports_out);
	if(n != 0) {
		printf("%s: can't forward iopf: n=%d\n", __func__, n);
		muidl_raise_no_reply();
	}
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
		if(t->wait_link.next != NULL) {
			list_del_from(&t->space->waiting_threads, &t->wait_link);
			t->wait_link.next = NULL;
		}
	} else {
		t = malloc(sizeof(struct fs_thread));
		*t = (struct fs_thread){ .tid = tid, .space = sp };
		assert(t->wait_link.next == NULL);
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
	struct fs_space *copy_space = calloc(1, sizeof(*copy_space));
	for(int i=0; i < THREADS_PER_SPACE; i++) copy_space->threads[i] = NULL;
	copy_space->id = copy_id;
	copy_space->parent_id = sp->id;
	copy_space->prog_brk = sp->prog_brk;
	copy_space->utcb_area = sp->utcb_area;
	copy_space->kip_area = sp->kip_area;
	copy_space->mgr_tid = L4_nilthread;
	copy_space->child_redir_tid = sp->child_redir_tid;
	list_head_init(&copy_space->children);
	list_head_init(&copy_space->dead_children);
	list_head_init(&copy_space->waiting_threads);
	list_add(&sp->children, &copy_space->child_link);
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
		htable_add(&copy_space->pages, int_hash(cvp->address),
			&cvp->address);

		if(CHECK_FLAG(ovp->access, L4_Writable)) {
			ovp->access &= ~L4_Writable;

			unmapbuf[num_unmap] = L4_FpageLog2(
				ovp->page->local_addr, PAGE_BITS);
			/* FIXME: setting this to L4_FullyAccessible (which isn't
			 * necessary here) makes SelfThreUnca:3 blow up with an
			 * inconsistent mapdb/pagetable fail. but not if DEBUG_ME_HARDER
			 * is set.
			 */
			L4_Set_Rights(&unmapbuf[num_unmap], L4_Writable);
			num_unmap++;

			if(num_unmap == 64) {
				L4_UnmapFpages(64, unmapbuf);
				num_unmap = 0;
			}
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
	L4_Fpage_t kip_area, L4_Fpage_t utcb_area)
{
	assert(root_space_id == -1 || root_space_id == space_id);
	root_space_id = space_id;

	/* this is kind of a lousy way to trigger it, but w/e. */
	if(!pump_done && space_id == 1) pump_sigma0_mem();

	struct fs_space *sp = get_space(space_id);
	if(sp == NULL) {
		sp = make_initial_space(space_id);
	}
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
	L4_Fpage_t local[64];
	int outpos = 0;
	for(int i=0, in_len = pages_len; i < in_len && outpos < 64; i++) {
		for(L4_Word_t fp_addr = L4_Address(pages[i]),
					  fp_max = fp_addr + L4_Size(pages[i]);
			fp_addr < fp_max && outpos < 64;
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
	if(outpos > 0) L4_UnmapFpages(outpos, local);
	for(int i=0; i < outpos; i++) {
		L4_Set_Rights(&output[i], L4_Rights(local[i]));
	}
	*output_len = outpos;
}


static void handle_discontig(L4_Fpage_t page, int32_t grain)
{
	assert(invariants("discontiguate[start]"));

	struct fs_space *sp = get_space_by_tid(muidl_get_sender());
	if(sp == NULL) return;
	printf("forkserv: pid=%lu: discontiguate(%#lx:%#lx, %d)\n",
		sp->id, L4_Address(page), L4_Size(page), grain);

	if(grain < PAGE_BITS || grain > L4_SizeLog2(page)) {
		grain = L4_SizeLog2(page);
	}
	const int num_phys = 1 << (grain - PAGE_BITS);

	/* the correctness of this algorithm, when grain < page.sizelog2, depends
	 * on alloc_phys_pages() returning pages in decreasing address order. this
	 * is asserted against.
	 */
	struct fs_phys_page **phys = malloc(sizeof(*phys) * num_phys);
	if(phys == NULL) {
		printf("forkserv: %s: out of memory!\n", __func__);
		return;
	}
	uint32_t not = 0;	/* invalid first page id, per segment */
	for(L4_Word_t off = 0; off < L4_Size(page); off += 1ul << grain) {
		L4_Word_t first_addr = L4_Address(page) + off;

		int n = alloc_phys_pages(phys, grain);
		if(n < 0) {
			printf("forkserv: discontiguate failed at off=%#lx, addr=%#lx\n",
				off, first_addr);
			free(phys);
			return;
		}
		assert(phys[0]->local_addr >> PAGE_BITS != not);

		/* stick 'em in. */
		for(int i=0; i < num_phys; i++) {
			L4_Word_t addr = first_addr + i * PAGE_SIZE;
			struct fs_vpage *vp = get_vpage_at(sp, addr);
			if(vp != NULL) {
				/* copy contents & replace physpage in existing vpage. */
				memcpy((void *)phys[i]->local_addr,
					(const void *)vp->page->local_addr, PAGE_SIZE);
				L4_Fpage_t u = L4_FpageLog2(vp->page->local_addr, PAGE_BITS);
				L4_Set_Rights(&u, L4_FullyAccessible);
				L4_UnmapFpage(u);
				if(--vp->page->refcount == 0) {
					list_add(&quick_alloc_list, &vp->page->link);
				}
				vp->page = phys[i];
			} else {
				/* give it a new vpage & clear the fresh memory. */
				memset((void *)phys[i]->local_addr, 0, PAGE_SIZE);
				vp = kmem_cache_alloc(vpage_slab);
				*vp = (struct fs_vpage){
					.access = L4_ReadWriteOnly, .address = addr,
					.page = phys[i],
				};
				bool ok = htable_add(&sp->pages, int_hash(vp->address), vp);
				assert(ok);
			}
			phys[i]->refcount = 1;
		}

		not = phys[num_phys - 1]->local_addr >> PAGE_BITS;
	}
	free(phys);

	assert(invariants("discontiguate[end]"));
}


/* POSIX reparents children to PID1. since there's no PID1 in the testbench
 * personality, any dead children waiting to get reaped by @sp will be
 * recursively destroyed.
 */
static void destroy_space(struct fs_space *sp)
{
	/* reparent live children to space 0. */
	struct fs_space *child, *next_space, *zero = get_space(0);
	list_for_each_safe(&sp->children, child, next_space, child_link) {
		child->parent_id = 0;
		if(zero == NULL) list_del_from(&sp->children, &child->child_link);
	}
	if(zero != NULL) list_append_list(&zero->children, &sp->children);
	assert(list_empty(&sp->children));

	/* destroy dead children. */
	list_for_each_safe(&sp->dead_children, child, next_space, child_link) {
		list_del_from(&sp->dead_children, &child->child_link);
		destroy_space(child);
	}
	assert(list_empty(&sp->dead_children));

	htable_del(&space_hash, int_hash(sp->id), &sp->id);
	free(sp);
}


static int handle_wait(int child_pid, int *wstatus, int options)
{
	if(options & ~(WNOHANG)) return -EINVAL;
	if(child_pid < 0 && child_pid != -1) return -EINVAL;
	if(child_pid == 0) child_pid = -1;

	L4_ThreadId_t ipc_from = muidl_get_sender();
	assert(L4_IsGlobalId(ipc_from));
	struct fs_space *sp = get_space_by_tid(ipc_from);
	if(sp == NULL) {
		printf("forkserv: don't know space for %lu:%lu\n",
			L4_ThreadNo(ipc_from), L4_Version(ipc_from));
		return -1;
	}

	struct fs_space *dead, *live;
	if(child_pid == -1) {
		dead = list_pop(&sp->dead_children, struct fs_space, child_link);
		live = NULL;
	} else {
		live = get_space(child_pid);
		if(live == NULL) return -ECHILD;	/* doesn't exist */
		/* there may be a faster way to do this, in principle, but forkserv
		 * doesn't need to scale like that.
		 */
		if(list_member(&sp->dead_children, &live->child_link)) {
			dead = live;
			live = NULL;
			list_del_from(&sp->dead_children, &dead->child_link);
		} else if(!list_member(&sp->children, &live->child_link)) {
			/* b-but the kid is not my son */
			return -ECHILD;
		} else {
			dead = NULL;
		}
	}

	if(dead != NULL) {
		/* immediate success */
		assert(live == NULL);
		int id = dead->id;
		*wstatus = dead->wait_status;
		destroy_space(dead);
		return id;
	} else if(live != NULL || !list_empty(&sp->children)) {
		if(options & WNOHANG) {
			/* not yet waitable. */
			*wstatus = 0;
			return 0;
		} else {
			/* delayed */
			struct fs_thread *t = get_thread(ipc_from);
			assert(t->wait_link.next == NULL);
			assert(live != NULL || child_pid == -1);
			assert(live == NULL || child_pid == live->id);
			t->wait_pid = child_pid;
			list_add_tail(&sp->waiting_threads, &t->wait_link);
			muidl_raise_no_reply();
			return -666;
		}
	} else {
		/* immediate -ECHILD */
		*wstatus = 0;
		return -ECHILD;
	}
}


static int32_t handle_getpid(void)
{
	struct fs_space *sp = get_space_by_tid(muidl_get_sender());
	return sp == NULL ? -1 : sp->id;
}


static bool end_thread_by_tno(struct fs_space *sp, int tno)
{
	assert(tno >= 0 && tno < THREADS_PER_SPACE);
	if(sp->threads[tno] == NULL) return false;
	struct fs_thread *t = sp->threads[tno];
	sp->threads[tno] = NULL;

	L4_MsgTag_t tag;
	L4_Word_t ec = 0, res = fpager_threadctl(&tag, &ec, t->tid, L4_nilthread,
		L4_nilthread, L4_nilthread, (void *)-1);
	if(res != 1) {
		printf("forkserv/end_thread: threadctl failed, %s ec %#lx\n",
			L4_IpcFailed(tag) ? "ipc" : "syscall", ec);
		/* can't do much else. */
	}
	if(t->wait_link.next != NULL) {
		list_del_from(&t->space->waiting_threads, &t->wait_link);
		t->wait_link.next = NULL;
	}
	htable_del(&thread_hash, int_hash(L4_ThreadNo(t->tid)), &t->tid);
	free(t);

	return true;
}


static bool end_thread(L4_ThreadId_t tid)
{
	struct fs_thread *t = get_thread(tid);
	if(t == NULL) return false;

	struct fs_space *sp = t->space;
	int tno = -1;
	for(int i=0; i < THREADS_PER_SPACE; i++) {
		if(sp->threads[i] == t) {
			assert(tno < 0);		/* must only occur once. */
			tno = i;
		}
	}
	if(tno < 0) {
		printf("tid=%lu:%lu wasn't found\n",
			L4_ThreadNo(tid), L4_Version(tid));
		abort();
	}

	return end_thread_by_tno(sp, tno);
}


/* NOTE: IDL call. parameters from out-of-process. */
static void handle_exit(int32_t exit_status, bool was_segv)
{
	L4_ThreadId_t ipc_from = muidl_get_sender();
	assert(L4_IsGlobalId(ipc_from));
	if(!was_segv) {
		int32_t status = exit_status << 1;
		assert(WIFEXITED(status));
		exit_common(ipc_from, status);
	} else {
		struct fs_space *sp = get_space_by_tid(ipc_from);
		if(sp == NULL) {
			printf("forkserv/exit: unknown TID %lu:%lu\n",
				L4_ThreadNo(ipc_from), L4_Version(ipc_from));
			return;
		}

		exit_segv(ipc_from, sp->last_segv_addr);
	}
	muidl_raise_no_reply();
}


static void exit_common(L4_ThreadId_t target, int wait_status)
{
	struct fs_space *sp = get_space_by_tid(target);
	if(sp == NULL) {
		printf("forkserv/exit_common: unknown TID %lu:%lu\n",
			L4_ThreadNo(target), L4_Version(target));
		return;
	}

	/* chuck threads in a slightly redundant manner, i.e. by killing the ones
	 * currently in wait(2) first. this enforces a little bit of consistency
	 * under !NDEBUG, which is good in a test environment such as what
	 * forkserv provides.
	 */
	struct fs_thread *t, *next;
	list_for_each_safe(&sp->waiting_threads, t, next, wait_link) {
		list_del_from(&sp->waiting_threads, &t->wait_link);
		t->wait_link.next = NULL;
		bool was_found = end_thread(t->tid);
		assert(was_found);
	}
	assert(list_empty(&sp->waiting_threads));
	for(int i=0; i < THREADS_PER_SPACE; i++) end_thread_by_tno(sp, i);

	struct fs_space *parent = get_space(sp->parent_id);
	assert(parent != NULL);
	assert(parent->id == sp->parent_id);
	assert(sp != parent);		/* space 0 cannot die */

	/* destroy the virtual memory bits and zombify the address space */
	struct htable_iter it;
	for(struct fs_vpage *vp = htable_first(&sp->pages, &it);
		vp != NULL;
		vp = htable_next(&sp->pages, &it))
	{
		if(vp->page != NULL) {
			if(--vp->page->refcount == 0) {
				list_add(&quick_alloc_list, &vp->page->link);
				vp->page = NULL;
			}
		}
		kmem_cache_free(vpage_slab, vp);
	}
	htable_clear(&sp->pages);
	sp->utcb_area = L4_Nilpage;

	/* activate waiting thread, or make it immediately reapable (when there's
	 * no waiter)
	 */
	list_del_from(&parent->children, &sp->child_link);
	struct fs_thread *wakeup = NULL, *cur;
	list_for_each(&parent->waiting_threads, cur, wait_link) {
		if(cur->wait_pid == sp->id) {
			/* select the first per-PID waiter over wildcards. */
			wakeup = cur;
			break;
		} else if(cur->wait_pid == -1 && wakeup == NULL) {
			/* select the first wildcard waiter otherwise. */
			wakeup = cur;
		}
	}
	if(wakeup != NULL) {
		list_del_from(&parent->waiting_threads, &wakeup->wait_link);
		wakeup->wait_link.next = NULL;

		L4_Word_t dead_id = sp->id;
		destroy_space(sp);

		L4_LoadMR(0, (L4_MsgTag_t){ .X.u = 2 }.raw);
		L4_LoadMR(1, dead_id);
		L4_LoadMR(2, wait_status);
		L4_MsgTag_t tag = L4_Reply(wakeup->tid);
		if(L4_IpcFailed(tag)) {
			printf("forkserv: can't send wakeup, ec=%#lx\n",
				L4_ErrorCode());
		}

		/* if this was the parent's last child, send -ECHILD to other
		 * waiters; such as those that waited on @sp and weren't signaled
		 * because another thread got there first.
		 */
		if(list_empty(&parent->children)
			&& list_empty(&parent->dead_children))
		{
			list_for_each_safe(&parent->waiting_threads,
				wakeup, next, wait_link)
			{
				L4_LoadMR(0, (L4_MsgTag_t){ .X.u = 2 }.raw);
				L4_LoadMR(1, (L4_Word_t)-ECHILD);
				L4_LoadMR(2, 0);
				L4_MsgTag_t tag = L4_Reply(wakeup->tid);
				if(L4_IpcFailed(tag)) {
					printf("forkserv: can't send -ECHILD, ec=%#lx\n",
						L4_ErrorCode());
				}

				list_del_from(&parent->waiting_threads, &wakeup->wait_link);
				wakeup->wait_link.next = NULL;
			}
		}
	} else {
		/* the dead walk */
		list_add_tail(&parent->dead_children, &sp->child_link);
		sp->wait_status = wait_status;
		assert(WIFEXITED(sp->wait_status));
	}

	muidl_raise_no_reply();
}


/* encodes the WSEGVADDR() format. */
static void exit_segv(L4_ThreadId_t target, uintptr_t fault_addr)
{
	uintptr_t status = 1 | (11 << 1) | (fault_addr & ~0x3ful);
	assert(!WIFEXITED(status));
	assert(WIFSIGNALED(status));
	assert(WTERMSIG(status) == 11);
	assert(WSEGVADDR(status) == (fault_addr & ~0x3ful));
	exit_common(target, status);
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

	if(is_last && dest_tid.raw == from.raw) exit_common(from, 0);
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
		.handle_x86_io_fault = &handle_iopf,
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
		.discontiguate = &handle_discontig,
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
					printf("forkserv: unknown label %#lx from %lu:%lu\n",
						L4_Label(tag), L4_ThreadNo(sender),
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
	the_kip = L4_GetKernelInterface();
	fault_own_pages();
	L4_Set_ExceptionHandler(L4_Pager());
	console_tid = L4_Pager();
	fpager_tid = L4_Pager();
	forkserv_tid = L4_Myself();

	configure_heap();
	seed_heap();
	vpage_slab = KMEM_CACHE_NEW("fs_vpage", struct fs_vpage);
	phys_page_slab = KMEM_CACHE_NEW("fs_phys_page", struct fs_phys_page);
	start_helper_thread();
	forkserv_dispatch_loop();

	return 0;
}
