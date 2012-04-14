
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <stdbool.h>
#include <ccan/list/list.h>
#include <ccan/avl/avl.h>
#include <ccan/compiler/compiler.h>

#include <l4/types.h>
#include <l4/ipc.h>
#include <l4/thread.h>
#include <l4/vregs.h>
#include <l4/syscall.h>
#include <l4/kip.h>

#include <ukernel/mm.h>
#include <ukernel/slab.h>
#include <ukernel/util.h>


#define CHECK_FLAG(mask, bit) (((mask) & (bit)) != 0)
#define NUM_SEED_PAGES 12
#define PAGE_BUCKETS 20		/* word size - 12 */


struct track_page
{
	struct list_node link;
	L4_Fpage_t page;
	bool dedicated;
};

struct malloc_size
{
	struct list_node link;
	int size;
	struct kmem_cache *cache;
};


static struct list_head free_pages[PAGE_BUCKETS];	/* size_log2 = index + 12 */
static AVL *pages_by_range = NULL;
static LIST_HEAD(malloc_size_list);
static LIST_HEAD(slab_page_list);
static LIST_HEAD(dead_trk_list);
static struct kmem_cache *track_page_slab = NULL;


static void con_putstr(const char *str, size_t len);
static struct track_page *find_page_by_range(L4_Fpage_t key);
static void *get_free_page(int size_log2);
static void *get_free_page_at(L4_Word_t address, int size_log2);
static void free_page_range(
	L4_Word_t start,
	L4_Word_t length,
	bool dedicate);


int vprintf(const char *fmt, va_list al)
{
	char buffer[256];
	int n = vsnprintf(buffer, sizeof(buffer), fmt, al);
	if(n > 0) con_putstr(buffer, n);
	return n;
}


int printf(const char *format, ...)
{
	va_list al;
	va_start(al, format);
	int n = vprintf(format, al);
	va_end(al);
	return n;
}


L4_ThreadId_t L4_Pager(void) {
	void *utcb = __L4_Get_UtcbAddress();
	return (L4_ThreadId_t){ .raw = L4_VREG(utcb, L4_TCR_PAGER) };
}


L4_Word_t L4_ErrorCode(void) {
	void *utcb = __L4_Get_UtcbAddress();
	return L4_VREG(utcb, L4_TCR_ERRORCODE);
}


static L4_Word_t send_test(L4_Word_t payload)
{
	L4_LoadMR(0, (L4_MsgTag_t){ .X.label = 0x2369, .X.u = 1 }.raw);
	L4_LoadMR(1, payload);
	L4_ThreadId_t dummy;
	L4_MsgTag_t tag = L4_Ipc(L4_Pager(), L4_Pager(),
		L4_Timeouts(L4_Never, L4_Never), &dummy);
	if(L4_IpcFailed(tag)) return 0;
	else return L4_VREG(__L4_Get_UtcbAddress(), L4_TCR_MR(1));
}


static void con_putstr(const char *str, size_t len)
{
	L4_LoadMR(0, (L4_MsgTag_t){
		.X.label = 0x5370, /* "pS" */
		.X.u = (len + 3) / 4,
	}.raw);
	for(int i=0; i * 4 < len; i++) {
		L4_LoadMR(i + 1, *(L4_Word_t *)&str[i * 4]);
	}
	L4_ThreadId_t dummy;
	L4_Ipc(L4_Pager(), L4_Pager(), L4_Timeouts(L4_Never, L4_Never), &dummy);
}


void __assert_failure(
	const char *condition,
	const char *file,
	unsigned int line,
	const char *function)
{
	printf("assert(%s) failed in `%s' (%s:%u)\n", condition, function,
		file, line);
	for(;;) { /* spin */ }
}


void abort(void)
{
	printf("abort() called!\n");
	for(;;) { /* wheeee */ }
}


static int sigma0_ipc_loop(void *kip_base)
{
	printf("entering IPC loop\n");

	/* FIXME: add "proper" L4_LoadMR(), L4_StoreMR() functions */
	void *utcb = __L4_Get_UtcbAddress();
	for(;;) {
		L4_ThreadId_t from = L4_nilthread;
		L4_MsgTag_t tag = L4_Ipc(L4_nilthread, L4_anythread,
			L4_Timeouts(L4_Never, L4_Never), &from);

		for(;;) {
			if(L4_IpcFailed(tag)) {
				printf("pager ipc failed (errorcode %#x)\n", L4_ErrorCode());
				break;
			}

			L4_ThreadId_t sender = from;
			if((tag.X.label & 0xfff0) == 0xffe0
				&& tag.X.u == 2 && tag.X.t == 0)
			{
				L4_Word_t ip = L4_VREG(utcb, L4_TCR_MR(2)),
					addr = L4_VREG(utcb, L4_TCR_MR(1));
				printf("pf in %d:%d (ip %#x, addr %#x)\n",
					from.global.X.thread_no, from.global.X.version,
					ip, addr);
				static L4_Word_t last_fault = 0;
				if(last_fault == addr) {
					printf("... two faults at the same address! s0 rejects this.\n");
					break;
				} else {
					last_fault = addr;
				}
				void *ptr = get_free_page_at(addr & ~PAGE_MASK, 12);
				if(ptr == NULL) {
					printf("page at %#x unavailable in fault handler\n",
						addr);
					break;
				}
				assert((L4_Word_t)ptr == (addr & ~PAGE_MASK));
				L4_Fpage_t page = L4_FpageLog2(addr, PAGE_BITS);
				L4_Set_Rights(&page, L4_FullyAccessible);
				L4_MapItem_t idemp = L4_MapItem(page, addr & ~PAGE_MASK);
				L4_LoadMR(0, (L4_MsgTag_t){ .X.label = 666, .X.t = 2 }.raw);
				L4_LoadMR(1, idemp.raw[0]);
				L4_LoadMR(2, idemp.raw[1]);
			} else if((tag.X.label & 0xfff0) == 0xffa0
				&& tag.X.u == 2 && tag.X.t == 0)
			{
				/* s0 user protocol: fpage request */
				L4_Fpage_t req_fpage = { .raw = L4_VREG(utcb, L4_TCR_MR(1)) };
				L4_Word_t req_attr = L4_VREG(utcb, L4_TCR_MR(2));
				printf("roottask requested page %#x:%#x attr %#x\n",
					L4_Address(req_fpage), L4_Size(req_fpage), req_attr);
				if((req_fpage.raw & (~0u << PAGE_BITS)) == (~0u << PAGE_BITS)) {
					/* any address. */
				} else {
					printf("definite address not supported\n");
					break;
				}
				void *ptr = get_free_page(L4_SizeLog2(req_fpage));
				if(ptr == NULL) {
					/* reject */
					L4_LoadMR(0, (L4_MsgTag_t){ .X.t = 2 }.raw);
					L4_LoadMR(1, 0x8);
					L4_LoadMR(2, L4_Nilpage.raw);
				} else {
					L4_Fpage_t map_page = L4_FpageLog2((L4_Word_t)ptr,
						L4_SizeLog2(req_fpage));
					L4_Set_Rights(&map_page, L4_FullyAccessible);
					L4_MapItem_t map = L4_MapItem(map_page, 0);
					L4_LoadMR(0, (L4_MsgTag_t){ .X.t = 2 }.raw);
					L4_LoadMR(1, map.raw[0]);
					L4_LoadMR(2, map.raw[1]);
				}
			} else {
				printf("unknown IPC label %#x (u %d, t %d) from %u:%u\n",
					tag.X.label, tag.X.u, tag.X.t, from.global.X.thread_no,
					from.global.X.version);
				break;
			}

			/* ReplyWait */
			from = L4_nilthread;
			tag = L4_Ipc(sender, L4_anythread,
				L4_Timeouts(L4_ZeroTime, L4_Never), &from);
		}
	}

	/* never reached, but whatever. */
	return 0;
}


static void tid_test(void)
{
	printf("threadid test start.\n");

	printf("\tL4_Myself() == %#x\n", (unsigned)L4_Myself().raw);
	printf("\tL4_MyLocalId() == %#x\n", (unsigned)L4_MyLocalId().raw);
	printf("\tL4_LocalIdOf(L4_MyGlobalId()) == %#x\n",
		(unsigned)L4_LocalIdOf(L4_MyGlobalId()).raw);
	printf("\tL4_GlobalIdOf(L4_MyLocalId()) == %#x\n",
		(unsigned)L4_GlobalIdOf(L4_MyLocalId()).raw);

	printf("threadid test ends.\n");
}


void unmap_test(void)
{
	printf("unmap test start.\n");

	void *utcb = __L4_Get_UtcbAddress();
	L4_Fpage_t pages[] = {
		L4_FpageLog2(0xdeadbeef, 12),
		L4_FpageLog2(0xcafebabe, 12),
	};
	const int n_pages = sizeof(pages) / sizeof(pages[0]);
	for(int i=0; i < n_pages; i++) {
		L4_Set_Rights(&pages[i], L4_FullyAccessible);
		L4_LoadMR(i, pages[i].raw);
	}
	L4_Fpage_t out_pages[n_pages];
	L4_Unmap(1);
	for(int i=0; i < n_pages; i++) {
		out_pages[i].raw = L4_VREG(utcb, L4_TCR_MR(i));
	}

	for(int i=0; i < n_pages; i++) {
		L4_Fpage_t fp = out_pages[i];
		printf("page %d: %#x:%#x, was %c%c%c\n", i,
			(unsigned)L4_Address(fp), (unsigned)L4_Size(fp),
			CHECK_FLAG(L4_Rights(fp), L4_Readable) ? 'r' : '-',
			CHECK_FLAG(L4_Rights(fp), L4_Writable) ? 'w' : '-',
			CHECK_FLAG(L4_Rights(fp), L4_eXecutable) ? 'x' : '-');
	}

	printf("unmap test ends.\n");
}


static void threadctl_test(void)
{
	printf("threadcontrol test start.\n");

	/* thread creation. */
	L4_ThreadId_t dest = { .global = { .X.version = 1, .X.thread_no = 129 } };
	L4_Word_t result = L4_ThreadControl(dest, L4_Myself(), L4_nilthread,
		L4_Pager(), __L4_Get_UtcbAddress() + 512);
	printf("creating threadcontrol result %u\n", (unsigned)result);
	if(result == 0) {
		printf("error code %#x\n", L4_ErrorCode());
	}

	printf("threadcontrol test ends.\n");
}


void spacectl_test(void)
{
	printf("spacecontrol test start.\n");

	L4_Word_t old_ctl, result = L4_SpaceControl(L4_Myself(),
		0xcafeb00b, L4_FpageLog2(0x10000, 12), L4_FpageLog2(0x11000, 15),
		L4_nilthread, &old_ctl);
	printf("spacecontrol result %#x, old_ctl %#x\n", (unsigned)result,
		(unsigned)old_ctl);

	printf("spacecontrol test ends.\n");
}


static void threadswitch_test(void)
{
	printf("threadswitch test start.\n");
	L4_ThreadSwitch(L4_nilthread);
	printf("threadswitch test end.\n");
}


static void schedule_test(void)
{
	printf("schedule test start.\n");
	L4_Word_t old_tc;
	L4_Word_t res = L4_Schedule(L4_Myself(), 1, 2, 3, 4, &old_tc);
	printf("L4_Schedule() returned %#x (old_timectl %#x)\n",
		(unsigned)res, (unsigned)old_tc);
	printf("schedule test end.\n");
}


static void *get_free_page_at(L4_Word_t address, int size_log2)
{
	assert((address & PAGE_MASK) == 0);

	L4_Fpage_t key = L4_FpageLog2(address, size_log2);
	assert(L4_Address(key) == address);
	assert(L4_SizeLog2(key) == size_log2);
	struct track_page *pg = find_page_by_range(key);
	if(pg == NULL) return NULL;

	L4_Fpage_t page = pg->page;
	bool dedicated = pg->dedicated;
	avl_remove(pages_by_range, &pg->page);
	if(!dedicated) {
		list_del_from(&free_pages[size_log2 - PAGE_BITS], &pg->link);
	}
	kmem_cache_free(track_page_slab, pg);
	pg = (void *)0xdeadbeef;

	if(L4_Address(page) != address || L4_SizeLog2(page) != size_log2) {
		/* complex case. */
		L4_Word_t start_len = address - L4_Address(page);
		free_page_range(L4_Address(page), start_len, dedicated);
		L4_Word_t top = address + (1 << size_log2),
			pg_top = L4_Address(page) + L4_Size(page);
		if(top < pg_top) {
			free_page_range(top, pg_top - top, dedicated);
		}
	}

	return (void *)address;
}


static void *get_free_page(int size_log2)
{
	struct track_page *pg = NULL;
	for(int i=size_log2 - PAGE_BITS; i < PAGE_BUCKETS; i++) {
		if(!list_empty(&free_pages[i])) {
			pg = list_top(&free_pages[i], struct track_page, link);
			list_del_from(&free_pages[i], &pg->link);
			break;
		}
	}
	if(pg == NULL) return NULL;

	assert(!pg->dedicated);
	void *ret;
	if(L4_SizeLog2(pg->page) == size_log2) {
		ret = (void *)L4_Address(pg->page);
	} else {
		/* FIXME: implement splitting for propers */
		printf("can't handle complex get_free_page() yet! %#x:%#x dropped.\n",
			L4_Address(pg->page), L4_Size(pg->page));
		return get_free_page(size_log2);
	}

	/* this should remove the page from pages_by_range, but we can't call
	 * AVL-tree functions from here as this indirectly backs its allocator.
	 * instead remove these items lazily in find_page_by_range().
	 */
	list_add_tail(&dead_trk_list, &pg->link);

	return ret;
}


static void free_phys_page(void *ptr, int size_log2, bool dedicate)
{
	if(track_page_slab == NULL) {
		track_page_slab = KMEM_CACHE_NEW("track_page_slab", struct track_page);
	}

	struct track_page *pg = kmem_cache_alloc(track_page_slab);
	if(pg == NULL) {
		printf("warning: can't allocate track_page for %#x:%#x; memory was lost\n",
			(unsigned)ptr, (unsigned)(1 << size_log2));
		return;
	}

	pg->dedicated = dedicate;
	pg->page = L4_FpageLog2((L4_Word_t)ptr, size_log2);
	avl_insert(pages_by_range, &pg->page, pg);
	if(!dedicate) {
		list_add_tail(&free_pages[size_log2 - PAGE_BITS], &pg->link);
	}
}


static void free_page_range(
	L4_Word_t start,
	L4_Word_t length,
	bool dedicate)
{
	int bit;
	L4_Word_t addr;
	for_page_range(start, start + length, addr, bit) {
		free_phys_page((void *)addr, bit, dedicate);
	}
}


static struct track_page *find_page_by_range(L4_Fpage_t key)
{
	struct track_page *tp, *next;
	list_for_each_safe(&dead_trk_list, tp, next, link) {
		avl_remove(pages_by_range, &tp->page);
		list_del_from(&dead_trk_list, &tp->link);
		if(kmem_cache_find(tp) != NULL) {
			kmem_cache_free(track_page_slab, tp);
		} else {
			/* TODO: track the statically allocated struct track_page in a "usable
			 * track_page list" or some such, and recycle those in
			 * free_phys_page().
			 */
		}
	}

	struct track_page *pg = avl_lookup(pages_by_range, &key);
	return pg;
}


static int compare_disjoint_fpages(const void *keyptr, const void *candptr)
{
	if(keyptr == candptr) return 0;

	const L4_Fpage_t *key = keyptr, *cand = candptr;
	if(L4_Address(*key) + L4_Size(*key) <= L4_Address(*cand)) return -1;
	if(L4_Address(*key) >= L4_Address(*cand) + L4_Size(*cand)) return 1;
	return 0;
}


static void build_heap(void *kip_base)
{
	for(int i=0; i < PAGE_BUCKETS; i++) list_head_init(&free_pages[i]);

	/* a little bit of seed memory for the slabs and whatnot.
	 *
	 * the s_page[] members can be distinguished from those allocated from
	 * slabs by how kmem_cache_find() returns NULL for them.
	 */
	static uint8_t mem_seed[PAGE_SIZE * NUM_SEED_PAGES] PAGE_ALIGN;
	static struct track_page s_page[NUM_SEED_PAGES] PAGE_ALIGN;
	for(int i=0; i < NUM_SEED_PAGES; i++) {
		s_page[i].dedicated = false;
		s_page[i].page = L4_FpageLog2(
			(L4_Word_t)&mem_seed[i * PAGE_SIZE], 12);
		list_add_tail(&free_pages[0], &s_page[i].link);
	}

	/* add these to the AVL tree. this triggers most of the allocation
	 * mechanisms.
	 */
	pages_by_range = avl_new(&compare_disjoint_fpages);
	for(int i=0; i < NUM_SEED_PAGES; i++) {
		avl_insert(pages_by_range, &s_page[i].page, &s_page[i]);
	}

	L4_Word_t meminfo = *(L4_Word_t *)(kip_base + 0x54),
		num_mds = meminfo & 0xffff;
	L4_MemoryDesc_t *mds = kip_base + (meminfo >> 16);
	printf("%d memory descriptors at %p\n", (int)num_mds, mds);

	/* discover system memory.
	 *
	 * the way this works is, it finds ranges of conventional memory that
	 * overlap virtual memory and don't overlap a reserved range. those areas
	 * are added to the free lists and the AVL tree. ranges that further
	 * overlap dedicated memory are added only to the AVL tree but not the
	 * freelists.
	 */
	L4_Word_t v_start = 0, v_end = 0;
	int v_at = -1;
	for(int i=0; i < num_mds; i++) {
		if(L4_IsMemoryDescVirtual(&mds[i])) {
			v_start = L4_MemoryDescLow(&mds[i]);
			v_end = L4_MemoryDescHigh(&mds[i]);
			if(v_end - v_start > 1024 * 1024 * 1024) {
				v_at = i;
				break;
			}
		}
	}
	if(v_at < 0) {
		printf("virtual address space isn't declared in KIP; assuming lower 3 GiB\n");
		v_start = 0;
		v_end = 0xc0000000ul - 1;
	}
	for(int i = v_at + 1; i < num_mds; i++) {
		printf("range %#x .. %#x: type %d, virtual %d\n",
			L4_MemoryDescLow(&mds[i]), L4_MemoryDescHigh(&mds[i]),
			L4_MemoryDescType(&mds[i]), L4_IsMemoryDescVirtual(&mds[i]));

		if(L4_MemoryDescType(&mds[i]) != L4_ConventionalMemoryType
			|| L4_IsMemoryDescVirtual(&mds[i]))
		{
			/* skip non-conventional, or non-physical memory */
			continue;
		}

		/* FIXME: handle x86 low memory. */
		L4_Word_t last_start = MAX(L4_Word_t, 0x100000,
			L4_MemoryDescLow(&mds[i]));
		bool last_ded = false;
		for(L4_Word_t addr = last_start;
			addr < L4_MemoryDescHigh(&mds[i]);
			addr += PAGE_SIZE)
		{
			bool dedicate = false, skip = false;
			for(int j = i + 1; j < num_mds && !skip; j++) {
				const L4_MemoryDesc_t *sub = &mds[j];
				if(L4_IsMemoryDescVirtual(sub)
					|| L4_MemoryDescLow(sub) > (addr | PAGE_BITS)
					|| L4_MemoryDescHigh(sub) < addr)
				{
					continue;
				}

				switch(L4_MemoryDescType(sub)) {
					case L4_ConventionalMemoryType:
						/* stacked? that's OK. */
						break;

					/* NOTE: should this also include SharedMemoryType? if
					 * it's actual, mappable memory then the root server would
					 * likely want to have access and whatnot. same for
					 * ArchitectureSpecificMemoryType.
					 */
					case L4_DedicatedMemoryType:
					case L4_BootLoaderSpecificMemoryType:
						dedicate = true;
						break;

					default:
						skip = true;
						break;
				}
			}

			if(skip) {
				free_page_range(last_start, addr - last_start, last_ded);
				last_start = addr + PAGE_SIZE;
				continue;
			}

			if(dedicate != last_ded) {
				free_page_range(last_start, addr - last_start, last_ded);
				last_ded = dedicate;
				last_start = addr;
			}
		}

		free_page_range(last_start,
			L4_MemoryDescHigh(&mds[i]) + 1 - last_start, last_ded);
	}
}


/* support interface for the slab allocator */
void *kmem_alloc_new_page(void) {
	return get_free_page(12);
}


void kmem_free_page(void *ptr) {
	free_phys_page(ptr, 12, false);
}


/* an ugly malloc() for ccan-avl.o's use */
void *malloc(size_t size)
{
	if(size > 4000) {
		printf("failing malloc(%u)\n", (unsigned)size);
		return NULL;
	}

	size = (size + 15) & ~15ul;
	struct malloc_size *ms;
	bool found = false;
	list_for_each(&malloc_size_list, ms, link) {
		if(ms->size == size) {
			found = true;
			break;
		}
	}
	if(!found) {
		static struct kmem_cache *malloc_size_slab = NULL;
		if(malloc_size_slab == NULL) {
			malloc_size_slab = KMEM_CACHE_NEW("malloc_size_slab",
				struct malloc_size);
		}
		ms = kmem_cache_alloc(malloc_size_slab);
		ms->size = size;
		ms->cache = kmem_cache_create("malloc slab", size, 16, 0, NULL, NULL);
		list_add(&malloc_size_list, &ms->link);
	}

	return kmem_cache_alloc(ms->cache);
}


void free(void *ptr)
{
	if(ptr == NULL) return;
	struct kmem_cache *cache = kmem_cache_find(ptr);
	if(cache != NULL) kmem_cache_free(cache, ptr);
	else {
		printf("can't find slab to free memory at %p (leaked!)\n", ptr);
	}
}


int main(void)
{
	printf("hello, world!\n");

	L4_Word_t apiver, apiflags, kernelid;
	void *kip = L4_KernelInterface(&apiver, &apiflags, &kernelid);

	send_test(0xdeadbeef);
	send_test(L4_SystemClock().raw);

	tid_test();
	unmap_test();
	threadctl_test();
	threadswitch_test();
	schedule_test();
	spacectl_test();

	build_heap(kip);

	return sigma0_ipc_loop(kip);
}
