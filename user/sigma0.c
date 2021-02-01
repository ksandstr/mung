
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <stdbool.h>
#include <string.h>
#include <ccan/likely/likely.h>
#include <ccan/list/list.h>
#include <ccan/compiler/compiler.h>

#include <l4/types.h>
#include <l4/ipc.h>
#include <l4/thread.h>
#include <l4/vregs.h>
#include <l4/syscall.h>
#include <l4/kip.h>
#include <l4/kdebug.h>

#include <ukernel/mm.h>
#include <ukernel/slab.h>
#include <ukernel/util.h>
#include <ukernel/rbtree.h>
#include <ukernel/memdesc.h>


#define NUM_SEED_PAGES 12
#define PAGE_BUCKETS 20		/* word size - 12 */

#define PF_TRACING 0


struct track_page
{
	struct rb_node rb;		/* in pages_by_range */
	struct list_node link;
	L4_Fpage_t page;
	bool dedicated:1, readonly:1;
};


static struct list_head free_pages[PAGE_BUCKETS];	/* size_log2 = index + 12 */
static struct rb_root pages_by_range;
static LIST_HEAD(slab_page_list);
static LIST_HEAD(reuse_trk_list);		/* for static <struct track_page> */
static struct kmem_cache *track_page_slab = NULL;


static L4_Fpage_t get_free_page(int size_log2);
static L4_Fpage_t get_free_page_at(L4_Word_t address, int size_log2);
static void free_phys_page(
	void *ptr, int size_log2,
	bool dedicate, bool readonly);
static struct track_page *find_page_by_range(L4_Fpage_t range);


static bool invariants(void)
{
#ifndef NDEBUG
	/* 1. all pages in free_pages[] should have `!dedicated', and appear in
	 * pages_by_range.
	 */
	for(int i=0; i < PAGE_BUCKETS; i++) {
		struct track_page *pg;
		list_for_each(&free_pages[i], pg, link) {
			assert(!pg->dedicated);
			assert(find_page_by_range(pg->page) == pg);
		}
	}

	/* 2. pages in slab_page_list shouldn't appear either in pages_by_range or
	 * free_pages[].
	 */
	struct track_page *pg;
	list_for_each(&slab_page_list, pg, link) {
		assert(find_page_by_range(pg->page) == NULL);
		assert(L4_IsNilFpage(get_free_page_at(
			L4_Address(pg->page), L4_SizeLog2(pg->page))));
	}
#endif

	return true;
}


void con_putstr(const char *str) {
	L4_KDB_PrintString((char *)str);
}


void abort(void)
{
	printf("abort() called!\n");
	for(;;) {
		/* wheeee */
		L4_Sleep(L4_Never);
	}
}


void __assert_failure(
	const char *condition,
	const char *file,
	unsigned int line,
	const char *function)
{
	printf("assert(%s) failed in `%s' (%s:%u)\n", condition, function,
		file, line);
	abort();
}


static int sigma0_ipc_loop(void *kip_base)
{
	printf("entering IPC loop\n");

	for(;;) {
		L4_ThreadId_t from;
		L4_MsgTag_t tag = L4_Wait(&from);

		for(;;) {
			if(L4_IpcFailed(tag)) {
				printf("pager ipc failed (errorcode %#lx)\n", L4_ErrorCode());
				break;
			}

			L4_ThreadId_t sender = from;
			if((tag.X.label & 0xfff0) == 0xffe0
				&& tag.X.u == 2 && tag.X.t == 0)
			{
				L4_Word_t ip, addr;
				L4_StoreMR(1, &addr);
				L4_StoreMR(2, &ip);
#if PF_TRACING
				printf("pf in %d:%d (ip %#lx, addr %#lx)\n",
					from.global.X.thread_no, from.global.X.version,
					ip, addr);
#endif
				static L4_Word_t last_fault = 0;
				if(last_fault == addr) {
					printf("... two faults at the same address! s0 rejects this.\n");
					break;
				} else {
					last_fault = addr;
				}
				L4_Fpage_t page = get_free_page_at(addr & ~PAGE_MASK,
					PAGE_BITS);
				if(L4_IsNilFpage(page)) {
					printf("page at %#lx not found (sender=%lu:%lu, ip=%#lx)\n",
						addr, L4_ThreadNo(sender), L4_Version(sender), ip);
					break;
				}
				assert(L4_Address(page) == (addr & ~PAGE_MASK));
				L4_GrantItem_t idemp = L4_GrantItem(page, addr & ~PAGE_MASK);
				L4_LoadMR(0, (L4_MsgTag_t){ .X.t = 2 }.raw);
				L4_LoadMR(1, idemp.raw[0]);
				L4_LoadMR(2, idemp.raw[1]);
			} else if((tag.X.label & 0xfff0) == 0xffa0
				&& tag.X.t == 0 && (tag.X.u == 2 || tag.X.u == 3))
			{
				/* s0 user protocol: fpage request (as from L4_Sigma0_GetPage,
				 * L4_Sigma0_GetAny)
				 */
				L4_Fpage_t req_fpage;
				L4_Word_t req_attr, req_high;
				L4_StoreMR(1, &req_fpage.raw);
				L4_StoreMR(2, &req_attr);
				L4_StoreMR(3, &req_high);
#ifdef DEBUG_ME_HARDER
				printf("user tid=%d:%d req=%#lx:%#lx attr=%#lx\n",
					sender.global.X.thread_no, sender.global.X.version,
					L4_Address(req_fpage), L4_Size(req_fpage), req_attr);
#endif
				L4_Fpage_t map_page;
				if((req_fpage.raw & (~0u << PAGE_BITS)) == (~0u << PAGE_BITS)) {
					map_page = get_free_page(L4_SizeLog2(req_fpage));
				} else {
					map_page = get_free_page_at(L4_Address(req_fpage),
						L4_SizeLog2(req_fpage));
				}
				if(L4_IsNilFpage(map_page)) {
					/* reject */
					L4_LoadMR(0, (L4_MsgTag_t){ .X.t = 2 }.raw);
					L4_LoadMR(1, 0x8);
					L4_LoadMR(2, L4_Nilpage.raw);
				} else {
					L4_GrantItem_t map = L4_GrantItem(map_page,
						L4_Address(map_page));
					L4_LoadMR(0, (L4_MsgTag_t){ .X.t = 2 }.raw);
					L4_LoadMR(1, map.raw[0]);
					L4_LoadMR(2, map.raw[1]);
				}
			} else if((tag.X.label & 0xfff0) == 0xff80
				&& tag.X.u == 2 && tag.X.t == 0)
			{
				/* I/O faults (ia32, amd64) */
				L4_Fpage_t iofp;
				L4_StoreMR(1, &iofp.raw);
				if(!L4_IsIoFpage(iofp)) {
					printf("I/O fault didn't deliver I/O fpage? what.\n");
					break;
				}
#if PF_TRACING
				printf("iopf: port=%#lx:%#lx\n",
					L4_IoFpagePort(iofp), L4_IoFpageSize(iofp));
#endif
				/* just map it. FIXME: only map each port once. */
				L4_MapItem_t map = L4_MapItem(
					L4_IoFpageLog2(L4_IoFpagePort(iofp),
						L4_IoFpageSizeLog2(iofp)), 0);
				L4_Set_Rights(&map.X.snd_fpage, L4_FullyAccessible);
				L4_LoadMR(0, (L4_MsgTag_t){ .X.t = 2 }.raw);
				L4_LoadMR(1, map.raw[0]);
				L4_LoadMR(2, map.raw[1]);
			} else {
				printf("unknown IPC label %#lx (u %d, t %d) from %lu:%lu\n",
					(L4_Word_t)tag.X.label, tag.X.u, tag.X.t,
					L4_ThreadNo(from), L4_Version(from));
				break;
			}

#ifdef DEBUG_ME_HARDER
			assert(invariants());
#endif

			tag = L4_ReplyWait(sender, &from);
		}
	}

	/* never reached, but whatever. */
	return 0;
}


static void free_track_page(struct track_page *tp)
{
	if(kmem_cache_find(tp) != NULL) {
		kmem_cache_free(track_page_slab, tp);
	} else {
		/* from s_page[], reused in free_phys_page(). */
		list_add(&reuse_trk_list, &tp->link);
	}
}


static int fpage_cmp(L4_Fpage_t a, L4_Fpage_t b)
{
	if(fpage_overlap(a, b)) return 0;

	const L4_Fpage_t *key = &a, *cand = &b;
	if(L4_Address(*key) + L4_Size(*key) <= L4_Address(*cand)) return -1;
	if(L4_Address(*key) >= L4_Address(*cand) + L4_Size(*cand)) return 1;

	/* mysterious. */
	return 0;
}


static struct track_page *find_page_by_range(L4_Fpage_t key)
{
	struct rb_node *n = pages_by_range.rb_node;
	while(n != NULL) {
		struct track_page *pg = rb_entry(n, struct track_page, rb);
		int c = fpage_cmp(key, pg->page);
		if(c < 0) n = n->rb_left;
		else if(c > 0) n = n->rb_right;
		else return pg;
	}
	return NULL;
}


static void free_page_range(
	L4_Word_t start, L4_Word_t length,
	bool dedicate, bool readonly)
{
	int bit;
	L4_Word_t addr;
	for_page_range(start, start + length, addr, bit) {
		free_phys_page((void *)addr, bit, dedicate, readonly);
	}
}


static L4_Fpage_t get_free_page_at(L4_Word_t address, int want_size_log2)
{
	assert((address & PAGE_MASK) == 0);
	assert((address & ((1ul << want_size_log2) - 1)) == 0);

	L4_Fpage_t outfp = L4_FpageLog2(address, want_size_log2);
	struct track_page *pg = find_page_by_range(outfp);
	if(pg == NULL) return L4_Nilpage;
	assert(fpage_overlap(outfp, pg->page));

	L4_Fpage_t page = pg->page;
	bool dedicated = pg->dedicated, readonly = pg->readonly;
	__rb_erase(&pg->rb, &pages_by_range);
	if(!dedicated) {
		list_del_from(&free_pages[L4_SizeLog2(pg->page) - PAGE_BITS],
			&pg->link);
	}
	free_track_page(pg);

	if(L4_Address(page) == address && L4_SizeLog2(page) == want_size_log2) {
		/* simple case; nothing to do. */
	} else if(L4_SizeLog2(page) < want_size_log2) {
		/* subpage case. */
		outfp = page;
	} else {
		/* superpage case: release the unwanted part back. */
		L4_Word_t start_len = address - L4_Address(page);
		free_page_range(L4_Address(page), start_len, dedicated, readonly);
		L4_Word_t top = address + (1 << want_size_log2),
			pg_top = L4_Address(page) + L4_Size(page);
		if(top < pg_top) {
			free_page_range(top, pg_top - top, dedicated, readonly);
		}
	}

	L4_Set_Rights(&outfp, readonly ? L4_ReadeXecOnly : L4_FullyAccessible);
	return outfp;
}


static L4_Fpage_t get_free_page(int size_log2)
{
	struct track_page *pg = NULL;
	for(int i = size_log2 - PAGE_BITS; i < PAGE_BUCKETS; i++) {
		pg = list_pop(&free_pages[i], struct track_page, link);
		if(pg != NULL) break;
	}
	if(pg == NULL) return L4_Nilpage;
	__rb_erase(&pg->rb, &pages_by_range);

	assert(!pg->dedicated);
	assert(!pg->readonly);
	L4_Fpage_t ret = pg->page;
	if(L4_SizeLog2(pg->page) > size_log2) {
		/* release the part we don't need. */
		free_page_range(L4_Address(pg->page) + (1 << size_log2),
			L4_Size(pg->page) - (1 << size_log2), false, false);
	}
	free_track_page(pg);

	L4_Set_Rights(&ret, L4_FullyAccessible);
	return ret;
}


static struct track_page *insert_track_page_helper(
	struct rb_root *root,
	struct track_page *pg)
{
	struct rb_node **p = &root->rb_node, *parent = NULL;
	while(*p != NULL) {
		parent = *p;
		struct track_page *other = rb_entry(parent, struct track_page, rb);
		int c = fpage_cmp(pg->page, other->page);
		if(c < 0) p = &(*p)->rb_left;
		else if(c > 0) p = &(*p)->rb_right;
		else return other;
	}

	rb_link_node(&pg->rb, parent, p);
	return NULL;
}


static struct track_page *insert_track_page(struct track_page *pg)
{
	struct track_page *dupe = insert_track_page_helper(&pages_by_range, pg);
	if(likely(dupe == NULL)) __rb_insert_color(&pg->rb, &pages_by_range);
	return dupe;
}


static void free_phys_page(
	void *ptr, int size_log2,
	bool dedicate, bool readonly)
{
	if(track_page_slab == NULL) {
		track_page_slab = KMEM_CACHE_NEW("track_page_slab", struct track_page);
	}

	struct track_page *pg = list_pop(&reuse_trk_list, struct track_page, link);
	if(pg == NULL) pg = kmem_cache_alloc(track_page_slab);
	if(pg == NULL) {
		printf("warning: can't allocate track_page for %#lx:%#lx; memory was lost\n",
			(L4_Word_t)ptr, 1ul << size_log2);
		return;
	}

	pg->dedicated = dedicate;
	pg->readonly = readonly;
	pg->page = L4_FpageLog2((L4_Word_t)ptr, size_log2);
	if(insert_track_page(pg) != NULL) {
		printf("warning: track_page for %#lx:%#lx already exists!\n",
			L4_Address(pg->page), L4_Size(pg->page));
		free_track_page(pg);
		return;
	}
	if(!dedicate) {
		list_add_tail(&free_pages[size_log2 - PAGE_BITS], &pg->link);
	}
}


static void build_heap(void *kip_base)
{
	for(int i=0; i < PAGE_BUCKETS; i++) list_head_init(&free_pages[i]);
	pages_by_range = RB_ROOT;

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
		insert_track_page(&s_page[i]);
	}

	L4_Word_t meminfo = *(L4_Word_t *)(kip_base + 0x54),
		num_mds = meminfo & 0xffff;
	struct memdescbuf mds = {
		.ptr = kip_base + (meminfo >> 16),
		.len = num_mds, .size = num_mds,
	};

	/* discover system memory.
	 *
	 * the way this works is, it finds ranges of conventional memory that
	 * overlap virtual memory and don't overlap a reserved or dedicated range.
	 * the former criteria is because sigma0 currently cannot access physical
	 * memory outside of its virtual memory space, and the latter follows from
	 * the kernel not assigning reserved or dedicated memory to sigma0
	 * (besides program pages belonging to sigma0).
	 *
	 * ranges of shared, bootloader-defined, and architecture-specific memory
	 * are added only to the pages_by_range tree but not the freelists, so
	 * they'll only be handed out at request.
	 */
	L4_Word_t v_low = ~0ul, v_high = 0, q_start = 0, q_end = ~0ul;
	for(;;) {
		L4_Fpage_t part = mdb_query(&mds, q_start, q_end,
			true, false, L4_ConventionalMemoryType);
		if(L4_IsNilFpage(part)) break;
		if(v_low == ~0ul) v_low = L4_Address(part);
		v_high = MAX(L4_Word_t, FPAGE_HIGH(part), v_high);
		q_start = FPAGE_HIGH(part) + 1;
	}
	printf("virtual address space is %#lx..%#lx\n", v_low, v_high);

	int as_mask = 0, b_mask = 0;
	for(int i=0; i < mds.len; i++) {
		L4_Word_t t = L4_MemoryDescType(&mds.ptr[i]);
		switch(t & 0xf) {
			case L4_ArchitectureSpecificMemoryType:
				as_mask |= 1 << (t >> 4);
				break;
			case L4_BootLoaderSpecificMemoryType:
				b_mask |= 1 << (t >> 4);
				break;
		}
	}

	static const L4_Word_t types[] = {
		L4_ConventionalMemoryType,
		L4_SharedMemoryType,
		L4_BootLoaderSpecificMemoryType,
		L4_ArchitectureSpecificMemoryType,
	};
	int t_masks[] = { 1, 1, b_mask, as_mask };
	assert(NUM_ELEMENTS(types) == NUM_ELEMENTS(t_masks));
	for(int i=0; i < NUM_ELEMENTS(types); i++) {
		while(t_masks[i]) {
			L4_Word_t subtype = ffsl(t_masks[i]) - 1;
			assert(CHECK_FLAG(t_masks[i], 1 << subtype));
			t_masks[i] &= ~(1 << subtype);
			q_start = v_low; q_end = v_high;
			for(;;) {
				L4_Fpage_t part = mdb_query(&mds, q_start, q_end,
					false, false, types[i] | (subtype << 4));
				if(L4_IsNilFpage(part)) break;
#ifdef DEBUG_ME_HARDER
				printf("memdesc part=[%#lx, %#lx] (%lu KiB), type=%#lx\n",
					FPAGE_LOW(part), FPAGE_HIGH(part), L4_Size(part) / 1024,
					types[i] | (subtype << 4));
#endif
				if(L4_SizeLog2(part) >= PAGE_BITS) {
					free_phys_page((void *)L4_Address(part), L4_SizeLog2(part),
						types[i] != L4_ConventionalMemoryType,
						types[i] == L4_BootLoaderSpecificMemoryType
							&& subtype == L4_ReadOnlyBIOSRegion);
				}
				q_start = FPAGE_HIGH(part) + 1;
			}
		}
	}
}


/* support interface for the slab allocator */
void *kmem_alloc_new_page(void) {
	L4_Fpage_t p = get_free_page(12);
	return L4_IsNilFpage(p) ? NULL : (void *)L4_Address(p);
}

void kmem_free_page(void *ptr) {
	free_phys_page(ptr, 12, false, false);
}


/* for strdup(), which nothing in sigma0 calls */
void *malloc(size_t n) {
	abort();
}

void free(void *ptr) {
	abort();
}


int main(void)
{
	L4_Word_t apiver, apiflags, kernelid;
	void *kip = L4_KernelInterface(&apiver, &apiflags, &kernelid);
	build_heap(kip);
	assert(invariants());
	return sigma0_ipc_loop(kip);
}
