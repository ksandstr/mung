
#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include <assert.h>
#include <errno.h>

#include <ccan/likely/likely.h>
#include <ccan/list/list.h>
#include <ccan/htable/htable.h>
#include <ccan/compiler/compiler.h>

#include <l4/types.h>
#include <l4/kip.h>
#include <l4/vregs.h>

#include <ukernel/space.h>
#include <ukernel/mm.h>
#include <ukernel/x86.h>
#include <ukernel/slab.h>
#include <ukernel/misc.h>
#include <ukernel/util.h>
#include <ukernel/thread.h>
#include <ukernel/ipc.h>
#include <ukernel/sched.h>
#include <ukernel/mapdb.h>
#include <ukernel/kip.h>
#include <ukernel/ptab.h>


#define NUM_KERN_UTCB_PAGES ((UTCB_SIZE * NUM_KERNEL_THREADS \
	+ PAGE_SIZE - 1) / PAGE_SIZE)


/* allocated statically for bootstrapping */
static struct utcb_page kernel_utcb_pages[NUM_KERN_UTCB_PAGES];

struct space *current_space = NULL;

struct space *kernel_space = NULL;
static struct space kernel_space_mem;

static struct kmem_cache *space_slab = NULL, *utcb_page_slab = NULL;
static struct list_head space_list = LIST_HEAD_INIT(space_list);


/* hash & cmp functions for utcb_pages */
static size_t hash_utcb_page(const void *ptr, void *priv) {
	const struct utcb_page *p = ptr;
	return int_hash(p->pos);
}


static inline bool cmp_utcb_page(const void *cand, void *key) {
	const struct utcb_page *p = cand;
	return p->pos == *(uint16_t *)key;
}


#define NO_PTAB_TO_MAPDB (1 << 0)

#ifndef NDEBUG
#include <ukernel/invariant.h>

static UNNEEDED uint32_t max_page_id(void)
{
	static uint32_t max_page = 0;
	if(unlikely(max_page == 0)) {
		L4_Word_t md_num = 0;
		L4_MemoryDesc_t *md;
		while((md = L4_MemoryDesc(kip_mem, md_num)) != NULL) {
			if(L4_MemoryDescType(md) == L4_ConventionalMemoryType
				&& !L4_IsMemoryDescVirtual(md))
			{
				L4_Word_t hi = L4_MemoryDescHigh(md);
				if((hi >> 12) > max_page) max_page = hi >> 12;
			}
			md_num++;
		}
	}
	return max_page;
}


/* TODO: this function is needlessly heavy. it makes unit tests very slow. */
static bool check_space(int opt, struct space *sp)
{
#ifdef DEBUG_ME_HARDER
	INV_CTX;

	L4_ThreadId_t sp_tid = space_name(sp);
	inv_push("space %lu:%lu", L4_ThreadNo(sp_tid), L4_Version(sp_tid));

	inv_ok(!CHECK_FLAG_ANY(sp->flags, ~(uint16_t)(SF_PRIVILEGE)),
		"no undefined flags");
	if(sp == kernel_space) goto end;	/* TODO: move this down */

	inv_ok1(sp->pdirs != NULL);

	/* check that pagetable pages referenced in the directory exist in
	 * ->ptab_pages .
	 */
	const uint32_t *pdir_mem = sp->pdirs->vm_addr;
	for(int i=0; i < PAGE_SIZE / sizeof(uint32_t); i++) {
		const uint32_t pde = pdir_mem[i];
		inv_push("checking pdir entry %d (%#x):", i, pde);
		/* PRESENT is obvious. !USER occurs with the kernel's directories
		 * reflected into the space's high address range.
		 */
		if(CHECK_FLAG_ALL(pde, PDIR_PRESENT | PDIR_USER)) {
			uint32_t pt_id = pde >> 12;
			inv_log("pt_id %u", pt_id);
			inv_ok1(pt_id <= max_page_id());
			uint32_t grp_addr = PT_UPPER_SIZE * i;
			struct map_group *grp = htable_get(&sp->ptab_groups,
				int_hash(grp_addr), &cmp_group_addr, &grp_addr);
			inv_ok(grp != NULL, "group for i=%d must exist", i);
			inv_ok(grp->ptab_page->id == pt_id,
				"expected grp->ptab_page->id == %u, found %u",
				pt_id, grp->ptab_page->id);
		}
		inv_pop();
	}

	/* check that correct pages, or holes, are mapped for utcb_pages[]. */
	if(sp->utcb_pages.elems > 0) {
		inv_ok1(sp->utcb_area.raw != L4_Nilpage.raw);
		for(int i=0; i < NUM_UTCB_PAGES(sp->utcb_area); i++) {
			L4_Word_t page_addr = L4_Address(sp->utcb_area) + i * PAGE_SIZE;
			inv_push("checking utcb page %d/%d at %#lx:", i,
				(int)NUM_UTCB_PAGES(sp->utcb_area), page_addr);
			uint16_t page_pos = i;
			struct utcb_page *up = htable_get(&sp->utcb_pages,
				int_hash(page_pos), &cmp_utcb_page, &page_pos);

			/* (this just accesses the page tables.) */
			const uint32_t *ptab_mem;
			int dir_ix = page_addr >> 22, pg_ix = (page_addr >> 12) & 0x3ff;
			if(CHECK_FLAG(pdir_mem[dir_ix], PDIR_PRESENT)) {
				uint32_t grp_addr = page_addr & ~PT_UPPER_MASK;
				struct map_group *grp = htable_get(&sp->ptab_groups,
					int_hash(grp_addr), &cmp_group_addr, &grp_addr);
				assert(grp != NULL);		/* checked earlier */
				assert(grp->ptab_page->id == pdir_mem[dir_ix] >> 12);
				ptab_mem = map_vm_page(grp->ptab_page, VM_SYSCALL);
				assert(ptab_mem != NULL);		/* if-else block output */
			} else {
				ptab_mem = NULL;
			}

			if(up == NULL || ptab_mem == NULL) {
				uint32_t ent = ptab_mem != NULL ? ptab_mem[pg_ix] : 0;
				inv_log("no utcb page; ptab_mem is %p, entry %d is %#x",
					ptab_mem, pg_ix, ent);
				inv_ok1(ptab_mem == NULL || !CHECK_FLAG(ent, PT_PRESENT));
			} else {
				inv_log("utcb page present; entry %d is %#x (want id %u)",
					pg_ix, ptab_mem[pg_ix], up->pg->id);
				inv_ok1(!CHECK_FLAG(ptab_mem[pg_ix], PT_PRESENT)
					|| (ptab_mem[pg_ix] >> 12) == up->pg->id);

				inv_ok1(up->pg != NULL);
				for(int j=0; j < UTCB_PER_PAGE; j++) {
					inv_ok1(!CHECK_FLAG(up->occmap, 1 << j)
						|| up->slots[j] != NULL);
					inv_ok1(CHECK_FLAG(up->occmap, 1 << j)
						|| up->slots[j] == NULL);
				}
			}
			inv_pop();
		}
	}

	if(!CHECK_FLAG(opt, NO_PTAB_TO_MAPDB)) {
		inv_push("checking page table to mapdb");
		for(int pd_ix = 0; pd_ix < 1024; pd_ix++) {
			const uint32_t pde = pdir_mem[pd_ix];
			if(!CHECK_FLAG_ALL(pde, PDIR_PRESENT | PDIR_USER)) continue;

			inv_push("pdir entry %d (%#x)", pd_ix, pde);
			uint32_t grp_addr = pd_ix * PT_UPPER_SIZE;
			struct map_group *grp = htable_get(&sp->ptab_groups,
				int_hash(grp_addr), &cmp_group_addr, &grp_addr);
			assert(grp != NULL);			/* checked earlier */
			assert(grp->ptab_page->id == pde >> 12);
			uint32_t *ptab_mem = map_vm_page(grp->ptab_page, VM_SYSCALL);

			for(int i = 0; i < 1024; i++) {
				L4_Word_t addr = ((L4_Word_t)pd_ix << 22) | (i << 12);
				if(ADDR_IN_FPAGE(sp->kip_area, addr)) continue;

				const uint32_t ent = ptab_mem[i], page_id = ent >> 12;
				if(!CHECK_FLAG_ALL(ent, PT_PRESENT | PT_USER)) continue;
				inv_push("table entry at %d is %#x (page_id %u)", i, ent,
					page_id);
				inv_ok(page_id <= max_page_id(),
					"page_id must be <= %u", max_page_id());

				inv_log("probing mapdb for ix %d, address %#lx, ent %#x",
					i, addr, ent);
				struct map_entry *e = mapdb_probe(sp, addr);
				inv_ok1(e != NULL);
				inv_ok1(mapdb_page_id_in_entry(e, addr) == page_id);
				inv_ok(!CHECK_FLAG(ent, PT_RW) ||
					CHECK_FLAG(L4_Rights(e->range), L4_Writable),
					"PT_RW implies L4_Writable");
				inv_pop();
			}

			inv_pop();
		}
		inv_pop();
	}

end:
	inv_pop();

	return true;

inv_fail:
	return false;
#else
	return true;
#endif
}


static bool check_all_spaces(int opt)
{
	struct space *sp, *next;
	list_for_each_safe(&space_list, sp, next, link) {
		if(!check_space(opt, sp)) return false;
	}

	return true;
}
#else
static bool check_space(int opt, struct space *sp) { return true; }
static bool check_all_spaces(int opt) { return true; }
#endif


static void space_init(struct space *sp, struct list_head *resv_list)
{
	sp->utcb_top = 0;
	sp->kip_area = L4_Nilpage;
	sp->utcb_area = L4_Nilpage;

	htable_init(&sp->utcb_pages, &hash_utcb_page, NULL);

	sp->pdirs = get_kern_page(0);
	if(unlikely(resv_list != NULL)) {
		list_add(resv_list, &sp->pdirs->link);
	}

	pdir_t *dirs = sp->pdirs->vm_addr;
	if(unlikely(resv_list != NULL)) {
		/* creation of the kernel's <struct space>. */
		memset(dirs, '\0', PAGE_SIZE);
	} else {
		const pdir_t *kernel_pdirs = kernel_space->pdirs->vm_addr;
		for(int i=0; i < 1024; i++) {
			if(i < KERNEL_SEG_START >> 22) dirs[i] = 0;
			else dirs[i] = kernel_pdirs[i];
		}
	}

	list_add(&space_list, &sp->link);
}


/* call @fn for each thread in @sp. stop early if @fn returns false. */
static inline void for_each_thread_in_space(
	struct space *sp,
	bool (*fn)(struct thread *t, void *priv),
	void *priv)
{
	struct htable_iter it;
	for(struct utcb_page *up = htable_first(&sp->utcb_pages, &it);
		up != NULL;
		up = htable_next(&sp->utcb_pages, &it))
	{
		for(int i=0; i < UTCB_PER_PAGE; i++) {
			if(up->slots[i] != NULL && !(*fn)(up->slots[i], priv)) return;
		}
	}
}


static bool store_first_tid(struct thread *t, void *priv) {
	((L4_ThreadId_t *)priv)->raw = t->id;
	return false;
}


/* spaces without threads are nameless. this is also used to check whether
 * there's at least one active thread in the address space, active being
 * defined as having an UTCB slot.
 */
L4_ThreadId_t space_name(struct space *sp)
{
	L4_ThreadId_t ret = L4_nilthread;
	for_each_thread_in_space(sp, &store_first_tid, &ret);
	return ret;
}


struct space *space_new(void)
{
	assert(space_slab != NULL);

	struct space *sp = kmem_cache_alloc(space_slab);
	space_init(sp, NULL);
	mapdb_init(sp);
	sp->tss = NULL;
	sp->tss_len = 0;
	sp->tss_seg = 0;
	sp->flags = 0;
	sp->redirector = NULL;

	return sp;
}


static void clear_utcb_pages(struct space *sp)
{
	assert(sp != kernel_space);
	struct htable_iter it;
	for(struct utcb_page *up = htable_first(&sp->utcb_pages, &it);
		up != NULL;
		up = htable_next(&sp->utcb_pages, &it))
	{
		assert(up->occmap == 0);
		free_kern_page(up->pg);
		kmem_cache_free(utcb_page_slab, up);
	}
	htable_clear(&sp->utcb_pages);
}


void space_free(struct space *sp)
{
	mapdb_destroy(sp);
	clear_utcb_pages(sp);
	free_kern_page(sp->pdirs);

	list_del_from(&space_list, &sp->link);

	if(sp->tss != NULL) {
		assert(sp->tss != &kernel_tss);
		free(sp->tss);
		free_gdt_slot(sp->tss_seg);
	}

	kmem_cache_free(space_slab, sp);
}


void space_add_thread(struct space *sp, struct thread *t)
{
	assert(t->space == sp);
	assert(t->utcb_pos >= 0);
	assert(t->utcb_page != NULL);

	int page = t->utcb_pos / UTCB_PER_PAGE,
		slot = t->utcb_pos % UTCB_PER_PAGE;
	assert(t->utcb_page->pos == page);
	assert(!CHECK_FLAG(t->utcb_page->occmap, 1 << slot));
	assert(t->utcb_page->slots[slot] == NULL);

	t->utcb_page->slots[slot] = t;
	t->utcb_page->occmap |= 1 << slot;
}


void space_remove_thread(struct space *sp, struct thread *t)
{
	assert(t->space == sp);
	assert(t->utcb_pos >= 0);
	assert(t->utcb_page != NULL);

	int page = t->utcb_pos / UTCB_PER_PAGE,
		slot = t->utcb_pos % UTCB_PER_PAGE;
	struct utcb_page *up = t->utcb_page;
	assert(t->utcb_page->pos == page);
	assert(CHECK_FLAG(t->utcb_page->occmap, 1 << slot));
	assert(t->utcb_page->slots[slot] == t);

	up->slots[slot] = NULL;
	up->occmap &= ~(1 << slot);

	if(up->occmap == 0 && likely(!IS_KERNEL_THREAD(t))) {
		/* toss the UTCB page. */
		L4_Fpage_t fp = L4_FpageLog2(
			L4_Address(sp->utcb_area) + up->pos * PAGE_SIZE,
			PAGE_BITS);
		mapdb_erase_special(sp, fp);

#ifndef NDEBUG
		struct pt_iter it;
		pt_iter_init(&it, sp);
		assert(!pt_page_present(&it, L4_Address(fp)));
		pt_iter_destroy(&it);
#endif

		/* can't toss statically allocated UTCB pages, though. */
		assert(up < &kernel_utcb_pages[0]
			|| up >= &kernel_utcb_pages[NUM_KERN_UTCB_PAGES]);
		free_kern_page(up->pg);
		htable_del(&sp->utcb_pages, int_hash(up->pos), up);
		kmem_cache_free(utcb_page_slab, up);

		if(sp->utcb_pages.elems == 0) space_free(sp);
	}

	t->utcb_page = NULL;
	t->utcb_pos = -1;
}


struct utcb_page *space_get_utcb_page(struct space *sp, uint16_t page_pos)
{
	struct utcb_page *up = htable_get(&sp->utcb_pages, int_hash(page_pos),
		&cmp_utcb_page, &page_pos);
	if(up == NULL) {
		if(page_pos >= L4_Size(sp->utcb_area) / PAGE_SIZE) return NULL;

		assert(sp != kernel_space);
		up = kmem_cache_alloc(utcb_page_slab);
		up->pos = page_pos;
		up->occmap = 0;
		up->pg = get_kern_page(0);
		memset(up->pg->vm_addr, 0, PAGE_SIZE);
		for(int i=0; i < UTCB_PER_PAGE; i++) up->slots[i] = NULL;
		htable_add(&sp->utcb_pages, int_hash(page_pos), up);

		if(likely(sp != kernel_space)) {
			L4_Fpage_t u_page = L4_FpageLog2(L4_Address(sp->utcb_area)
				+ page_pos * PAGE_SIZE, PAGE_BITS);
			L4_Set_Rights(&u_page, L4_Readable | L4_Writable);
			/* TODO: pass error result from mapdb_add_map() */
			mapdb_add_map(sp, NULL, 2, u_page, up->pg->id);

#ifndef NDEBUG
			struct pt_iter it;
			pt_iter_init(&it, sp);
			bool upper = false;
			assert(pt_get_pgid(&it, &upper, L4_Address(u_page)) == up->pg->id
				|| !upper);
			pt_iter_destroy(&it);
#endif
		}
	}

	return up;
}


struct space *space_switch(struct space *next)
{
	struct space *old = current_space;
	if(unlikely(old == NULL)) old = kernel_space;

	if(old == next) return old;

	if(old->tss != next->tss) {
		int slot = next->tss_seg;
		/* (XXX what is this for anyway?) */
		if(slot == 0) slot = SEG_KERNEL_TSS;
		unbusy_tss(slot);
		set_current_tss(slot);
	}

	/* page directory base register */
	asm volatile ("movl %0, %%cr3"
		:: "a" (next->pdirs->id << 12)
		: "memory");
	/* (TODO: if interrupted here, current_space won't correspond to the page
	 * tables. unlikely to be an issue unless schedule() is called within an
	 * interrupt handler.)
	 */
	current_space = next;

	return old;
}


struct space *space_find(thread_id tid)
{
	assert(L4_IsGlobalId((L4_ThreadId_t){ .raw = tid }));

	struct thread *t = thread_find(tid);
	return t == NULL || t->id != tid ? NULL : t->space;
}


/* the UTCB setting part of SpaceControl. no checks, no brakes. */
int space_set_utcb_area(struct space *sp, L4_Fpage_t area)
{
	if(FPAGE_LOW(area) >= KERNEL_SEG_START
		|| FPAGE_HIGH(area) >= KERNEL_SEG_START)
	{
		/* UTCB outside address space. */
		return 6;
	}

	clear_utcb_pages(sp);
	sp->utcb_area = area;
	sp->utcb_top = L4_Address(area) + L4_Size(area) - UTCB_SIZE / 2 - 1;

	return 0;
}


int space_set_kip_area(struct space *sp, L4_Fpage_t area)
{
	assert(sp->utcb_pages.elems == 0);

	if(FPAGE_LOW(area) >= KERNEL_SEG_START
		|| FPAGE_HIGH(area) >= KERNEL_SEG_START)
	{
		/* KIP outside address space. */
		return 7;
	}

	if(!L4_IsNilFpage(sp->kip_area)) {
		mapdb_erase_special(sp, sp->kip_area);

	/* NOTE: this may be redundant with mapdb_erase_special()'s
	 * postconditions.
	 */
#ifndef NDEBUG
		struct pt_iter it;
		pt_iter_init(&it, sp);
		for(L4_Word_t pos = 0; pos < L4_Size(sp->kip_area); pos += PAGE_SIZE) {
			assert(!pt_page_present(&it, L4_Address(sp->kip_area) + pos));
		}
		pt_iter_destroy(&it);
#endif
	}

	sp->kip_area = area;
	L4_Fpage_t k_page = L4_FpageLog2(L4_Address(area),
		MIN(int, L4_SizeLog2(area), PAGE_BITS));
	L4_Set_Rights(&k_page, L4_Readable);
	/* TODO: pass error result from mapdb_add_map() */
	uint32_t kip_pgid = (L4_Word_t)kip_mem >> PAGE_BITS;
	mapdb_add_map(sp, NULL, 2, k_page, kip_pgid);

	/* NOTE: this may be redundant with mapdb_add_map()'s postconditions. */
#ifndef NDEBUG
	struct pt_iter it;
	pt_iter_init(&it, sp);
	bool upper = false;
	assert(pt_get_pgid(&it, &upper, L4_Address(k_page)) == kip_pgid
		|| !upper);
	pt_iter_destroy(&it);
#endif

	return 0;
}


struct thread *space_find_local_thread(struct space *sp, L4_LthreadId_t ltid)
{
	assert(ltid.X.zeros == 0);

	/* (this micro-optimization saves 3 insns.) */
	assert((sp->utcb_area.raw & ~PAGE_MASK) == L4_Address(sp->utcb_area));
	intptr_t off = (intptr_t)ltid.raw - (sp->utcb_area.raw & ~PAGE_MASK) - 256;

	/* check malformed LTID */
	if(unlikely((off & (UTCB_SIZE - 1)) != 0)) return NULL;

	uint16_t page_pos = off / PAGE_SIZE;
	struct utcb_page *up = htable_get(&sp->utcb_pages,
		int_hash(page_pos), &cmp_utcb_page, &page_pos);
	if(likely(up != NULL)) {
		int slot = (off / UTCB_SIZE) % UTCB_PER_PAGE;
		struct thread *t = up->slots[slot];
		assert(t == NULL || t->utcb_pos == off / UTCB_SIZE);
		assert(t == NULL || t->space == sp);
		return t;
	}

	return NULL;
}


static bool clear_empty_redir_wait(struct thread *t, void *unused)
{
	if(t->status != TS_SEND_WAIT) return true;
	if(!CHECK_FLAG(t->flags, TF_REDIR_WAIT)) return true;
	if(t->u1.waited_redir.raw != L4_nilthread.raw) return true;

	/* FIXME: handle this by temporarily clearing TF_HALT over ipc_send_half()
	 * where applicable. this changes the interpretation of TF_HALT to just
	 * code execution rather than that and IPC.
	 *
	 * needs a provocation test, first.
	 */
	assert(!CHECK_FLAG(t->flags, TF_HALT));

	t->flags &= ~TF_REDIR_WAIT;
	redo_ipc_send_half(t);

	return true;
}


static void restart_redir_waits(struct space *sp) {
	for_each_thread_in_space(sp, &clear_empty_redir_wait, NULL);
}


static bool invalidate_redir_wait(struct thread *t, void *unused)
{
	if(CHECK_FLAG(t->flags, TF_REDIR_WAIT)) {
		assert(t->status == TS_SEND_WAIT);
		remove_redir_wait(t);
		t->u1.waited_redir = L4_nilthread;
	}

	return true;
}


void space_remove_redirector(struct thread *t)
{
	assert(CHECK_FLAG(t->flags, TF_REDIR));

	struct space *sp;
	list_for_each(&space_list, sp, link) {
		if(CHECK_FLAG(sp->flags, SF_REDIRECT) && sp->redirector == t) {
			sp->redirector = NULL;
			for_each_thread_in_space(sp, &invalidate_redir_wait, NULL);
		}
	}
}


bool space_prefill_upper(struct space *space, L4_Word_t fault_addr)
{
	bool ret;

	struct pt_iter it;
	pt_iter_init(&it, space);
	if(pt_upper_present(&it, fault_addr)) ret = false;
	else {
		int n = mapdb_fill_page_table(space, fault_addr);
		if(n < 0) {
			/* FIXME: this function's interface can't properly communicate
			 * -ENOMEM. rip it out in exception.c and make that call either
			 *  pt_*() or mapdb_*() as required.
			 */
			printf("!!! n=%d\n", n);
			panic("ENOMEM in space_prefill_upper()");
		}
		ret = n > 0;
	}
	pt_iter_destroy(&it);

	return ret;
}


static size_t space_memcpy_from_fast(
	struct space *sp,
	void *dest,
	L4_Word_t address,
	size_t size)
{
	L4_Word_t fault_addr;
	if((fault_addr = catch_pf()) != 0) {
		return 0;
	} else {
		/* curious x86 segment games */
		uint32_t wrap_addr = (uint32_t)address + KERNEL_SEG_SIZE;
		memcpy(dest, (void *)wrap_addr, size);

		uncatch_pf();
		return size;
	}
}


size_t space_memcpy_from(
	struct space *sp,
	void *dest,
	L4_Word_t address,
	size_t size)
{
	assert(check_space(0, sp));

	if(size == 0) return 0;

	if(sp == current_space) {
		size_t ret = space_memcpy_from_fast(sp, dest, address, size);
		if(likely(ret > 0)) return ret;
	}

	/* the long ungrateful slog */
	uintptr_t heap_addr = reserve_heap_page();
	size_t pos = 0;
	while(pos < size) {
		int seg = MIN(int, size - pos, PAGE_SIZE - (address & PAGE_MASK));
		const struct map_entry *e = mapdb_probe(sp, address & ~PAGE_MASK);
		if(e == NULL) break;
		put_supervisor_page(heap_addr, mapdb_page_id_in_entry(e, address));
		memcpy(dest + pos, (void *)(heap_addr | (address & PAGE_MASK)), seg);

		address += seg;
		pos += seg;

		assert(pos >= size || (address & PAGE_MASK) == 0);
	}
	put_supervisor_page(heap_addr, 0);
	free_heap_page(heap_addr);

	return pos;
}


/* x86/amd64 bits */


/* the goal is that regardless of @size, the TSS structure should be allocated
 * in such a way that the TSS before the I/O bitmap falls within a single
 * memory page. most reasonably this happens with a number of regular
 * allocations first and if those don't pay up, a slightly less efficient
 * valloc().
 *
 * also the static analyzer from clang screams potential memory leak about
 * this code, twice, in both cases spuriously -- it mistakenly regards the two
 * "p0 < p1" expressions as orthogonal.
 */
static void *alloc_tss(size_t size)
{
	assert(size >= sizeof(struct tss));
	const int max_trash = 8;

	struct list_head trash_list;
	int n_trash = 0;
	void *ptr;
	L4_Word_t p0, p1;
	do {
		ptr = malloc(size);
		p0 = (L4_Word_t)ptr >> PAGE_BITS;
		p1 = ((L4_Word_t)ptr + sizeof(struct tss) - 1) >> PAGE_BITS;
		if(p0 < p1) {
			if(unlikely(ptr == NULL)) break;
			if(n_trash == 0) list_head_init(&trash_list);
			list_add(&trash_list, ptr);
		}
	} while(p0 < p1 && ++n_trash < max_trash);
	if(n_trash > 0) {
		struct trash_entry { struct list_node n; } *ent, *next;
		list_for_each_safe(&trash_list, ent, next, n) {
			free(ent);
		}
	}
	if(ptr != NULL && p0 < p1) {
		/* punt case. */
		assert(n_trash == max_trash);
		ptr = valloc(size);
	}
	return ptr;
}


bool space_add_ioperm(struct space *sp, L4_Word_t base_port, int size)
{
	assert(check_space(0, sp));
	int last_byte = (base_port + size - 1 + 7) / 8;

	int map_len = 0;
	if(sp->tss_len > sizeof(struct tss)) {
		map_len = sp->tss_len - sizeof(struct tss);
	}
	uint8_t *map;
	if(last_byte >= map_len) {
		size_t newlen = ((last_byte + 15) & ~15) + 1;
		struct tss *newt = alloc_tss(sizeof(struct tss) + newlen);
		if(newt == NULL) return false;
		struct tss *old_tss = sp->tss;
		if(old_tss != NULL) {
			memcpy(newt, old_tss, sizeof(struct tss) + map_len);
		} else {
			*newt = kernel_tss;
			newt->iopb_offset = sizeof(struct tss);
		}
		map = (void *)&newt[1];
		memset(&map[map_len], 0xff, newlen - map_len);
		map_len = newlen;
		sp->tss = newt;
		sp->tss_len = sizeof(struct tss) + newlen;
		if(sp->tss_seg > 0) free_gdt_slot(sp->tss_seg);
		assert(sp->tss_len >= sizeof(struct tss) + 1);
		sp->tss_seg = set_gdt_slot(KERNEL_TO_LINEAR((L4_Word_t)sp->tss),
			sp->tss_len, DESC_A_PRESENT | DESC_A_TSS_32BIT, DESC_F_SZ);
		if(sp->tss_seg == 0) {
			panic("ran out of segment table entries!");
		}

		if(get_current_thread()->space == sp) {
			/* the fresh segment descriptor has a 0 "busy" flag. so this TSS
			 * is good to go!
			 */
			set_current_tss(sp->tss_seg);
		}

		free(old_tss);
	} else {
		map = (void *)&sp->tss[1];
	}

	/* brute force. */
	for(L4_Word_t i = base_port; i < base_port + size; i++) {
		int pos = i >> 3, off = i & 0x7;
		map[pos] &= ~(1 << off);
	}

	assert(map[map_len - 1] == 0xff);

	assert(check_space(0, sp));
	return true;
}


/* syscalls. */

SYSCALL void sys_unmap(L4_Word_t control, void *utcb)
{
	assert(check_all_spaces(0));

	struct thread *current = get_current_thread();
	struct space *cur_space = current->space;
	int page_count = (control & 0x3f) + 1;
	unsigned mode = UM_RECURSIVE | UM_GET_ACCESS;
	if(CHECK_FLAG(control, 0x40)) mode |= UM_IMMEDIATE;	/* flush bit */
	for(int i=0; i < page_count; i++) {
		L4_Fpage_t fp = { .raw = L4_VREG(utcb, L4_TCR_MR(i)) };
		if(L4_SizeLog2(fp) < PAGE_BITS) continue;
#if 0
		printf("  %s %#x:%#x (%c%c%c) for thread %lu:%lu\n",
			CHECK_FLAG(mode, UM_IMMEDIATE) ? "flushing" : "unmapping",
			(unsigned)L4_Address(fp), (unsigned)L4_Size(fp),
			CHECK_FLAG(L4_Rights(fp), L4_Readable) ? 'r' : '-',
			CHECK_FLAG(L4_Rights(fp), L4_Writable) ? 'w' : '-',
			CHECK_FLAG(L4_Rights(fp), L4_eXecutable) ? 'x' : '-',
			TID_THREADNUM(current->id), TID_VERSION(current->id));
#endif

		int access = mapdb_unmap_fpage(cur_space, fp, mode);
		L4_Set_Rights(&fp, access);
		L4_VREG(utcb, L4_TCR_MR(i)) = fp.raw;
	}

	assert(check_all_spaces(0));
}


/* TODO: this isn't at all robust against things like space_set_kip_area()'s
 * failure result, compounded by that function in itself leaving the space's
 * mapping database without any kip_area at all.
 *
 * these should be handled with an in-kernel OOM mechanism once the kernel
 * heap can grow to 256M, and once a sigma1 is specified that can return
 * userspace memory to the kernel.
 */
SYSCALL L4_Word_t sys_spacecontrol(
	L4_ThreadId_t spacespec,
	L4_Word_t control,
	L4_Fpage_t kip_area,
	L4_Fpage_t utcb_area,
	L4_ThreadId_t redirector,
	L4_Word_t *old_control)
{
	L4_Word_t old_ctl = 0, result;

	assert(check_all_spaces(0));

	struct thread *current = get_current_thread();
	void *utcb = thread_get_utcb(current);
	L4_Word_t *ec_p = &L4_VREG(utcb, L4_TCR_ERRORCODE);
	if(unlikely(!CHECK_FLAG(current->space->flags, SF_PRIVILEGE))) {
		*ec_p = 1;		/* no privilege */
		result = 0;
		goto end;
	}

	if(unlikely(control != 0)) {
		*ec_p = 0;		/* enforce zero control error */
		result = 0;
		goto end;
	}

	struct space *sp;
	if(L4_IsNilThread(spacespec)
		|| L4_IsLocalId(spacespec)
		|| (sp = space_find(spacespec.raw)) == NULL
		|| unlikely(sp == kernel_space))
	{
		*ec_p = 3;		/* invalid space */
		result = 0;
		goto end;
	}

	struct thread *new_red = NULL;
	if((L4_IsGlobalId(redirector)
			&& redirector.raw != L4_anythread.raw
			&& L4_ThreadNo(redirector) >= first_user_threadno())
	   || (!L4_IsNilThread(redirector)
			&& L4_IsLocalId(redirector)
			&& redirector.raw != L4_anylocalthread.raw))
	{
		new_red = resolve_tid_spec(current->space, redirector);
	} else {
		/* read pre-user range numbers, anylocalthread, and threads that can't
		 * be matched correctly as nilthread (no change). this part could also
		 * report invalid thread (ec=2, result=0, goto end) but it'd be
		 * contrary to specification.
		 */
		if(redirector.raw != L4_anythread.raw) redirector = L4_nilthread;
	}

	bool t_active = !L4_IsNilThread(space_name(sp));
	if(!t_active) {
		/* consider the pie. */
		const L4_KernelInterfacePage_t *kip = kip_mem;
		if(!L4_IsNilFpage(utcb_area)
			&& L4_SizeLog2(utcb_area) < kip->UtcbAreaInfo.X.s)
		{
			*ec_p = 6;	/* invalid UTCB area */
			result = 0;
			goto end;
		}
		if(!L4_IsNilFpage(kip_area)
			&& (L4_SizeLog2(kip_area) < kip->KipAreaInfo.X.s
				|| (!L4_IsNilFpage(utcb_area)
					&& fpage_overlap(kip_area, utcb_area))))
		{
			*ec_p = 7;	/* invalid KIP area */
			result = 0;
			goto end;
		}

		int rc = !L4_IsNilFpage(kip_area)
			? space_set_kip_area(sp, kip_area) : 0;
		if(rc == 0 && !L4_IsNilFpage(utcb_area)) {
			rc = space_set_utcb_area(sp, utcb_area);
		}
		if(rc != 0) {
			*ec_p = rc;
			result = 0;
			goto end;
		}

		assert(sp->kip_area.raw == kip_area.raw);
		assert(sp->utcb_area.raw == utcb_area.raw);
	}

	/* NOTE: this is somewhat inelegant in first invalidating existing
	 * redir_waits, and then re-running the relevant IPC segments on reset.
	 * instead a for_each_thread_in_space() could rewrite the redir_wait as
	 * appropriate and re-do at most one.
	 *
	 * however, changing from one redirector to another is a rare operation.
	 */
	bool need_restart = false;
	struct thread *old_red = CHECK_FLAG(sp->flags, SF_REDIRECT)
		? sp->redirector : NULL;
	if(redirector.raw == L4_anythread.raw) {
		need_restart = CHECK_FLAG(sp->flags, SF_REDIRECT)
			&& sp->redirector == NULL;

		if(old_red != NULL) {
			need_restart = true;
			for_each_thread_in_space(sp, &invalidate_redir_wait, NULL);
		}

		sp->flags &= ~SF_REDIRECT;
		sp->redirector = NULL;
	} else if(new_red != NULL) {
		need_restart = CHECK_FLAG(sp->flags, SF_REDIRECT)
			&& sp->redirector == NULL;

		if(old_red != new_red) {
			need_restart = true;
			for_each_thread_in_space(sp, &invalidate_redir_wait, NULL);
		}

		assert(new_red->id == redirector.raw);
		new_red->flags |= TF_REDIR;
		sp->redirector = new_red;
		sp->flags |= SF_REDIRECT;
	}
	if(need_restart) {
		/* TODO: this may pre-empt the SpaceControl caller. that should be
		 * tested for.
		 */
		restart_redir_waits(sp);
	}

	result = 1;
	old_ctl = 0;

end:
	assert(check_all_spaces(0));

	*old_control = old_ctl;
	return result;
}


/* NOTE: this runs in the pre-heap environment, so htable ops aren't
 * available. instead, the pages that init_spaces() reserves are added to the
 * list given as parameter. kernel_space->mapdb is left uninitialized for the
 * same reason.
 */
COLD void init_spaces(struct list_head *resv_list)
{
	memset(&kernel_space_mem, 0, sizeof(kernel_space_mem));
	kernel_space = &kernel_space_mem;
	space_init(kernel_space, resv_list);
	kernel_space->tss = &kernel_tss;
	kernel_space->tss_len = sizeof(struct tss);
	kernel_space->tss_seg = SEG_KERNEL_TSS;
	assert(kernel_space->flags == 0);
	assert(kernel_space->redirector == NULL);

	/* preallocate page table pages for the kernel segment, so that it makes
	 * sense to copy the page table pointers.
	 */
	pdir_t *pdirs = kernel_space->pdirs->vm_addr;
	for(int i = KERNEL_SEG_START >> 22; i < 1024; i++) {
		struct page *pg = get_kern_page(0);
		pdirs[i] = pg->id << 12 | PDIR_PRESENT | PDIR_RW;
		list_add(resv_list, &pg->link);
		memset(pg->vm_addr, 0, PAGE_SIZE);
	}

	/* UTCB pages */
	int ua_shift = size_to_shift(UTCB_SIZE * NUM_KERNEL_THREADS);
	if(ua_shift < PAGE_BITS) ua_shift = PAGE_BITS;
	uintptr_t utcb_base = reserve_heap_range(1 << ua_shift);
	kernel_space->utcb_area = L4_FpageLog2(utcb_base, ua_shift);
	printf("kernel UTCB area at %#lx:%#lx\n",
		L4_Address(kernel_space->utcb_area),
		L4_Size(kernel_space->utcb_area));
	const int n_ups = NUM_KERN_UTCB_PAGES;
	/* keep pointer for finalize */
	kernel_space->utcb_pages.priv = kernel_utcb_pages;
	for(int i=0; i < n_ups; i++) {
		struct utcb_page *up = &kernel_utcb_pages[i];
		/* we can't let get_kern_page() call put_supervisor_page() this early
		 * in the boot process, because it'll encounter next_dir_page == NULL
		 * and explode. instead we'll put these wherever for the time being,
		 * and move them over in the post-heap init.
		 */
		up->pg = get_kern_page(0);
		list_add(resv_list, &up->pg->link);
		memset(up->pg->vm_addr, 0, PAGE_SIZE);
		up->occmap = 0;
		up->pos = i;
		for(int j=0; j < UTCB_PER_PAGE; j++) up->slots[j] = NULL;
	}

	/* module inits */
	space_slab = KMEM_CACHE_NEW("space_slab", struct space);
	utcb_page_slab = KMEM_CACHE_NEW("utcb_page_slab", struct utcb_page);
}


static COLD size_t hash_page_by_id(const void *page_ptr, void *priv) {
	const struct page *p = page_ptr;
	return int_hash(p->id);
}


static COLD bool cmp_page_to_id(const void *cand_ptr, void *key) {
	const struct page *p = cand_ptr;
	return p->id == *(const uint32_t *)key;
}


/* the init_spaces() parts that require the malloc heap and mapdb
 * initialization, i.e. htable operations and map_group_slab.
 */
COLD void space_finalize_kernel(
	struct space *sp,
	struct list_head *resv_list)
{
	assert(sp == kernel_space);

	/* create map_group structs for the kernel's boot-time page tables. these
	 * won't carry any mappings (hence MGF_ROOT) as there's no justifiable
	 * reason to have a child reference to the KIP for each address space, but
	 * this'll let kernel_space pass consistency checks for very little cost
	 * so it's fine.
	 */
	assert(map_group_slab != NULL);
	struct htable by_id;
	htable_init(&by_id, &hash_page_by_id, NULL);
	struct page *p;
	list_for_each(resv_list, p, link) {
		bool ok = htable_add(&by_id, int_hash(p->id), p);
		if(!ok) panic("borkage!");
	}
	const pdir_t *dirs = sp->pdirs->vm_addr;
	for(uintptr_t i = 0; i < KERNEL_SEG_SIZE; i += PT_UPPER_SIZE) {
		uint32_t pgid = dirs[(KERNEL_SEG_START + i) >> PT_UPPER_BITS] >> 12;
		p = htable_get(&by_id, int_hash(pgid), &cmp_page_to_id, &pgid);
		if(p == NULL) panic("spuriously!");		/* but also gravely. */
		struct map_group *g = kmem_cache_alloc(map_group_slab);
		if(g == NULL) panic("decongestuous!");
		*g = (struct map_group){
			.addr = (KERNEL_SEG_START + i) | MGF_ROOT,
			.entries = NULL, .ptab_page = p, .space = sp,
		};
		bool ok = htable_add(&sp->ptab_groups, int_hash(MG_START(g)), g);
		if(!ok) panic("nacritude!");
	}
	htable_clear(&by_id);

	assert(sp->utcb_pages.priv != NULL);
	struct utcb_page *ups = sp->utcb_pages.priv;
	sp->utcb_pages.priv = NULL;
	for(int i=0; i < NUM_KERN_UTCB_PAGES; i++) {
		htable_add(&sp->utcb_pages, int_hash(ups[i].pos), &ups[i]);
		/* move the page into the kernel's UTCB segment. this'll make a
		 * permanent hole in the idempotent kernel reservation to avoid VM
		 * aliasing of UTCB pages.
		 */
		uintptr_t old_addr = (uintptr_t)ups[i].pg->vm_addr;
		put_supervisor_page(old_addr, 0);
		/* (i.e. the address space needn't be free_heap_page()'d) */
		assert(CHECK_FLAG(ups[i].pg->flags, PAGEF_INITMEM));
		uintptr_t new_addr = L4_Address(sp->utcb_area) + i * PAGE_SIZE;
		ups[i].pg->vm_addr = (void *)new_addr;
		put_supervisor_page(new_addr, ups[i].pg->id);
	}
}
