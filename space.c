
#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include <assert.h>
#include <errno.h>

#include <ccan/likely/likely.h>
#include <ccan/list/list.h>
#include <ccan/htable/htable.h>
#include <ccan/alignof/alignof.h>
#include <ccan/compiler/compiler.h>

#include <l4/types.h>
#include <l4/kip.h>
#include <l4/vregs.h>

#include <ukernel/mm.h>
#include <ukernel/x86.h>
#include <ukernel/slab.h>
#include <ukernel/misc.h>
#include <ukernel/util.h>
#include <ukernel/thread.h>
#include <ukernel/sched.h>
#include <ukernel/mapdb.h>
#include <ukernel/kip.h>
#include <ukernel/space.h>


struct space *kernel_space = NULL;
static struct space kernel_space_mem;

static struct kmem_cache *space_slab = NULL;
static struct list_head space_list = LIST_HEAD_INIT(space_list);
static struct space *current_space = NULL;


/* get-cmp function for struct space's ptab_pages */
static bool cmp_page_id_to_key(const void *cand, void *key) {
	const struct page *pg = cand;
	return pg->id == *(uint32_t *)key;
}


/* rehash for same */
static size_t hash_page_by_id(const void *page_ptr, void *priv)
{
	const struct page *p = page_ptr;
	return int_hash(p->id);
}


#ifndef NDEBUG
#include <ukernel/invariant.h>

#define NO_PTAB_TO_MAPDB (1 << 0)


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


/* FIXME: this function is needlessly heavy. it makes unit tests very slow. */
static bool check_space(int opt, struct space *sp)
{
#ifdef DEBUG_ME_HARDER
	INV_CTX;

	L4_ThreadId_t sp_tid = L4_nilthread;
	if(!list_empty(&sp->threads)) {
		sp_tid.raw = list_top(&sp->threads, struct thread, space_link)->id;
		struct thread *t;
		list_for_each(&sp->threads, t, space_link) {
			if(TID_THREADNUM(sp_tid.raw) < TID_THREADNUM(t->id)) {
				sp_tid.raw = t->id;
			}
		}
	}
	inv_push("space of mapdb %u (%lu:%lu)", sp->mapdb.ref_id,
		TID_THREADNUM(sp_tid.raw), TID_VERSION(sp_tid.raw));

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
			struct page *ptab_page = htable_get(&sp->ptab_pages,
				int_hash(pt_id), &cmp_page_id_to_key, &pt_id);
			inv_ok(ptab_page != NULL, "ptab must exist");
			inv_ok(ptab_page->id == pt_id,
				"expected ptab_page->id == %u, found %u",
				pt_id, ptab_page->id);
		}
		inv_pop();
	}

	/* check that correct pages, or holes, are mapped for utcb_pages[]. */
	if(sp->utcb_pages != NULL) {
		inv_ok1(sp->utcb_area.raw != L4_Nilpage.raw);
		for(int i=0; i < NUM_UTCB_PAGES(sp->utcb_area); i++) {
			L4_Word_t page_addr = L4_Address(sp->utcb_area) + i * PAGE_SIZE;
			inv_push("checking utcb page %d/%d at %#lx:", i,
				(int)NUM_UTCB_PAGES(sp->utcb_area), page_addr);

			/* (this just accesses the page tables.) */
			const uint32_t *ptab_mem;
			int dir_ix = page_addr >> 22, pg_ix = (page_addr >> 12) & 0x3ff;
			if(CHECK_FLAG(pdir_mem[dir_ix], PDIR_PRESENT)) {
				uint32_t pt_id = pdir_mem[dir_ix] >> 12;
				struct page *ptab_page = htable_get(&sp->ptab_pages,
					int_hash(pt_id), &cmp_page_id_to_key, &pt_id);
				assert(ptab_page != NULL);		/* checked earlier */
				ptab_mem = ptab_page->vm_addr;
				assert(ptab_mem != NULL);		/* if-else block output */
			} else {
				ptab_mem = NULL;
			}

			if(sp->utcb_pages[i] == NULL) {
				uint32_t ent = ptab_mem != NULL ? ptab_mem[pg_ix] : 0;
				inv_log("no utcb page; ptab_mem is %p, entry %d is %#x",
					ptab_mem, pg_ix, ent);
				inv_ok1(ptab_mem == NULL || !CHECK_FLAG(ent, PT_PRESENT));
			} else {
				inv_log("utcb page present; entry %d is %#x (want id %u)",
					pg_ix, ptab_mem[pg_ix], sp->utcb_pages[i]->id);
				inv_ok1(!CHECK_FLAG(ptab_mem[pg_ix], PT_PRESENT)
					|| (ptab_mem[pg_ix] >> 12) == sp->utcb_pages[i]->id);
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
			uint32_t pt_id = pde >> 12;
			struct page *ptab_page = htable_get(&sp->ptab_pages,
				int_hash(pt_id), &cmp_page_id_to_key, &pt_id);
			inv_ok1(ptab_page != NULL);		/* checked earlier */
			const uint32_t *ptab_mem = ptab_page->vm_addr;

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
				struct map_entry *e = mapdb_probe(&sp->mapdb, addr);
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
#endif


static void space_init(struct space *sp, struct list_head *resv_list)
{
	sp->kip_area = L4_Nilpage;
	sp->utcb_area = L4_Nilpage;
	sp->utcb_pages = NULL;

	list_head_init(&sp->threads);
	htable_init(&sp->ptab_pages, &hash_page_by_id, NULL);

	sp->pdirs = get_kern_page(0);
	if(unlikely(resv_list != NULL)) list_add(resv_list, &sp->pdirs->link);
	else htable_add(&sp->ptab_pages, int_hash(sp->pdirs->id), sp->pdirs);
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


struct space *space_new(void)
{
	assert(space_slab != NULL);

	struct space *sp = kmem_cache_alloc(space_slab);
	space_init(sp, NULL);
	mapdb_init(&sp->mapdb, sp);
	sp->tss = NULL;
	sp->tss_len = 0;
	sp->tss_seg = 0;
	sp->flags = 0;
	sp->redirector = L4_nilthread.raw;

	return sp;
}


void space_free(struct space *sp)
{
	assert(list_empty(&sp->threads));

	mapdb_destroy(&sp->mapdb);

	if(sp->utcb_pages != NULL) {
		for(int i=0; i < NUM_UTCB_PAGES(sp->utcb_area); i++) {
			if(sp->utcb_pages[i] == NULL) continue;
			free_kern_page(sp->utcb_pages[i]);
		}
		free(sp->utcb_pages);
	}

	struct htable_iter it;
	for(struct page *p = htable_first(&sp->ptab_pages, &it);
		p != NULL;
		p = htable_next(&sp->ptab_pages, &it))
	{
		free_kern_page(p);
	}
	htable_clear(&sp->ptab_pages);

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
	assert(t->space == NULL);

	list_add(&sp->threads, &t->space_link);
	t->space = sp;
}


void space_remove_thread(struct space *sp, struct thread *t)
{
	assert(t->space == sp);
	list_del_from(&sp->threads, &t->space_link);
	t->space = NULL;

	if(list_empty(&sp->threads)) space_free(sp);
}


void space_switch(struct space *next)
{
	struct space *old = current_space;
	if(unlikely(old == NULL)) old = kernel_space;

	if(old == next) return;

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

	if(sp->utcb_pages != NULL) {
		for(int i=0; i < NUM_UTCB_PAGES(sp->utcb_area); i++) {
			free_kern_page(sp->utcb_pages[i]);
		}
		free(sp->utcb_pages);
		sp->utcb_pages = NULL;
	}

	sp->utcb_area = area;

	return 0;
}


int space_set_kip_area(struct space *sp, L4_Fpage_t area)
{
	if(FPAGE_LOW(area) >= KERNEL_SEG_START
		|| FPAGE_HIGH(area) >= KERNEL_SEG_START)
	{
		/* KIP outside address space. */
		return 7;
	}

	if(!L4_IsNilFpage(sp->kip_area)) {
		/* FIXME: remove old KIP mapping */
	}

	sp->kip_area = area;
	space_put_page(sp, L4_Address(sp->kip_area),
		(L4_Word_t)kip_mem >> PAGE_BITS, L4_Readable | L4_eXecutable);

	return 0;
}


struct thread *space_find_local_thread(
	struct space *sp,
	L4_LthreadId_t ltid)
{
	assert(ltid.X.zeros == 0);

	if(ltid.raw >= L4_Address(sp->utcb_area)
		&& ltid.raw < L4_Address(sp->utcb_area) + L4_Size(sp->utcb_area))
	{
		L4_Word_t off = ltid.raw - L4_Address(sp->utcb_area) - 256;
		int ix = off / UTCB_SIZE;
		if(unlikely(L4_Address(sp->utcb_area) + 256 + ix * UTCB_SIZE
			!= ltid.raw))
		{
			/* malformed local TID. */
			return NULL;
		}
		struct thread *t;
		list_for_each(&sp->threads, t, space_link) {
			if(t->utcb_pos == ix) return t;
		}
	}

	return NULL;
}


void space_put_page(
	struct space *sp,
	uintptr_t addr,
	uint32_t page_id,
	int access)
{
	assert(check_space(NO_PTAB_TO_MAPDB, sp));
	assert(addr < KERNEL_SEG_START);

	int dir_ix = addr >> 22, ptab_ix = (addr >> 12) & 0x3ff;
	assert(sp->pdirs->vm_addr != NULL);
	uint32_t *pdir_mem = sp->pdirs->vm_addr;
	struct page *ptab_page;
	if(unlikely(!CHECK_FLAG(pdir_mem[dir_ix], PDIR_PRESENT))) {
		if(page_id == 0) goto end;

		ptab_page = get_kern_page(0);
		/* TODO: prefill it? */
		memset(ptab_page->vm_addr, 0, PAGE_SIZE);
		pdir_mem[dir_ix] = ptab_page->id << 12 | PDIR_PRESENT | PDIR_USER
			| PDIR_RW;
		/* FIXME: catch ENOMEM! */
		htable_add(&sp->ptab_pages, int_hash(ptab_page->id), ptab_page);
	} else {
		uint32_t pt_id = pdir_mem[dir_ix] >> 12;
		ptab_page = htable_get(&sp->ptab_pages, int_hash(pt_id),
			&cmp_page_id_to_key, &pt_id);
		assert(ptab_page != NULL);
	}
	assert(ptab_page->vm_addr != NULL);

	uint32_t *ptab_mem = ptab_page->vm_addr;
	if(page_id == 0 || !CHECK_FLAG(access, L4_Readable)) {
		ptab_mem[ptab_ix] = 0;
	} else {
		assert(CHECK_FLAG(access, L4_Readable));
		ptab_mem[ptab_ix] = page_id << 12 | PT_PRESENT | PT_USER
			| (CHECK_FLAG(access, L4_Writable) ? PT_RW : 0);
	}

end:
	/* TODO: skip this if _sp_'s page table is not currently loaded */
	x86_invalidate_page(addr & ~PAGE_MASK);

	assert(check_space(NO_PTAB_TO_MAPDB, sp));
}


int space_probe_pt_access(
	L4_Word_t *next_addr_p,
	struct space *sp,
	L4_Word_t address)
{
	assert(check_space(0, sp));

	L4_Word_t dir = address >> 22, p_ix = (address >> 12) & 0x3ff,
		dummy = 0, *skip_to = next_addr_p == NULL ? &dummy : next_addr_p;
	const pdir_t *dirs = sp->pdirs->vm_addr;
	if(!CHECK_FLAG(dirs[dir], PDIR_PRESENT)) {
		*skip_to = (dir + 1) << 22;
		return -ENOENT;
	}

	uint32_t pt_id = dirs[dir] >> 12;
	struct page *ptab_page = htable_get(&sp->ptab_pages, int_hash(pt_id),
		&cmp_page_id_to_key, &pt_id);
	if(unlikely(ptab_page == NULL)) {
		struct thread *t = list_top(&sp->threads, struct thread,
			space_link);
		printf("warning: page table for address %#lx in space %lu:%lu not found\n",
			address, t != NULL ? TID_THREADNUM(t->id) : 0,
			t != NULL ? TID_VERSION(t->id) : 0);
		*skip_to = 0;
		return -ENOENT;
	}

	assert(ptab_page->vm_addr != NULL);
	page_t *ptab = ptab_page->vm_addr;
	if(!CHECK_FLAG(ptab[p_ix], PT_PRESENT)) {
		if(next_addr_p != NULL) {
			/* scan the page table forward a bit, just to amortize some of the
			 * hash table access cost.
			 */
			L4_Word_t lim = MIN(L4_Word_t, p_ix + 32, 0x3ff);
			while(p_ix <= lim && !CHECK_FLAG(ptab[p_ix], PT_PRESENT)) p_ix++;
			*next_addr_p = p_ix <= lim ? dir << 22 | p_ix << 12 : 0;
		}
		return -ENOENT;
	} else {
		int rwx = 0;
		if(CHECK_FLAG(ptab[p_ix], PT_ACCESSED)) {
			rwx = L4_Readable | L4_eXecutable;
		}
		if(CHECK_FLAG(ptab[p_ix], PT_DIRTY)) rwx |= L4_Writable;
		ptab[p_ix] &= ~(PT_DIRTY | PT_ACCESSED);
		return rwx;
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

	/* TODO: play weird segment games when sp == current_thread->space */

	uintptr_t heap_addr = reserve_heap_page();
	size_t pos = 0;
	while(pos < size) {
		int seg = MIN(int, size - pos, PAGE_SIZE - (address & PAGE_MASK));
		const struct map_entry *e = mapdb_probe(&sp->mapdb,
			address & ~PAGE_MASK);
		if(e == NULL) break;
		put_supervisor_page(heap_addr, mapdb_page_id_in_entry(e, address));
		/* FIXME: fix put_supervisor_page() somehow */
		x86_flush_tlbs();

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

static void *alloc_tss(size_t size)
{
	assert(size >= sizeof(struct tss));

	struct list_head trash_list;
	list_head_init(&trash_list);

	void *ptr;
	L4_Word_t p0, p1;
	do {
		ptr = malloc(size);
		if(ptr == NULL) break;
		p0 = (L4_Word_t)ptr >> PAGE_BITS;
		p1 = ((L4_Word_t)ptr + sizeof(struct tss)) >> PAGE_BITS;
		if(p0 < p1) list_add(&trash_list, ptr);
	} while(p0 < p1);
	struct trash_entry { struct list_node n; } *ent, *next;
	list_for_each_safe(&trash_list, ent, next, n) {
		free(ent);
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
		/* TODO: it'd be nice if dlmalloc's posix_memalign() worked. */
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

void sys_unmap(L4_Word_t control)
{
	assert(check_all_spaces(0));

	struct thread *current = get_current_thread();
	void *utcb = thread_get_utcb(current);
	struct map_db *mdb = &current->space->mapdb;
	const bool flush = CHECK_FLAG(control, 0x40);
	int page_count = (control & 0x3f) + 1, remove_agg = 0;
	for(int i=0; i < page_count; i++) {
		L4_Fpage_t fp = { .raw = L4_VREG(utcb, L4_TCR_MR(i)) };
		int remove = L4_Rights(fp);
		remove_agg |= remove;
#if 0
		printf("  %s %#x:%#x (%c%c%c) for thread %lu:%lu\n",
			flush ? "flushing" : "unmapping",
			(unsigned)L4_Address(fp), (unsigned)L4_Size(fp),
			CHECK_FLAG(L4_Rights(fp), L4_Readable) ? 'r' : '-',
			CHECK_FLAG(L4_Rights(fp), L4_Writable) ? 'w' : '-',
			CHECK_FLAG(L4_Rights(fp), L4_eXecutable) ? 'x' : '-',
			TID_THREADNUM(current->id), TID_VERSION(current->id));
#endif

		L4_Set_Rights(&fp, mapdb_unmap_fpage(mdb, fp, flush, true, true));
		L4_VREG(utcb, L4_TCR_MR(i)) = fp.raw;

		if(remove != 0) {
			/* make the unmap take effect in the caller's address space.
			 *
			 * TODO: use space_put_page() to modify the page table's access
			 * bits instead using an and/or mask pair. this current solution
			 * causes minor refaults over the entire range later on.
			 */
			for(L4_Word_t addr = FPAGE_LOW(fp);
				addr < FPAGE_HIGH(fp);
				addr += PAGE_SIZE)
			{
				space_put_page(current->space, addr, 0, 0);
			}
		}
	}
	if(remove_agg != 0) space_commit(current->space);

	assert(check_all_spaces(0));
}


/* FIXME: this isn't at all robust against things like space_set_kip_area()'s
 * various failure modes. an attempt should be made to restore the previous
 * values as appropriate.
 */
L4_Word_t sys_spacecontrol(
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
		|| (sp = space_find(spacespec.raw)) == NULL)
	{
		*ec_p = 3;		/* invalid space */
		result = 0;
		goto end;
	}

	/* is there at least one active thread in this space? */
	bool t_active = false;
	struct thread *t;
	list_for_each(&sp->threads, t, space_link) {
		/* active being defined as "has UTCB slot" */
		if(t->utcb_pos >= 0) {
			t_active = true;
			break;
		}
	}
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
				|| RANGE_OVERLAP(FPAGE_LOW(kip_area), FPAGE_HIGH(kip_area),
					FPAGE_LOW(utcb_area), FPAGE_HIGH(utcb_area))))
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

	if(redirector.raw == L4_anythread.raw) {
		sp->redirector = L4_anythread.raw;
	} else if(!L4_IsNilThread(redirector)
		&& redirector.raw != L4_anylocalthread.raw)
	{
		sp->redirector = redirector.raw;
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
	kernel_space->tss_seg = 5;

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
	kernel_space->utcb_area = L4_FpageLog2(
		ALIGN_TO_SHIFT(KERNEL_HEAP_TOP, ua_shift), ua_shift);
	static struct page *kernel_utcb_pages[(UTCB_SIZE * NUM_KERNEL_THREADS + PAGE_SIZE - 1)
		/ PAGE_SIZE];
	int n_utcb_pages = sizeof(kernel_utcb_pages) / sizeof(kernel_utcb_pages[0]);
	kernel_space->utcb_pages = kernel_utcb_pages;
	for(int i=0; i < n_utcb_pages; i++) {
		struct page *pg = get_kern_page(0);
		/* TODO: map the pages to actually appear at the UTCB area? */
		kernel_space->utcb_pages[i] = pg;
		list_add(resv_list, &pg->link);
		memset(pg->vm_addr, 0, PAGE_SIZE);
	}

	/* module inits */
	space_slab = kmem_cache_create("space_slab", sizeof(struct space),
		ALIGNOF(struct space), 0, NULL, NULL);
}


COLD void space_add_resv_pages(
	struct space *sp,
	struct list_head *resv_list)
{
	struct page *p;
	list_for_each(resv_list, p, link) {
		htable_add(&sp->ptab_pages, int_hash(p->id), p);
	}
}
