
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
#include <ukernel/rangealloc.h>
#include <ukernel/misc.h>
#include <ukernel/util.h>
#include <ukernel/bug.h>
#include <ukernel/rbtree.h>
#include <ukernel/thread.h>
#include <ukernel/ipc.h>
#include <ukernel/sched.h>
#include <ukernel/mapdb.h>
#include <ukernel/kip.h>
#include <ukernel/ptab.h>


static size_t space_memcpy_from_fast(
	struct space *sp,
	void *dest, L4_Word_t address, size_t size);

static void space_memcpy_from_unsafe(
	struct space *sp,
	void *dest, L4_Word_t address, size_t size);

static struct utcb_page *find_utcb_page(struct space *sp, int pos);


struct space *current_space = NULL;

struct space *kernel_space = NULL;
static struct space kernel_space_mem;

static struct kmem_cache *space_slab = NULL, *utcb_page_slab = NULL;
static struct list_head space_list = LIST_HEAD_INIT(space_list);


/* accessors for <struct space>.utcb_pages */

static inline struct utcb_page *insert_utcb_page_helper(
	struct rb_root *root, struct utcb_page *u)
{
	struct rb_node **p = &root->rb_node, *parent = NULL;
	while(*p != NULL) {
		parent = *p;
		struct utcb_page *oth = rb_entry(parent, struct utcb_page, rb);
		int v = (int)u->pos - (int)oth->pos;	/* ->pos, ascending */
		if(v < 0) p = &(*p)->rb_left;
		else if(v > 0) p = &(*p)->rb_right;
		else return oth;
	}
	rb_link_node(&u->rb, parent, p);
	return NULL;
}


static void insert_utcb_page(struct space *sp, struct utcb_page *u)
{
	struct utcb_page *dupe = insert_utcb_page_helper(&sp->utcb_pages, u);
	BUG_ON(dupe != NULL,
		"utcb_page with pos=%u already in tree (space tno=%lu)",
		u->pos, L4_ThreadNo(space_name(sp)));
	rb_insert_color(&u->rb, &sp->utcb_pages);
}


static struct utcb_page *find_utcb_page(struct space *sp, int pos)
{
	struct rb_node *n = sp->utcb_pages.rb_node;
	struct utcb_page *p;
	while(n != NULL) {
		p = rb_entry(n, struct utcb_page, rb);
		int v = pos - (int)p->pos;
		if(v < 0) n = n->rb_left;
		else if(v > 0) n = n->rb_right;
		else return p;
	}
	return NULL;
}


static void space_init(struct space *sp, struct list_head *resv_list)
{
	sp->utcb_top = 0;
	sp->kip_area = L4_Nilpage;
	sp->utcb_area = L4_Nilpage;
	sp->utcb_pages = RB_ROOT;

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
	for(struct rb_node *rb = rb_first(&sp->utcb_pages);
		rb != NULL;
		rb = rb_next(rb))
	{
		struct utcb_page *up = rb_entry(rb, struct utcb_page, rb);
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
	for(struct rb_node *next, *rb = rb_first(&sp->utcb_pages);
		rb != NULL;
		rb = next)
	{
		next = rb_next(rb);
		struct utcb_page *up = rb_entry(rb, struct utcb_page, rb);
		assert(up->occmap == 0);
		rb_erase(rb, &sp->utcb_pages);
		free_kern_page(up->pg);
		kmem_cache_free(utcb_page_slab, up);
	}
	assert(RB_EMPTY_ROOT(&sp->utcb_pages));
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

	if(up->occmap == 0) {
		/* UTCB page became empty, so toss it. */
		L4_Fpage_t fp = L4_FpageLog2(
			L4_Address(sp->utcb_area) + up->pos * PAGE_SIZE,
			PAGE_BITS);
		mapdb_erase_exempt(sp, fp);

		rb_erase(&up->rb, &sp->utcb_pages);
		free_kern_page(up->pg);
		kmem_cache_free(utcb_page_slab, up);

		if(RB_EMPTY_ROOT(&sp->utcb_pages)) space_free(sp);
	}

	t->utcb_page = NULL;
	t->utcb_pos = -1;
}


struct utcb_page *space_get_utcb_page(struct space *sp, uint16_t page_pos)
{
	struct utcb_page *up = find_utcb_page(sp, page_pos);
	if(up != NULL) return up;

	/* allocate and insert, where possible. */
	if(page_pos >= L4_Size(sp->utcb_area) / PAGE_SIZE) return NULL;

	assert(sp != kernel_space);
	up = kmem_cache_alloc(utcb_page_slab);
	up->pos = page_pos;
	up->occmap = 0;
	up->pg = get_kern_page(0);
	memset(up->pg->vm_addr, 0, PAGE_SIZE);
	for(int i=0; i < UTCB_PER_PAGE; i++) up->slots[i] = NULL;
	insert_utcb_page(sp, up);

	if(likely(sp != kernel_space)) {
		L4_Fpage_t u_page = L4_FpageLog2(L4_Address(sp->utcb_area)
			+ page_pos * PAGE_SIZE, PAGE_BITS);
		L4_Set_Rights(&u_page, L4_Readable | L4_Writable);
		int n = mapdb_put(sp, u_page, up->pg->id, true);
		if(n < 0) {
			/* FIXME: it's tricky to pass errors through a lazy add, and
			 * indeed we shouldn't be doing that in the first place. UTCB
			 * pages should be created lazily where threads are assigned to an
			 * UTCB slot, not here; and NULL should be the return value for
			 * UTCB page not present instead of failure.
			 */
			printf("%s: mapdb_put() failed, n=%d!!!\n", __func__, n);
			panic("it was inevitable.");
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


struct space *space_find(thread_id raw)
{
	L4_ThreadId_t tid = { .raw = raw };
	assert(L4_IsGlobalId(tid));
	struct thread *t = thread_get(tid);
	return t == NULL ? NULL : t->space;
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
	assert(RB_EMPTY_ROOT(&sp->utcb_pages));

	if(FPAGE_LOW(area) >= KERNEL_SEG_START
		|| FPAGE_HIGH(area) >= KERNEL_SEG_START)
	{
		/* KIP outside address space. */
		return 7;
	}

	if(!L4_IsNilFpage(sp->kip_area)) mapdb_erase_exempt(sp, sp->kip_area);

	sp->kip_area = area;
	L4_Fpage_t k_page = L4_FpageLog2(L4_Address(area),
		MIN(int, L4_SizeLog2(area), PAGE_BITS));
	L4_Set_Rights(&k_page, L4_Readable | L4_eXecutable);
	uint32_t kip_pgid = (L4_Word_t)kip_mem >> PAGE_BITS;
	int n = mapdb_put(sp, k_page, kip_pgid, true);
	if(n < 0) {
		/* FIXME: update this function's call sites to handle negative returns
		 * from mapdb_put(). they currently don't, but should.
		 */
		printf("%s: mapdb_put() failed, n=%d!!\n", __func__, n);
		panic("can't hack this heat");
	}

	return 0;
}


struct thread *space_find_local_thread(struct space *sp, L4_LthreadId_t ltid)
{
	assert(ltid.X.zeros == 0);

	struct thread *t;
	if(sp == current_space && catch_pf() == 0) {
		/* this avoids access to cold cache lines and a htable_get() to find
		 * the relevant UTCB page. this should be in the black even with the
		 * catch_pf().
		 */
		L4_ThreadId_t gtid;
		L4_Word_t gt_addr = ltid.raw + L4_TCR_MYGLOBALID * sizeof(L4_Word_t);
		space_memcpy_from_unsafe(sp, &gtid, gt_addr, sizeof(gtid));
		t = ra_id2ptr(thread_ra, L4_ThreadNo(gtid));
		if(t->space == sp && get_local_id(t).raw == ltid.raw) {
			uncatch_pf();
			return t;
		}
		/* otherwise, proceed down to the full lookup. */
		uncatch_pf();
	}

	/* (this micro-optimization saves 3 insns.) */
	assert((sp->utcb_area.raw & ~PAGE_MASK) == L4_Address(sp->utcb_area));
	intptr_t off = (intptr_t)ltid.raw - (sp->utcb_area.raw & ~PAGE_MASK) - 256;

	/* check malformed LTID */
	if(unlikely((off & (UTCB_SIZE - 1)) != 0)) return NULL;

	uint16_t page_pos = off / PAGE_SIZE;
	struct utcb_page *up = find_utcb_page(sp, page_pos);
	if(likely(up != NULL)) {
		int slot = (off / UTCB_SIZE) % UTCB_PER_PAGE;
		t = up->slots[slot];
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


static void space_memcpy_from_unsafe(
	struct space *sp,
	void *dest, L4_Word_t address, size_t size)
{
	assert(sp == current_space);

	/* curious x86 segment games */
	uint32_t wrap_addr = (uint32_t)address + KERNEL_SEG_SIZE;
	memcpy(dest, (void *)wrap_addr, size);
}


static size_t space_memcpy_from_fast(
	struct space *sp,
	void *dest, L4_Word_t address, size_t size)
{
	assert(sp == current_space);

	L4_Word_t fault_addr;
	if((fault_addr = catch_pf()) != 0) {
		return 0;
	} else {
		space_memcpy_from_unsafe(sp, dest, address, size);
		uncatch_pf();
		return size;
	}
}


size_t space_memcpy_from(
	void *dest,
	struct space *sp, L4_Word_t address, size_t size,
	struct pt_iter *sp_iter)
{
	if(size == 0) return 0;

	if(sp == current_space) {
		size_t ret = space_memcpy_from_fast(sp, dest, address, size);
		if(likely(ret > 0)) return ret;
	}

	/* the long ungrateful slog */
	struct pt_iter my_iter;
	if(sp_iter == NULL) {
		pt_iter_init(&my_iter, sp);
		sp_iter = &my_iter;
	}
	uintptr_t heap_addr = reserve_heap_page();
	size_t pos = 0;
	while(pos < size) {
		int seg = MIN(int, size - pos, PAGE_SIZE - (address & PAGE_MASK));
		uint32_t pgid = pt_probe(sp_iter, NULL, NULL, address, false, 0);
		if(pgid == 0) break;
		put_supervisor_page(heap_addr, pgid);
		memcpy(dest + pos, (void *)(heap_addr | (address & PAGE_MASK)), seg);

		address += seg;
		pos += seg;

		assert(pos >= size || (address & PAGE_MASK) == 0);
	}
	put_supervisor_page(heap_addr, 0);
	free_heap_page(heap_addr);
	if(sp_iter == &my_iter) pt_iter_destroy(&my_iter);

	return pos;
}


/* x86/amd64 bits */


/* the goal is that regardless of @size, the TSS structure should be allocated
 * in such a way that the TSS before the I/O bitmap falls within a single
 * memory page. this is guaranteed by alignment to the next biggest power of
 * two greater than @size. dlmalloc will pony up with such a block.
 */
static void *alloc_tss(size_t size) {
	return aligned_alloc(1 << size_to_shift(size), size);
}


bool space_add_ioperm(struct space *sp, L4_Word_t base_port, int size)
{
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

		struct thread *current = get_current_thread();
		if(likely(current != NULL) && current->space == sp) {
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
	return true;
}


/* syscalls. */

SYSCALL void sys_unmap(L4_Word_t control, void *utcb)
{
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

		int access = mapdb_unmap(cur_space, fp, mode);
		L4_Set_Rights(&fp, access);
		L4_VREG(utcb, L4_TCR_MR(i)) = fp.raw;
	}
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

	/* module inits */
	space_slab = KMEM_CACHE_NEW("space_slab", struct space);
	utcb_page_slab = KMEM_CACHE_NEW("utcb_page_slab", struct utcb_page);
}
