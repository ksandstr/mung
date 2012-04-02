
#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include <assert.h>

#include <ccan/likely/likely.h>
#include <ccan/list/list.h>
#include <ccan/htable/htable.h>
#include <ccan/alignof/alignof.h>
#include <ccan/compiler/compiler.h>

#include <l4/types.h>
#include <l4/vregs.h>
#include <ukernel/mm.h>
#include <ukernel/x86.h>
#include <ukernel/slab.h>
#include <ukernel/misc.h>
#include <ukernel/thread.h>
#include <ukernel/mapdb.h>
#include <ukernel/space.h>


struct space *kernel_space = NULL;
static struct space kernel_space_mem;

static struct kmem_cache *space_slab = NULL;
static struct list_head space_list = LIST_HEAD_INIT(space_list);


static size_t hash_page_by_id(const void *page_ptr, void *priv)
{
	const struct page *p = page_ptr;
	return int_hash(p->id);
}


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
	assert(sp->ptab_pages.elems == 0);
	htable_clear(&sp->ptab_pages);

	list_del_from(&space_list, &sp->link);

	kmem_cache_free(space_slab, sp);
}


void space_add_thread(struct space *sp, struct thread *t)
{
	assert(t->space == NULL);

	list_add(&sp->threads, &t->space_link);
	t->space = sp;
}


/* the UTCB setting part of SpaceControl. */
int space_set_utcb_area(struct space *sp, L4_Fpage_t area)
{
	assert(list_empty(&sp->threads));

	/* TODO: check overlap with KIP */
	if(L4_Size(area) < UTCB_SIZE) return 6;

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
	/* FIXME: change these to error returns */
	assert(L4_IsNilFpage(sp->kip_area));
	assert(L4_Address(area) < KERNEL_SEG_START);
	assert(L4_Address(area) + L4_Size(area) < KERNEL_SEG_START);
	assert(L4_Size(area) >= PAGE_SIZE);

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
		struct thread *t;
		list_for_each(&sp->threads, t, space_link) {
			if(t->utcb_pos == ix) return t;
		}
	}

	return NULL;
}


/* get-cmp function for struct space's ptab_pages */
static bool cmp_page_id_to_key(const void *cand, void *key) {
	const struct page *pg = cand;
	return pg->id == *(uint32_t *)key;
}


void space_put_page(
	struct space *sp,
	uintptr_t addr,
	uint32_t page_id,
	int access)
{
	assert(addr < KERNEL_SEG_START);

	int dir_ix = addr >> 22, ptab_ix = (addr >> 12) & 0x3ff;
	assert(sp->pdirs->vm_addr != NULL);
	uint32_t *pdir_mem = sp->pdirs->vm_addr;
	struct page *ptab_page;
	if(unlikely(!CHECK_FLAG(pdir_mem[dir_ix], PDIR_PRESENT))) {
		ptab_page = get_kern_page(0);
		/* TODO: prefill it? */
		memset(ptab_page->vm_addr, 0, PAGE_SIZE);
		pdir_mem[dir_ix] = ptab_page->id << 12 | PDIR_PRESENT | PDIR_USER
			| PDIR_RW;
		htable_add(&sp->ptab_pages, int_hash(ptab_page->id), ptab_page);
	} else {
		uint32_t pt_id = pdir_mem[dir_ix] >> 12;
		ptab_page = htable_get(&sp->ptab_pages, int_hash(pt_id),
			&cmp_page_id_to_key, &pt_id);
		assert(ptab_page != NULL);
	}
	assert(ptab_page->vm_addr != NULL);

	uint32_t *ptab_mem = ptab_page->vm_addr;
	bool invalidate = CHECK_FLAG(ptab_mem[ptab_ix], PT_PRESENT)
		&& (ptab_mem[ptab_ix] >> 12) != page_id;
	ptab_mem[ptab_ix] = page_id << 12 | PT_PRESENT | PT_USER
		| (CHECK_FLAG(access, L4_Writable) ? PT_RW : 0);
	if(invalidate) {
		/* TODO: skip this if _sp_'s page table is not currently loaded */
		x86_invalidate_page(addr & ~PAGE_MASK);
	}
}


/* syscalls.
 *
 * TODO: divorce them from the x86_regs representation.
 */

void sys_unmap(struct x86_exregs *regs)
{
	L4_Word_t control = regs->eax;
	printf("%s: called. control %#x\n", __func__, (unsigned)control);

	struct thread *current = get_current_thread();
	void *utcb = thread_get_utcb(current);
	if((control & 0x3f) > 0) L4_VREG(utcb, L4_TCR_MR(0)) = regs->esi;
	for(int i=0; i <= (control & 0x3f); i++) {
		L4_Fpage_t fp = { .raw = L4_VREG(utcb, L4_TCR_MR(i)) };
		printf("  would %s %#x:%#x (%c%c%c) for thread %d:%d\n",
			CHECK_FLAG(control, 0x40) ? "flush" : "unmap",
			(unsigned)L4_Address(fp), (unsigned)L4_Size(fp),
			CHECK_FLAG(L4_Rights(fp), L4_Readable) ? 'r' : '-',
			CHECK_FLAG(L4_Rights(fp), L4_Writable) ? 'w' : '-',
			CHECK_FLAG(L4_Rights(fp), L4_eXecutable) ? 'x' : '-',
			TID_THREADNUM(current->id), TID_VERSION(current->id));
		L4_Set_Rights(&fp, L4_NoAccess);
		L4_VREG(utcb, L4_TCR_MR(i)) = fp.raw;
	}

	regs->esi = L4_VREG(utcb, L4_TCR_MR(0));
}


void sys_spacecontrol(struct x86_exregs *regs)
{
	L4_Word_t control = regs->ecx, result, old_ctl;
	L4_ThreadId_t spacespec = { .raw = regs->eax };
	L4_Fpage_t kip_area = { .raw = regs->edx },
		utcb_area = { .raw = regs->esi },
		redirector = { .raw = regs->edi };

	printf("%s: called; ctl %#x, spacespec %d:%d, kip_area %#x:%#x\n",
		__func__, control,
		TID_THREADNUM(spacespec.raw), TID_VERSION(spacespec.raw),
		(unsigned)L4_Address(kip_area), (unsigned)L4_Size(kip_area));
	printf("%s: ... utcb_area %#x:%#x, redirector %d:%d\n", __func__,
		(unsigned)L4_Address(utcb_area), (unsigned)L4_Size(utcb_area),
		TID_THREADNUM(redirector.raw), TID_VERSION(redirector.raw));
	result = 0xdeadbeef;
	old_ctl = 0x42424242;

	regs->eax = result;
	regs->ecx = old_ctl;
}


/* NOTE: this runs in the pre-heap environment. so htable ops aren't
 * available; instead the pages end up in the list given as parameter.
 * likewise kernel_space->mapdb is left uninitialized.
 */
COLD void init_spaces(struct list_head *resv_list)
{
	memset(&kernel_space_mem, 0, sizeof(kernel_space_mem));
	kernel_space = &kernel_space_mem;
	space_init(kernel_space, resv_list);

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
	kernel_space->utcb_area = L4_Fpage(KERNEL_HEAP_TOP,
		UTCB_SIZE * NUM_KERNEL_THREADS);
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
