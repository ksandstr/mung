
#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include <assert.h>
#include <ccan/list/list.h>
#include <ccan/likely/likely.h>
#include <ccan/container_of/container_of.h>

#include <l4/types.h>
#include <l4/kcp.h>
#include <l4/kip.h>

#include <ukernel/slab.h>
#include <ukernel/rbtree.h>
#include <ukernel/thread.h>
#include <ukernel/space.h>
#include <ukernel/misc.h>
#include <ukernel/mm.h>


#define N_FIRST_PAGES (2 * 1024 * 1024 / PAGE_SIZE)
#define HEAP_MARGIN 12		/* # of pages not given to heap */


/* a page of unused address space in the kernel. */
struct as_free {
	struct rb_node rb;	/* in free_as_tree */
	L4_Fpage_t fp;		/* rights bits ignored */
};


static struct list_head k_free_pages = LIST_HEAD_INIT(k_free_pages),
	k_slab_pages = LIST_HEAD_INIT(k_slab_pages),
	k_heap_pages = LIST_HEAD_INIT(k_heap_pages);

static struct kmem_cache *mm_page_cache = NULL,	/* <struct page> */
	*free_as_cache = NULL;		/* <struct as_free> */

static uintptr_t heap_pos = ~0ul, resv_pos = KERNEL_HEAP_TOP;
static size_t n_free_pages = 0;
static struct rb_root free_as_tree;		/* sorted by sizelog2, address */


void *sbrk(intptr_t increment)
{
	if(increment > 0) {
		size_t n_pages = ((uintptr_t)increment + PAGE_SIZE - 1) >> PAGE_BITS;
		if(n_free_pages < n_pages + HEAP_MARGIN) {
			/* you fail it. your skill is not enough */
			return (void *)~0ull;
		}
		uintptr_t start_pos = heap_pos;
		heap_pos += n_pages << PAGE_BITS;
		assert(heap_pos <= resv_pos);
		for(size_t i=0; i < n_pages; i++) {
			struct page *pg = get_kern_page(start_pos + i * PAGE_SIZE);
			assert((uintptr_t)pg->vm_addr == start_pos + i * PAGE_SIZE);
			list_add(&k_heap_pages, &pg->link);
		}
		return (void *)start_pos;
	}

	if(increment < 0) {
		size_t dec = -increment, n_pages = dec >> PAGE_BITS;
		for(size_t i=1; i <= n_pages; i++) {
			uintptr_t expect = heap_pos - i * PAGE_SIZE;
			struct page *pg = list_pop(&k_heap_pages, struct page, link);
			if(pg == NULL) panic("attempted to shrink heap past start!");
			if(unlikely((L4_Word_t)pg->vm_addr != expect)) {
				/* FIXME: prove that this never occurs, then assert against
				 * it
				 */
				printf("%s: failed to shrink, exp. vm_addr %#lx, got %p\n",
					__func__, (L4_Word_t)expect, pg->vm_addr);
				list_add_tail(&k_heap_pages, &pg->link);
			} else {
				/* without these, free_kern_page() releases the address space
				 * back into the range allocator; that isn't what we want.
				 */
				put_supervisor_page((uintptr_t)pg->vm_addr, 0);
				pg->vm_addr = NULL;
				free_kern_page(pg);
			}
		}
		heap_pos -= n_pages * PAGE_SIZE;
	}

	return (void *)heap_pos;
}


static struct as_free *insert_as_free_helper(
	struct rb_root *root,
	struct as_free *f)
{
	struct rb_node **p = &root->rb_node, *parent = NULL;
	while(*p != NULL) {
		parent = *p;
		struct as_free *oth = rb_entry(parent, struct as_free, rb);
		int cmp = (int)L4_SizeLog2(oth->fp) - (int)L4_SizeLog2(f->fp);
		if(cmp == 0) {
			cmp = (intptr_t)L4_Address(oth->fp) - L4_Address(f->fp);
			if(cmp == 0) return oth;
		}
		if(cmp < 0) p = &(*p)->rb_left; else p = &(*p)->rb_right;
	}
	rb_link_node(&f->rb, parent, p);
	return NULL;
}


static struct as_free *insert_as_free(struct rb_root *root, struct as_free *f)
{
#if 0
	printf("%s: insert %#lx:%#lx\n", __func__,
		L4_Address(f->fp), L4_Size(f->fp));
#endif
	struct as_free *dupe = insert_as_free_helper(root, f);
	if(dupe != NULL) return dupe;

	rb_insert_color(&f->rb, root);
	/* try to coalesce @f with its neighbour. */
	struct rb_node *n = CHECK_FLAG(L4_Address(f->fp), L4_Size(f->fp))
		? rb_next(&f->rb) : rb_prev(&f->rb);
	if(n != NULL) {
		struct as_free *oth = rb_entry(n, struct as_free, rb);
		if(L4_SizeLog2(oth->fp) == L4_SizeLog2(f->fp)
			&& L4_Address(oth->fp) == (L4_Address(f->fp) ^ L4_Size(f->fp)))
		{
			/* roight! toss both items, delete @oth, enlarge @f and
			 * reinsert it.
			 */
			rb_erase(&oth->rb, root);
			kmem_cache_free(free_as_cache, oth);
			rb_erase(&f->rb, root);
			f->fp = L4_FpageLog2(L4_Address(f->fp) & ~L4_Size(f->fp),
				L4_SizeLog2(f->fp) + 1);
			return insert_as_free(root, f);
		}
	}

	return NULL;
}


static struct as_free *put_as_free(uintptr_t addr, int sizelog2)
{
	struct as_free *f = kmem_cache_alloc(free_as_cache);
	f->fp = L4_FpageLog2(addr, sizelog2);
	struct as_free *dupe = insert_as_free(&free_as_tree, f);
	if(likely(dupe == NULL)) dupe = f; else kmem_cache_free(free_as_cache, f);
	return dupe;
}


/* allocates room from the top of the kernel address space, and from a tree of
 * free address space released with free_heap_page(), preferring the latter.
 *
 * FIXME: this'll happily run over the heap area in the new-allocation part.
 */
uintptr_t reserve_heap_range(size_t size)
{
	assert(size == (1 << size_to_shift(size)));

	/* try to find an aligned fpage of the right size. */
	int sizelog2 = size_to_shift(size);
	struct rb_node *n = free_as_tree.rb_node;
	struct as_free *f = NULL;
	while(n != NULL) {
		f = rb_entry(n, struct as_free, rb);
		int cmp = (int)L4_SizeLog2(f->fp) - sizelog2;
		if(cmp == 0) break;
		if(cmp < 0) n = n->rb_left; else n = n->rb_right;
	}
	/* wind a smaller entry forward until a larger is found, or not. */
	while(f != NULL && L4_SizeLog2(f->fp) < sizelog2) {
		n = rb_next(&f->rb);
		if(n == NULL) f = NULL; else f = rb_entry(n, struct as_free, rb);
	}
	if(f != NULL) {
		/* (in recognition that all cases where this is true start with
		 * rb_erase()...)
		 */
		rb_erase(&f->rb, &free_as_tree);
	}
	uintptr_t addr;
	if(f != NULL && L4_SizeLog2(f->fp) == sizelog2) {
		/* whole case */
		addr = L4_Address(f->fp);
		kmem_cache_free(free_as_cache, f);
	} else if(f != NULL && L4_SizeLog2(f->fp) == sizelog2 + 1) {
		/* simple split case. keep the high half, shrink the other down and
		 * reinsert.
		 */
		addr = L4_Address(f->fp) + L4_Size(f->fp) / 2;
		f->fp = L4_FpageLog2(L4_Address(f->fp),
			L4_SizeLog2(f->fp) - 1);
		insert_as_free(&free_as_tree, f);
	} else if(f != NULL) {
		/* grab ours at the upper end, & make shrapnel for rest */
		addr = L4_Address(f->fp) + L4_Size(f->fp) - size;
		L4_Word_t r_addr;
		int r_sizelog2;
		for_page_range(L4_Address(f->fp), addr, r_addr, r_sizelog2) {
			put_as_free(r_addr, r_sizelog2);
		}
		kmem_cache_free(free_as_cache, f);
	} else {
		/* align up. or rather, down */
		L4_Word_t r_addr;
		int r_sizelog2;
		for_page_range(resv_pos & ~(size - 1), resv_pos, r_addr, r_sizelog2) {
			put_as_free(r_addr, r_sizelog2);
		}
		resv_pos &= ~(size - 1);
		resv_pos -= size;
		addr = resv_pos;
	}

	return addr;
}


void free_heap_page(uintptr_t addr)
{
	assert((addr & PAGE_MASK) == 0);

	put_supervisor_page(addr, 0);
	put_as_free(addr, PAGE_BITS);
}


/* maps @pg into a reserve_heap_page() area. */
void *map_vm_page(struct page *pg, int duration)
{
	if(CHECK_FLAG(pg->flags, PAGEF_VMREF)) {
		if(duration == VM_REF) pg->refcount++;
		return pg->vm_addr;
	}

	if(pg->vm_addr == NULL) {
		pg->vm_addr = (void *)reserve_heap_page();
		if(duration == VM_REF) {
			pg->refcount = 1;
			pg->flags |= PAGEF_VMREF;
		} else {
			assert(duration == VM_SYSCALL);
			pg->refcount = 0;
			pg->flags &= ~PAGEF_VMREF;
			/* TODO: set VM_SYSCALL epoch */
		}
		put_supervisor_page((uintptr_t)pg->vm_addr, pg->id);
	}

	return pg->vm_addr;
}


void unref_vm_page(struct page *pg)
{
	if(CHECK_FLAG(pg->flags, PAGEF_VMREF)) {
		assert(pg->refcount > 0);
		if(--pg->refcount == 0) {
			/* downgrade to VM_SYSCALL scope. */
			pg->flags &= ~PAGEF_VMREF;
			/* TODO: set VM_SYSCALL epoch */
		}
	}
}


static COLD bool page_is_available(
	const L4_KernelConfigurationPage_t *kcp,
	L4_Word_t addr)
{
	assert(offsetof(L4_KernelConfigurationPage_t, MemoryInfo) == 0x54);
	L4_MemoryDesc_t *mds = (void *)kcp + kcp->MemoryInfo.MemDescPtr;
	int md_count = kcp->MemoryInfo.n;
	bool virt_ok = false, conv_ok = false, reserved = false;
//	printf("%s: address %#x (%d memorydescs)...\n", __func__, addr, md_count);
	for(int i=0; i < md_count; i++) {
		L4_Word_t low = L4_MemoryDescLow(&mds[i]),
			high = L4_MemoryDescHigh(&mds[i]);
		int type = L4_MemoryDescType(&mds[i]);
		bool virtual = L4_IsMemoryDescVirtual(&mds[i]);

		if(addr < low || addr > high) continue;

#if 0
		size_t size = (high - low + 1) / 1024;
		printf("memdesc %d: %#x .. %#x, size %u KiB, type %x, %s\n",
			i, low, high, (unsigned)size, type,
			virtual ? "virtual" : "physical");
#endif

		if(!virt_ok && virtual) {
			virt_ok = true;
		} else if(!virtual && type == L4_ConventionalMemoryType
			&& virt_ok && !conv_ok)
		{
			conv_ok = true;
		} else if(!virtual && conv_ok && type != L4_ConventionalMemoryType) {
			reserved = true;
			break;
		}
	}

	return virt_ok && conv_ok && !reserved;
}


/* reserves initial memory for the kernel. this is subsequently used to
 * allocate spaces, threads, UTCB pages, page tracking structures, copies of
 * ACPI tables, etc...
 *
 * the kernel sbrk() heap will be positioned 2 megs after *resv_end. hopefully
 * this leaves enough room for ACPI tables and such.
 */
void init_kernel_heap(
	void *kcp_base,
	uintptr_t *resv_start,
	uintptr_t *resv_end)
{
	const L4_KernelConfigurationPage_t *kcp = kcp_base;

	/* grab early pages from conventional memory that isn't reserved by a
	 * bootloader-defined object.
	 */
	extern char _start, _end;
	L4_Word_t next_addr = (L4_Word_t)&_end;	/* ... or the kernel binary. */
	next_addr = (next_addr + PAGE_SIZE - 1) & ~PAGE_MASK;
	printf("kernel early memory low address is %#x\n", (unsigned)next_addr);
	int got = 0;
	static struct page first_pages[N_FIRST_PAGES];
	while(got < N_FIRST_PAGES) {
		if(next_addr > (64 * 1024 * 1024)) {
			/* stop at the 64 MiB mark. */
			panic("init_kernel_heap limit reached");
		}
		if(page_is_available(kcp, next_addr)) {
			struct page *pg = &first_pages[got++];
			*pg = (struct page){
				.id = next_addr >> PAGE_BITS,
				.vm_addr = (void *)next_addr,
				.flags = PAGEF_INITMEM,
			};
			list_add(&k_free_pages, &pg->link);
			n_free_pages++;
		}
		next_addr += PAGE_SIZE;
	}
	printf("uppermost reserved byte is at %#x\n", (unsigned)next_addr - 1);

	/* initialize page slab & return. */
	mm_page_cache = KMEM_CACHE_NEW("mm_page_cache", struct page);
	free_as_cache = KMEM_CACHE_NEW("free_as_cache", struct as_free);
	free_as_tree = RB_ROOT;
	*resv_start = MIN(uintptr_t, (uintptr_t)&_start, *resv_start);
	*resv_end = MAX(uintptr_t, next_addr - 1, *resv_end);	/* (inclusive.) */

	const size_t heap_start_align = 2 * 1024 * 1024;
	heap_pos = (*resv_end + heap_start_align) & ~(heap_start_align - 1);
	printf("kernel heap starts at %#lx\n", (L4_Word_t)heap_pos);

	L4_Word_t siz = *resv_end + 1 - *resv_start;
	printf("... total kernel reservation is %lu KiB (~%lu MiB).\n",
		siz / 1024, (siz + 1024 * 1024 - 1) / (1024 * 1024));
}


struct page *get_kern_page(uintptr_t vm_addr)
{
	assert((vm_addr & PAGE_MASK) == 0);
	assert(!list_empty(&k_free_pages));
	/* (get from tail of list, as that's where the idempotent heap is during
	 * early boot. otherwise there is a chance of endless recursion through
	 * put_supervisor_page()'s not finding the page directory for the vm heap.
	 * [... more recently though, .next seems to work just as well.])
	 */
	struct page *p;
	do {
		p = container_of(k_free_pages.n.prev, struct page, link);
		list_del(&p->link);
		n_free_pages--;
		/* don't return pages with a physical address below 0x100000, as these
		 * are special on the x86 (video memory, etc)
		 */
		if(p->id < (0x100000 >> PAGE_BITS)) {
			/* TODO: stick these in a reserved list. maybe. */
			printf("%s: NOTE: skipping page id %u (physaddr 0x%x)\n",
				__func__, p->id, (uintptr_t)p->id << PAGE_BITS);
			p = NULL;
		}
	} while(p == NULL);

	if(vm_addr == 0) {
		if(p->vm_addr == NULL) {
			/* map it in at some address. */
			vm_addr = reserve_heap_page();
			put_supervisor_page(vm_addr, p->id);
			p->vm_addr = (void *)vm_addr;
		}
	} else {
		if(p->vm_addr != NULL
			&& likely(!CHECK_FLAG(p->flags, PAGEF_INITMEM)))
		{
			/* remove the heap reservation. */
			uintptr_t addr = (uintptr_t)p->vm_addr;
			put_supervisor_page(addr, 0);
			free_heap_page(addr);
			p->vm_addr = NULL;
		}

		put_supervisor_page(vm_addr, p->id);
		p->vm_addr = (void *)vm_addr;
	}

	return p;
}


void free_kern_page(struct page *page)
{
	/* better here than at the call sites. */
	if(page == NULL) return;

	if(page->vm_addr != NULL
		&& likely(!CHECK_FLAG(page->flags, PAGEF_INITMEM)))
	{
		free_heap_page((uintptr_t)page->vm_addr);
		page->vm_addr = NULL;
	}
	/* grow the freelist at the end for t_slab.c's sake */
	list_add_tail(&k_free_pages, &page->link);
	n_free_pages++;
}


/* interface for slab.c */
void *kmem_alloc_new_page(void)
{
	struct page *pg = get_kern_page(0);
	if(pg != NULL) {
		list_add(&k_slab_pages, &pg->link);
		assert(pg->vm_addr != NULL);
		return pg->vm_addr;
	} else {
		return NULL;
	}
}


void kmem_free_page(void *ptr)
{
	/* TODO: proper data structures, man. hash tables won't work because
	 * kmem_alloc_new_page() will be called before the sbrk heap is ready;
	 * so a custom solution seems to be required. (perhaps a chain of pages,
	 * each containing 1000 or 500 pointers hashed by address. would require a
	 * bigger test of this mechanism in t_slab.c, and also an bigger slice of
	 * kernel-reserved memory.)
	 */
	struct page *pg;
	list_for_each(&k_slab_pages, pg, link) {
		if(pg->vm_addr == ptr) {
			list_del_from(&k_slab_pages, &pg->link);
			free_kern_page(pg);
			return;
		}
	}

	printf("warning: %s(%p) refers to an unknown page\n", __func__, ptr);
}


#include <ukernel/ktest.h>
#if KTEST

/* simple and brutal: do a big alloc_range, free it all, allocate one page at
 * a time until the tree becomes empty (which this'll observe directly).
 */
START_TEST(t_alloc_from_large)
{
	const size_t reserve_size = 1024 * 128;
	plan_tests(2);

	uintptr_t big = reserve_heap_range(reserve_size);
	assert(big != 0);
	diag("big=%#lx", (L4_Word_t)big);
	for(size_t i=0; i < reserve_size; i += PAGE_SIZE) {
		free_heap_page(big + i);
	}
	ok(true, "prep didn't crash");

	/* count how many pages we'll get.
	 *
	 * TODO: vary the drain-reservation size; it could also be 8k, but then
	 * it'd need to check for whether the tree is empty of 8k and larger
	 * pages.
	 */
	int n_ptrs = 0;
	for(struct rb_node *rb = rb_first(&free_as_tree);
		rb != NULL;
		rb = rb_next(rb))
	{
		struct as_free *f = rb_entry(rb, struct as_free, rb);
		n_ptrs += 1 << (L4_SizeLog2(f->fp) - 12);
	}
	diag("n_ptrs=%d", n_ptrs);
	uintptr_t *ptrs = malloc(sizeof(*ptrs) * n_ptrs);
	assert(ptrs != NULL);	/* in the absence of a fail_unless(), ... */
	for(int i=0; i < n_ptrs; i++) {
		ptrs[i] = reserve_heap_page();
		assert(ptrs[i] != 0);
	}
	ok(RB_EMPTY_ROOT(&free_as_tree), "tree became empty");

	/* clean up. */
	for(int i=0; i < n_ptrs; i++) free_heap_page(ptrs[i]);
	free(ptrs);
}
END_TEST


void ktest_heap(void) {
	RUN(t_alloc_from_large);
}

#endif
