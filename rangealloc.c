
#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include <assert.h>
#include <ccan/htable/htable.h>

#include <ukernel/mm.h>
#include <ukernel/bug.h>
#include <ukernel/slab.h>
#include <ukernel/util.h>
#include <ukernel/rangealloc.h>


struct ra_page
{
	uintptr_t address;
	struct ra_page *next;
	struct page *page;
	short n_free;
	uint32_t freemap[];
};


static size_t hash_ra_page(const void *ptr, void *priv) {
	const struct ra_page *p = ptr;
	return int_hash(p->address);
}


static bool cmp_ra_page_to_addr(const void *cand, void *addrptr) {
	const struct ra_page *p = cand;
	return p->address == *(const uintptr_t *)addrptr;
}


static int fmap_limbs(const struct rangealloc *ra) {
	int ret = ((PAGE_SIZE >> ra->ob_size_log2) + 31) >> 5;
	assert(ret > 0);
	return ret;
}


/* allocate and free kernel memory without inadvertently causing recycling of
 * the rangealloc range. this is necessary because free_kern_page() insists on
 * recycling its parameter's vm_addr, where present, in addition to the
 * physical page.
 */
static struct page *alloc_vm_page(uintptr_t address)
{
	struct page *p = get_kern_page(address);
	memset(p->vm_addr, 0, PAGE_SIZE);
	return p;
}


static void free_vm_page(struct page *p)
{
	assert(p->vm_addr != NULL);
	put_supervisor_page((uintptr_t)p->vm_addr, 0);
	p->vm_addr = NULL;
	free_kern_page(p);
}


struct rangealloc *ra_create(
	int range_log2,
	unsigned short obj_size, unsigned short align)
{
	assert(range_log2 >= PAGE_BITS && range_log2 <= sizeof(L4_Word_t) * 8);
	struct rangealloc *ra = malloc(sizeof(*ra));
	if(ra == NULL) return NULL;

	uintptr_t start = reserve_heap_range(1 << range_log2);
	BUG_ON(start == 0, "can't start rangealloc at address 0");
	*ra = (struct rangealloc){
		.range = L4_FpageLog2(start, range_log2),
		.ob_size_log2 = size_to_shift((obj_size + align - 1) & ~(align - 1)),
		.page_hash = HTABLE_INITIALIZER(ra->page_hash, &hash_ra_page, NULL),
	};
	ra->and_mask = (L4_Size(ra->range) - 1) & (~0ul << ra->ob_size_log2);
	ra->or_mask = L4_Address(ra->range) & ~ra->and_mask;
	ra->id_shift = ffsl(ra->and_mask) - 1;
	ra->meta_slab = kmem_cache_create("ra_page slab",
		sizeof(struct ra_page) + sizeof(uint32_t) * fmap_limbs(ra),
		alignof(struct ra_page), 0, NULL, NULL);

	return ra;
}


static struct ra_page *alloc_ra_page(
	struct rangealloc *ra, uintptr_t address)
{
	if(address == 0) {
		/* synthesize an unused page address within @ra->range. */
		address = L4_Address(ra->range) + ra->page_hash.elems * PAGE_SIZE;
		bool wrapped = false;
		while(htable_get(&ra->page_hash, int_hash(address),
			&cmp_ra_page_to_addr, &address) != NULL)
		{
			address += PAGE_SIZE;
			if(address > FPAGE_HIGH(ra->range)) {
				if(wrapped) return NULL;
				address = FPAGE_LOW(ra->range);
				wrapped = true;
			}
		}
	}
	assert(ADDR_IN_FPAGE(ra->range, address));

	struct ra_page *p = kmem_cache_alloc(ra->meta_slab);
	p->address = address;
	p->n_free = PAGE_SIZE >> ra->ob_size_log2;
	p->page = alloc_vm_page(address);
	assert(p->page->vm_addr == (void *)address);
	if(fmap_limbs(ra) == 1) {
		p->freemap[0] = (1u << p->n_free) - 1;
	} else {
		for(int i=0; i < fmap_limbs(ra); i++) p->freemap[i] = ~0u;
	}
	bool ok = htable_add(&ra->page_hash, hash_ra_page(p, NULL), p);
	if(!ok) {
		free_vm_page(p->page);
		kmem_cache_free(ra->meta_slab, p);
		p = NULL;
	}

	return p;
}


static void free_ra_page(struct rangealloc *ra, struct ra_page *p)
{
	htable_del(&ra->page_hash, hash_ra_page(p, NULL), p);
	free_vm_page(p->page);
	kmem_cache_free(ra->meta_slab, p);
}


static void delist_page(struct ra_page **p_head, struct ra_page *p)
{
	while(*p_head != NULL && *p_head != p) {
		p_head = &(*p_head)->next;
	}
	assert(*p_head != NULL);
	*p_head = p->next;
	p->next = NULL;
}


void *ra_alloc(struct rangealloc *ra, long id)
{
	struct ra_page **p_head, *p;
	if(id < 0) {
		/* grab a partial page, or allocate a new page, and synthesize an
		 * identifier using its freemap.
		 */
		p_head = &ra->partial_head;
		if(*p_head != NULL) p = *p_head;
		else {
			p = alloc_ra_page(ra, 0);
			if(p == NULL) return NULL;
			p->next = *p_head;
			*p_head = p;
		}
		for(int i=0; i < fmap_limbs(ra); i++) {
			int pos = ffsl(p->freemap[i]);
			if(pos > 0) {
				id = ((p->address - L4_Address(ra->range)) >> ra->ob_size_log2)
					+ i * 32 + pos - 1;
				break;
			}
		}
		assert(id >= 0);
		assert(p->n_free > 0);
	} else {
		uintptr_t address = L4_Address(ra->range)
			+ ((id << ra->ob_size_log2) & ~PAGE_MASK);
		p = htable_get(&ra->page_hash, int_hash(address),
			&cmp_ra_page_to_addr, &address);
		if(p == NULL) {
			p = alloc_ra_page(ra, address);
			if(p == NULL) return NULL;
			p_head = &ra->partial_head;
			p->next = *p_head;
			*p_head = p;
		} else {
			p_head = p->n_free > 0 ? &ra->partial_head : &ra->full_head;
		}
		assert(p->address == address);
	}

	long pos = id - ((p->address - L4_Address(ra->range)) >> ra->ob_size_log2),
		limb = pos / 32, ix = pos % 32;
	assert(pos >= 0 && pos < (PAGE_SIZE >> ra->ob_size_log2));
	if(!CHECK_FLAG(p->freemap[limb], 1u << ix)) return NULL;
	p->freemap[limb] &= ~(1u << ix);
	if(--p->n_free == 0) {
		*p_head = p->next;
		p->next = ra->full_head;
		ra->full_head = p;
	}
	uintptr_t ret = p->address + (pos << ra->ob_size_log2);
	assert(ret == L4_Address(ra->range) + ((uintptr_t)id << ra->ob_size_log2));
	assert(ADDR_IN_FPAGE(ra->range, ret));
	assert(BETWEEN(p->address, p->address + PAGE_SIZE - 1, ret));
	assert(BETWEEN(p->address, p->address + PAGE_SIZE - 1,
		ret + (1 << ra->ob_size_log2) - 1));
	return (void *)ret;
}


void *ra_zalloc(struct rangealloc *ra, long id)
{
	void *ptr = ra_alloc(ra, id);
	if(ptr != NULL) memset(ptr, 0, 1 << ra->ob_size_log2);
	return ptr;
}


void ra_free(struct rangealloc *ra, void *deadptr)
{
	uintptr_t address = (uintptr_t)deadptr & ~PAGE_MASK;
	struct ra_page *p = htable_get(&ra->page_hash, int_hash(address),
		&cmp_ra_page_to_addr, &address);
	BUG_ON(p == NULL, "didn't find ra_page for deadptr=%p (address=%#lx)",
		deadptr, (L4_Word_t)address);

	long id = ((uintptr_t)deadptr - L4_Address(ra->range)) >> ra->ob_size_log2,
		pos = id - ((p->address - L4_Address(ra->range)) >> ra->ob_size_log2),
		limb = pos / 32, ix = pos % 32;
	BUG_ON(CHECK_FLAG(p->freemap[limb], 1 << ix),
		"deadptr=%p was already freed", deadptr);
	p->freemap[limb] |= 1 << ix;
	p->n_free++;

	/* pagelist management */
	int n_per_page = PAGE_SIZE >> ra->ob_size_log2;
	if(p->n_free == n_per_page) {
		/* free the page.
		 *
		 * TODO: this can cause awful pagetable pingpong when the sole item is
		 * being freed, allocated, etc. over and over. address that.
		 */
		delist_page(p->n_free == 1 ? &ra->full_head : &ra->partial_head, p);
		free_ra_page(ra, p);
	} else if(p->n_free == 1) {
		/* move back to partial list. */
		delist_page(&ra->full_head, p);
		p->next = ra->partial_head;
		ra->partial_head = p;
	}
}


void *ra_id2ptr_safe(struct rangealloc *ra, long id)
{
	void *ptr = ra_id2ptr(ra, id);
	uintptr_t address = (uintptr_t)ptr & ~PAGE_MASK;
	struct ra_page *p = htable_get(&ra->page_hash, int_hash(address),
		&cmp_ra_page_to_addr, &address);
	if(p == NULL) return NULL;
	long pos = id - ((p->address - L4_Address(ra->range)) >> ra->ob_size_log2),
		limb = pos / 32, ix = pos % 32;
	if(CHECK_FLAG(p->freemap[limb], 1 << ix)) ptr = NULL;
	return ptr;
}


void *ra_first(const struct rangealloc *ra, struct ra_iter *it)
{
	it->p = htable_first(&ra->page_hash, &it->hti);
	if(it->p == NULL) return NULL;
	else {
		it->pos = -1;
		return ra_next(ra, it);
	}
}


void *ra_next(const struct rangealloc *ra, struct ra_iter *it)
{
	assert(it->p != NULL);
	int n_per_page = PAGE_SIZE >> ra->ob_size_log2;

	if(it->pos >= n_per_page) {
		it->p = htable_next(&ra->page_hash, &it->hti);
		if(it->p == NULL) return NULL;
		it->pos = -1;
	}
	for(int i = MIN(int, 0, it->pos) / 32; i < fmap_limbs(ra); i++) {
		uint32_t limb = ~it->p->freemap[i];
		if(i == fmap_limbs(ra) - 1) {
			/* clear the tail bits off. */
			limb &= (1u << (n_per_page % 32)) - 1;
		}
		/* and the previously-visited low bits also. */
		limb &= ~((1u << ((it->pos + 1) % 32)) - 1);
		int ix = ffsl(limb);
		if(ix > 0) {
			it->pos = i * 32 + ix - 1;
			break;
		} else {
			it->pos = -1;
		}
	}
	if(it->pos == -1) {
		it->pos = n_per_page;
		return ra_next(ra, it);
	}

	uintptr_t ret = it->p->address + (it->pos << ra->ob_size_log2);
	assert(ADDR_IN_FPAGE(ra->range, ret));
	assert(BETWEEN(it->p->address, it->p->address + PAGE_SIZE - 1, ret));
	assert(BETWEEN(it->p->address, it->p->address + PAGE_SIZE - 1,
		ret + (1 << ra->ob_size_log2) - 1));
	return (void *)ret;
}


#include <ukernel/ktest.h>
#if KTEST

/* in-kernel tests for rangealloc id2ptr, ptr2id ops. */

struct tobj {
	struct list_node link;
	char name[32];
	int value;
};


static void dancing_rals(struct rangealloc *ra, int test_size)
{
	struct tobj *ary[test_size];
	for(int i=0; i < test_size; i++) ary[i] = ra_alloc(ra, -1);
	for(int i=0; i < test_size; i += 2) ra_free(ra, ary[i]);
	for(int i=0; i < test_size; i += 2) ary[i] = ra_alloc(ra, -1);
	for(int i=0; i < test_size; i++) ra_free(ra, ary[i]);
}


START_TEST(masks)
{
	plan_tests(3);

	assert(alignof(struct tobj) <= 64);
	struct rangealloc *ra = ra_create(20, sizeof(struct tobj),
		alignof(struct tobj));

	uintptr_t and_mask = ra->and_mask, or_mask = ra->or_mask;
	diag("and_mask=%#lx, or_mask=%#lx", and_mask, or_mask);
	ok1(POPCOUNT(and_mask) == 14);
	ok1((or_mask & and_mask) == 0);
	ok1((and_mask & 0x3f) == 0);
}
END_TEST


/* allocations must have the common bits given in or_mask . */
START_TEST(alloc)
{
	plan_tests(4);
	const size_t test_size = 5000, ob_align = 64;

	assert(alignof(struct tobj) <= ob_align);
	struct rangealloc *ra = ra_create(20,
		sizeof(struct tobj), ob_align);

	uintptr_t and_mask = ra->and_mask, or_mask = ra->or_mask;
	diag("and_mask=%#lx, or_mask=%#lx", and_mask, or_mask);

	struct tobj **tobs = calloc(test_size, sizeof(struct tobj *));
	bool all_ok = true, all_aligned = true;
	for(size_t i=0; i < test_size; i++) {
		struct tobj *o = ra_zalloc(ra, -1);
		assert(o != NULL);
		tobs[i] = o;

		uintptr_t op = (uintptr_t)o;
		if((op & (ob_align - 1)) != 0) {
			if(all_aligned) {
				diag("first misalign is o=%p (align=%u)",
					o, (unsigned)ob_align);
			}
			all_aligned = false;
		}
		if((op & ~and_mask) != or_mask) {
			diag("tobs[%u]=%p, masked=%#lx (!= or_mask=%#lx)",
				(unsigned)i, o, (uintptr_t)o & ~and_mask, or_mask);
			all_ok = false;
			break;
		}
	}
	ok(all_ok, "initial allocations");
	ok1(all_aligned);

	dancing_rals(ra, 234);
	ok(true, "alive after dancing_rals()");

	/* free the first few. */
	for(size_t i=0; i < 456; i++) {
		if(tobs[i] == NULL) {
			ra_free(ra, tobs[i]);
			tobs[i] = NULL;
		}
	}
	/* and allocate some of them again, start to finish. */
	for(size_t i=0; i < 456; i+=3) tobs[i] = ra_zalloc(ra, -1);
	ok(true, "still alive after recycling");

	/* toss all. */
	for(size_t i=0; i < test_size; i++) {
		if(tobs[i] != NULL) ra_free(ra, tobs[i]);
	}

	free(tobs);
}
END_TEST


/* test that the reference macros work for casting between IDs and pointers in
 * the aligned segment.
 */
START_TEST(id_casting)
{
	plan_tests(1);
	const size_t test_size = 5007;

	assert(alignof(struct tobj) <= 64);
	struct rangealloc *ra = ra_create(20, sizeof(struct tobj), 64);

	/* NOTE: zero_ix handling is pointless because no object will be allocated
	 * over the slab in the first page.
	 */
	struct tobj **tobs = calloc(test_size, sizeof(struct tobj *));
	uint16_t *tids = calloc(test_size, sizeof(uint16_t));
	int zero_ix = -1;
	for(size_t i=0; i < test_size; i++) {
		tobs[i] = ra_zalloc(ra, -1);
		assert(tobs[i] != NULL);

		tids[i] = ra_ptr2id(ra, tobs[i]);
		if(tids[i] == 0) {
			assert(zero_ix == -1);
			zero_ix = i;
		}
	}
	diag("initial allocs ok");

	/* go back to pointer from id. */
	bool all_ok = true;
	for(size_t i=0; i < test_size; i++) {
		if(tids[i] == 0 && (int)i != zero_ix) {
			diag("skipping i=%u for being 0 (tobs[%u]=%p)", i, i, tobs[i]);
			continue;
		}

		struct tobj *o = ra_id2ptr(ra, tids[i]);
		if(o != tobs[i]) {
			diag("i=%u: o=%p, tobs[i]=%p, tids[i]=%#x",
				i, o, tobs[i], tids[i]);
			all_ok = false;
			break;
		}
	}
	ok1(all_ok);

	free(tobs);
	free(tids);
}
END_TEST


START_TEST(id2ptr_safe)
{
	plan_tests(4);
	const size_t test_size = 5005;		/* sAUcE!1! */

	assert(alignof(struct tobj) <= 64);
	struct rangealloc *ra = ra_create(20, sizeof(struct tobj), 64);
	void *burn0 = ra_alloc(ra, 0);
	ok1(burn0 != NULL);

	struct tobj **tobs = calloc(test_size, sizeof(struct tobj *));
	uint16_t *tids = calloc(test_size, sizeof(uint16_t));
	bool all_ok = true;
	for(size_t i=0; i < test_size; i++) {
		tobs[i] = ra_zalloc(ra, -1);
		assert(tobs[i] != NULL);

		tids[i] = ra_ptr2id(ra, tobs[i]);
		if(tids[i] == 0) {
			diag("id for ptr=%p is zero?");
			all_ok = false;
			break;
		}
	}
	ok(all_ok, "zero id didn't occur");		/* because it was burnt */

	/* check that all returned objects are valid. */
	all_ok = true;
	for(size_t i=0; i < test_size; i++) {
		if(tids[i] == 0) continue;
		void *ptr = ra_id2ptr_safe(ra, tids[i]);
		if(ptr != tobs[i]) {
			diag("ptr=%p, tobs[%d]=%p, tid=%u",
				ptr, (int)i, tobs[i], tids[i]);
			all_ok = false;
			break;
		}
	}
	ok(all_ok, "returned objects are valid");

	/* perturb tobs[] by freeing groups of 345 and 456 first; these should
	 * cause page drops. then re-test validity.
	 */
	int steps[] = { 345, 456 };
	for(int j=0; j < NUM_ELEMENTS(steps); j++) {
		for(size_t i = j * 1000 + steps[j]; i < j * 1000 + steps[j] * 2; i++) {
			if(tobs[i] == NULL) continue;
			ra_free(ra, tobs[i]);
			tobs[i] = NULL;
		}
	}
	all_ok = true;
	for(size_t i=0; i < test_size; i++) {
		if(tids[i] == 0) continue;
		void *ptr = ra_id2ptr_safe(ra, tids[i]);
		if(ptr == NULL && tobs[i] != NULL) {
			diag("ptr=%p, tobs[%d]=%p, tid=%u",
				ptr, (int)i, tobs[i], tids[i]);
			all_ok = false;
			break;
		}
	}
	ok(all_ok, "post-free IDs convert properly");

	free(tobs);
	free(tids);
}
END_TEST


void ktest_rangealloc(void)
{
	RUN(masks);
	RUN(alloc);
	RUN(id_casting);
	RUN(id2ptr_safe);
}

#endif
