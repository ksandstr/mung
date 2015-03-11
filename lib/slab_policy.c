
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <assert.h>

#include <ccan/list/list.h>
#include <ccan/likely/likely.h>

#include <ukernel/mm.h>
#include <ukernel/util.h>
#include <ukernel/slab.h>


#define BM_BITS (sizeof(unsigned long) * 8)
#define BM_WORDS(nbits) ((nbits) / (PAGE_SIZE * BM_BITS))
#define BM_TEST(map, pos) CHECK_FLAG((map)[(pos) / BM_BITS], 1 << ((pos) % BM_BITS))

#define MAX_FREE 16		/* policy maximum free-page count */
#define MAX_SCAN 6		/* hashed scan depth */

/* slab_policy->flags */
#define P_DIRTY 1		/* has something in free_ptrs[] */


/* this could be a union per policy type. thusfar there's just one policy, so
 * this is its state.
 */
struct slab_policy
{
	struct slab_policy_fns vtab;

	/* the policy says: physical pages get placed in a particular virtual
	 * address range. this has a start address and a size; in addition this
	 * tries to allocate pages at as low an address as possible to facilitate
	 * future use of the unused slots for e.g. UTCB maps.
	 *
	 * available slots are tracked in a bitmap, one per page.
	 */
	uintptr_t start;
	size_t size;
	uint16_t low_free, high_used, align_log2, flags;
	void *free_ptrs[MAX_FREE];	/* batch-free cache. */
	struct list_head phys_pages;
	unsigned long avail[];		/* 1 for available, 0 for not. lsb first. */
};


static void policy_flush(struct slab_policy *pol, void *page)
{
	/* the longer road. luckily free_ptrs[] is already in a hash order. */
	struct page *p, *next;
	list_for_each_safe(&pol->phys_pages, p, next, link) {
		assert(p->vm_addr != NULL);
		int ix = int_hash((uintptr_t)p->vm_addr) % MAX_FREE;
		for(int c=0; c < MAX_SCAN; c++) {
			if(pol->free_ptrs[ix] == p->vm_addr) break;
			if(++ix == MAX_FREE) ix = 0;
		}
		if(pol->free_ptrs[ix] == p->vm_addr || p->vm_addr == page) {
			if(pol->free_ptrs[ix] == p->vm_addr) pol->free_ptrs[ix] = NULL;
			else page = NULL;

			int s = ((uintptr_t)p->vm_addr - pol->start) / PAGE_SIZE;
			assert(!BM_TEST(pol->avail, s));
			pol->avail[s / BM_BITS] |= 1 << (s % BM_BITS);
			assert(BM_TEST(pol->avail, s));
			pol->low_free = MIN(int, pol->low_free, s);
			if(pol->high_used == s) {
				/* scan backward to find the lowest zero bit. */
				int c = s;
				while(c >= 0 && ~pol->avail[c / BM_BITS] == 0) {
					c -= BM_BITS;
				}
				if(c >= 0) {
					pol->high_used = MSB(~pol->avail[c / BM_BITS])
						+ (c & ~(BM_BITS - 1));
					assert(!BM_TEST(pol->avail, pol->high_used));
				} else {
					assert(pol->low_free == 0);
					pol->high_used = 0;
				}
			}
			list_del_from(&pol->phys_pages, &p->link);
			free_kern_page(p);
		}
	}

#ifndef NDEBUG
	assert(page == NULL);
	for(int i=0; i < MAX_FREE; i++) {
		assert(pol->free_ptrs[i] == NULL);
	}
#endif

	pol->flags &= ~P_DIRTY;
}


static void *policy_alloc_page(struct slab_policy_fns *polptr)
{
	struct slab_policy *pol = container_of(polptr, struct slab_policy, vtab);

	/* early recycle so that it doesn't OOM itself. */
	if(CHECK_FLAG(pol->flags, P_DIRTY)) policy_flush(pol, NULL);

	int s = pol->low_free;
	while(!BM_TEST(pol->avail, s) && s < pol->size / PAGE_SIZE) {
		s++;
	}
	if(unlikely(!BM_TEST(pol->avail, s))) return NULL;
	pol->low_free = s + 1;
	pol->high_used = MAX(int, pol->high_used, s);
	pol->avail[s / BM_BITS] &= ~(1 << (s % BM_BITS));

	struct page *p = get_kern_page(pol->start + s * PAGE_SIZE);
	if(unlikely(p == NULL)) return NULL;
	list_add(&pol->phys_pages, &p->link);

	return p->vm_addr;
}


static void policy_free_page(struct slab_policy_fns *polptr, void *page)
{
	struct slab_policy *pol = container_of(polptr, struct slab_policy, vtab);
	assert(page != NULL);

	int ix = int_hash((uintptr_t)page) % MAX_FREE;
	for(int c=0; c < MAX_SCAN; c++) {
		if(pol->free_ptrs[ix] == NULL) break;
		if(++ix == MAX_FREE) ix = 0;
	}
	if(pol->free_ptrs[ix] == NULL) {
		pol->free_ptrs[ix] = page;
		pol->flags |= P_DIRTY;
	} else {
		policy_flush(pol, page);
	}
}


static COLD void policy_destroy(struct slab_policy_fns *polptr)
{
	struct slab_policy *pol = container_of(polptr, struct slab_policy, vtab);

	struct page *p, *next;
	list_for_each_safe(&pol->phys_pages, p, next, link) {
		list_del_from(&pol->phys_pages, &p->link);
		free_kern_page(p);
	}

	free(pol);
}


/* public interface. */
void *kmem_policy_align(size_t seg_length, size_t elem_size)
{
	assert(elem_size == (1 << size_to_shift(elem_size)));

	/* (rounds up to power of 2) */
	seg_length = 1 << size_to_shift(seg_length);
	struct slab_policy *pol = malloc(sizeof(struct slab_policy)
		+ sizeof(unsigned long) * BM_WORDS(seg_length));
	if(unlikely(pol == NULL)) {
		return (void *)0xdeadbeef;	/* landmine option */
	}

	pol->start = reserve_heap_range(seg_length);
	pol->size = seg_length;
	pol->low_free = 0;
	pol->high_used = 0;		/* ... so high_used isn't always in use. */
	pol->align_log2 = size_to_shift(elem_size);
	pol->flags = 0;
	for(int i=0; i < BM_WORDS(pol->size); i++) pol->avail[i] = ~0ul;
	for(int i=0; i < MAX_FREE; i++) pol->free_ptrs[i] = NULL;
	list_head_init(&pol->phys_pages);
	pol->vtab = (struct slab_policy_fns){
		.alloc_page = &policy_alloc_page,
		.free_page = &policy_free_page,
		.destroy = &policy_destroy,
	};

	return pol;
}


inline void kmem_get_align_masks(
	void *polptr, uintptr_t *and_p, uintptr_t *or_p)
{
	struct slab_policy *pol = polptr;
	*and_p = (pol->size - 1) & (~0ul << pol->align_log2);
	*or_p = pol->start & ~*and_p;
}


void *kmem_id2ptr_safe(void *polptr, uintptr_t id)
{
	struct slab_policy *pol = polptr;
	int lim = (pol->high_used + 1) * (PAGE_SIZE >> pol->align_log2);
	if(id > lim) return NULL;

	uintptr_t and_mask, or_mask;
	kmem_get_align_masks(pol, &and_mask, &or_mask);
	void *ptr = (void *)((id << pol->align_log2) | or_mask);
	assert(ptr == kmem_id2ptr(id, and_mask, or_mask));
	/* avoid the ones near start-of-page, since that's where <struct slab>
	 * lives. the clause is unlikely because the slab allocator will never
	 * generate those IDs.
	 */
	if(unlikely(((uintptr_t)ptr & PAGE_MASK) < 32)) return NULL;
	int page_pos = ((uintptr_t)ptr - pol->start) / PAGE_SIZE;
	return BM_TEST(pol->avail, page_pos) ? NULL : ptr;
}
