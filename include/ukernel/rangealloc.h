
/* aligned range allocator for fixed-size items (map_group, thread).
 *
 * replaces the old "policy slab" mechanism.
 */

#ifndef SEEN_UKERNEL_RANGEALLOC_H
#define SEEN_UKERNEL_RANGEALLOC_H

#include <stdalign.h>
#include <ccan/htable/htable.h>

#include <l4/types.h>

#include <ukernel/slab.h>
#include <ukernel/util.h>


struct ra_page;		/* private to rangealloc.c */

struct rangealloc
{
	L4_Fpage_t range;
	short ob_size_log2, id_shift;
	uintptr_t and_mask, or_mask;
	struct htable page_hash;
	struct ra_page *partial_head, *full_head;

	struct kmem_cache *meta_slab;
};

struct ra_iter {
	struct htable_iter hti;
	struct ra_page *p;
	int pos;
};


#define RA_NEW(type, count) ra_create( \
	size_to_shift((count) << size_to_shift(sizeof(type))), \
	sizeof(type), alignof(type))


extern struct rangealloc *ra_create(
	int range_log2,
	unsigned short obj_size, unsigned short obj_align);

/* NOTE: there's no ra_destroy(). */

extern void ra_free(struct rangealloc *ra, void *ptr);
extern void *ra_alloc(struct rangealloc *ra, long id)
	__attribute__((malloc));
extern void *ra_zalloc(struct rangealloc *ra, long id)
	__attribute__((malloc));

/* unsafe conversions. use catch_pf() to manage. */
#define ra_ptr2id(ra, ptr) \
	(((uintptr_t)(ptr) & (ra)->and_mask) >> (ra)->id_shift)
#define ra_id2ptr(ra, id) \
	((void *)(((uintptr_t)(id) << (ra)->id_shift) | (ra)->or_mask))

extern void *ra_id2ptr_safe(struct rangealloc *ra, long id);

/* iteration over all allocated objects. robust against ra_free() from inside
 * the loop iff htable_del() is robust for htable_iter. (so yes, for now.)
 */
extern void *ra_first(const struct rangealloc *ra, struct ra_iter *it);
extern void *ra_next(const struct rangealloc *ra, struct ra_iter *it);

#endif
