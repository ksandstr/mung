
/* aligned range allocator for fixed-size items (map_group, thread). provides
 * a very fast bijective mapping between identifiers and objects.
 */

#ifndef SEEN_UKERNEL_RANGEALLOC_H
#define SEEN_UKERNEL_RANGEALLOC_H

#include <stdalign.h>
#include <ccan/htable/htable.h>

#include <l4/types.h>

#include <ukernel/slab.h>

#ifdef __KERNEL__
/* non-kernel stuff should provide size_to_shift(). */
#include <ukernel/util.h>
#endif


struct ra_page;		/* private to rangealloc.c */

struct rangealloc
{
	L4_Fpage_t range;
	uint8_t ob_size_log2, id_shift;
	uint16_t flags;
	uintptr_t and_mask, or_mask;
	struct htable page_hash;
	struct ra_page *partial_head, *full_head;

	struct kmem_cache *meta_slab;
};


#define RA_NEW(type, count) ra_create( \
	size_to_shift((count) << size_to_shift(sizeof(type))), \
	sizeof(type), alignof(type))


extern struct rangealloc *ra_create(
	int range_log2,
	unsigned short obj_size, unsigned short obj_align);

/* NOTE: there's no ra_destroy(). */

/* only valid before anything has been allocated on @ra. causes allocs on @ra
 * to never have ID=0.
 */
extern void ra_disable_id_0(struct rangealloc *ra);

extern void ra_free(struct rangealloc *ra, void *ptr);
extern void *ra_alloc(struct rangealloc *ra, long id);
extern void *ra_zalloc(struct rangealloc *ra, long id);

/* unsafe conversions. use catch_pf() to manage. */
#define ra_ptr2id(ra, ptr) \
	(((uintptr_t)(ptr) & (ra)->and_mask) >> (ra)->id_shift)
#define ra_id2ptr(ra, id) \
	((void *)(((uintptr_t)(id) << (ra)->id_shift) | (ra)->or_mask))

extern void *ra_id2ptr_safe(struct rangealloc *ra, long id);

/* is @ptr within @ra? this works for pointers into objects as well. */
#define ra_has_ptr(ra, ptr) (((uintptr_t)(ptr) & ~(ra)->and_mask) == (ra)->or_mask)


/* iteration over all allocated objects. robust against ra_free() from inside
 * the loop iff htable_del() is robust for htable_iter. (so yes, for now.)
 *
 * note that this is guaranteed to cover all objects allocated, but not in any
 * particular order due to the use of an internal hash table. pointers
 * returned are guaranteed valid per ra_id2ptr_safe(ra_ptr2id(ret)).
 */
struct ra_iter {
	struct htable_iter hti;
	struct ra_page *p;
	int pos;
};

extern void *ra_first(const struct rangealloc *ra, struct ra_iter *it);
extern void *ra_next(const struct rangealloc *ra, struct ra_iter *it);

#endif
