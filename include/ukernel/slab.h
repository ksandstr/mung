
#ifndef SEEN_UKERNEL_SLAB_H
#define SEEN_UKERNEL_SLAB_H

#include <stdlib.h>
#include <stdint.h>
#include <ccan/alignof/alignof.h>


/* from slab.c, in imitation of Linux */

struct kmem_cache;
typedef void (*kmem_cache_ctor)(void *, struct kmem_cache *, unsigned long);


/* utility because kmem_cache_create() is horrible */
/* (TODO: rename to KMEM_SLAB_NEW(), reuse KMEM_CACHE_NEW() for one that has
 * ctor and dtor also)
 */
#define KMEM_CACHE_NEW(name, type) \
	kmem_cache_create((name), sizeof(type), ALIGNOF(type), \
		0, NULL, NULL)


#define KMEM_NO_RECYCLE_CTOR 1	/* don't call ctor on recycled objects */
#define KMEM_POLICY 2			/* disallow alloc/free until _set_policy() */


extern struct kmem_cache *kmem_cache_create(
	const char *name,
	size_t size,
	size_t align,
	unsigned long flags,		/* mask of KMEM_* */
	kmem_cache_ctor ctor,
	kmem_cache_ctor dtor);

extern void kmem_cache_destroy(struct kmem_cache *cache);

extern void *kmem_cache_alloc(struct kmem_cache *cache)
	__attribute__((malloc));
/* NOTE: when called on a cache that has an object constructor, _zalloc
 * returns NULL.
 */
extern void *kmem_cache_zalloc(struct kmem_cache *cache);
extern void kmem_cache_free(struct kmem_cache *cache, void *ptr);
extern int kmem_cache_shrink(struct kmem_cache *cache);

/* accessors */
extern size_t kmem_cache_size(struct kmem_cache *cache);
extern const char *kmem_cache_name(struct kmem_cache *cache);

/* does Grodey Memory Hax to find the slab that swallowed the rat. or not. */
extern struct kmem_cache *kmem_cache_find(void *allocation);

/* external API that must be implemented by users of slab.c, unless
 * KMEM_POLICY is used for all slabs without depending on them.
 */
extern void *kmem_alloc_new_page(void);
extern void kmem_free_page(void *ptr);


/* the policy mechanism allows for flexible strategies in the allocation of
 * address space and/or physical memory for kmem_cache slabs. (consumers must
 * link lib/slab_policy.c in addition to lib/slab.c, and provide the
 * kernel-like get_kern_page() etc. interface.)
 *
 * currently the only implemented policy puts allocated slabs in a particular
 * range, so that objects aligned to 2**n can be identified by bits in their
 * address, making id<->object conversions a matter of a few bit operations.
 * for this reason, @elem_size must be a power of 2.
 */
extern void *kmem_policy_align(size_t seg_length, size_t elem_size);

/* policy setter. valid only for a single call per kmem_cache, which must
 * happen between creation and first alloc. the policy object will be
 * destroyed at kmem_cache_destroy().
 */
extern void kmem_cache_set_policy(struct kmem_cache *cache, void *polptr);

/* kmem_get_align_masks() returns the common bits in *or_p, and their mask's
 * complement in *and_p. these can be used to convert to a known-valid object
 * in inline code (i.e. ((uintptr_t)PTR & *and_p) >> (ffsl(*and_p) - 1) for
 * ptr2id, and back with (void *)((ID << (ffsl(*and_p) - 1)) | *or_p)) faster
 * than with a macro that doesn't stash ffsl(*and_p) somewhere.
 */
extern void kmem_get_align_masks(
	void *polptr, uintptr_t *and_p, uintptr_t *or_p);

/* utility macros. and_mask and or_mask come from kmem_get_align_masks(). it's
 * notable that due to the storage of slab-allocation metadata at the start of
 * page, no object allocated with this policy will have id=0.
 *
 * NOTE: these cause too much ffsl() evaluation. cache the value somewhere
 * instead and use a manual expansion of these macros.
 */
#define kmem_ptr2id(ptr, and_mask, or_mask) \
	(((uintptr_t)(ptr) & (and_mask)) >> (ffsl((and_mask)) - 1))
#define kmem_id2ptr(id, and_mask, or_mask) \
	((void *)(((uintptr_t)(id) << (ffsl((and_mask)) - 1)) | (or_mask)))

/* safe conversion from ID to pointer. returns NULL when the page isn't
 * mapped. caller must validate non-NULL result to determine if it's a live
 * object or not.
 */
extern void *kmem_id2ptr_safe(void *polptr, uintptr_t id);


/* interface between slab.c and slab_policy.c . */
struct slab_policy;
struct slab_policy_fns {
	void *(*alloc_page)(struct slab_policy_fns *pol);
	void (*free_page)(struct slab_policy_fns *pol, void *page);
	void (*destroy)(struct slab_policy_fns *pol);
};


#endif
