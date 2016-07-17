
#ifndef SEEN_UKERNEL_SLAB_H
#define SEEN_UKERNEL_SLAB_H

#include <stdlib.h>
#include <stdint.h>
#include <stdalign.h>


/* from slab.c, in imitation of Linux */

struct kmem_cache;
typedef void (*kmem_cache_ctor)(void *, struct kmem_cache *, unsigned long);


/* utility because kmem_cache_create() is horrible */
#define KMEM_CACHE_NEW(name, type) \
	kmem_cache_create((name), sizeof(type), alignof(type), \
		0, NULL, NULL)


#define KMEM_NO_RECYCLE_CTOR 1	/* don't call ctor on recycled objects */


extern struct kmem_cache *kmem_cache_create(
	const char *name,
	size_t size, size_t align,
	unsigned long flags,		/* mask of KMEM_* */
	kmem_cache_ctor ctor, kmem_cache_ctor dtor);

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

/* external API that must be implemented by users of slab.c. */
extern void *kmem_alloc_new_page(void);
extern void kmem_free_page(void *ptr);

#endif
