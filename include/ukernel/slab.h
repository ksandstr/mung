
#ifndef SEEN_UKERNEL_SLAB_H
#define SEEN_UKERNEL_SLAB_H

#include <stdlib.h>
#include <stdint.h>


/* from slab.c, in imitation of Linux */

struct kmem_cache;
typedef void (*kmem_cache_ctor)(void *, struct kmem_cache *, unsigned long);

extern struct kmem_cache *kmem_cache_create(
	const char *name,
	size_t size,
	size_t align,
	unsigned long flags,
	kmem_cache_ctor ctor,
	kmem_cache_ctor dtor);

extern void kmem_cache_destroy(struct kmem_cache *cache);

extern void *kmem_cache_alloc(struct kmem_cache *cache);
extern void *kmem_cache_zalloc(struct kmem_cache *cache);
extern void kmem_cache_free(struct kmem_cache *cache, void *ptr);

/* accessors */
extern size_t kmem_cache_size(struct kmem_cache *cache);
extern const char *kmem_cache_name(struct kmem_cache *cache);
extern int kmem_cache_shrink(struct kmem_cache *cache);


#endif
