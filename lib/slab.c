/* per-page fixed-length allocator for somewhat efficient management of
 * non-heap (early) memory, kernel objects, and whatnot.
 */

#include <stdlib.h>
#include <stdint.h>
#include <assert.h>
#include <stdbool.h>

#include <ccan/list/list.h>
#include <ccan/container_of/container_of.h>
#include <ccan/compiler/compiler.h>
#include <ccan/likely/likely.h>

#include <ukernel/util.h>
#include <ukernel/mm.h>
#include <ukernel/slab.h>


#define CACHE_SIZE 16

#define KMEM_CACHE_MAGIC 0x61e98d0e
#define SLAB_MAGIC 0x99274b34

/* <struct slab>.flags */
#define SLAB_WAS_FULL (1 << 0)	/* no more freelist bumps */


struct kmem_cache
{
	uint16_t size, align;		/* object size, alignment */
	unsigned int flags;			/* KMEM_* */
	kmem_cache_ctor ctor, dtor;

	struct list_head free_list, partial_list, full_list;
	uint32_t magic;				/* for kmem_cache_find() */

	struct list_node link;		/* in cache_list */
	const char *name;

	/* cached objects are used in a LIFO order, so n_cached is enough to track
	 * it. kmem_cache_shrink() flushes the cache immediately.
	 */
	int n_cached;
	void *cache[CACHE_SIZE];
};


/* this is located at the start of each slab page. */
struct slab
{
	struct list_node link;	/* in free_list or partial_list */
	void *freelist;			/* freelist (may be chained) */
	uint32_t magic;
	unsigned short in_use;
	uint16_t flags;
};


static struct list_head cache_list = LIST_HEAD_INIT(cache_list);


/* initialize the slab that allocates all kmem_caches. */
static COLD void init_kmem_cache_slab(void)
{
	static struct kmem_cache meta_cache = {
		.size = sizeof(struct kmem_cache),
		.align = alignof(struct kmem_cache),
		.name = "kmem_cache",
		.magic = KMEM_CACHE_MAGIC,
	};
	list_head_init(&meta_cache.free_list);
	list_head_init(&meta_cache.partial_list);
	list_head_init(&meta_cache.full_list);
	list_add(&cache_list, &meta_cache.link);
}


struct kmem_cache *kmem_cache_create(
	const char *name,
	size_t size,
	size_t align,
	unsigned long flags,
	kmem_cache_ctor ctor,
	kmem_cache_ctor dtor)
{
	assert(!CHECK_FLAG(flags, KMEM_NO_RECYCLE_CTOR) || ctor != NULL);

	/* TODO: do this in a concurrency-safe manner with an once() function */
	static bool first = true;
	if(unlikely(first)) {
		first = false;
		init_kmem_cache_slab();
	}

	struct kmem_cache *meta = container_of(cache_list.n.prev,
		struct kmem_cache, link);
	assert(meta->size == sizeof(struct kmem_cache));

	struct kmem_cache *c = kmem_cache_alloc(meta);
	c->size = size;
	c->align = align;
	c->flags = flags;
	c->name = name;
	c->magic = KMEM_CACHE_MAGIC;
	c->ctor = ctor;
	c->dtor = dtor;
	c->n_cached = 0;
	list_head_init(&c->free_list);
	list_head_init(&c->partial_list);
	list_head_init(&c->full_list);
	list_add(&cache_list, &c->link);

	return c;
}


void kmem_cache_destroy(struct kmem_cache *cache)
{
	/* TODO: destroy the actual cache and release all memory. for now,
	 * half-assing.
	 */

	kmem_cache_shrink(cache);
}


static void *get_new_slab(struct kmem_cache *cache) {
	return kmem_alloc_new_page();
}


static void free_slab(struct kmem_cache *cache, void *slab) {
	kmem_free_page(slab);
}


void *kmem_cache_alloc(struct kmem_cache *cache)
{
	if(cache->n_cached > 0) {
		void *ret = cache->cache[--cache->n_cached];
		if(cache->ctor != NULL
			&& !CHECK_FLAG(cache->flags, KMEM_NO_RECYCLE_CTOR))
		{
			(*cache->ctor)(ret, cache, 0);
		}
		return ret;
	}

	struct slab *slab = list_top(&cache->partial_list, struct slab, link);
	if(slab == NULL) {
		slab = list_pop(&cache->free_list, struct slab, link);
		if(slab == NULL) {
			void *kpage = get_new_slab(cache);
			if(unlikely(kpage == NULL)) return NULL;
			slab = kpage;
			slab->flags = 0;
		}

		uintptr_t f_base = (uintptr_t)&slab[1];
		slab->freelist = (void *)((f_base + cache->align - 1)
			& ~(cache->align - 1));
		assert(((uintptr_t)slab->freelist & (cache->align - 1)) == 0);

		*(void **)slab->freelist = NULL;
		slab->in_use = 0;
		slab->magic = SLAB_MAGIC;
		list_add(&cache->partial_list, &slab->link);
		assert(!CHECK_FLAG(slab->flags, SLAB_WAS_FULL));
	}

	assert(slab->freelist != NULL);
	void *ret = slab->freelist, *next = *(void **)ret;
	slab->in_use++;
	intptr_t bump = (cache->size + cache->align - 1) & ~(cache->align - 1);
	if(next != NULL) {
		slab->freelist = next;		/* pop. */
	} else if(CHECK_FLAG(slab->flags, SLAB_WAS_FULL)
		|| slab->freelist + bump * 2 >= (void *)slab + PAGE_SIZE)
	{
		/* freelist and unallocated space were both exhausted. */
		list_del_from(&cache->partial_list, &slab->link);
		list_add(&cache->full_list, &slab->link);
		slab->freelist = NULL;
		slab->flags |= SLAB_WAS_FULL;
	} else {
		/* advance. */
		slab->freelist += bump;
		*(void **)slab->freelist = NULL;
	}

	assert((uintptr_t)(ret + cache->size) <= ((uintptr_t)slab | 0xfff));
	if(cache->ctor != NULL) {
		(*cache->ctor)(ret, cache, 0);
	}
	return ret;
}


void *kmem_cache_zalloc(struct kmem_cache *cache)
{
	if(unlikely(cache->ctor != NULL)) return NULL;

	void *ptr = kmem_cache_alloc(cache);
	if(likely(ptr != NULL)) memset(ptr, '\0', cache->size);
	return ptr;
}


static void free_object(struct kmem_cache *cache, void *ptr)
{
	struct slab *slab = (struct slab *)((intptr_t)ptr & ~PAGE_MASK);
	assert(slab->magic == SLAB_MAGIC);
	assert(slab->in_use > 0);
	if(slab->freelist == NULL) {
		/* reinstate into the partial slab list */
		list_del_from(&cache->full_list, &slab->link);
		list_add(&cache->partial_list, &slab->link);
		assert(CHECK_FLAG(slab->flags, SLAB_WAS_FULL));
	}

#ifndef NDEBUG
	/* assert against free twice. */
	void **next = &slab->freelist;
	while(*next != NULL) {
		assert(ptr != *next);
		next = *next;
	}
	assert(ptr < *next || *next == NULL);
#endif

	if(cache->dtor != NULL) {
		(*cache->dtor)(ptr, cache, 0);
	}

	*(void **)ptr = slab->freelist;
	slab->freelist = ptr;
	slab->in_use--;

	if(slab->in_use == 0) {
		list_del_from(&cache->partial_list, &slab->link);
		list_add(&cache->free_list, &slab->link);
		slab->magic = 0xdeadbeef;
		slab->flags &= ~SLAB_WAS_FULL;	/* became empty again. */
	}
}


void kmem_cache_free(struct kmem_cache *cache, void *ptr)
{
	if(ptr == NULL) return;

	assert(kmem_cache_find(ptr) == cache);

	assert(cache->n_cached < CACHE_SIZE);
	cache->cache[cache->n_cached++] = ptr;
	if(cache->n_cached == CACHE_SIZE) {
		/* destroy objects as a group. */
		while(cache->n_cached > 2) {
			free_object(cache, cache->cache[--cache->n_cached]);
		}
	}
}


size_t kmem_cache_size(struct kmem_cache *cache) {
	return cache->size;
}


const char *kmem_cache_name(struct kmem_cache *cache) {
	return cache->name;
}


int kmem_cache_shrink(struct kmem_cache *cache)
{
	while(cache->n_cached > 0) {
		free_object(cache, cache->cache[--cache->n_cached]);
	}

	struct slab *next, *slab;
	int n_freed = 0;
	list_for_each_safe(&cache->free_list, slab, next, link) {
		assert(slab->in_use == 0);
		list_del_from(&cache->free_list, &slab->link);
		slab->magic = 0xfaceb007u;
		free_slab(cache, slab);
		n_freed++;
	}
	assert(list_empty(&cache->free_list));
	return n_freed;
}


struct kmem_cache *kmem_cache_find(void *allocation)
{
	if(allocation == NULL) return NULL;

	struct slab *slab = (void *)((uintptr_t)allocation & ~PAGE_MASK);
	if(slab->magic != SLAB_MAGIC) return NULL;
	for(struct list_node *n = slab->link.next;
		n != &slab->link;
		n = n->next)
	{
		struct kmem_cache *cache;
		if(slab->freelist == NULL) {
			cache = container_of((struct list_head *)n, struct kmem_cache,
				full_list);
		} else if(slab->in_use == 0) {
			cache = container_of((struct list_head *)n, struct kmem_cache,
				free_list);
		} else {
			cache = container_of((struct list_head *)n, struct kmem_cache,
				partial_list);
		}
		if(cache->magic == KMEM_CACHE_MAGIC) return cache;
	}

	return NULL;
}
