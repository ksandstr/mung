
/* per-page object allocator for management of non-heap (early) kernel memory. */

#include <stdlib.h>
#include <stdint.h>
#include <assert.h>
#include <stdbool.h>
#include <ccan/list/list.h>
#include <ccan/container_of/container_of.h>
#include <ccan/compiler/compiler.h>
#include <ccan/alignof/alignof.h>
#include <ccan/likely/likely.h>

#include <ukernel/slab.h>
#include <ukernel/mm.h>


#define SLAB_FIRST(cache, slab) \
	((void *)((((intptr_t)&(slab)[1]) + (cache)->align - 1) & ~((cache)->align - 1)))

#define KMEM_CACHE_MAGIC 0x61e98d0e
#define SLAB_MAGIC 0x99274b34


struct kmem_cache
{
	size_t size, align;
	unsigned long flags;

	struct list_head free_list, partial_list, full_list;
	uint32_t magic;				/* for kmem_cache_find() */

	struct list_node link;		/* in cache_list */
	const char *name;
};


/* this is located at the start of each slab page. */
struct slab
{
	struct list_node link;	/* in free_list or partial_list */
	void *freelist;			/* freelist (may be chained) */
	uint32_t magic;
	unsigned short in_use;
};


static struct list_head cache_list = LIST_HEAD_INIT(cache_list);


/* initialize the slab that allocates all kmem_caches. */
static COLD void init_kmem_cache_slab(void)
{
	static struct kmem_cache meta_cache = {
		.size = sizeof(struct kmem_cache),
		.align = ALIGNOF(struct kmem_cache),
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
	list_head_init(&c->free_list);
	list_head_init(&c->partial_list);
	list_head_init(&c->full_list);
	list_add(&cache_list, &c->link);

	return c;
}


void kmem_cache_destroy(struct kmem_cache *cache)
{
	/* TODO */
}


void *kmem_cache_alloc(struct kmem_cache *cache)
{
	struct slab *slab;
	if(list_empty(&cache->partial_list)) {
		if(!list_empty(&cache->free_list)) {
			slab = container_of(cache->free_list.n.next, struct slab, link);
			list_del(cache->free_list.n.next);
		} else {
			void *kpage = kmem_alloc_new_page();
			if(unlikely(kpage == NULL)) return NULL;
			slab = kpage;
		}

		slab->freelist = SLAB_FIRST(cache, slab);
		*(void **)slab->freelist = NULL;
		slab->in_use = 0;
		slab->magic = SLAB_MAGIC;
		list_add(&cache->partial_list, &slab->link);
	} else {
		slab = container_of(cache->partial_list.n.next, struct slab, link);
	}

	void *ret = slab->freelist, *next = *(void **)ret;
	slab->in_use++;
	intptr_t bump = (cache->size + cache->align - 1) & ~(cache->align - 1);
	if(next != NULL) slab->freelist = next;
	else {
		slab->freelist += bump;
		*(void **)slab->freelist = NULL;
	}
	if(slab->freelist + bump >= (void *)slab + PAGE_SIZE) {
		/* it became full. */
		list_del_from(&cache->partial_list, &slab->link);
		list_add(&cache->full_list, &slab->link);
		slab->freelist = NULL;
	}

	return ret;
}


void *kmem_cache_zalloc(struct kmem_cache *cache)
{
	void *ptr = kmem_cache_alloc(cache);
	if(likely(ptr != NULL)) memset(ptr, '\0', cache->size);
	return ptr;
}


void kmem_cache_free(struct kmem_cache *cache, void *ptr)
{
	struct slab *slab = (struct slab *)((intptr_t)ptr & ~PAGE_MASK);
	assert(slab->in_use > 0);
	if(slab->freelist == NULL) {
		/* reinstate into the partial slab list */
		list_del_from(&cache->full_list, &slab->link);
		list_add(&cache->partial_list, &slab->link);
	}

	*(void **)ptr = slab->freelist;
	slab->freelist = ptr;
	slab->in_use--;

	if(slab->in_use == 0) {
		list_del_from(&cache->partial_list, &slab->link);
		list_add(&cache->free_list, &slab->link);
		slab->magic = 0xdeadbeef;
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
	struct slab *next, *slab;
	int n_freed = 0;
	list_for_each_safe(&cache->free_list, slab, next, link) {
		assert(slab->in_use == 0);
		list_del_from(&cache->free_list, &slab->link);
		slab->magic = 0xfaceb007u;
		kmem_free_page(slab);
		n_freed++;
	}
	assert(list_empty(&cache->free_list));
	return n_freed;
}


struct kmem_cache *kmem_cache_find(void *allocation)
{
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
