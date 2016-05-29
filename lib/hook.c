
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <assert.h>
#include <ccan/list/list.h>
#include <ccan/container_of/container_of.h>
#include <ccan/likely/likely.h>

#include <ukernel/slab.h>
#include <ukernel/hook.h>


/* special value for hook->current, post-detach */
#define DETACHED ((struct hook_fn *)0xdababb1e)


struct hook_fn
{
	struct list_node link;		/* in struct hook @fn_list */
	hook_call_t fn;
	void *dataptr;				/* per entry */
};


static struct kmem_cache *hook_fn_slab = NULL;


static struct hook_fn *h_fn(hook_call_t fn, void *ptr)
{
	assert(hook_fn_slab != NULL);
	struct hook_fn *ent = kmem_cache_alloc(hook_fn_slab);
	ent->fn = fn;
	ent->dataptr = ptr;
	return ent;
}


void hook_init(struct hook *h, void *dataptr)
{
	static bool first = true;
	if(unlikely(first)) {
		first = false;
		hook_fn_slab = KMEM_CACHE_NEW("hook_fn_slab", struct hook_fn);
	}

	list_head_init(&h->fn_list);
	h->dataptr = dataptr;
	h->current = NULL;
}


void hook_push_front(struct hook *h, hook_call_t fn, void *dataptr) {
	list_add(&h->fn_list, &h_fn(fn, dataptr)->link);
}


void hook_push_back(struct hook *h, hook_call_t fn, void *dataptr) {
	list_add_tail(&h->fn_list, &h_fn(fn, dataptr)->link);
}


int hook_call_front(struct hook *h, void *param, uintptr_t code)
{
	int num_called = 0;
	struct hook_fn *member, *next;
	list_for_each_safe(&h->fn_list, member, next, link) {
		assert(member != NULL);
		h->current = member;
		(*member->fn)(h, param, code, member->dataptr);
		num_called++;

		if(h->current == NULL) {
			/* recursion most foul!
			 *
			 * while there's likely a fancier way to solve this, we'll just
			 * assert that the recursion emptied the hook and exit early.
			 */
			assert(hook_empty(h));
			return num_called;
		}
	}
	h->current = NULL;

	return num_called;
}


int hook_call_back(struct hook *h, void *param, uintptr_t code)
{
	/* a DIY list_for_each_safe_rev() */
	int num_called = 0;
	for(struct list_node *cur = h->fn_list.n.prev, *next = cur->prev;
		cur->prev != &h->fn_list.n;
		cur = next, next = cur->prev)
	{
		struct hook_fn *member = container_of(cur, struct hook_fn, link);
		assert(member != NULL);
		h->current = member;
		(*member->fn)(h, param, code, member->dataptr);
		num_called++;

		if(h->current == NULL) {
			/* recursion case. see comment in hook_call_front(). */
			assert(hook_empty(h));
			return num_called;
		}
	}
	h->current = NULL;

	return num_called;
}


void hook_detach(struct hook *h)
{
	assert(h->current != NULL);
	assert(h->current != DETACHED);

	list_del_from(&h->fn_list, &h->current->link);
	kmem_cache_free(hook_fn_slab, h->current);
	h->current = DETACHED;
}
