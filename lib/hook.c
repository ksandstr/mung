
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <assert.h>
#include <ccan/list/list.h>
#include <ccan/container_of/container_of.h>

#include <ukernel/slab.h>
#include <ukernel/hook.h>


/* special value for hook->current, post-detach */
#define DETACHED ((struct hook_fn *)0xdababb1e)


struct hook_fn
{
	struct list_node link;		/* in struct hook @fn_list */
	hook_call_t fn;
	void *priv;
};


static struct kmem_cache *hook_fn_slab = NULL;


static struct hook_fn *h_fn(hook_call_t fn, void *priv)
{
	static bool first = true;
	if(first) {
		hook_fn_slab = KMEM_CACHE_NEW("hook_fn_slab", struct hook_fn);
		if(hook_fn_slab == NULL) return NULL;
		first = false;
	}

	struct hook_fn *ent = kmem_cache_alloc(hook_fn_slab);
	if(ent != NULL) *ent = (struct hook_fn){ .fn = fn, .priv = priv };
	return ent;
}


struct hook_fn *_hook_push_front(struct hook *h, hook_call_t fn, const void *priv) {
	struct hook_fn *ret = h_fn(fn, (void *)priv);
	if(ret != NULL) list_add(&h->fn_list, &ret->link);
	return ret;
}


struct hook_fn *_hook_push_back(struct hook *h, hook_call_t fn, const void *priv) {
	struct hook_fn *ret = h_fn(fn, (void *)priv);
	if(ret != NULL) list_add_tail(&h->fn_list, &ret->link);
	return ret;
}


int hook_call_front(struct hook *h, void *param, uintptr_t code)
{
	int num_called = 0;
	struct hook_fn *next;
	list_for_each_safe(&h->fn_list, h->current, next, link) {
		(*h->current->fn)(h, param, code, h->current->priv);
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
	int num_called = 0;
	struct hook_fn *next;
	list_for_each_rev_safe(&h->fn_list, h->current, next, link) {
		(*h->current->fn)(h, param, code, h->current->priv);
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


void hook_remove(struct hook *h, struct hook_fn *entry)
{
	assert(entry != NULL && entry != DETACHED);

	if(entry == h->current) h->current = DETACHED;
	list_del_from(&h->fn_list, &entry->link);
	kmem_cache_free(hook_fn_slab, entry);
}
