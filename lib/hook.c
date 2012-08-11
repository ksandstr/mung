
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <ccan/list/list.h>
#include <ccan/container_of/container_of.h>
#include <ccan/alignof/alignof.h>
#include <ccan/likely/likely.h>

#include <ukernel/slab.h>
#include <ukernel/hook.h>


struct hook_fn
{
	struct list_node link;		/* in struct hook @fn_list */
	hook_call_t fn;
	void *dataptr;				/* per entry */
};


static struct kmem_cache *hook_fn_slab = NULL;


static struct hook_fn *h_fn(hook_call_t fn, void *ptr)
{
	struct hook_fn *ent = kmem_cache_alloc(hook_fn_slab);
	ent->fn = fn;
	ent->dataptr = ptr;
	return ent;
}


void hook_init(struct hook *h, void *dataptr)
{
	static bool first = true;
	if(unlikely(first)) {
		hook_fn_slab = kmem_cache_create("hook_fn_slab",
			sizeof(struct hook_fn), ALIGNOF(struct hook_fn),
			0, NULL, NULL);
	}

	list_head_init(&h->fn_list);
	h->dataptr = dataptr;
	h->current = NULL;
}


void hook_push_front(struct hook *h, hook_call_t fn, void *dataptr)
{
	struct hook_fn *ent = h_fn(fn, dataptr);
	list_add(&h->fn_list, &ent->link);
}


void hook_push_back(struct hook *h, hook_call_t fn, void *dataptr)
{
	struct hook_fn *ent = h_fn(fn, dataptr);
	list_add_tail(&h->fn_list, &ent->link);
}


static void invoke_hook_fn(
	struct hook *h,
	struct hook_fn *member,
	uintptr_t code,
	bool remove)
{
	h->current = member;
	(*member->fn)(h, code, member->dataptr);
	if(remove && h->current == member) {
		list_del_from(&h->fn_list, &member->link);
		kmem_cache_free(hook_fn_slab, member);
	}
}


int hook_call_front(
	struct hook *h,
	int num,
	bool remove,
	uintptr_t code)
{
	int num_called = 0;
	struct hook_fn *member, *next;
	list_for_each_safe(&h->fn_list, member, next, link) {
		invoke_hook_fn(h, member, code, remove);
		if(++num_called == num && num > 0) break;
	}

	return num_called;
}


int hook_call_back(
	struct hook *h,
	int num,
	bool remove,
	uintptr_t code)
{
	/* my own list_for_each_safe_rev() */
	int num_called = 0;
	for(struct list_node *cur = h->fn_list.n.prev, *next = cur->prev;
		cur->prev != &h->fn_list.n;
		cur = next, next = cur->prev)
	{
		struct hook_fn *member = container_of(cur, struct hook_fn, link);
		invoke_hook_fn(h, member, code, remove);
		if(++num_called == num && num > 0) break;
	}

	return num_called;
}


void hook_detach(struct hook *h)
{
	assert(h->current != NULL);

	list_del_from(&h->fn_list, &h->current->link);
	kmem_cache_free(hook_fn_slab, h->current);
	h->current = NULL;
}
