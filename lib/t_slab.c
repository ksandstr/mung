
/* in-kernel tests for lib/slab.c */

#include <stdio.h>
#include <string.h>
#include <ccan/list/list.h>
#include <ccan/alignof/alignof.h>

#include <ukernel/slab.h>
#include <ukernel/ktest.h>

#if KTEST


struct t_object {
	struct list_node link;
	char name[32];
	int value;
};


START_TEST(many_alloc)
{
	plan_tests(2);
	const unsigned int test_len = 10000;
	diag("test_len=%u, sizeof=%u", test_len, sizeof(struct t_object));

	struct kmem_cache *cache = KMEM_CACHE_NEW("test slab", struct t_object);
	struct list_head list;
	list_head_init(&list);
	bool saw_nulls = false;
	for(int i=0; i < test_len; i++) {
		struct t_object *o;
		if((i & 1) == 0) o = kmem_cache_alloc(cache);
		else o = kmem_cache_zalloc(cache);
		if(o == NULL) {
			if(!saw_nulls) diag("first NULL was on i=%d", i);
			saw_nulls = true;
			continue;
		}
		o->value = i;
		snprintf(o->name, sizeof(o->name), "hello%04u", (unsigned)i);
		list_add_tail(&list, &o->link);
	}
	ok1(!saw_nulls);

	bool in_order = true;
	for(int i=0; i < test_len; i++) {
		struct t_object *o = list_pop(&list, struct t_object, link);
		if(o == NULL) break;
		if(o->value != i) {
			if(in_order) {
				diag("first out-of-order was on i=%d (value=%d)",
					i, o->value);
			}
			in_order = false;
		}

		kmem_cache_free(cache, o);
	}
	ok1(in_order);

	kmem_cache_shrink(cache);
	kmem_cache_destroy(cache);
}
END_TEST


static int ctor_count = 0, dtor_count = 0;


static void ct_ctor(void *p, struct kmem_cache *s, unsigned long foo) {
	ctor_count++;
}


static void ct_dtor(void *p, struct kmem_cache *s, unsigned long foo) {
	dtor_count++;
}


START_TEST(ctor_and_dtor)
{
	plan_tests(4);

	/* part #0: no ctor or dtor at all. */
	ctor_count = 0; dtor_count = 0;
	struct kmem_cache *slab = KMEM_CACHE_NEW("no ctor or dtor",
		struct t_object);
	struct t_object *o = kmem_cache_alloc(slab);
	ok(o != NULL, "alloc ok (base case)");
	kmem_cache_free(slab, o);
	ok(ctor_count == 0 && dtor_count == 0, "counts (base case)");
	kmem_cache_destroy(slab);

	/* part #1: ctor and dtor present */
	ctor_count = 0; dtor_count = 0;
	slab = kmem_cache_create("ctor and dtor present",
		sizeof(struct t_object), ALIGNOF(struct t_object),
		0, &ct_ctor, &ct_dtor);
	struct t_object *ary[4];
	for(int i=0; i < 4; i++) ary[i] = kmem_cache_alloc(slab);
	if(!ok(ctor_count >= 4, "ctor was called")) {
		diag("ctor_count=%d", ctor_count);
	}
	for(int i=0; i < 4; i++) kmem_cache_free(slab, ary[i]);
	kmem_cache_destroy(slab);
	if(!ok(dtor_count > 0, "dtor was called")) {
		diag("dtor_count=%d", dtor_count);
	}

	/* TODO: test SLAB_NO_RECYCLE_CTOR */
}
END_TEST


void ktest_slab(void)
{
	/* TODO: add a basic allocation test, checking if zalloc returns a zeroed
	 * segment, kmem_cache_size() and _name() return values, and so forth
	 */
	RUN(many_alloc);
	RUN(ctor_and_dtor);
	/* TODO: add test for kmem_cache_find() */
}

#endif
