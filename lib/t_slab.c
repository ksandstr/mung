
/* in-kernel tests for lib/slab.c */

#include <stdio.h>
#include <string.h>
#include <ccan/list/list.h>
#include <ccan/alignof/alignof.h>
#include <ccan/str/str.h>

#include <ukernel/slab.h>
#include <ukernel/ktest.h>

#if KTEST


struct t_object {
	struct list_node link;
	char name[32];
	int value;
};


static bool alloc_chunks_and_test(
	void *(*alloc_fn)(struct kmem_cache *),
	struct kmem_cache *cache)
{
	const int test_size = 1200;
	struct t_object *o[test_size];
	bool all_zero = true;
	for(int i=0; i < test_size; i++) {
		o[i] = (*alloc_fn)(cache);
		if(all_zero) {
			uint8_t *mem = (uint8_t *)o[i];
			for(int j=0; j < sizeof(struct t_object); j++) {
				if(mem[j] != 0) {
					all_zero = false;
					break;
				}
			}
		}
	}
	for(int i=0; i < test_size; i++) {
		kmem_cache_free(cache, o[i]);
	}

	return all_zero;
}


/* basic allocation test, checking if zalloc returns a zeroed segment,
 * and the kmem_cache_size() and _name() return values (because otherwise it'd
 * be a fairly small test)
 */
START_TEST(basic_api)
{
	plan_tests(8);

	struct kmem_cache *cache = KMEM_CACHE_NEW("test slab", struct t_object);

	struct t_object *o = kmem_cache_alloc(cache);
	ok1(o != NULL);
	struct t_object *o2 = kmem_cache_alloc(cache);
	ok1(o2 != NULL);
	ok1(o != o2);

	/* dirty the two objects to see if regular alloc may sometimes return
	 * non-zeroed objects.
	 */
	memset(o, 1, sizeof(*o));
	memset(o2, 1, sizeof(*o2));
	kmem_cache_free(cache, o);
	kmem_cache_free(cache, o2);

	bool all_zero = alloc_chunks_and_test(&kmem_cache_alloc, cache);
	ok(!all_zero, "not all regular alloc chunks were zero");
	all_zero = alloc_chunks_and_test(&kmem_cache_zalloc, cache);
	ok(all_zero, "zalloc chunks were all zeroed");

	/* name and size */
	ok1(kmem_cache_size(cache) == sizeof(struct t_object));
	ok1(streq(kmem_cache_name(cache), "test slab"));

	/* try to free an object that doesn't exist. */
	diag("freeing NULL");
	kmem_cache_free(cache, NULL);
	ok(true, "didn't die");

	kmem_cache_destroy(cache);
}
END_TEST


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

	/* part 0: no ctor or dtor at all. */
	ctor_count = 0; dtor_count = 0;
	struct kmem_cache *slab = KMEM_CACHE_NEW("no ctor or dtor",
		struct t_object);
	struct t_object *o = kmem_cache_alloc(slab);
	ok(o != NULL, "alloc ok (base case)");
	kmem_cache_free(slab, o);
	ok(ctor_count == 0 && dtor_count == 0, "counts (base case)");
	kmem_cache_destroy(slab);

	/* part 1: ctor and dtor present */
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
}
END_TEST


static void dancing_allocs(
	struct kmem_cache *slab,
	const int test_size)
{
	struct t_object *ary[test_size];
	for(int i=0; i < test_size; i++) {
		ary[i] = kmem_cache_alloc(slab);
	}
	for(int i=0; i < test_size; i += 2) {
		kmem_cache_free(slab, ary[i]);
	}
	for(int i=0; i < test_size; i += 2) {
		ary[i] = kmem_cache_alloc(slab);
	}
	for(int i=0; i < test_size; i++) {
		kmem_cache_free(slab, ary[i]);
	}
}


START_TEST(recycling)
{
	plan_tests(2);
	const int test_size = 200;
	diag("test_size=%d", test_size);

	/* first without the SLAB_NO_RECYCLE_CTOR flag. */
	diag("base case:");
	struct kmem_cache *slab = kmem_cache_create("many ctor calls",
		sizeof(struct t_object), ALIGNOF(struct t_object),
		0, &ct_ctor, &ct_dtor);
	ctor_count = 0; dtor_count = 0;
	dancing_allocs(slab, test_size);
	kmem_cache_destroy(slab);
	if(!ok1(ctor_count > dtor_count)) {
		diag("ctor_count=%d, dtor_count=%d", ctor_count, dtor_count);
	}

	/* then, with. */
	diag("main half:");
	slab = kmem_cache_create("many ctor calls",
		sizeof(struct t_object), ALIGNOF(struct t_object),
		SLAB_NO_RECYCLE_CTOR, &ct_ctor, &ct_dtor);
	ctor_count = 0; dtor_count = 0;
	dancing_allocs(slab, test_size);
	kmem_cache_destroy(slab);
	if(!ok1(ctor_count == dtor_count)) {
		diag("ctor_count=%d, dtor_count=%d", ctor_count, dtor_count);
	}
}
END_TEST


START_TEST(find_test)
{
	plan_tests(4);

	ok1(kmem_cache_find(NULL) == NULL);
	ok1(kmem_cache_find("not a heap pointer") == NULL);

	struct kmem_cache *cache = KMEM_CACHE_NEW("bleh", struct t_object);
	struct t_object *o = kmem_cache_alloc(cache);

	ok1(kmem_cache_find(o) == cache);

	kmem_cache_free(cache, o);
	kmem_cache_destroy(cache);

	ok1(kmem_cache_find(o) == NULL);
}
END_TEST


START_TEST(kmem_test)
{
	plan_tests(3);
	const int test_size = 100;
	diag("test_size=%d", test_size);

	void *pages[test_size];
	bool success = true;
	for(int i=0; i < test_size; i++) {
		if(!success) pages[i] = NULL;
		else {
			pages[i] = kmem_alloc_new_page();
			if(pages[i] == NULL) {
				diag("alloc %d failed", i);
				success = false;
			}
		}
	}
	ok(success, "page allocs succeeded");

	for(int i=0; i < test_size; i++) {
		if(pages[i] != NULL) kmem_free_page(pages[i]);
	}
	ok(true, "page free didn't die");

	/* now allocate some further pages, checking that they're ones that were
	 * allocated and freed right before.
	 */
	const int tail_size = test_size / 17;
	void *other[tail_size];
	success = true;
	for(int i=0; i < tail_size; i++) {
		other[i] = kmem_alloc_new_page();
		if(other[i] == NULL) success = false;
		bool found = false;
		for(int j=0; j < test_size; j++) {
			if(pages[j] == other[i]) {
				found = true;
				break;
			}
		}
		if(!found && other[i] != NULL) diag("%p not found", other[i]);
		success = found && success;
	}
	ok(success, "pages were recycled");
	for(int i=0; i < tail_size; i++) {
		if(other[i] != NULL) kmem_free_page(other[i]);
	}
}
END_TEST


void ktest_slab(void)
{
	RUN(basic_api);
	RUN(many_alloc);
	RUN(ctor_and_dtor);
	RUN(recycling);
	RUN(find_test);

	/* tangentially slab-related tests, i.e. the page-grain allocator thing */
	RUN(kmem_test);
}

#endif
