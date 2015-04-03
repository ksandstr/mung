
/* in-kernel tests for lib/slab.c */

#include <stdio.h>
#include <stdalign.h>
#include <string.h>
#include <ccan/list/list.h>
#include <ccan/str/str.h>

#include <ukernel/util.h>
#include <ukernel/mm.h>
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
		sizeof(struct t_object), alignof(struct t_object),
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

	/* first without the KMEM_NO_RECYCLE_CTOR flag. */
	diag("base case:");
	struct kmem_cache *slab = kmem_cache_create("many ctor calls",
		sizeof(struct t_object), alignof(struct t_object),
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
		sizeof(struct t_object), alignof(struct t_object),
		KMEM_NO_RECYCLE_CTOR, &ct_ctor, &ct_dtor);
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
	plan_tests(3);

	ok1(kmem_cache_find(NULL) == NULL);
	ok1(kmem_cache_find("not a heap pointer") == NULL);

	struct kmem_cache *cache = KMEM_CACHE_NEW("bleh", struct t_object);
	struct t_object *o = kmem_cache_alloc(cache);

	ok1(kmem_cache_find(o) == cache);

	kmem_cache_destroy(cache);
}
END_TEST


/* policy tests. */

START_TEST(pol_masks)
{
	plan_tests(3);

	assert(alignof(struct t_object) <= 64);
	struct kmem_cache *c = kmem_cache_create("somewhat unpleasant",
		sizeof(struct t_object), 64, KMEM_POLICY, NULL, NULL);
	void *pol = kmem_policy_align(1024 * 1024, 64);
	kmem_cache_set_policy(c, pol);

	uintptr_t and_mask = 0, or_mask = 0;
	kmem_get_align_masks(pol, &and_mask, &or_mask);
	diag("and_mask=%#lx, or_mask=%#lx", and_mask, or_mask);
	ok1(POPCOUNT(and_mask) == 14);
	ok1((or_mask & and_mask) == 0);
	ok1((and_mask & 0x3f) == 0);

	kmem_cache_destroy(c);
}
END_TEST


/* checks that align-policy allocations have the common bits specified in the
 * returned or_mask .
 */
START_TEST(pol_alloc)
{
	plan_tests(4);
	const size_t test_size = 5000, ob_align = 64;

	assert(ALIGNOF(struct t_object) <= ob_align);
	struct kmem_cache *c = kmem_cache_create("mildly horrific",
		sizeof(struct t_object), ob_align, KMEM_POLICY, NULL, NULL);
	void *pol = kmem_policy_align(1024 * 1024, 64);
	kmem_cache_set_policy(c, pol);

	uintptr_t and_mask = 0, or_mask = 0;
	kmem_get_align_masks(pol, &and_mask, &or_mask);
	diag("and_mask=%#lx, or_mask=%#lx", and_mask, or_mask);

	struct t_object **tobs = calloc(test_size, sizeof(struct t_object *));
	bool all_ok = true, all_aligned = true;
	for(size_t i=0; i < test_size; i++) {
		struct t_object *o = kmem_cache_zalloc(c);
		assert(o != NULL);
		tobs[i] = o;

		uintptr_t op = (uintptr_t)o;
		if((op & (ob_align - 1)) != 0) {
			if(all_aligned) {
				diag("first misalign is o=%p (align=%u)",
					o, (unsigned)ob_align);
			}
			all_aligned = false;
		}
		if((op & ~and_mask) != or_mask) {
			diag("tobs[%u]=%p, masked=%#lx (!= or_mask=%#lx)",
				(unsigned)i, o, (uintptr_t)o & ~and_mask, or_mask);
			all_ok = false;
			break;
		}
	}
	ok(all_ok, "initial allocations");
	ok1(all_aligned);

	dancing_allocs(c, 234);
	ok(true, "alive after dancing_allocs()");

	/* free the first few. */
	for(size_t i=0; i < 456; i++) {
		if(tobs[i] == NULL) {
			kmem_cache_free(c, tobs[i]);
			tobs[i] = NULL;
		}
	}
	/* and allocate some of them again, start to finish. */
	for(size_t i=0; i < 456; i+=3) tobs[i] = kmem_cache_zalloc(c);
	ok(true, "still alive after recycling");

	/* toss all. */
	for(size_t i=0; i < test_size; i++) {
		if(tobs[i] != NULL) kmem_cache_free(c, tobs[i]);
	}

	free(tobs);
	kmem_cache_destroy(c);
}
END_TEST


/* test that the reference macros work for casting between IDs and pointers in
 * the aligned segment.
 */
START_TEST(pol_id_casting)
{
	plan_tests(1);
	const size_t test_size = 5007;

	assert(alignof(struct t_object) <= 64);
	struct kmem_cache *c = kmem_cache_create("malodorously latrinelike",
		sizeof(struct t_object), 64, KMEM_POLICY, NULL, NULL);
	void *pol = kmem_policy_align(1024 * 1024, 64);
	kmem_cache_set_policy(c, pol);

	uintptr_t and_mask = 0, or_mask = 0;
	kmem_get_align_masks(pol, &and_mask, &or_mask);
	diag("and_mask=%#lx, or_mask=%#lx", and_mask, or_mask);

	/* NOTE: zero_ix handling is pointless because no object will be allocated
	 * over the slab in the first page.
	 */
	struct t_object **tobs = calloc(test_size, sizeof(struct t_object *));
	uint16_t *tids = calloc(test_size, sizeof(uint16_t));
	int zero_ix = -1;
	for(size_t i=0; i < test_size; i++) {
		tobs[i] = kmem_cache_zalloc(c);
		assert(tobs[i] != NULL);

		tids[i] = kmem_ptr2id(tobs[i], and_mask, or_mask);
		if(tids[i] == 0) {
			assert(zero_ix == -1);
			zero_ix = i;
		}
	}
	diag("initial allocs ok");

	/* go back to pointer from id. */
	bool all_ok = true;
	for(size_t i=0; i < test_size; i++) {
		if(tids[i] == 0 && (int)i != zero_ix) {
			diag("skipping i=%u for being 0 (tobs[%u]=%p)", i, i, tobs[i]);
			continue;
		}

		struct t_object *o = kmem_id2ptr(tids[i], and_mask, or_mask);
		if(o != tobs[i]) {
			diag("i=%u: o=%p, tobs[i]=%p, tids[i]=%#x",
				i, o, tobs[i], tids[i]);
			all_ok = false;
			break;
		}
	}
	ok1(all_ok);

	free(tobs);
	free(tids);
	kmem_cache_destroy(c);
}
END_TEST


START_TEST(pol_id2ptr_safe)
{
	plan_tests(4);
	const size_t test_size = 5005;		/* sAUcE!1! */

	assert(alignof(struct t_object) <= 64);
	struct kmem_cache *c = kmem_cache_create("abhorrently maladious",
		sizeof(struct t_object), 64, KMEM_POLICY, NULL, NULL);
	void *pol = kmem_policy_align(1024 * 1024, 64);
	kmem_cache_set_policy(c, pol);

	uintptr_t and_mask = 0, or_mask = 0;
	kmem_get_align_masks(pol, &and_mask, &or_mask);
	diag("and_mask=%#lx, or_mask=%#lx", and_mask, or_mask);

	struct t_object **tobs = calloc(test_size, sizeof(struct t_object *));
	uint16_t *tids = calloc(test_size, sizeof(uint16_t));
	bool all_ok = true;
	for(size_t i=0; i < test_size; i++) {
		tobs[i] = kmem_cache_zalloc(c);
		assert(tobs[i] != NULL);

		tids[i] = kmem_ptr2id(tobs[i], and_mask, or_mask);
		if(tids[i] == 0) {
			diag("id for ptr=%p is zero?");
			all_ok = false;
			break;
		}
	}
	ok(all_ok, "zero id didn't occur");

	/* check that no object lies before the 33th byte. */
	all_ok = true;
	for(size_t i=0; i < test_size; i++) {
		uintptr_t p = (uintptr_t)tobs[i] & PAGE_MASK;
		if(p < 32) {
			diag("low bits for %p < 32", tobs[i]);
			all_ok = false;
			break;
		}
	}
	ok(all_ok, "objects respect slabs");

	/* check that all returned objects are valid. */
	all_ok = true;
	for(size_t i=0; i < test_size; i++) {
		if(tids[i] == 0) continue;
		void *ptr = kmem_id2ptr_safe(pol, tids[i]);
		if(ptr != tobs[i]) {
			diag("ptr=%p, tobs[%d]=%p, tid=%u",
				ptr, (int)i, tobs[i], tids[i]);
			all_ok = false;
			break;
		}
	}
	ok(all_ok, "returned objects are valid");

	/* perturb tobs[] by freeing groups of 345 and 456 first; these should
	 * cause page drops. then re-test validity.
	 */
	int steps[] = { 345, 456 };
	for(int j=0; j < NUM_ELEMENTS(steps); j++) {
		for(size_t i = j * 1000 + steps[j]; i < j * 1000 + steps[j] * 2; i++) {
			if(tobs[i] == NULL) continue;
			kmem_cache_free(c, tobs[i]);
			tobs[i] = NULL;
		}
	}
	all_ok = true;
	for(size_t i=0; i < test_size; i++) {
		if(tids[i] == 0) continue;
		void *ptr = kmem_id2ptr_safe(pol, tids[i]);
		if(ptr == NULL && tobs[i] != NULL) {
			diag("ptr=%p, tobs[%d]=%p, tid=%u",
				ptr, (int)i, tobs[i], tids[i]);
			all_ok = false;
			break;
		}
	}
	ok(all_ok, "post-free IDs convert properly");

	free(tobs);
	free(tids);
	kmem_cache_destroy(c);
}
END_TEST


/* base page allocator test. */
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

	/* policy */
	RUN(pol_masks);
	RUN(pol_alloc);
	RUN(pol_id2ptr_safe);
	RUN(pol_id_casting);

	/* tangentially slab-related tests, i.e. the page-grain allocator thing */
	RUN(kmem_test);
}

#endif
