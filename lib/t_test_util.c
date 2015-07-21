
/* tests for lib/test_util.c */

#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include <assert.h>

#include <ccan/htable/htable.h>

#include <ukernel/util.h>
#include <ukernel/ktest.h>

#if KTEST


/* the uglybad properly recursive factorial. */
static unsigned slowfac(unsigned x) {
	if(x <= 1) return x; else return x * slowfac(x - 1);
}


START_TEST(factorial_smoke)
{
	plan_tests(13);

	for(int i=0; i < 13; i++) {
		unsigned slow_res = slowfac(i), fast_res = factorial(i);
		if(!ok(slow_res == fast_res, "%d! is correct", i)) {
			diag("slow_res=%u, fast_res=%u", slow_res, fast_res);
		}
	}
}
END_TEST


/* check that outputs [0, iter) all appear, and none beside them. */
START_LOOP_TEST(gen_perm_smoke, iter, 1, 10)
{
	const size_t n_perms = factorial(iter);

	diag("iter=%d, n_perms=%u", iter, (unsigned)n_perms);
	plan_tests(3);

	bool all_present = true, all_in_range = true, no_repeats = true;
	unsigned p[iter];
	for(int i=0; i < n_perms; i++) {
		for(int j=0; j < iter; j++) p[j] = 666 - j + i;
		gen_perm(p, iter, i);
		uint32_t bits = 0;
		for(int j=0; j < iter; j++) {
			if(p[j] >= iter && all_in_range) {
				diag("i=%d, p[%d] = %u (out of [0, %d) range)",
					i, j, p[j], iter);
				all_in_range = false;
			}

			if(CHECK_FLAG(bits, 1 << p[j]) && no_repeats) {
				diag("i=%d, p[%d] = %u (more than once)", i, j, p[j]);
				no_repeats = false;
			}
			bits |= 1 << p[j];
		}
		if(bits != (1 << iter) - 1 && all_present) {
			diag("i=%d, bits=%#x (not all present)", i, bits);
			all_present = false;
		}
	}

	ok1(all_present);
	ok1(all_in_range);
	ok1(no_repeats);
}
END_TEST


static size_t hash_u16_perm(const void *key, void *priv)
{
	const uint16_t *vals = key;
	const uintptr_t n_vals = (uintptr_t)priv;

	uint32_t acc = int_hash(vals[0]);
	/* rotation should ensure order-dependence. */
	for(int i=1; i < n_vals; i++) {
		size_t h = int_hash(vals[i]);
		acc -= (h << i) | ((h >> i) & ~((1 << i) - 1));
	}
	return acc;
}


/* test that gen_perm() for width 1..7 has no duplicate outputs, as promised.
 * (8 is too long.)
 */
START_LOOP_TEST(gen_perm_unique, iter, 1, 7)
{
	const size_t n_perms = factorial(iter);
	assert(n_perms < 65536);

	diag("n_perms=%u", (unsigned)n_perms);
	plan_tests(1);

	uint16_t *permbuf = malloc(sizeof(uint16_t) * n_perms * iter);
	assert(permbuf != NULL);
	struct htable u_table;
	htable_init_sized(&u_table, &hash_u16_perm, (void *)iter, n_perms);

	bool all_unique = true;
	unsigned ptmp[iter];
	for(int i=0; i < n_perms; i++) {
		uint16_t *p = &permbuf[i * iter];
		gen_perm(ptmp, iter, i);
		for(int j=0; j < iter; j++) p[j] = ptmp[j];
		size_t hash = hash_u16_perm(p, (void *)iter);
		bool found = false;
		struct htable_iter it;
		for(void *cand = htable_firstval(&u_table, &it, hash);
			cand != NULL;
			cand = htable_nextval(&u_table, &it, hash))
		{
			if(memcmp(cand, p, sizeof(uint16_t) * iter) == 0) {
				found = true;
				break;
			}
		}
		if(!found) htable_add(&u_table, hash, p);
		else if(all_unique) {
			diag("i=%d is a duplicate", i);
			all_unique = false;
		}
	}

	ok1(all_unique);

	htable_clear(&u_table);
	free(permbuf);
}
END_TEST


/* selftests */

START_TEST(self_hash_u16_perm)
{
	plan_tests(2);

	/* should get distinct values for inputs having disjoint items. */
	uint16_t v0[] = { 1, 2, 3, 4 }, v1[] = { 0, 1, 2, 3 };
	size_t h0 = hash_u16_perm(v0, (void *)4),
		h1 = hash_u16_perm(v1, (void *)4);
	if(!ok1(h0 != h1)) {
		diag("h0=%#x, h1=%#x", (unsigned)h0, (unsigned)h1);
	}

	/* should get distinct values for inputs having the same items, but in a
	 * different ordering.
	 */
	uint16_t v2[] = { 1, 2, 4, 3 }, v3[] = { 4, 3, 2, 1 };
	size_t h2 = hash_u16_perm(v2, (void *)4),
		h3 = hash_u16_perm(v3, (void *)4);
	if(!ok1(h0 != h2 && h0 != h3)) {
		diag("h0=%#x, h2=%#x, h3=%#x",
			(unsigned)h0, (unsigned)h2, (unsigned)h3);
	}
}
END_TEST


void ktest_test_util(void)
{
	RUN(self_hash_u16_perm);
	RUN(factorial_smoke);
	RUN(gen_perm_smoke);
	RUN(gen_perm_unique);
}

#endif
