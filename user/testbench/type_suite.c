
/* tests wrt the types defined by <l4/types.h> */

#ifndef DEBUG_ME_HARDER
#define DEBUG_ME_HARDER
#endif

#include <stdlib.h>
#include <l4/types.h>
#include <l4/syscall.h>

#include "defs.h"
#include "test.h"


#define CLOCK_BASE 1369580262754491ull


/* compare time for equality within precision determined from exponent (as for
 * L4_Time_t's time point values).
 */
static bool time_cmp(uint64_t a, uint64_t b, int exp)
{
	int dist = (0x3ff << exp) / 850;
	diag("dist=%#x, actual=%#x",
		(unsigned)dist, (unsigned)llabs(a - b));
	return llabs(a - b) < dist;
}


/* tests that time points are stable at creation, in the middle, next to the
 * end, and in between the three.
 *
 * TODO: add tiny test for 1, 2, and 4 too?
 */
START_LOOP_TEST(pt_stable, iter, 3, 15)
{
	const unsigned span_us = ((iter <= 10 ? 1 : 0x3ff) << iter) - 1;
	L4_Clock_t base = { .raw = CLOCK_BASE };
	const uint64_t point = base.raw + span_us;
	const struct {
		int64_t b;
		const char *desc;
	} cases[] = {
		{ 0, "stable at creation" },
		{ span_us - 1, "stable next to end" },
		{ span_us / 2, "stable at midpoint" },
		{ span_us / 4, "stable at 1/4" },
		{ span_us / 4 * 3, "stable at 3/4" },
	};
	const int n_cases = sizeof(cases) / sizeof(cases[0]);
	plan_tests(n_cases);

	L4_Time_t t = L4_TimePoint2_NP(base, (L4_Clock_t){ .raw = point });
	fail_unless(L4_IsTimePoint_NP(t));
	diag("span_us=%u, t={e=%u, m=%#x, c=%d}",
		span_us, t.point.e, t.point.m, t.point.c);

	for(int i=0; i < n_cases; i++) {
		L4_Clock_t b2 = { .raw = base.raw + cases[i].b };
		ok(time_cmp(L4_PointClock_NP(b2, t).raw, point, t.point.e),
			cases[i].desc);
	}
}
END_TEST


Suite *type_suite(void)
{
	Suite *s = suite_create("type");

	TCase *timept_case = tcase_create("timept");
	tcase_add_test(timept_case, pt_stable);
	suite_add_tcase(s, timept_case);

	return s;
}
