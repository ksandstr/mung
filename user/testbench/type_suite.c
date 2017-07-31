
/* tests wrt the types defined by <l4/types.h> */

#ifndef DEBUG_ME_HARDER
#define DEBUG_ME_HARDER
#endif

#include <stdlib.h>

#include <l4/types.h>
#include <l4/thread.h>
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
		int b;
		const char *desc;
	} cases[] = {
		{ 0, "at creation" },
		{ span_us - 1, "next to end" },
		{ span_us / 2, "at midpoint" },
		{ span_us / 4, "at 1/4" },
		{ span_us / 4 * 3, "at 3/4" },
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
			"stable %s", cases[i].desc);
	}
}
END_TEST


static bool pt_is_valid(L4_Clock_t base, L4_Time_t t)
{
	uint32_t max = 0x3ff << t.point.e,
		us = L4_PointClock_NP(base, t).raw - base.raw;
	// diag("max=%#x, us=%#x", max, us);
	return max >= us;
}


/* much the same, but test validity and lack thereof when base >= point. also,
 * instead of span_us, we'll use the actual length of the interval; that's
 * what validity is based on, anyway.
 *
 * as it stands L4_TimePoint2_NP() rounds the interval down. this is something
 * to be aware of.
 */
START_LOOP_TEST(pt_valid, iter, 3, 15)
{
	const unsigned span_us = ((iter <= 10 ? 1 : 0x3ff) << iter) - 1;
	L4_Clock_t base = { .raw = CLOCK_BASE };
	const uint64_t point = base.raw + span_us;

	L4_Time_t t = L4_TimePoint2_NP(base, (L4_Clock_t){ .raw = point });
	fail_unless(L4_IsTimePoint_NP(t));
	const unsigned actual_us = L4_PointClock_NP(base, t).raw - base.raw + 1;
	diag("span_us=%u, actual=%u, t={e=%u, m=%#x, c=%d}",
		span_us, actual_us, t.point.e, t.point.m, t.point.c);

	const struct {
		int b;
		const char *desc;
	} cases[] = {
		{ 0, "at creation" },
		{ actual_us - 1, "next to end (imprecise)" },
		{ actual_us / 2, "at midpoint" },
		{ actual_us / 4, "at 1/4" },
		{ actual_us / 4 * 3, "at 3/4" },
	};
	const int n_cases = sizeof(cases) / sizeof(cases[0]);
	plan_tests(2 * n_cases);

	/* check that pt_is_valid() shows them as such when they're supposed to
	 * be.
	 */
	for(int i=0; i < n_cases; i++) {
		L4_Clock_t b2 = { .raw = base.raw + cases[i].b };
		ok(pt_is_valid(b2, t), "valid %s", cases[i].desc);
	}

	/* and not, when not, for up to actual_us after the original base. */
	for(int i=0; i < n_cases; i++) {
		L4_Clock_t b2 = { .raw = base.raw + actual_us + cases[i].b };
		ok(!pt_is_valid(b2, t), "invalid %s + actual_us", cases[i].desc);
	}
}
END_TEST


START_TEST(period_basic)
{
	plan_tests(1);

	L4_Time_t t = L4_TimePeriod(500000);
	diag("half a second is e %#x, m %#x (%lu µs)\n",
		t.period.e, t.period.m, L4_PeriodUs_NP(t));
	ok(abs(500000 - L4_PeriodUs_NP(t)) < 1000, "close enough");
}
END_TEST


START_TEST(threadid_basic)
{
	plan_tests(2);

	diag("L4_Myself() == %#lx", L4_Myself().raw);
	diag("L4_MyLocalId() == %#lx", L4_MyLocalId().raw);
	diag("L4_LocalIdOf(L4_MyGlobalId()) == %#lx",
		L4_LocalIdOf(L4_MyGlobalId()).raw);
	diag("L4_GlobalIdOf(L4_MyLocalId()) == %#lx",
		L4_GlobalIdOf(L4_MyLocalId()).raw);

	ok1(L4_GlobalIdOf(L4_MyLocalId()).raw == L4_MyGlobalId().raw);
	ok1(L4_LocalIdOf(L4_MyGlobalId()).raw == L4_MyLocalId().raw);
}
END_TEST


static Suite *type_suite(void)
{
	Suite *s = suite_create("type");

	{
		TCase *tc = tcase_create("timept");
		tcase_add_test(tc, pt_stable);
		tcase_add_test(tc, pt_valid);
		suite_add_tcase(s, tc);
	}

	{
		TCase *tc = tcase_create("period");
		tcase_add_test(tc, period_basic);
		suite_add_tcase(s, tc);
	}

	{
		TCase *tc = tcase_create("threadid");
		tcase_add_test(tc, threadid_basic);
		suite_add_tcase(s, tc);
	}

	return s;
}


static const struct suite_spec s = { &type_suite, 100 };
AUTODATA(testsuites, &s);
