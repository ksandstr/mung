
/* this test suite is instrumented in comments for the benefit of 30_bench.pl.
 * see that file for details.
 */

#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "defs.h"
#include "test.h"


/* some fail, others don't. */
START_TEST(basic_test_points)
{
	plan_tests(2);

	ok1(true);		/* POINT 1: ok */
	ok1(false);		/* POINT 2: not ok */
}
END_TEST


START_LOOP_TEST(iterated_test_points, iter, 0, 1)
{
	plan_tests(4);

	ok1(true);		/* POINT 1: ok */
	ok1(false);		/* POINT 2: not ok */
	ok1(iter == 0);	/* POINT 3: [0: ok] [1: not ok] */
	ok1(iter == 1);	/* POINT 4: [0: not ok] [1: ok] */
}
END_TEST


Suite *meta_suite(void)
{
	Suite *s = suite_create("meta");

	{
		TCase *tc = tcase_create("point");
		tcase_add_test(tc, basic_test_points);
		tcase_add_test(tc, iterated_test_points);
		suite_add_tcase(s, tc);
	}

	/* TODO: add cases for different kinds of plan, TODOs, skipping, fails,
	 * assert failures, calling abort(), and becoming stuck in a perpetual
	 * loop.
	 */

	return s;
}
