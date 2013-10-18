
/* this test suite is instrumented in comments for the benefit of 30_bench.pl.
 * see that file for details.
 */

#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "defs.h"
#include "test.h"


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


START_TEST(todo_test_points)
{
	plan_tests(2);

	todo_start("always under construction");
	ok1(true);	/* POINT 1: ok T */
	ok1(false);	/* POINT 2: not ok T */
	todo_end();
}
END_TEST


START_LOOP_TEST(todo_iter_points, iter, 0, 1)
{
	plan_tests(4);

	if(iter == 0) todo_start("never finished");
	ok1(true);		/* POINT 1: [0: ok T] [1: ok] */
	ok1(false);		/* POINT 2: [0: not ok T] [1: not ok] */
	ok1(iter == 0);	/* POINT 3: [0: ok T] [1: not ok] */
	ok1(iter == 1);	/* POINT 4: [0: not ok T] [1: ok] */
	todo_end();
}
END_TEST


START_TEST(skip_plan)
{
	plan_skip_all("nothing useful can be determined here.");
}
END_TEST


START_TEST(no_plan)
{
	plan_no_plan();
	ok1(true);		/* POINT 1: ok */
	ok1(false);		/* POINT 2: not ok */
}
END_TEST


START_TEST(late_plan)
{
	ok1(true);		/* POINT 1: ok */
	ok1(false);		/* POINT 2: not ok */


	ok1(true);		/* POINT 3: ok */
	plan_tests(3);
}
END_TEST


START_TEST(fail_basic)
{
	plan_tests(1);
	fail_if(true, "going down!");
}
END_TEST


START_TEST(fail_assert)
{
	plan_tests(1);
	assert("ghost blowjob! woohoo" == NULL);
}
END_TEST


START_TEST(fail_segv)
{
	plan_tests(1);

	void *ptr = malloc(8192);
	volatile uint8_t *fault = ptr + 0x40000000;
	diag("faulting at %p", fault);
	for(int i=0; i < 256; i++) fault[i] = 123 ^ i;

	fail("faulting failed.");
}
END_TEST


static void add_fail_tests(TCase *tc)
{
	tcase_add_test(tc, fail_basic);
	tcase_add_test(tc, fail_assert);
	tcase_add_test(tc, fail_segv);
}


Suite *meta_suite(void)
{
	Suite *s = suite_create("meta");

	{
		TCase *tc = tcase_create("point");
		tcase_add_test(tc, basic_test_points);
		tcase_add_test(tc, iterated_test_points);
		tcase_add_test(tc, todo_test_points);
		tcase_add_test(tc, todo_iter_points);
		suite_add_tcase(s, tc);
	}

	{
		TCase *tc = tcase_create("plan");
		tcase_add_test(tc, skip_plan);
		tcase_add_test(tc, no_plan);
		tcase_add_test(tc, late_plan);
		suite_add_tcase(s, tc);
	}

	{
		TCase *tc = tcase_create("fail");
		add_fail_tests(tc);
		suite_add_tcase(s, tc);
	}

	/* same, but without forking. */
	{
		TCase *tc = tcase_create("failnf");
		tcase_set_fork(tc, false);
		add_fail_tests(tc);
		suite_add_tcase(s, tc);
	}

	/* TODO: add cases for different kinds of plan, TODOs, skipping, fails,
	 * assert failures, calling abort(), and becoming stuck in a perpetual
	 * loop.
	 */

	return s;
}
