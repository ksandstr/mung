/* unit testing framework modelled after Check. */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <assert.h>
#include <ccan/list/list.h>
#include <ccan/container_of/container_of.h>

#include "defs.h"
#include "test.h"


struct SRunner
{
	struct list_head suites;
};


struct Suite
{
	struct list_node runner_link;
	struct list_head cases;
	char name[];
};


struct TCase
{
	struct list_node suite_link;
	struct list_head tests, u_fixtures, c_fixtures;
	char name[];
};


struct fixture
{
	struct list_node link;	/* in u_fixtures or c_fixtures */
	SFun setup, teardown;
};


struct test
{
	struct list_node tcase_link;
	void (*t_fun)(int);
	int low, high;
	char name[];
};


struct test_status {
	bool rc;		/* run_test() return value */
};


Suite *suite_create(const char *name)
{
	int len = strlen(name);
	Suite *s = malloc(sizeof(Suite) + len + 1);
	list_head_init(&s->cases);
	memcpy(&s->name, name, len + 1);
	return s;
}


void suite_add_tcase(Suite *s, TCase *tc) {
	list_add_tail(&s->cases, &tc->suite_link);
}


TCase *tcase_create(const char *name)
{
	int len = strlen(name);
	TCase *tc = malloc(sizeof(TCase) + len + 1);
	memset(tc, '\0', sizeof(*tc));
	tc->suite_link = (struct list_node){ };
	list_head_init(&tc->tests);
	list_head_init(&tc->c_fixtures);
	list_head_init(&tc->u_fixtures);
	memcpy(tc->name, name, len + 1);
	return tc;
}


static void add_fixture(
	struct list_head *list,
	SFun setup,
	SFun teardown)
{
	struct fixture *f = malloc(sizeof(*f));
	*f = (struct fixture){ .setup = setup, .teardown = teardown };
	list_add_tail(list, &f->link);
}


/* TODO: proper support for test fixtures requires controlled restarting of
 * the emulator & skipping past a point in the test suite runner's loop. so
 * for now test.c implements fixtures without anything like forking.
 */

void tcase_add_unchecked_fixture(TCase *tc, SFun setup, SFun teardown) {
	add_fixture(&tc->u_fixtures, setup, teardown);
}


void tcase_add_checked_fixture(TCase *tc, SFun setup, SFun teardown) {
	add_fixture(&tc->c_fixtures, setup, teardown);
}


void tcase_add_test_full(
	TCase *tc,
	void (*t_fun)(int),
	const char *name,
	int low, int high)
{
	int len = strlen(name);
	struct test *t = malloc(sizeof(struct test) + len + 1);
	*t = (struct test){
		.t_fun = t_fun, .low = low, .high = high,
	};
	memcpy(t->name, name, len + 1);
	list_add_tail(&tc->tests, &t->tcase_link);
}


struct test_thread_param
{
	Suite *s;
	TCase *tc;
	struct test *t;
	int val;
};


/* TODO: set up a mechanism for nonlocal exits */
static void test_wrapper_fn(void *param_ptr)
{
	const struct test_thread_param *p = param_ptr;
	(*p->t->t_fun)(p->val);
	/* TODO: fill in a test_status report, return it through exit_thread() */
}


void exit_on_fail(void)
{
	struct test_status *st = malloc(sizeof(*st));
	if(st != NULL) {
		*st = (struct test_status){
			.rc = false,
		};
	}
	exit_thread(st);
}


static void fixture_wrapper_fn(void *param_ptr)
{
	SFun *param = param_ptr;
	(**param)();
}


static bool run_fixture_list(
	struct list_head *list,
	bool teardown)
{
	bool rc = true;
	struct fixture *f;
	list_for_each(list, f, link) {
		SFun *param = malloc(sizeof(*param));
		*param = teardown ? f->teardown : f->setup;
		L4_ThreadId_t thread = start_thread(&fixture_wrapper_fn, param);
		if(L4_IsNilThread(thread)) {
			printf("*** %s: start_thread() failed\n", __func__);
			free(param);
			return false;
		}

		struct test_status *status = join_thread(thread);
		if(status != NULL) {
			rc = status->rc;
			free(status);
		}

		free(param);

		if(!rc) break;
	}

	return rc;
}


static bool run_test(Suite *s, TCase *tc, struct test *t, int test_value)
{
	bool rc = true;

	struct test_thread_param *param = malloc(sizeof(*param));
	*param = (struct test_thread_param){
		.s = s, .tc = tc, .t = t, .val = test_value,
	};
	L4_ThreadId_t thread = start_thread(&test_wrapper_fn, param);
	if(L4_IsNilThread(thread)) {
		printf("*** %s: start_thread() failed\n", __func__);
		rc = false;
		goto end;
	}

	struct test_status *status = join_thread(thread);
	if(status != NULL) {
		/* early terminations, etc. */
		rc = status->rc;
		free(status);
	}

end:
	free(param);
	return rc;
}


SRunner *srunner_create(Suite *first_suite)
{
	SRunner *sr = malloc(sizeof(SRunner));
	list_head_init(&sr->suites);
	if(first_suite != NULL) srunner_add_suite(sr, first_suite);
	return sr;
}


void srunner_add_suite(SRunner *run, Suite *s)
{
	list_add_tail(&run->suites, &s->runner_link);
}


/* TODO: run fixture setup and teardown in threads so that they can call
 * fail_unless() etc. also
 */
void srunner_run_all(SRunner *sr, int report_mode)
{
	Suite *s;
	list_for_each(&sr->suites, s, runner_link) {
		printf("*** begin suite `%s'\n", s->name);
		TCase *tc;
		list_for_each(&s->cases, tc, suite_link) {
			printf("*** begin tcase `%s'\n", tc->name);
			if(!list_empty(&tc->u_fixtures)
				&& !run_fixture_list(&tc->u_fixtures, false))
			{
				/* FIXME: see below */
				printf("*** abort tcase `%s' (unchecked fixture failed)\n",
					tc->name);
				continue;
			}
			struct test *t;
			list_for_each(&tc->tests, t, tcase_link) {
				int high = t->high;
				if(high < t->low) high = t->low;
				for(int val = t->low; val <= high; val++) {
					printf("*** begin test `%s'", t->name);
					if(t->low < high) printf(" iter %d", val);
					printf("\n");
					if(!list_empty(&tc->c_fixtures)
						&& !run_fixture_list(&tc->c_fixtures, false))
					{
						/* TODO: do an exit-to-restart here just in case the
						 * microkernel has become fucked. but note the test
						 * sequence run so far, or something.
						 *
						 * FIXME: at least signal that a test case was
						 * skipped!
						 *
						 * for now this just goes on to the next tcase,
						 * leaving fixtures before the failed one in place.
						 * that's pretty bad.
						 */
						printf("*** abort test `%s' (checked fixture failed)\n",
							t->name);
						continue;
					}

					flush_log(false);
					tap_reset();
					bool failed = !run_test(s, tc, t, val);

					int rc = exit_status();
					if(rc > 0 || failed) {
						/* TODO: gather results for unplanned and unexecuted
						 * test points.
						 */
						flush_log(true);
					}

					if(!list_empty(&tc->c_fixtures)
						&& !run_fixture_list(&tc->c_fixtures, true))
					{
						printf("*** test `%s': checked fixture teardown failed\n",
							t->name);
						/* FIXME: do exit-to-restart or something */
					}

					if(failed) {
						printf("*** test `%s' failed, rc %d\n", t->name, rc);
					} else {
						printf("*** end test `%s' rc %d\n", t->name, rc);
					}
				}
			}
			if(!list_empty(&tc->u_fixtures)
				&& !run_fixture_list(&tc->u_fixtures, true))
			{
				printf("*** tcase `%s': unchecked fixture teardown failed\n",
					tc->name);
				/* FIXME: see above */
			}
			printf("*** end tcase `%s'\n", tc->name);
		}
		printf("*** end suite `%s'\n", s->name);
	}
}
