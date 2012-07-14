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
	struct list_head tests;
	SFun uf_setup, uf_teardown;
	SFun cf_setup, cf_teardown;
	char name[];
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
	memcpy(tc->name, name, len + 1);
	return tc;
}


/* TODO: proper support for test fixtures requires controlled restarting of
 * the emulator & skipping past a point in the test suite runner's loop. so
 * for now test.c implements fixtures without anything like forking.
 */

void tcase_add_unchecked_fixture(TCase *tc, SFun setup, SFun teardown)
{
	tc->uf_setup = setup;
	tc->uf_teardown = teardown;
}


void tcase_add_checked_fixture(TCase *tc, SFun setup, SFun teardown)
{
	tc->cf_setup = setup;
	tc->cf_teardown = teardown;
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
			if(tc->uf_setup != NULL) (*tc->uf_setup)();
			struct test *t;
			list_for_each(&tc->tests, t, tcase_link) {
				int high = t->high;
				if(high < t->low) high = t->low;
				for(int val = t->low; val <= high; val++) {
					printf("*** begin test `%s'", t->name);
					if(t->low < high) printf(" iter %d", val);
					printf("\n");
					if(tc->cf_setup != NULL) (*tc->cf_setup)();

					flush_log(false);
					tap_reset();
					if(!run_test(s, tc, t, val)) {
						/* FIXME: handle this somehow */
						printf("*** the gait of the least graceful hippopotamus\n");
						return;
					}

					int rc = exit_status();
					if(rc > 0) {
						/* TODO: gather results for unplanned and unexecuted
						 * test points.
						 */
						flush_log(true);
					}

					if(tc->cf_teardown != NULL) (*tc->cf_teardown)();
					printf("*** end test `%s' rc %d\n", t->name, rc);
				}
			}
			if(tc->uf_teardown != NULL) (*tc->uf_teardown)();
			printf("*** end tcase `%s'\n", tc->name);
		}
		printf("*** end suite `%s'\n", s->name);
	}
}
