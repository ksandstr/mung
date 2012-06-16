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
	char name[];
};


struct test
{
	struct list_node tcase_link;
	void (*t_fun)(int);
	int low, high;
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
	tc->suite_link = (struct list_node){ };
	list_head_init(&tc->tests);
	memcpy(tc->name, name, len + 1);
	return tc;
}


/* TODO: add testcase ranges */
void tcase_add_test(TCase *tc, void (*t_fun)(int))
{
	struct test *t = calloc(1, sizeof(struct test));
	t->t_fun = t_fun;
	list_add_tail(&tc->tests, &t->tcase_link);
}


void tcase_add_loop_test(TCase *tc, void (*t_fun)(int), int low, int high)
{
	tcase_add_test(tc, t_fun);
	struct test *t = container_of(tc->tests.n.prev, struct test,
		tcase_link);
	assert(t->t_fun == t_fun);
	t->low = low;
	t->high = high;
}


/* TODO: set up a mechanism for nonlocal exits */
static void test_wrapper_fn(void *param_ptr)
{
	void **param = param_ptr;
	const struct test *t = param[2];
	(*t->t_fun)(0);
}


static bool run_test(Suite *s, TCase *tc, struct test *t)
{
	bool rc = true;

	void **param = calloc(3, sizeof(void *));
	param[0] = s;
	param[1] = tc;
	param[2] = t;
	L4_ThreadId_t thread = start_thread(&test_wrapper_fn, param);
	if(L4_IsNilThread(thread)) {
		printf("*** %s: start_thread() failed\n", __func__);
		rc = false;
		goto end;
	}

	/* FIXME: catch early termination, nonlocal exit, etc */

	join_thread(thread);

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


void srunner_run_all(SRunner *sr, int report_mode)
{
	Suite *s;
	list_for_each(&sr->suites, s, runner_link) {
		printf("*** begin suite `%s'\n", s->name);
		TCase *tc;
		list_for_each(&s->cases, tc, suite_link) {
			printf("*** begin tcase `%s'\n", tc->name);
			flush_log(false);
			struct test *t;
			list_for_each(&tc->tests, t, tcase_link) {
				int high = t->high;
				if(high < t->low) high = t->low;
				int rc_tot = 0;
				for(int val = t->low; val <= high; val++) {
					tap_reset();
					if(!run_test(s, tc, t)) {
						/* FIXME: handle this somehow */
						printf("*** the gait of the least graceful hippopotamus\n");
						return;
					}
					/* TODO: gather results */
					int rc = exit_status();
					if(rc > 0) rc_tot += rc;
				}
				if(rc_tot > 0) {
					printf("*** %d failed or anomalous tests\n", rc_tot);
					flush_log(true);
				}
			}
			printf("*** end tcase `%s'\n", tc->name);
		}
		printf("*** end suite `%s'\n", s->name);
	}
}
