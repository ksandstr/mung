
/* tests of forkserv's own process management features. these are of limited
 * utility when those mechanisms are broken, but at least that'll break the
 * whole test sequence rather than going unnoticed.
 */

#include <assert.h>

#include "defs.h"
#include "test.h"


START_TEST(basic_fork_and_wait)
{
	plan_tests(2);

	int spid = fork();
	if(spid == 0) {
		/* child side */
		exit(0);
		assert(false);
	}
	ok(spid > 0, "fork succeeded");
	int status = 0, dead = wait(&status);
	ok(dead > 0 && dead == spid, "child exited");
	/* TODO: test the status, too */
}
END_TEST


Suite *process_suite(void)
{
	Suite *s = suite_create("process");

	TCase *fork_case = tcase_create("fork");
	tcase_add_test(fork_case, basic_fork_and_wait);
	suite_add_tcase(s, fork_case);

	return s;
}
