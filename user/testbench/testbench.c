
/* TODO: extend this roottask to support enough of the Check unit testing
 * framework to be useful in running proper unit tests. output via serial port
 * and so forth.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include <l4/types.h>
#include <l4/thread.h>
#include <l4/ipc.h>
#include <l4/syscall.h>

#include <ukernel/util.h>

#include "defs.h"
#include "test.h"


void abort(void)
{
	printf("testbench abort() called!\n");
	L4_ThreadId_t dummy;
	for(;;) {
		L4_Ipc(L4_nilthread, L4_nilthread, L4_Timeouts(L4_Never, L4_Never),
			&dummy);
	}
}


void malloc_panic(void) {
	printf("%s: called!\n", __func__);
	abort();
}


int sched_yield(void) {
	L4_ThreadSwitch(L4_nilthread);
	return 0;
}


void __assert_failure(
	const char *condition,
	const char *file,
	unsigned int line,
	const char *function)
{
	printf("testbench %s(`%s', `%s', %u, `%s')\n", __func__,
		condition, file, line, function);
	abort();
	for(;;) { }
}


int main(void)
{
	printf("hello, world!\n");
	calibrate_delay_loop();

	/* proper test suite */
	static Suite *(* const suites[])(void) = {
		&thread_suite,
		&space_suite,
		&sched_suite,
	};
	SRunner *run = srunner_create(NULL);
	for(int i=0; i < sizeof(suites) / sizeof(suites[0]); i++) {
		Suite *s = (*suites[i])();
		srunner_add_suite(run, s);
	}
	srunner_run_all(run, 0);

	printf("*** legacy tests follow\n");
	legacy_tests();

	printf("*** testbench completed.\n");

	return 0;
}
