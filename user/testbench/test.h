/* exports from tap.c
 *
 * (these are separated from those in defs.h for reasons of hygiene.)
 */

#ifndef SEEN_MUNG_TESTBENCH_TEST_H
#define SEEN_MUNG_TESTBENCH_TEST_H

#include <stdbool.h>


/* imitation of tap.h from libtap by Nik Clayton */

#define ok(cond, test, ...) \
	_gen_result(!!(cond), __func__, __FILE__, __LINE__, \
		test, ##__VA_ARGS__)

#define ok1(cond) \
	_gen_result(!!(cond), __func__, __FILE__, __LINE__, "%s", #cond)

#define pass(test, ...) ok(true, (test), ##__VA_ARGS__)
#define fail(test, ...) ok(false, (test), ##__VA_ARGS__)

/* (note the unclosed do-while block.) */
#define skip_start(cond, n, fmt, ...) \
	do { \
		if((cond)) { skip((n), (fmt), ##__VA_ARGS__); continue; }

#define skip_end \
	} while(false)


/* Check-style unit testing asserts */

/* fail() inherited from the libtap imitation */

#define fail_unless(expr, ...) \
	_fail_unless(expr, __FILE__, __LINE__, \
		"Assertion `" #expr "' failed", ## __VA_ARGS__, NULL)

#define fail_if(expr, ...) \
	_fail_unless(!(expr), __FILE__, __LINE__, \
		"Failure `" #expr "' occurred", ## __VA_ARGS__, NULL)


/* from tap.c */

extern void _fail_unless(
	int result,
	const char *file,
	int line,
	const char *expr,
	...);

extern int _gen_result(
	bool ok,
	const char *func,
	const char *file,
	unsigned int line,
	const char *test_name,
	...);

extern void tap_reset(void);	/* called by the test harness */

extern void plan_no_plan(void);
extern void plan_skip_all(const char *reason);
extern void plan_tests(unsigned int num_tests);

extern int diag(const char *fmt, ...);
extern int skip(unsigned int num_skip, const char *reason, ...);
extern void todo_start(const char *fmt, ...);
extern void todo_end(void);

extern int exit_status(void);

#endif
