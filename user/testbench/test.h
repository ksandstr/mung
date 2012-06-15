/* exports from tap.c
 *
 * (these are separated from those in defs.h because of ok() overlapping a
 * genuinely useful variable called "ok" that appears in some function scopes.
 * sort of like index(3) [oh how we hate thee].)
 */

#ifndef SEEN_MUNG_TESTBENCH_TEST_H
#define SEEN_MUNG_TESTBENCH_TEST_H


/* unit testing things */
#define fail(...) _fail_unless(0, __FILE__, __LINE__, "Failed", ## __VA_ARGS__, NULL)

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

#endif
