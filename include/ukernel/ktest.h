#ifndef SEEN_UKERNEL_KTEST_H
#define SEEN_UKERNEL_KTEST_H

/* macros etc. for writing kernel-side unit tests. this isn't as nice as the
 * one in testbench, and the two ought to be merged in the future -- but for
 * now, this'll do.
 */

#ifdef ENABLE_SELFTEST
#define KTEST 1

extern void plan_tests(unsigned int count);
extern void plan_no_plan(void);
extern void plan_skip_all(const char *reason);

extern void todo_start(const char *fmt, ...);
extern void todo_end(void);

extern void tap_reset(void);
extern int exit_status(void);
extern int diag(const char *fmt, ...);

extern int _gen_result(
	bool ok,
	const char *func,
	const char *file,
	unsigned int line,
	const char *test_name,
	...);

#define ok1(x) _gen_result(!!(x), __func__, __FILE__, __LINE__, #x)
#define ok(x, ...) \
	_gen_result(!!(x), __func__, __FILE__, __LINE__, ##__VA_ARGS__)

#define START_TEST(name) \
	static void name(void) {

#define END_TEST }

/* no fail_if, fail_unless, bail_out */


/* run a test. suite name will be decided from __FILE__, and tcase from
 * __func__ (with a leading "ktest_" skipped).
 */
#define RUN(testfn) \
	_run_ktest(&testfn, __FILE__, __func__, #testfn)

extern void _run_ktest(
	void (*testfn)(void),
	const char *file,
	const char *func,
	const char *testname);

/* called from kmain.c via pager_thread() */
extern void run_all_tests(void);
extern void describe_all_tests(void);


#endif

#endif
