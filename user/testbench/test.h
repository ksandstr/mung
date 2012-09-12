/* exports from tap.c
 *
 * (these are separated from those in defs.h for reasons of hygiene.)
 */

#ifndef SEEN_MUNG_TESTBENCH_TEST_H
#define SEEN_MUNG_TESTBENCH_TEST_H

#include <stdbool.h>
#include <ccan/compiler/compiler.h>


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


/* Check-style unit testing things */

struct Suite;
typedef struct Suite Suite;
struct TCase;
typedef struct TCase TCase;
struct SRunner;
typedef struct SRunner SRunner;

typedef void (*SFun)(void);

#define START_TEST(name) \
	static void name (int _i) {
#define START_LOOP_TEST(name, _var) \
	static void name (int _i) { \
		int _var = _i;
#define END_TEST }

/* fail() inherited from the libtap imitation */

#define fail_unless(expr, ...) \
	_fail_unless(expr, __FILE__, __LINE__, \
		"Assertion `" #expr "' failed", ## __VA_ARGS__, NULL)

#define fail_if(expr, ...) \
	_fail_unless(!(expr), __FILE__, __LINE__, \
		"Failure `" #expr "' occurred", ## __VA_ARGS__, NULL)

extern Suite *suite_create(const char *name);
extern void suite_add_tcase(Suite *s, TCase *tc);
extern TCase *tcase_create(const char *name);
extern void tcase_add_unchecked_fixture(TCase *tc, SFun setup, SFun teardown);
extern void tcase_add_checked_fixture(TCase *tc, SFun setup, SFun teardown);

extern void tcase_add_test_full(
	TCase *tc,
	void (*t_fun)(int),
	const char *name,
	int low, int high);

#define tcase_add_test(tc, tfun) tcase_add_test_full((tc), (tfun), #tfun, 0, 0)
#define tcase_add_loop_test(tc, tfun, lo, hi) \
	tcase_add_test_full((tc), (tfun), #tfun, (lo), (hi))

extern SRunner *srunner_create(Suite *first_suite);
extern void srunner_add_suite(SRunner *run, Suite *s);
extern void srunner_run_all(SRunner *sr, int report_mode);
extern void srunner_run_path(SRunner *sr, const char *path, int report_mode);
extern void srunner_describe(SRunner *sr);


/* internal API for test exit from _fail_unless() */
extern NORETURN void exit_on_fail(void);

/* same for test from __assert_failure() */
extern bool in_test(void);


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
