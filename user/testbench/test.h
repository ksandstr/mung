/* exports from tap.c
 *
 * (these are separated from those in defs.h for reasons of hygiene.)
 */

#ifndef SEEN_MUNG_TESTBENCH_TEST_H
#define SEEN_MUNG_TESTBENCH_TEST_H

#include <stdbool.h>
#include <stdnoreturn.h>
#include <sys/types.h>


/* imitation of tap.h from libtap by Nik Clayton */

#define ok(cond, test, ...) \
	_gen_result(!!(cond), __func__, __FILE__, __LINE__, \
		test, ##__VA_ARGS__)

#define ok1(cond) \
	_gen_result(!!(cond), __func__, __FILE__, __LINE__, "%s", #cond)

/* >implying implications */
#define imply_ok1(left, right) \
	ok(!(left) || (right), "%s --> %s", #left, #right)

#define imply_ok(left, right, test, ...) \
	ok(!(left) || (right), test, ##__VA_ARGS__)

/* alias for left == right, printed as "iff". */
#define iff_ok1(left, right) \
	ok(!!(left) == !!(right), "%s iff %s", #left, #right)

#define pass(test, ...) ok(true, (test), ##__VA_ARGS__)
#define fail(test, ...) ok(false, (test), ##__VA_ARGS__)

/* (note the unclosed do-while block.) */
#define skip_start(cond, n, fmt, ...) \
	do { \
		if((cond)) { skip((n), (fmt), ##__VA_ARGS__); continue; }

#define skip_end \
	} while(false)

#define subtest_f(test_fn, param, fmt, ...) \
	({ subtest_start(fmt, ##__VA_ARGS__); \
	   (*(test_fn))((param)); \
	   subtest_end(); \
	 })

/* forked subtests. copypasta'd from <sneks/test.h>. #include <sys/wait.h> to
 * make these compile.
 */
#define fork_subtest_start(_fmt, ...) ({ \
		char *_msg = malloc(512); \
		snprintf(_msg, 512, (_fmt), ##__VA_ARGS__); \
		int __stc = fork(); \
		if(__stc == 0) { \
			subtest_start("%s", _msg); \

#define fork_subtest_end \
			int _rc; \
			subtest_pop(&_rc, NULL); \
			exit(_rc); \
		} else { \
			stash_subtest_msg(__stc, _msg); \
		} \
		__stc; \
	})

/* returns the waitpid() status, for WIFEXITED() and the like. when exit
 * status is 255, propagates bail-out by exit(255)'ing.
 *
 * the subtest title can be retrieved with fetch_subtest_msg(_child,
 * "otherwise").
 */
#define fork_subtest_join(_child) ({ \
		int __st, __dead, __c = (_child); \
		do { \
			__dead = waitpid(__c, &__st, 0); \
		} while(__dead < 0 && errno == EINTR); \
		fail_unless(__dead == __c); \
		if(WIFEXITED(__st) && WEXITSTATUS(__st) == 255) { \
			exit(255); \
		} \
		__st; \
	})

/* joins the forked subtest as a test point in the parent. will eventually
 * print the test name in the ok-line. returns like ok1(), bails like
 * fork_subtest_join().
 */
#define fork_subtest_ok1(_child) ({ \
		int __st, __dead, __c = (_child); \
		do { \
			__dead = waitpid(__c, &__st, 0); \
		} while(__dead < 0 && errno == EINTR); \
		if(WIFEXITED(__st) && WEXITSTATUS(__st) == 255) { \
			exit(255); \
		} \
		ok(__dead == __c \
			&& WIFEXITED(__st) && WEXITSTATUS(__st) == 0, \
			fetch_subtest_msg(__c, "unknown subtest")); \
	})


/* Check-style unit testing things */

struct Suite;
typedef struct Suite Suite;
struct TCase;
typedef struct TCase TCase;
struct SRunner;
typedef struct SRunner SRunner;

typedef void (*SFun)(void);

struct test_info
{
	void (*test_fn)(int);
	const char *name;
	int iter_low, iter_high;
};

#define START_TEST(NAME) \
	static void NAME (int); \
	static const struct test_info NAME ## _info = { \
		.test_fn = &NAME, .name = #NAME, \
	}; \
	static void NAME (int _i) {
#define START_LOOP_TEST(NAME, VAR, LOW, HIGH) \
	static void NAME (int); \
	static const struct test_info NAME ## _info = { \
		.test_fn = &NAME, .name = #NAME, \
		.iter_low = (LOW), .iter_high = (HIGH), \
	}; \
	static void NAME (int _i) { \
		int VAR = _i;
#define END_TEST }

/* fail() inherited from the libtap imitation */

#define fail_unless(expr, ...) \
	_fail_unless((expr), __FILE__, __LINE__, \
		"Assertion `" #expr "' failed", ## __VA_ARGS__, NULL)

#define fail_if(expr, ...) \
	_fail_unless(!(expr), __FILE__, __LINE__, \
		"Failure `" #expr "' occurred", ## __VA_ARGS__, NULL)

extern Suite *suite_create(const char *name);
extern void suite_add_tcase(Suite *s, TCase *tc);
extern TCase *tcase_create(const char *name);
extern void tcase_add_unchecked_fixture(TCase *tc, SFun setup, SFun teardown);
extern void tcase_add_checked_fixture(TCase *tc, SFun setup, SFun teardown);
extern void tcase_set_fork(TCase *tc, bool do_fork);

extern void tcase_add_test_info(TCase *tc, const struct test_info *info);

#define tcase_add_test(tc, tfun) tcase_add_test_info((tc), &tfun ## _info)

extern SRunner *srunner_create(Suite *first_suite);
extern void srunner_add_suite(SRunner *run, Suite *s);
extern void srunner_run_all(SRunner *sr, int report_mode);
extern void srunner_run_path(SRunner *sr, const char *path, int report_mode);
extern void srunner_describe(SRunner *sr);


/* internal API for test exit from _fail_unless() */
extern noreturn void exit_on_fail(void);

/* same for test from __assert_failure() */
extern bool in_test(void);


/* from tap.c */

extern void _fail_unless(int result,
	const char *file, int line, const char *expr, ...);

extern int _gen_result(bool ok,
	const char *func, const char *file, unsigned int line,
	const char *fmt, ...);

extern void tap_reset(void);	/* called by the test harness */

extern void plan_no_plan(void);
extern void plan_skip_all(const char *reason, ...);
extern void plan_tests(unsigned int num_tests);

extern int diag(const char *fmt, ...);
extern int skip(unsigned int num_skip, const char *reason, ...);
extern void todo_start(const char *fmt, ...);
extern void todo_end(void);

extern void subtest_start(const char *fmt, ...);
extern int subtest_end(void);
extern char *subtest_pop(int *rc_p, void **freeptr_p);
extern void stash_subtest_msg(pid_t pid, char *msg);
extern const char *fetch_subtest_msg(pid_t pid, const char *orelse);

extern int exit_status(void);
extern void close_no_plan(void);

#endif
