
/* macros etc. for in-kernel TAP stuff. not everything that's available to
 * user-space tests is available to ktests; for example, tests will never
 * fork, cannot have fixtures, and fail_{if,unless}() panic the kernel instead
 * of exiting a single test.
 */

#ifndef SEEN_UKERNEL_KTEST_H
#define SEEN_UKERNEL_KTEST_H

#if defined(__KERNEL__) && defined(ENABLE_SELFTEST)
#define KTEST 1

#include "user/testbench/test.h"


/* run a test. suite name will be decided from __FILE__, and tcase from
 * __func__ (with a leading "ktest_" skipped).
 */
#define RUN(testname) \
	_run_ktest(&testname ## _info, __FILE__, __func__)

extern void _run_ktest(
	const struct test_info *info,
	const char *file,
	const char *func);

/* called from kmain.c via pager_thread() */
extern void run_all_tests(void);
extern void describe_all_tests(void);


/* utility routines from lib/test_util.c */

extern unsigned factorial(unsigned x);	/* computes x! */

/* generates a numbered permutation of [0, n). outputs distinct sequences
 * while @perm < n! .
 */
extern void gen_perm(unsigned *buf, unsigned n, unsigned perm);


#endif

#endif
