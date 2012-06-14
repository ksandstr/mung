
#ifndef SEEN_TESTBENCH_DEFS_H
#define SEEN_TESTBENCH_DEFS_H

#include <stdio.h>
#include <stdarg.h>
#include <stdbool.h>

#include <l4/types.h>


/* unit testing things */
#define fail(...) _fail_unless(0, __FILE__, __LINE__, "Failed", ## __VA_ARGS__, NULL)

#define fail_unless(expr, ...) \
	_fail_unless(expr, __FILE__, __LINE__, \
		"Assertion `" #expr "' failed", ## __VA_ARGS__, NULL)

#define fail_if(expr, ...) \
	_fail_unless(!(expr), __FILE__, __LINE__, \
		"Failure `" #expr "' occurred", ## __VA_ARGS__, NULL)



extern void threadctl_test(void);
extern void sched_test(void);


/* from thread.c */

extern L4_ThreadId_t start_thread(void (*fn)(void *), void *param);
extern L4_ThreadId_t start_thread_long(
	void (*fn)(void *),
	void *param,
	int priority,
	L4_Time_t ts_len,
	L4_Time_t total_quantum);

extern void join_thread(L4_ThreadId_t tid);


/* from delay.c */

extern unsigned long iters_per_tick;

extern void calibrate_delay_loop(void);
extern void delay_loop(unsigned long iters);
extern void nsleep(unsigned long nanoseconds);
extern void usleep(unsigned long microseconds);


/* from log.c */

extern int log_f(const char *fmt, ...);
extern void flush_log(bool print);


/* from tap.c */

extern void _fail_unless(
	int result,
	const char *file,
	int line,
	const char *expr,
	...);

#endif
