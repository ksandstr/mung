
#ifndef SEEN_TESTBENCH_DEFS_H
#define SEEN_TESTBENCH_DEFS_H

#include <stdio.h>
#include <stdarg.h>
#include <stdbool.h>
#include <ccan/compiler/compiler.h>

#include <l4/types.h>


struct Suite;


/* various test suites */

extern struct Suite *sched_suite(void);
extern struct Suite *space_suite(void);


/* pre-formal tests */
extern void threadctl_test(void);
extern void thread_test(void);


/* from thread.c */

extern L4_ThreadId_t start_thread(void (*fn)(void *), void *param);
extern L4_ThreadId_t start_thread_long(
	void (*fn)(void *),
	void *param,
	int priority,
	L4_Time_t ts_len,
	L4_Time_t total_quantum);

extern NORETURN void exit_thread(void *return_value);
extern void *join_thread(L4_ThreadId_t tid);


/* from delay.c */

extern unsigned long iters_per_tick;

extern void calibrate_delay_loop(void);
extern void delay_loop(unsigned long iters);
extern void nsleep(unsigned long nanoseconds);
extern void usleep(unsigned long microseconds);


/* from log.c */

extern int log_f(const char *fmt, ...);
extern void flush_log(bool print);

#endif
