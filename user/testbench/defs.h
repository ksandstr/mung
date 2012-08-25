
#ifndef SEEN_TESTBENCH_DEFS_H
#define SEEN_TESTBENCH_DEFS_H

#include <stdio.h>
#include <stdarg.h>
#include <stdbool.h>
#include <ccan/compiler/compiler.h>

#include <l4/types.h>

/* TODO: fetch from KIP at init */
#define PAGE_SIZE 4096
#define PAGE_MASK 0xfff


struct Suite;


/* various test suites */

extern struct Suite *process_suite(void);
extern struct Suite *sched_suite(void);
extern struct Suite *space_suite(void);
extern struct Suite *thread_suite(void);


/* pre-formal tests */
extern void threadctl_test(void);
extern void thread_test(void);

extern void legacy_tests(void);		/* from legacy.c */


/* from testbench.c */

extern void add_fs_tid(L4_Word_t space_id, L4_ThreadId_t tid);


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
extern void for_each_thread(
	void (*fn)(L4_ThreadId_t tid, void *ptr),
	void *ptr);

extern void tsd_key_create(int *key_p, void (*destructor)(void *ptr));
extern void tsd_set(int key, void *ptr);
extern void *tsd_get(int key);


/* from delay.c */

extern unsigned long iters_per_tick;

extern void calibrate_delay_loop(void);
extern void delay_loop(unsigned long iters);
extern void nsleep(unsigned long nanoseconds);
extern void usleep(unsigned long microseconds);


/* from heap.c */

extern void *sbrk(intptr_t adjustment);

extern L4_Word_t find_phys_mem_top(void);
extern L4_Word_t get_heap_top(void);	/* valid after first sbrk() */

/* (no attrs are defined, so pass 0.) */
extern L4_Fpage_t sigma0_get_page(L4_Fpage_t page, L4_Word_t attrs);

/* "adjustment" allows programs to step their own heap down from what
 * heap_init() would otherwise give. this lets forkserv have a private heap in
 * a different location from that used by testbench before forkserv's launch.
 */
extern void heap_init(int adjustment);

/* when set to true, sbrk() will use the forkserv sbrk protocol instead of
 * asking sigma0 for more delicious RAMs.
 */
extern bool use_forkserv_sbrk;


/* from process.c */

/* returns address space ID on parent, 0 on child. */
extern int fork(void);
extern int wait(int *status);
extern NORETURN void exit(int status);


/* from log.c */

extern int log_f(const char *fmt, ...);
extern void flush_log(bool print);

#endif
