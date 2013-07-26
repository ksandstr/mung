
#ifndef SEEN_TESTBENCH_DEFS_H
#define SEEN_TESTBENCH_DEFS_H

#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <assert.h>
#include <ccan/compiler/compiler.h>

#include <l4/types.h>
#include <muidl.h>

#include "common-defs.h"


/* TODO: fetch from KIP at init */
#define PAGE_BITS 12
#define PAGE_SIZE (1 << PAGE_BITS)
#define PAGE_MASK (PAGE_SIZE - 1)
#define UTCB_SIZE 512

/* the appropriate delay for IPC reception before failure. applied wherever
 * it's necessary to let the other get scheduled first.
 */
#define TEST_IPC_DELAY L4_TimePeriod(100 * 1000)

/* common IPC labels */
#define QUIT_LABEL 0xdead
#define RESET_LABEL 0xf579
#define DELAY_LABEL	0x7a5a		/* "zZ" */

#define LOG_SIZE DROP_PAGER_LOG_SIZE


#define btos(x) (!!(x) ? "true" : "false")


/* declare and use an IDL dispatcher fixture. symbols static by default.
 * idl_fixture_teardown() is defined in util.c . setting quit_cond to false
 * makes it perpetual, which is usually not how test fixtures are supposed to
 * behave.
 *
 * externally defined IDL dispatchers would typically say FIX_QUIT_COND, which
 * tests a TSD key; correspondingly, IDL dispatchers that implement Quittable
 * will point the vtable's .quit to idl_fixture_quit which is a function that
 * sets that same key.
 */
#define IDL_FIXTURE(fixture_name, iface_name, vtable, quit_cond) \
static L4_ThreadId_t fixture_name ## _tid; \
static int fixture_name ## _pid = -1; \
static void fixture_name ## _thread_fn(void *param UNUSED) { \
	while(!(quit_cond)) { \
		L4_Word_t status = _muidl_ ## iface_name ## _dispatch((vtable)); \
		if(status == MUIDL_UNKNOWN_LABEL \
			&& muidl_get_tag().X.label == 0xcbad) \
		{ \
			/* ignore (used by idl_fixture_teardown(); causes re-eval of the \
			 * exit condition) \
			 */ \
		} else if(status != 0 && !MUIDL_IS_L4_ERROR(status)) { \
			printf("%s:%s: dispatch status %#lx (last tag %#lx)\n", \
				#iface_name, #fixture_name, status, muidl_get_tag().raw); \
		} \
	} \
} \
static inline void fixture_name ## _setup(void) { \
	assert(L4_IsNilThread(fixture_name ## _tid)); \
	fixture_name ## _tid = start_thread(&fixture_name ## _thread_fn, NULL); \
	assert(!L4_IsNilThread(fixture_name ## _tid)); \
} \
static inline void fixture_name ## _teardown(void) { \
	idl_fixture_teardown(fixture_name ## _tid); \
	fixture_name ## _tid = L4_nilthread; \
} \
static inline void fixture_name ## _fork_setup(void) { \
	assert(fixture_name ## _pid == -1); \
	assert(L4_IsNilThread(fixture_name ## _tid)); \
	fixture_name ## _pid = fork_tid(&fixture_name ## _tid); \
	fail_if(fixture_name ## _pid < 0, \
		"failed setup for `%s'", #fixture_name); \
	if(fixture_name ## _pid == 0) { \
		fixture_name ## _thread_fn(NULL); \
		exit(0); \
	} \
} \
static inline void fixture_name ## _fork_teardown(void) { \
	idl_fixture_teardown_fork(fixture_name ## _pid, fixture_name ## _tid); \
	fixture_name ## _tid = L4_nilthread; \
	fixture_name ## _pid = -1; \
}

/* regular fixtures are run _within_ the forked test process.
 * _U fixtures are run in the pre-test process, which may be an unprivileged
 * process. _F fixtures are forked from the pre-test process.
 *
 * (_P fixtures will be run in the privileged roottask process,
 * TODO: and should be added for completeness' sake at some point.)
 */
#define ADD_IDL_FIXTURE(tcase, fixture_name) \
	tcase_add_checked_fixture((tcase), &fixture_name ## _setup, \
		&fixture_name ## _teardown)
#define ADD_IDL_FIXTURE_U(tcase, fixture_name) \
	tcase_add_unchecked_fixture((tcase), &fixture_name ## _setup, \
		&fixture_name ## _teardown)
#define ADD_IDL_FIXTURE_F(tcase, fixture_name) \
	tcase_add_unchecked_fixture((tcase), &fixture_name ## _fork_setup, \
		&fixture_name ## _fork_teardown)

extern void idl_fixture_teardown(L4_ThreadId_t tid);
extern void idl_fixture_teardown_fork(int pid, L4_ThreadId_t tid);

/* Quittable protocol as it ties into IDL fixtures */
extern bool idl_fixture_running(void);
extern void idl_fixture_quit(void);

#define FIX_QUIT_COND (!idl_fixture_running())



struct Suite;


struct pager_stats
{
	int n_faults, n_read, n_write, n_exec, n_fail;
	int log_top;	/* [0 .. LOG_SIZE) */
	L4_Fpage_t log[LOG_SIZE];
};


/* various test suites */

extern struct Suite *self_suite(void);
extern struct Suite *sched_suite(void);
extern struct Suite *space_suite(void);
extern struct Suite *thread_suite(void);
extern struct Suite *ipc_suite(void);
extern struct Suite *string_suite(void);
extern struct Suite *type_suite(void);
extern struct Suite *x86_suite(void);
extern struct Suite *interrupt_suite(void);


/* pre-formal tests */
extern void threadctl_test(void);
extern void thread_test(void);

extern void legacy_tests(void);		/* from legacy.c */


/* from testbench.c */

extern void add_fs_tid(L4_Word_t space_id, L4_ThreadId_t tid);


/* from thread.c */

extern int thread_self(void);

extern L4_ThreadId_t start_thread(void (*fn)(void *), void *param);
extern L4_ThreadId_t start_thread_long(
	void (*fn)(void *),
	void *param,
	int priority,
	L4_Time_t ts_len,
	L4_Time_t total_quantum);

/* NOTE: as there is no manager thread in the testbench process, exit_thread()
 * and join_thread() do an active handshake to make sure that a join_thread()
 * will only terminate a thread that's called exit_thread(). for threads that
 * wait for IPC from L4_anythread or L4_anylocalthread, a join_thread can send
 * PREJOIN_LABEL (0xf00d) with u=t=0 in an attempt to sync with a call to
 * exit_thread().
 *
 * therefore as a rule of thumb, threads that do waits should have an exit
 * handshake that guarantees the next IPC receive will be done from
 * exit_thread(). (that one filters until it hits PREJOIN_LABEL, so the
 * converse problem doesn't apply.)
 *
 * be warned that an out-of-order join_thread() can terribly confuse an
 * unprepared receiver.
 */
extern NORETURN void exit_thread(void *return_value);
extern void *join_thread(L4_ThreadId_t tid);
extern void *join_thread_long(
	L4_ThreadId_t tid,
	L4_Time_t timeout,
	L4_Word_t *ec_p);

extern void for_each_thread(
	void (*fn)(L4_ThreadId_t tid, void *ptr),
	void *ptr);

/* 0 on success, errno on failure. the thread indicated by *caller_tid_p will
 * be preserved & recreated, and the new TID stored in *caller_tid_p . the new
 * thread starts with ip, sp.
 */
extern int thread_on_fork(
	L4_ThreadId_t *caller_tid_p,
	L4_Word_t caller_ip,
	L4_Word_t caller_sp,
	int new_base_tnum);


/* from tsd.c */

extern void tsd_clear(void);
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
extern int fork_tid(L4_ThreadId_t *tid_p);	/* fork(), and return child TID */
extern int wait(int *status);
extern NORETURN void exit(int status);
extern int getpid(void);

extern bool is_privileged(void);


/* from util.c */

extern bool send_reset(L4_ThreadId_t thread);
extern bool send_delay(
	L4_ThreadId_t thread,
	L4_Time_t delay,
	int repeat,
	bool spin);
extern uint32_t rand32(uint32_t *state_p);
extern void random_string(char *buf, size_t size, uint32_t *seed_p);

/* (actually in sched_suite.c) */
extern int find_own_priority(void);

/* (not in util.c anymore.) */
static inline bool send_quit(L4_ThreadId_t serv) {
	return __common_quit_timeout((serv), TEST_IPC_DELAY) == 0;
}


/* from log.c */

extern int log_f(const char *fmt, ...);
extern void flush_log(bool print);


/* from string_suite.c (TODO: move elsewhere!) */

extern void flush_byte_range(
	L4_Word_t first_address,
	L4_Word_t size,
	L4_Word_t access);


/* from pg_stats.c */

/* call these from a checked fixture. */
extern L4_ThreadId_t start_stats_pager(struct pager_stats *stats_mem);
extern L4_Word_t stop_stats_pager(L4_ThreadId_t tid);

/* returns L4_Nilpage when the address wasn't in the log, with rights
 * cleared.
 */
extern L4_Fpage_t get_fault(struct pager_stats *stats, L4_Word_t addr);


/* from pg_drop.c */
struct drop_pager_vtable;
extern struct drop_pager_vtable pg_drop_vtab;

#endif
