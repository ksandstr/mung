
#ifndef SEEN_TESTBENCH_DEFS_H
#define SEEN_TESTBENCH_DEFS_H


extern void threadctl_test(void);
extern void sched_test(void);


/* from thread.c */

extern L4_ThreadId_t start_thread(void (*fn)(void *), void *param);
extern void join_thread(L4_ThreadId_t tid);


#endif
