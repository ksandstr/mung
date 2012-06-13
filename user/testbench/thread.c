
/* simple threading for the purposes of the testbench personality. */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <assert.h>
#include <ccan/likely/likely.h>

#include <l4/types.h>
#include <l4/thread.h>
#include <l4/ipc.h>
#include <l4/schedule.h>
#include <l4/syscall.h>

#include "defs.h"


#define THREAD_STACK_SIZE (32 * 1024)
#define MAX_THREADS 12


static uint8_t *thread_stack[MAX_THREADS];
static int thread_version[MAX_THREADS];
static bool thread_alive[MAX_THREADS];
static int base_tnum;
static L4_Word_t utcb_base;


static L4_ThreadId_t tid_of(int t) {
	return L4_GlobalId(base_tnum + t, abs(thread_version[t]));
}


static void init_threading(void)
{
	for(int i=0; i < MAX_THREADS; i++) {
		thread_version[i] = 0;
		thread_alive[i] = false;
		thread_stack[i] = NULL;
	}

	base_tnum = L4_ThreadNo(L4_Myself()) + 2;
	/* TODO: pull mask, UTCB size from KIP */
	utcb_base = (L4_MyLocalId().raw & ~511ul) + 512 + 512;
}


static void thread_wrapper(void)
{
	L4_ThreadId_t self = L4_MyGlobalId();
	L4_ThreadId_t parent = { .raw = L4_UserDefinedHandle() };
	L4_Set_ExceptionHandler(parent);

	L4_Word_t tnum;
	L4_MsgTag_t tag = L4_Receive(parent);
	if(L4_IpcFailed(tag)) {
		printf("%s: initial IPC failed (ec %#lx), doing early exit\n",
			__func__, L4_ErrorCode());
		tnum = L4_ThreadNo(self) - base_tnum;
		assert(tnum < MAX_THREADS);
		goto end;
	}
	L4_Word_t fn, param;
	L4_StoreMR(1, &fn);
	L4_StoreMR(2, &param);
	L4_StoreMR(3, &tnum);

	(*(void (*)(void *))fn)((void *)param);
#if 0
	printf("testbench thread %d (%u:%u) terminating\n", (int)tnum,
		L4_ThreadNo(self), L4_Version(self));
#endif

end:
	thread_version[tnum] = -thread_version[tnum];
	asm volatile ("int $1");
}


L4_ThreadId_t start_thread(void (*fn)(void *param), void *param)
{
	/* default timeslice is 50 ms just to avoid preemptions. */
	return start_thread_long(fn, param, -1, L4_TimePeriod(3 * 1000),
		L4_Never);
}


L4_ThreadId_t start_thread_long(
	void (*fn)(void *param),
	void *param,
	int priority,
	L4_Time_t ts_len,
	L4_Time_t total_quantum)
{
	static bool first = true;
	if(unlikely(first)) {
		init_threading();
		first = false;
	}

	int t;
	for(t = 0; t < MAX_THREADS; t++) {
		if(!thread_alive[t]) {
			assert(thread_version[t] <= 0);
			thread_version[t] = -thread_version[t] + 1;
			if(thread_version[t] >= 1 << 14) thread_version[t] = 1;
			break;
		}
	}
	if(t == MAX_THREADS) return L4_nilthread;

	thread_alive[t] = true;
	assert(thread_version[t] > 0);

	L4_ThreadId_t self = L4_Myself(), tid = tid_of(t);
#if 0
	printf("%s: creating thread %u:%u, utcb at %#lx\n", __func__,
		L4_ThreadNo(tid), L4_Version(tid), utcb_base + t * 512);
#endif
	L4_Word_t r = L4_ThreadControl(tid, self, self, L4_Pager(),
		(void *)(utcb_base + t * 512));
	if(r == 0) {
		printf("%s: ThreadControl failed, ErrorCode %#lx\n", __func__,
			L4_ErrorCode());
		thread_version[t] = -thread_version[t];
		assert(!thread_alive[t]);
		assert(thread_version[t] <= 0);
		return L4_nilthread;
	}

	uint8_t *stack = malloc(THREAD_STACK_SIZE);
	if(stack == NULL) {
		printf("%s: can't allocate stack for thread!\n", __func__);
		abort();
	}
	thread_stack[t] = stack;
	L4_Word_t stk_top = (L4_Word_t)stack + THREAD_STACK_SIZE - 16;
	L4_Set_UserDefinedHandleOf(tid, self.raw);
	if(priority != -1) {
		L4_Word_t r = L4_Set_Priority(tid, priority);
		if(r == L4_SCHEDRESULT_ERROR) {
			printf("%s: L4_Set_Priority() failed: errorcode %lu\n",
				__func__, L4_ErrorCode());
			/* TODO: cleanups? */
			return L4_nilthread;
		}
	}
	L4_Set_Timeslice(tid, ts_len, total_quantum);
	L4_Start_SpIp(tid, stk_top, (L4_Word_t)&thread_wrapper);
	L4_LoadMR(0, (L4_MsgTag_t){ .X.u = 3 }.raw);
	L4_LoadMR(1, (L4_Word_t)fn);
	L4_LoadMR(2, (L4_Word_t)param);
	L4_LoadMR(3, t);
	L4_MsgTag_t tag = L4_Send(tid);
	if(L4_IpcFailed(tag)) {
		printf("%s: initial IPC failed, ErrorCode %#lx\n", __func__,
			L4_ErrorCode());
		/* TODO: cleanups! */
		return L4_nilthread;
	}

	return tid;
}


void join_thread(L4_ThreadId_t tid)
{
	int t = L4_ThreadNo(tid) - base_tnum;
	assert(t < MAX_THREADS);
	assert(abs(thread_version[t]) == L4_Version(tid));

	L4_MsgTag_t tag = L4_Receive(tid_of(t));
	if(L4_IpcFailed(tag)) {
		printf("%s: receive from thread failed, ec %#lx\n", __func__,
			L4_ErrorCode());
		return;
	}
	/* TODO: verify the exception message (label, GP#) */

	/* destroy the thread. */
	L4_Word_t res = L4_ThreadControl(tid_of(t), L4_nilthread,
		L4_nilthread, L4_nilthread, (void *)-1);
	if(res == 0) {
		printf("%s: deleting ThreadControl failed, ec %#lx\n", __func__,
			L4_ErrorCode());
		return;
	}

	thread_alive[t] = false;
	thread_version[t] = -abs(thread_version[t]);
	free(thread_stack[t]);
	thread_stack[t] = NULL;
}
