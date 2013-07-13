
/* simple threading for the purposes of the testbench personality. */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <assert.h>
#include <ccan/likely/likely.h>
#include <ccan/compiler/compiler.h>

#include <l4/types.h>
#include <l4/thread.h>
#include <l4/ipc.h>
#include <l4/schedule.h>
#include <l4/syscall.h>

#include <ukernel/util.h>

#include "defs.h"
#include "forkserv.h"


#define THREAD_STACK_SIZE (32 * 1024)
#define MAX_THREADS 12

#define PREJOIN_LABEL 0xf00d


struct thread
{
	uint8_t *stack;
	int version;
	bool alive;
	void *retval;
};


static struct thread threads[MAX_THREADS];
static int base_tnum;
static L4_Word_t utcb_base;


static L4_ThreadId_t tid_of(int t) {
	return L4_GlobalId(base_tnum + t, abs(threads[t].version));
}


static COLD void init_threading(void)
{
	for(int i=0; i < MAX_THREADS; i++) {
		threads[i] = (struct thread){ .version = -1 };
	}

	base_tnum = L4_ThreadNo(L4_Myself());
	utcb_base = L4_MyLocalId().raw & ~511ul;	/* TODO: get mask from KIP */

	struct thread *self = &threads[0];
	self->stack = NULL;
	self->version = L4_Version(L4_Myself());
	self->alive = true;
}


int thread_self(void) {
	return L4_ThreadNo(L4_Myself()) - base_tnum;
}


int thread_on_fork(
	L4_ThreadId_t *caller_tid,
	L4_Word_t caller_ip,
	L4_Word_t caller_sp,
	int new_base_tnum)
{
	/* TODO: run atfork-style child-side hooks? */

	assert(L4_IsGlobalId(*caller_tid));
	int caller = L4_ThreadNo(*caller_tid) - base_tnum;
	assert(caller >= 0 && caller < MAX_THREADS);
	struct thread copy = threads[caller];
	base_tnum = new_base_tnum;
	for(int i=0; i < MAX_THREADS; i++) {
		if(!threads[i].alive) continue;

		if(i != caller) {
			free(threads[i].stack);
			threads[i].stack = NULL;
			threads[i].alive = false;
			threads[i].version = -abs(threads[i].version);
			threads[i].retval = NULL;

			/* TODO: destroy TSD bits for threads[i] */
		}
	}

	/* set up thread context for the child starter thread */
	int starter = L4_ThreadNo(L4_Myself()) - base_tnum;
	assert(starter < MAX_THREADS);
	threads[starter] = (struct thread){
		.version = L4_Version(L4_Myself()), .alive = true,
		/* (could use .stack to free the starter thread's param->stk_top. why
		 * bother though.)
		 */
	};

	/* TODO: instead figure out the caller's priority. */
	int pri = find_own_priority();
	int n = forkserv_new_thread(L4_Pager(), &caller_tid->raw, ~0ul,
		caller_ip, caller_sp, caller, L4_TimePeriod(10 * 10000), L4_Never,
		pri, pri, 0);
	if(n != 0) {
		printf("%s: new_thread failed, n=%d\n", __func__, n);
		abort();
	}
	int new_caller = L4_ThreadNo(L4_GlobalIdOf(*caller_tid)) - base_tnum;
	assert(new_caller == caller);	/* avoids cleaning threads[caller] */
	threads[caller] = copy;
	threads[caller].version = L4_Version(*caller_tid);

	return 0;
}


static void thread_wrapper(L4_ThreadId_t parent)
{
	L4_Set_UserDefinedHandle(0);
	L4_Set_ExceptionHandler(parent);

	L4_MsgTag_t tag = L4_Receive(parent);
	if(L4_IpcFailed(tag)) {
		printf("%s: initial IPC failed (ec %#lx), doing early exit\n",
			__func__, L4_ErrorCode());
		goto end;
	}
	L4_Word_t fn, param, tnum;
	L4_StoreMR(1, &fn);
	L4_StoreMR(2, &param);
	L4_StoreMR(3, &tnum);	/* TODO: not used -- remove. */

	(*(void (*)(void *))fn)((void *)param);

end:
	exit_thread(NULL);
}


void exit_thread(void *return_value)
{
	int tnum = L4_ThreadNo(L4_MyGlobalId()) - base_tnum;
	assert(tnum < MAX_THREADS);
	threads[tnum].retval = return_value;
	threads[tnum].version = -threads[tnum].version;

	tsd_clear();

	/* wait for message from the join_thread() caller, which may be any local
	 * thread.
	 */
	L4_MsgTag_t tag;
	L4_ThreadId_t new_parent;
	do {
		tag = L4_WaitLocal_Timeout(L4_Never, &new_parent);
		if(L4_IpcFailed(tag)) {
			printf("%s: local receive failed, ec=%#lx\n", __func__,
				L4_ErrorCode());
		} else if(tag.X.label != PREJOIN_LABEL) {
			printf("%s: weird IPC from %lu:%lu, tag=%#08lx\n", __func__,
				L4_ThreadNo(new_parent), L4_Version(new_parent),
				tag.raw);
		}
	} while(!L4_IpcSucceeded(tag) || tag.X.label != PREJOIN_LABEL);
	assert(!L4_IsNilThread(new_parent));
	L4_Set_ExceptionHandler(new_parent);

	for(;;) {
		asm volatile ("int $1");
	}
}


L4_ThreadId_t start_thread(void (*fn)(void *param), void *param)
{
	/* default timeslice is 50 ms just to avoid preemptions. */
	return start_thread_long(fn, param, -1,
		L4_TimePeriod(50 * 1000), L4_Never);
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
		first = false;
		init_threading();
	}

	int t;
	for(t = 0; t < MAX_THREADS; t++) {
		if(!threads[t].alive) {
			assert(threads[t].version <= 0);
			int newver = -threads[t].version + 1;
			if(newver >= 1 << 14) newver = 2;	/* wrap */
			else if((newver & 63) == 0) {
				/* avoid looking like a local thread ID */
				newver |= 1;
			}
			assert((newver & 63) != 0 && newver != 1);
			threads[t].version = newver;
			break;
		}
	}
	if(t == MAX_THREADS) return L4_nilthread;
	L4_ThreadId_t self = L4_Myself(), tid = tid_of(t);
	assert(L4_IsGlobalId(tid));

	threads[t].alive = true;
	assert(threads[t].version > 0);
	uint8_t *stack = malloc(THREAD_STACK_SIZE);
	if(stack == NULL) {
		printf("%s: can't allocate stack for thread!\n", __func__);
		abort();
	}
	threads[t].stack = stack;
	L4_Word_t stk_top = ((L4_Word_t)stack + THREAD_STACK_SIZE - 16) & ~0xfu;
#ifdef __SSE__
	/* align for the parameter. (FIXME: this is poorly understood. why does 4
	 * give the right alignment, but not 8?)
	 */
	stk_top += 4;
#endif
	L4_Word_t *sptr = (L4_Word_t *)stk_top;
	*(--sptr) = self.raw;
	*(--sptr) = 0xbabecafe;		/* >implying human trafficking */
	stk_top = (L4_Word_t)sptr;

	if(!is_privileged()) {
		/* FIXME: add setting of timeslice and priority, i.e. change forkserv
		 * to set the new_thread caller as the new thread's scheduler.
		 */
		L4_ThreadId_t out_tid;
		int n = forkserv_new_thread(L4_Pager(), &out_tid.raw, ~0ul,
			(L4_Word_t)&thread_wrapper, stk_top, t, ts_len, total_quantum,
			priority, priority, 0);
		if(n != 0 || L4_ThreadNo(out_tid) - base_tnum != t) {
			printf("%s: forkserv_new_thread() failed, n=%d, out_tid %lu:%lu (%d)\n",
				__func__, n, L4_ThreadNo(out_tid), L4_Version(out_tid),
				(int)L4_ThreadNo(out_tid) - base_tnum);
			/* TODO: problem, officer? */
			return L4_nilthread;
		}

		tid = out_tid;
		threads[t].version = L4_Version(tid);
	} else {
		L4_Word_t r = L4_ThreadControl(tid, self, self, L4_Pager(),
			(void *)(utcb_base + t * 512));
		if(r == 0) {
			printf("%s: ThreadControl failed, ErrorCode %#lx\n", __func__,
				L4_ErrorCode());
			threads[t].version = -threads[t].version;
			assert(!threads[t].alive);
			assert(threads[t].version <= 0);
			return L4_nilthread;
		}

		/* let forkserv know this should be paged for testbench, for which
		 * space_id=1
		 */
		add_fs_tid(1, tid);

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
	}

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


void *join_thread_long(L4_ThreadId_t tid, L4_Time_t timeout, L4_Word_t *ec_p)
{
	if(L4_IsNilThread(tid)) return NULL;
	tid = L4_GlobalIdOf(tid);

	int t = L4_ThreadNo(tid) - base_tnum;
	assert(t < MAX_THREADS);
	assert(abs(threads[t].version) == L4_Version(tid));

	L4_LoadMR(0, (L4_MsgTag_t){ .X.label = PREJOIN_LABEL }.raw);
	L4_MsgTag_t tag = L4_Send_Timeout(tid_of(t), timeout);
	if(L4_IpcFailed(tag)) {
		*ec_p = L4_ErrorCode();
		return NULL;
	}

	tag = L4_Receive_Timeout(tid_of(t), timeout);
	if(L4_IpcFailed(tag)) {
		L4_Word_t ec = L4_ErrorCode();
		if(ec != 3) {
			printf("%s: receive from thread failed, ec %#lx\n", __func__, ec);
		}
		*ec_p = ec;
		return NULL;
	}
	/* TODO: verify the exception message (label, GP#) */

	/* destroy the thread. */
	if(is_privileged()) {
		L4_Word_t res = L4_ThreadControl(tid_of(t), L4_nilthread,
			L4_nilthread, L4_nilthread, (void *)-1);
		if(res == 0) {
			printf("%s: deleting ThreadControl failed, ec %#lx\n", __func__,
				L4_ErrorCode());
			return NULL;
		}
	} else {
		int n = forkserv_exit_thread(L4_Pager(), tid.raw);
		if(n != 0) {
			printf("%s: forkserv_exit_thread failed, n=%d\n", __func__, n);
			/* FIXME: ... and then what? */
		}
	}

	threads[t].alive = false;
	threads[t].version = -abs(threads[t].version);
	free(threads[t].stack);
	threads[t].stack = NULL;
	void *rv = threads[t].retval;
	threads[t].retval = NULL;
	return rv;
}


void *join_thread(L4_ThreadId_t tid) {
	L4_Word_t dummy = 0;
	return join_thread_long(tid, L4_Never, &dummy);
}


void for_each_thread(void (*fn)(L4_ThreadId_t tid, void *ptr), void *ptr)
{
	for(int i=0; i < MAX_THREADS; i++) {
		if(!threads[i].alive) continue;
		L4_ThreadId_t tid = L4_GlobalId(base_tnum + i, threads[i].version);
		(*fn)(tid, ptr);
	}
}
