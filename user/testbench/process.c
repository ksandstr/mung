
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ccan/likely/likely.h>
#include <ccan/compiler/compiler.h>

#include <l4/types.h>
#include <l4/ipc.h>
#include <l4/thread.h>
#include <l4/syscall.h>

#include <ukernel/util.h>

#include "defs.h"
#include "forkserv.h"


#define SYNC_CHILD_LABEL 0xface


struct child_param
{
	L4_ThreadId_t parent_tid;	/* init message sender */
	L4_ThreadId_t fork_tid;		/* fork() caller's TID */
	void *stk_top;				/* base address of child_starter_fn() stack */
	int exn_size;				/* # of regs incl. tag at [0] */
	L4_Word_t exn_frame[];
};


static L4_ThreadId_t mgr_tid = { .raw = 0 };
static bool is_roottask = true;
static int task_pid = 1;


static void child_starter_fn(struct child_param *param);


bool is_privileged(void) {
	return is_roottask;
}


int getpid(void)
{
	assert(!is_roottask || task_pid == 1);
	assert(is_roottask || task_pid > 1);
	return task_pid;
}


static void stop_local_thread(L4_ThreadId_t tid, void *ptr)
{
	const L4_ThreadId_t *skip = ptr;
	for(int i=0; !L4_IsNilThread(skip[i]); i++) {
		if(tid.raw == skip[i].raw) return;
	}

	/* what this does to pending string transfers, I don't know. probably
	 * doesn't matter.
	 */
	L4_Stop(tid);
}


static void start_local_thread(L4_ThreadId_t tid, void *ptr)
{
	const L4_ThreadId_t *skip = ptr;
	for(int i=0; !L4_IsNilThread(skip[i]); i++) {
		if(tid.raw == skip[i].raw) return;
	}

	/* L4_Start() breaks send and receive IPC. this doesn't. */
	L4_ThreadId_t dummy_tid;
	L4_Word_t dummy;
	L4_ThreadId_t out = L4_ExchangeRegisters(tid, 0x100, 0, 0, 0, 0,
		L4_nilthread, &dummy, &dummy, &dummy, &dummy, &dummy,
		&dummy_tid);
	if(L4_IsNilThread(out)) {
		printf("thread %lu:%lu restart failed: ec %#lx\n",
			L4_ThreadNo(tid), L4_Version(tid), L4_ErrorCode());
	}
}


static bool handle_int(
	L4_ThreadId_t from,
	int int_num,
	L4_Word_t *exn_regs,
	int num_exn_regs)
{
	if(int_num != 23) {
		printf("%s: unhandled INT $%d in %#lx at eip %#lx\n",
			__func__, int_num, from.raw, exn_regs[0]);
		return false;
	}

	/* allocate memory for the child process entry thread's stack, and prepare
	 * a parameter structure.
	 */
	size_t param_size = sizeof(struct child_param)
			+ sizeof(L4_Word_t) * num_exn_regs,
		ct_stack_size = 16 * 1024;
	void *ct_stack = malloc(ct_stack_size);
	struct child_param *param = ct_stack + ct_stack_size - param_size - 32;
	param->parent_tid = L4_Myself();
	param->fork_tid = from;
	param->stk_top = ct_stack;
	param->exn_size = num_exn_regs + 1;
	memcpy(param->exn_frame, exn_regs, sizeof(L4_Word_t) * param->exn_size);
	/* prepare the stack for a call to child_starter_fn(). */
	L4_Word_t *stk_pos = (void *)param;
	*(--stk_pos) = (L4_Word_t)param;
	*(--stk_pos) = 0xbadc0d3;		/* returning would be bad indeed. */

	/* stop all local threads, except this one and the caller. */
	L4_ThreadId_t non[3] = { L4_Myself(), from, L4_nilthread };
	for_each_thread(&stop_local_thread, non);

	/* call fork. */
	int32_t retval = -1;
	int n = forkserv_fork(L4_Pager(), &retval);
	if(n != 0) {
		printf("forkserv_fork() failed, n=%d\n", n);
		retval = -1;		/* give up, go home */
	}

	/* restart our own threads now, and release the child stack on
	 * this side.
	 */
	for_each_thread(&start_local_thread, non);
	free(ct_stack);

	if(CHECK_FLAG(retval, 0x80000000ul)) goto end;

	/* create the child process' entry thread. */
	L4_ThreadId_t cp_tid;
	int my_pri = find_own_priority();
	n = forkserv_new_thread(L4_Pager(), &cp_tid.raw, retval,
		(L4_Word_t)&child_starter_fn, (L4_Word_t)stk_pos, thread_self(),
		L4_TimePeriod(10 * 1000), L4_Never, my_pri, my_pri, 0);
	if(n != 0) {
		/* TODO: cleanup */
		printf("%s: ffffuuuuuu (n=%d)\n", __func__, n);
		abort();
	}

	/* send it off. hand it the new base tnum also.
	 *
	 * TODO: could handshake with the child to confirm its side of the fork
	 * has succeeded in recreating the caller thread etc.
	 */
	L4_LoadMR(0, (L4_MsgTag_t){ .X.u = 1 }.raw);
	L4_LoadMR(1, L4_ThreadNo(cp_tid) - thread_self());
	L4_MsgTag_t tag = L4_Send(cp_tid);
	if(L4_IpcFailed(tag)) {
		printf("%s: child starter ipc failed, ec=%#lx\n", __func__,
			L4_ErrorCode());
	}

end:
	/* return the same stack frame, but set %eax to the fork() return value.
	 * (should also pass errno... or would if this were a proper C runtime.)
	 */
	L4_LoadMR(0, (L4_MsgTag_t){ .X.u = num_exn_regs }.raw);
	assert(num_exn_regs >= 12);
	exn_regs[10] = 0;			/* %ecx */
	exn_regs[11] = retval;		/* %eax */
	exn_regs[0] += 2;			/* %eip; bump for INT $n (2 bytes) */
	L4_LoadMRs(1, num_exn_regs, exn_regs);
	return true;
}


static void proc_mgr_fn(void *parameter UNUSED)
{
	for(;;) {
		L4_ThreadId_t from;
		L4_MsgTag_t tag = L4_Wait(&from);

		for(;;) {
			if(L4_IpcFailed(tag)) break;

			bool reply;
			if((tag.X.label & 0xfff0) == 0xffb0) {
				/* architecture exception. */
				L4_Word_t exn_regs[15];
				int exn_saved = MIN(int, tag.X.u, 15);
				L4_StoreMRs(1, exn_saved, exn_regs);
				L4_Word_t code = exn_regs[3];
				if((code & 0x7) == 2 && ((code - 2) >> 3) > 0) {
					reply = handle_int(from, (code - 2) >> 3, exn_regs,
						exn_saved);
				} else {
					printf("%s: exn %#lx (code %#lx) at eip %#lx\n", __func__,
						exn_regs[2], exn_regs[3], exn_regs[0]);
					reply = false;
				}
			} else {
				printf("%s: unknown label %#lx from %#lx\n", __func__,
					(L4_Word_t)tag.X.label, from.raw);
				reply = false;
			}

			if(!reply) break;
			tag = L4_ReplyWait(from, &from);
		}
	}
}


static void pop_int24_to(L4_ThreadId_t exh_tid)
{
	L4_Set_ExceptionHandler(exh_tid);
	asm volatile ("int $24");
}


static void child_starter_fn(struct child_param *param)
{
	/* initialization message from the parent process */
	L4_MsgTag_t tag = L4_Receive(param->parent_tid);
	if(L4_IpcFailed(tag)) {
		printf("%s: init IPC failed, code %#lx\n", __func__, L4_ErrorCode());
		goto fail;
	}
	L4_Word_t new_base_tnum;
	L4_StoreMR(1, &new_base_tnum);

	/* the technique is this: create a page-sized temporary stack for
	 * pop_int24_to(), which contains the exception handler parameter that
	 * receives a #GP for int $24. this exception is used to reload the fork
	 * caller's frame in that thread.
	 */
	void *popstack = malloc(4096);
	L4_Word_t *top = popstack + 4096 - 32;
	*(--top) = L4_Myself().raw;
	*(--top) = 0xb44dc0d3;		/* baaaaad. */
	L4_ThreadId_t caller_tid = param->fork_tid;
	if(thread_on_fork(&caller_tid, (L4_Word_t)&pop_int24_to,
		(L4_Word_t)top, new_base_tnum) != 0)
	{
		printf("%s: thread_on_fork failed\n", __func__);
		/* FIXME: abort properly */
		goto fail;
	}

	tag = L4_Receive(caller_tid);
	if(L4_IpcFailed(tag)) {
		printf("%s: exception receive failed, ec %#lx\n", __func__,
			L4_ErrorCode());
		goto fail;
	} else if((tag.X.label & 0xfff0) != 0xffb0) {
		printf("%s: weird exception label %#lx\n", __func__,
			(L4_Word_t)tag.X.label);
		goto fail;
	}
	free(popstack);
	L4_Word_t frame[64];
	int frame_len = MIN(int, 63, tag.X.u + tag.X.t);
	L4_StoreMRs(1, frame_len, &frame[1]);
	frame[0] = tag.raw;
	param->exn_frame[10] = (L4_Word_t)param;	/* %ecx */
	param->exn_frame[11] = 0;		/* %eax */
	param->exn_frame[0] += 2;		/* skip INT $n (2 bytes) */
	assert(param->exn_size >= frame_len + 1);
	L4_LoadMR(0, (L4_MsgTag_t){ .X.u = param->exn_size - 1 }.raw);
	L4_LoadMRs(1, param->exn_size - 1, param->exn_frame);
	tag = L4_Reply(caller_tid);
	if(L4_IpcFailed(tag)) {
		printf("%s: exception frame reply failed, ec %#lx\n", __func__,
			L4_ErrorCode());
		goto fail;
	}

	/* sync with the child side so that this thread can be properly joined off
	 * without interfering with process exit.
	 */
	L4_LoadMR(0, (L4_MsgTag_t){ .X.label = SYNC_CHILD_LABEL }.raw);
	tag = L4_Call(caller_tid);
	if(L4_IpcFailed(tag)) {
		printf("%s: child sync failed, ec %#lx\n", __func__, L4_ErrorCode());
		/* FIXME? */
		goto fail;
	}

	L4_Set_ExceptionHandler(caller_tid);
	exit_thread(NULL);

fail:
	exit_thread("child_starter_fn failed");
}


int fork(void)
{
	if(unlikely(L4_IsNilThread(mgr_tid))) {
		mgr_tid = start_thread(&proc_mgr_fn, NULL);
	}

	L4_ThreadId_t old_exh = L4_ExceptionHandler();
	L4_Set_ExceptionHandler(mgr_tid);

	bool was_roottask = is_roottask;
	is_roottask = false;
	int retval;
	struct child_param *param_ptr = NULL;
	asm volatile ("int $23": "=a" (retval), "=c" (param_ptr) :: "memory");

	if(retval != 0) {
		/* parent */
		L4_Set_ExceptionHandler(old_exh);
		is_roottask = was_roottask;
	} else {
		/* child */
		mgr_tid = L4_nilthread;
		assert(!is_roottask);

		/* ensure sync with child_starter_fn(), which is seen as the exception
		 * handler due to pop_int24_to()
		 */
		L4_ThreadId_t starter = L4_ExceptionHandler();
		L4_MsgTag_t tag = L4_Receive(starter);
		if(L4_IpcFailed(tag) || tag.X.label != SYNC_CHILD_LABEL) {
			printf("fork (child side): sync failed, ec %#lx (label %#lx)\n",
				L4_ErrorCode(), (L4_Word_t)tag.X.label);
			goto child_fail;
		}
		L4_LoadMR(0, 0);
		tag = L4_Reply(starter);
		if(L4_IpcFailed(tag)) {
			printf("fork (child side): sync reply failed, ec %#lx\n",
				L4_ErrorCode());
			goto child_fail;
		}

		void *start_val = join_thread(starter);
		if(start_val != NULL) {
			printf("fork (child side): starter join failed\n");
			goto child_fail;
		}

		L4_Set_ExceptionHandler(L4_nilthread);

		/* now, who the fuck was I again? */
		int32_t val;
		int n = forkserv_getpid(L4_Pager(), &val);
		if(n != 0) {
			printf("fork (child side): can't get own pid, n=%d\n", n);
			goto child_fail;
		}
		if(val <= 1) {
			printf("fork (child side): getpid() failed\n");
			goto child_fail;
		}
		task_pid = val;
	}
	if(param_ptr != NULL) {
		free(param_ptr->stk_top);	/* also destroys param */
	}

	return retval;

child_fail:
	for(;;) exit(-1);		/* or something */
}


int fork_tid(L4_ThreadId_t *tid_p)
{
	L4_MsgTag_t tag;
	L4_ThreadId_t parent = L4_MyGlobalId();
	int pid = fork();
	if(pid != 0) {
		tag = L4_Wait(tid_p);
	} else {
		*tid_p = L4_nilthread;
		L4_LoadMR(0, 0);
		tag = L4_Send(parent);
	}
	if(L4_IpcFailed(tag)) {
		printf("%s: ec %#lx\n", __func__, L4_ErrorCode());
		abort();
	}

	return pid;
}


int wait(int *status)
{
	int32_t id;
	int n = forkserv_wait(L4_Pager(), &id, status);
	return n == 0 ? id : -1;
}


void exit(int status)
{
	for(int retry = 0; retry < 5; retry++) {
		forkserv_exit(L4_Pager(), status);
	}
	/* ultimate failure path */
	asm volatile ("int $1");

	/* double ultimate failure path */
	L4_Set_ExceptionHandler(L4_nilthread);
	for(;;) {
		asm volatile ("int $1");
	}
}
