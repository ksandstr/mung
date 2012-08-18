
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


struct child_param
{
	L4_ThreadId_t parent_tid;	/* init message sender */
	L4_ThreadId_t fork_tid;		/* fork() caller's TID */
	void *stk_top;				/* base address of child_starter_fn() stack */
	int exn_size;				/* # of regs incl. tag at [0] */
	L4_Word_t exn_frame[];
};


static L4_ThreadId_t mgr_tid = { .raw = 0 };


static void child_starter_fn(struct child_param *param);


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
		printf("thread restart failed: ec %#lx\n", L4_ErrorCode());
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
	param->exn_size = num_exn_regs;
	memcpy(param->exn_frame, exn_regs, sizeof(L4_Word_t) * param->exn_size);
	/* prepare the stack for a call to child_starter_fn(). */
	L4_Word_t *stk_pos = (void *)param;
	*(--stk_pos) = (L4_Word_t)param;
	*(--stk_pos) = 0xbadc0d3;		/* returning would be bad indeed. */

	/* stop all local threads, except this one and the caller. */
	L4_ThreadId_t non[3] = { L4_Myself(), from, L4_nilthread };
	for_each_thread(&stop_local_thread, non);

	/* call fork. */
	L4_LoadMR(0, (L4_MsgTag_t){ .X.label = FORKSERV_FORK }.raw);
	L4_LoadBR(0, 0);
	L4_MsgTag_t tag = L4_Call(L4_Pager());
	L4_Word_t retval;
	if(L4_IpcFailed(tag)) {
		retval = ~0;		/* give up, go home */
	} else {
		L4_StoreMR(1, &retval);
	}

	/* restart our own threads now, and release the child stack on
	 * this side.
	 */
	for_each_thread(&start_local_thread, non);
	free(ct_stack);

	if(CHECK_FLAG(retval, 0x80000000ul)) goto end;

	/* create the child process' entry thread. */
	L4_LoadMR(0, (L4_MsgTag_t){ .X.label = FORKSERV_NEW_THREAD,
		.X.u = 3 }.raw);
	L4_LoadMR(1, retval);		/* space ID */
	L4_LoadMR(2, (L4_Word_t)&child_starter_fn);	/* ip */
	L4_LoadMR(3, (L4_Word_t)stk_pos); /* sp */
	tag = L4_Call(L4_Pager());
	if(L4_IpcFailed(tag)) {
		/* TODO: cleanup */
		printf("%s: ffffuuuuuu (ec %#lx)\n", __func__, L4_ErrorCode());
		abort();
	}
	L4_ThreadId_t cp_tid;
	L4_StoreMR(1, &cp_tid.raw);

	/* send it off.
	 *
	 * TODO: could handshake with the child to confirm its side of the fork
	 * has succeeded in recreating the caller thread etc.
	 */
	L4_LoadMR(0, (L4_MsgTag_t){ .X.u = 0 }.raw);
	L4_Send(cp_tid);

end:
	/* return the same stack frame, but set %eax to the fork() return value.
	 * (should also pass errno... or would if this were a proper C runtime.)
	 */
	L4_LoadMR(0, (L4_MsgTag_t){ .X.u = num_exn_regs }.raw);
	assert(num_exn_regs >= 12);
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


static void child_starter_fn(struct child_param *param)
{
	/* initialization message from the parent process */
	L4_MsgTag_t tag = L4_Receive(param->parent_tid);
	if(L4_IpcFailed(tag)) {
		printf("%s: init IPC failed, code %#lx\n", __func__, L4_ErrorCode());
		goto end;
	}

	/* it doesn't carry any information we'd like to save, though. just a
	 * synchronization.
	 */

	printf("%s: would do child-side fork things now.\n", __func__);
	/* FIXME: do them */

end:
	asm volatile ("int $1");
}


int fork(void)
{
	if(unlikely(L4_IsNilThread(mgr_tid))) {
		mgr_tid = start_thread(&proc_mgr_fn, NULL);
	}

	L4_ThreadId_t old_exh = L4_ExceptionHandler();
	L4_Set_ExceptionHandler(mgr_tid);
	int retval;
	asm volatile ("int $23": "=a" (retval));

	if(retval != 0) {
		/* only restore this in the parent */
		L4_Set_ExceptionHandler(old_exh);
	} else {
		/* child side */
	}

	return retval;
}


int wait(int *status)
{
	L4_LoadMR(0, (L4_MsgTag_t){ .X.label = FORKSERV_WAIT }.raw);
	L4_MsgTag_t tag = L4_Call(L4_Pager());
	if(L4_IpcFailed(tag) || tag.X.u < 2) return -1;

	L4_Word_t st, id;
	L4_StoreMR(1, &id);
	L4_StoreMR(2, &st);
	return id;
}


void exit(int status)
{
	for(;;) {
		asm volatile ("int $1");
	}
}
