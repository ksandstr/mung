
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


static L4_ThreadId_t mgr_tid = { .raw = 0 };


static void child_starter_fn(void);


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

#if 0
	printf("%s: forking in thread %#lx\n", __func__, L4_Myself().raw);
#endif

	/* allocate memory for the child process entry thread's stack. */
	void *ct_stack = malloc(16 * 1024);

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
	L4_LoadMR(3, (L4_Word_t)ct_stack + 16 * 1024 - 32);	/* sp */
	tag = L4_Call(L4_Pager());
	if(L4_IpcFailed(tag)) {
		/* FIXME: cleanup */
		printf("%s: ffffuuuuuu (ec %#lx)\n", __func__, L4_ErrorCode());
		abort();
	}
	L4_ThreadId_t cp_tid;
	L4_StoreMR(1, &cp_tid.raw);

	/* send it off */
	L4_LoadMR(0, (L4_MsgTag_t){ .X.u = 1 }.raw);
	L4_LoadMR(1, (L4_Word_t)ct_stack);
	tag = L4_Send(cp_tid);

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


static void child_starter_fn(void)
{
	printf("%s: awakened in %#lx! ffffuuuuu!\n",
		__func__, L4_Myself().raw);

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
	return -1;
}


void exit(int status)
{
	for(;;) {
		asm volatile ("int $1");
	}
}
