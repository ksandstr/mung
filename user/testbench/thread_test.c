
#include <stdio.h>
#include <assert.h>

#include <l4/types.h>
#include <l4/thread.h>
#include <l4/ipc.h>
#include <l4/syscall.h>

#include "defs.h"
#include "test.h"


static void test_thread_fn(void)
{
	L4_ThreadId_t self = L4_Myself();
	printf("test thread fn called! our ID is %lu:%lu\n",
		L4_ThreadNo(self), L4_Version(self));

	/* wait for initialization message. */
	L4_LoadBR(0, 0);
	L4_ThreadId_t sender;
	L4_MsgTag_t tag = L4_Wait(&sender);
	if(L4_IpcFailed(tag)) {
		printf("test thread initial IPC failed: errorcode %#lx\n",
			L4_ErrorCode());
		goto end;
	}
	L4_Word_t msg;
	L4_StoreMR(1, &msg);
	printf("test thread got initial message (MR1 = %#lx)\n", msg);

	L4_Set_ExceptionHandler(sender);
	printf("test thread set exceptionhandler to %lu:%lu\n",
		L4_ThreadNo(sender), L4_Version(sender));

end:
	asm volatile ("int $1");
}


void threadctl_test(void)
{
	printf("threadcontrol test start.\n");

	/* thread creation. */
	L4_ThreadId_t self = L4_Myself(),
		dest = L4_GlobalId(L4_ThreadNo(self) + 1, 1);
	L4_Word_t utcb_base = L4_MyLocalId().raw & ~0x1ffUL;
	printf("utcb_base is %#lx\n", utcb_base);
	L4_Word_t result = L4_ThreadControl(dest, self, self,
		L4_Pager(), (void *)(utcb_base + 512));
	if(result == 0) {
		printf("creating threadcontrol failed; errorcode %#lx\n",
			L4_ErrorCode());
	} else {
		/* statically allocated stack, because who gives a shit. */
		static uint8_t stack[8192] __attribute__((aligned(4096)));
		const L4_Word_t child_stk_top = (L4_Word_t)(stack
			+ sizeof(stack) - 16);
		L4_Start_SpIp(dest, child_stk_top, (L4_Word_t)&test_thread_fn);
		printf("test thread %lu:%lu started.\n",
			L4_ThreadNo(dest), L4_Version(dest));
		L4_ThreadSwitch(dest);

		/* send initial message. */
		printf("sending initial message to child thread %lu:%lu\n",
			L4_ThreadNo(dest), L4_Version(dest));
		L4_LoadMR(0, (L4_MsgTag_t){ .X.u = 1 }.raw);
		L4_LoadMR(1, 0x234269ff);
		L4_MsgTag_t tag = L4_Send(dest);
		if(L4_IpcFailed(tag)) {
			printf("initial message failed: errorcode %#lx\n",
				L4_ErrorCode());
		} else {
			printf("initial message sent.\n");
		}

		/* wait for exception. */
		tag = L4_Receive(dest);
		if(L4_IpcFailed(tag)) {
			printf("receive from child thread failed: errorcode %#lx\n",
				L4_ErrorCode());
			return;
		}

		if(tag.X.label == ((-5) & 0xfff) << 4) {
			L4_Word_t exno, err;
			L4_StoreMR(3, &exno);
			L4_StoreMR(4, &err);
			printf("child had exception %#lx, errorcode %#lx\n",
				exno, err);
		} else {
			printf("got some other thing from child (tag %#lx)\n",
				tag.raw);
			return;
		}

		/* reply with a save of the returned exception registers into a bit of
		 * static memory, and a complete restart of the the thread function.
		 */
		L4_LoadMR(0, (L4_MsgTag_t){ .X.u = 12 }.raw);
		extern void exn_store_regs();
		static L4_Word_t regs_out[20];
		L4_LoadMR(1, (L4_Word_t)&exn_store_regs);
		L4_LoadMR(2, 0);
		L4_LoadMR(3, 0);
		L4_LoadMR(4, 0);
		L4_LoadMR(5, 0xed1);		/* edi */
		L4_LoadMR(6, 0xe51);		/* esi */
		L4_LoadMR(7, 0xeb4);		/* ebp */
		L4_LoadMR(8, child_stk_top);	/* esp */
		L4_LoadMR(9, (L4_Word_t)&test_thread_fn);	/* ebx */
		L4_LoadMR(10, 0xed0);		/* edx */
		L4_LoadMR(11, 0xec0);		/* ecx */
		L4_LoadMR(12, (L4_Word_t)&regs_out[0]);		/* eax */
		tag = L4_Send(dest);
		printf("child reply sent.\n");

		/* re-init */
		L4_LoadMR(0, (L4_MsgTag_t){ .X.u = 1 }.raw);
		L4_LoadMR(1, 0xfedcba98);
		L4_Send(dest);

		/* examine the entrails. */
		printf("child thread restarted; saved regs are:\n");
		for(int i=0; i < 8; i++) {
			printf("  %d = %#lx\n", i, regs_out[i]);
		}

		/* re-fault */
		printf("waiting for another child fault...\n");
		tag = L4_Receive(dest);

		/* TODO: delete the thread */
	}

	printf("threadcontrol test ends.\n");
}
