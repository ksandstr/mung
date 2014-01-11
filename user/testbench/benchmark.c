
#include <stdio.h>
#include <assert.h>
#include <ccan/compiler/compiler.h>
#include <ccan/likely/likely.h>

#include <l4/types.h>
#include <l4/thread.h>
#include <l4/ipc.h>

#include "defs.h"


#define L_QUIT 0xdead	/* show's over */
#define L_PING 0x1234	/* reply with received words, untyped */
#define L_REPLY 0x1235	/* reply with empty IPC */
#define L_LPING 0x1236	/* reply with Lipc */


/* benchmark the Ipc/Lipc syscall. peer should answer L_PING and L_REPLY. */
static void bench_sys_ipc(
	const char *description,
	bool do_lipc,
	L4_ThreadId_t partner)
{
	printf("%s (%s); partner=%#lx (%s):\n", __func__, description,
		partner.raw, L4_IsLocalId(partner) ? "local" : "global");
	do_lipc = do_lipc && L4_IsLocalId(partner);

	for(int msg_type=0; msg_type < 3; msg_type++) {
		L4_Word_t label;
		switch(msg_type) {
			case 0: label = L_PING; break;
			case 1: label = L_LPING; break;
			case 2: label = L_REPLY; break;
			default: assert(false);
		}
		if(label == L_LPING && L4_IsNilThread(L4_LocalIdOf(partner))) {
			/* TODO: don't do lping with a remote thread for now. the kernel,
			 * or something else, seems to become stuck.
			 */
			continue;
		}
		for(int mrs_shift=0; mrs_shift < 7; mrs_shift++) {
			int n_mrs = (1 << mrs_shift) - 1;
			printf("  %s with %d MRs:\t",
				label == L_PING ? "ping"
					: (label == L_LPING ? "lping" : "bonk"),
				n_mrs);

			uint64_t total_cycles = 0;
			int total_iters = 0;
			for(int i = 0; i < 64; i++) {
				L4_LoadMR(0, (L4_MsgTag_t){
					.X.label = label, .X.u = n_mrs }.raw);
				for(int j=0; j < n_mrs; j++) {
					L4_LoadMR(j + 1, j + (mrs_shift << 6));
				}
				L4_MsgTag_t tag;
				uint64_t start, end;
				if(do_lipc) {
					start = x86_rdtsc();
					tag = L4_Lcall(partner);
					end = x86_rdtsc();
				} else {
					start = x86_rdtsc();
					tag = L4_Call(partner);
					end = x86_rdtsc();
				}
				if(L4_IpcFailed(tag)) {
					printf("!!! iter %d discarded (ec=%#lx)\n",
						i, L4_ErrorCode());
					continue;
				}

				/* toss results for first 8 iters for warmup's sake */
				if(i < 8) continue;

				total_cycles += end - start;
				total_iters++;
			}

			printf("cpi=%u\n",
				total_iters > 0 ? (unsigned)(total_cycles / total_iters) : 0);
		}
	}
}


static void ipc_peer_fn(void *param UNUSED)
{
	bool running = true;
	while(running) {
		L4_ThreadId_t sender;
		L4_MsgTag_t tag = L4_Wait(&sender);

		for(;;) {
			if(L4_IpcFailed(tag)) break;

			switch(L4_Label(tag)) {
				case L_QUIT: running = false; break;
				case L_REPLY:
					L4_LoadMR(0, 0);
					break;
				case L_PING:
				case L_LPING: {
					/* this could be done without a store-reload thing, but
					 * that's forbidden by the convenience programming
					 * interface.
					 */
					L4_Word_t mrs[64],
						n_mrs = L4_UntypedWords(tag) + L4_TypedWords(tag);
					L4_StoreMRs(1, n_mrs, mrs);
					L4_LoadMR(0, (L4_MsgTag_t){ .X.u = n_mrs }.raw);
					L4_LoadMRs(1, n_mrs, mrs);
					break;
				}
			}
			if(unlikely(!running)) {
				L4_LoadMR(0, 0);
				L4_Reply(sender);
				break;
			}

			if(L4_Label(tag) == L_LPING) {
				tag = L4_LreplyWait(sender, &sender);
			} else {
				tag = L4_ReplyWait(sender, &sender);
			}
		}
	}
}


void run_benchmarks(void)
{
	L4_ThreadId_t local_tid = xstart_thread(&ipc_peer_fn, NULL);
	L4_ThreadId_t remote_tid;
	int child = fork_tid(&remote_tid);
	if(child == 0) {
		ipc_peer_fn(NULL);
		exit(0);
	}

	local_tid = L4_LocalIdOf(local_tid);
	bench_sys_ipc("intra-space", false, L4_GlobalIdOf(local_tid));
	bench_sys_ipc("intra-space", false, local_tid);
	bench_sys_ipc("inter-space", false, remote_tid);

	bench_sys_ipc("intra-space (L)", true, local_tid);
	bench_sys_ipc("inter-space (L)", true, remote_tid);

	send_quit(local_tid);
	send_quit(remote_tid);
	int st, dead = wait(&st);
	if(dead != child) {
		printf("!!! unrecognized dead child %d (expected %d)\n", dead, child);
	}
}
