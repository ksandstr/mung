
/* TODO: all sampling should be fed to the CCAN tally module. plain averaging,
 * as is done right now, does a very bad job of disregarding outliers; and
 * those can happen due to VM host scheduling, its interrupts, or whatever
 * else besides.
 */

#define IDLBENCH_IMPL_SOURCE 1

#include <stdio.h>
#include <stdint.h>
#include <assert.h>
#include <ccan/compiler/compiler.h>
#include <ccan/likely/likely.h>
#include <ccan/tally/tally.h>

#include <l4/types.h>
#include <l4/thread.h>
#include <l4/ipc.h>

#include <ukernel/util.h>

#include "defs.h"
#include "benchmark-defs.h"


#define L_QUIT 0xdead	/* show's over */
#define L_PING 0x1234	/* reply with received words, untyped */
#define L_REPLY 0x1235	/* reply with empty IPC */
#define L_LPING 0x1236	/* reply with Lipc */


static void print_tally(struct tally *t)
{
	size_t err = 0;
	printf("[min=%d, mean=%d, median=%d",
		(int)tally_min(t), (int)tally_mean(t),
		(int)tally_approx_median(t, &err));
	if(err != 0) printf("+-%u", (unsigned)err);
	printf("]");
}


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
			struct tally *t = tally_new(256);
			int n_mrs = (1 << mrs_shift) - 1;
			printf("  %s with %d MRs:\t",
				label == L_PING ? "ping"
					: (label == L_LPING ? "lping" : "bonk"),
				n_mrs);

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

				tally_add(t, end - start);
			}

			printf("cpi="); print_tally(t); printf("\n");
			free(t);
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


static void bench_idl_ipc(const char *desc, L4_ThreadId_t partner)
{
	printf("%s (%s); partner=%#lx (%s):\n", __func__, desc,
		partner.raw, L4_IsLocalId(partner) ? "local" : "global");

	for(int msg_type=0; msg_type < 2; msg_type++) {
		struct tally *t = tally_new(256);
		int (*fn)(L4_ThreadId_t, int16_t *, int32_t, int32_t, int32_t *, int32_t *) =
			msg_type == 0 ? &__bench_ping : &__bench_other_ping;

		printf("  %s():\t", msg_type == 0 ? "ping" : "other_ping");

		for(int i = 0; i < 64; i++) {
			int32_t a = 0, b = 0;
			int16_t retval;
			uint64_t start = x86_rdtsc();
			int n = (*fn)(partner, &retval, 1, 2, &a, &b);
			uint64_t end = x86_rdtsc();

			if(n < 0) {
				printf("!!! iter %d discarded (n=%d)\n", i, (int)n);
				continue;
			}

			/* toss results for first 8 iters for warmup's sake */
			if(i < 8) continue;

			tally_add(t, end - start);
		}

		printf("cpi="); print_tally(t); printf("\n");
		free(t);
	}
}


static bool idl_peer_running;

static void impl_quit(void) {
	idl_peer_running = false;
}


static int16_t impl_ping(
	int32_t a, int32_t b,
	int32_t *i_p, int32_t *j_p)
{
	*i_p = a;
	*j_p = b;
	return 666;		/* HAIL SATAN */
}


static void idl_peer_fn(void *param UNUSED)
{
	static const struct idl_bench_vtable vtab = {
		.quit = &impl_quit,
		.ping = &impl_ping,
		.other_ping = &impl_ping,
	};

	idl_peer_running = true;
	while(idl_peer_running) {
		/* ignoring "status". */
		_muidl_idl_bench_dispatch(&vtab);
	}
}


static void start_pair(
	int *child_p,
	L4_ThreadId_t *local_tid_p,
	L4_ThreadId_t *remote_tid_p,
	void (*fn)(void *))
{
	*local_tid_p = xstart_thread(fn, NULL);
	*child_p = fork_tid(remote_tid_p);
	if(*child_p == 0) {
		(*fn)(NULL);
		exit(0);
	}
}


static void end_pair(int child, L4_ThreadId_t local, L4_ThreadId_t remote)
{
	local = L4_GlobalIdOf(local);
	send_quit(local);
	xjoin_thread(local);

	idl_fixture_teardown_fork(child, remote);
}


static void bench_threadswitch(void)
{
	const size_t n_iters = 512;
	struct {
		const char *name;
		L4_ThreadId_t tid;
	} alts[] = {
		{ "nil", L4_nilthread },
		{ "self[global]", L4_MyGlobalId() },
		{ "self[local]", L4_MyLocalId() },
		/* TODO: add intraspace (global and local), interspace */
	};

	for(int alt=0; alt < NUM_ELEMENTS(alts); alt++) {
		struct tally *t = tally_new(256);

		printf("%s; %s:\t", __func__, alts[alt].name);
		L4_ThreadId_t dest = alts[alt].tid;
		for(int i=0; i < n_iters; i++) {
			uint64_t start = x86_rdtsc();
			L4_ThreadSwitch(dest);
			uint64_t end = x86_rdtsc();

			if(i < 8) {
				/* discard early data */
				continue;
			}

			tally_add(t, (ssize_t)(end - start));
		}

		printf("cpi="); print_tally(t); printf("\n");
		free(t);
	}
}


static void bench_systemclock(void)
{
	const size_t n_iters = 512;

	struct tally *t = tally_new(256);
	printf("%s:\t", __func__);
	uint64_t acc = 0;
	for(int i=0; i < n_iters; i++) {
		uint64_t start = x86_rdtsc();
		L4_Clock_t clk = L4_SystemClock();
		uint64_t end = x86_rdtsc();
		acc += clk.raw;
		if(i < 8) continue;

		tally_add(t, (ssize_t)(end - start));
	}

	printf("cpi="); print_tally(t); printf("\n");
	free(t);
}


void run_benchmarks(void)
{
	L4_ThreadId_t local_tid, remote_tid;
	int child;

	/* basic IPC benchmarks. */
	start_pair(&child, &local_tid, &remote_tid, &ipc_peer_fn);
	local_tid = L4_LocalIdOf(local_tid);
	bench_sys_ipc("intra-space", false, L4_GlobalIdOf(local_tid));
	bench_sys_ipc("intra-space", false, local_tid);
	bench_sys_ipc("inter-space", false, remote_tid);
	bench_sys_ipc("intra-space (L)", true, local_tid);
	bench_sys_ipc("inter-space (L)", true, remote_tid);
	end_pair(child, local_tid, remote_tid);

	/* similar, mediated by the IDL compiler */
	start_pair(&child, &local_tid, &remote_tid, &idl_peer_fn);
	local_tid = L4_LocalIdOf(local_tid);
	bench_idl_ipc("intra-space", L4_GlobalIdOf(local_tid));
	bench_idl_ipc("intra-space", local_tid);
	bench_idl_ipc("inter-space", remote_tid);
	end_pair(child, local_tid, remote_tid);

	/* ThreadSwitch benchmark. mostly a measure of system call latency, as
	 * this will never switch context.
	 */
	bench_threadswitch();

	bench_systemclock();
}
