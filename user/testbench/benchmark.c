
#define IDLBENCH_IMPL_SOURCE 1

#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <assert.h>
#include <ccan/compiler/compiler.h>
#include <ccan/likely/likely.h>
#include <ccan/tally/tally.h>

#include <l4/types.h>
#include <l4/thread.h>
#include <l4/ipc.h>
#include <l4/schedule.h>

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
	const size_t n_iters = 128;
	printf("%s (%s); partner=%#lx (%s):\n", __func__, desc,
		partner.raw, L4_IsLocalId(partner) ? "local" : "global");

	for(int msg_type=0; msg_type < 2; msg_type++) {
		struct tally *t = tally_new(256);
		int (*fn)(L4_ThreadId_t, int16_t *, int32_t, int32_t, int32_t *, int32_t *) =
			msg_type == 0 ? &__bench_ping : &__bench_other_ping;

		printf("  %s():\t", msg_type == 0 ? "ping" : "other_ping");

		for(int i = 0; i < n_iters; i++) {
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

	/* for string transfers: strings of 31, 511, 2047, 8191, and 16k-1
	 * bytes. both send-only and echo.
	 */
	static const int str_sizes[] = { 31, 511, 2047, 8191, 16 * 1024 - 1 };
	uint32_t seed = 0xb0a7face;
	char *sendbuf = valloc(65536), *replybuf = valloc(65536);
	for(int mode = 0; mode < 2; mode++) {
		for(int size_ix = 0; size_ix < NUM_ELEMENTS(str_sizes); size_ix++) {
			const size_t ss = str_sizes[size_ix];
			random_string(sendbuf, ss, &seed);
			memset(replybuf, '\0', ss);
			struct tally *t = tally_new(256);

			printf("  %s[size=%u]:\t",
				mode == 0 ? "s_bonk" : "s_echo", (unsigned)ss);
			for(int i=0; i < n_iters; i++) {
				uint64_t start = x86_rdtsc();
				int n;
				if(mode == 0) n = __bench_string_bonk(partner, sendbuf);
				else n = __bench_string_echo(partner, sendbuf, replybuf);
				uint64_t end = x86_rdtsc();
				if(n < 0) {
					printf("!!! iter %d discarded (n=%d)\n", i, n);
					continue;
				}
				if(i < 8) continue;
				tally_add(t, end - start);
			}

			printf("cpi="); print_tally(t); printf("\n");
			free(t);
		}
	}
	free(sendbuf);
	free(replybuf);
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


static void impl_string_bonk(const char *text) {
	/* woo hoo */
}


static void impl_string_echo(const char *text, char *reply) {
	strlcpy(reply, text, 65536);
}


static void idl_peer_fn(void *param UNUSED)
{
	static const struct idl_bench_vtable vtab = {
		.quit = &impl_quit,
		.ping = &impl_ping,
		.other_ping = &impl_ping,
		.string_bonk = &impl_string_bonk,
		.string_echo = &impl_string_echo,
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


/* ThreadSwitch benchmark. mostly a measure of system call latency, as this
 * will never switch context.
 */
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


static void spin_thread_fn(void *param UNUSED)
{
	L4_ThreadId_t sender;
	L4_Wait(&sender);
	L4_Word_t n_ms = 0; L4_StoreMR(1, &n_ms);
	L4_LoadMR(0, 0);
	L4_Reply(sender);

	L4_Clock_t start = L4_SystemClock();
	while(start.raw + n_ms * 1000 > L4_SystemClock().raw) {
		usleep(250);
	}

	exit_thread("done");
}


/* benchmark of the Schedule syscall. creates a child spinner thread and
 * queries its remaining timeslice. number of clocks spent in Schedule is
 * measured.
 */
static void bench_schedule(void)
{
	const size_t ms_per_call = 2, n_calls = 256, n_iters = n_calls / 16,
		iters_per_thread = n_calls / n_iters;

	struct tally *t = tally_new(256);
	int n_ok = 0, n_fail = 0;
	for(int iter=0; iter < n_iters; iter++) {
		L4_ThreadId_t spinner = start_thread_long(&spin_thread_fn,
			NULL, find_own_priority() - 1, L4_TimePeriod(50 * 1000),
			L4_Never);
		L4_LoadMR(0, (L4_MsgTag_t){ .X.u = 1 }.raw);
		L4_LoadMR(1, ms_per_call * iters_per_thread + 1);
		L4_Call(spinner);
	
		for(int i=0; i < iters_per_thread; i++) {
			uint64_t start = x86_rdtsc();
			L4_Word_t timectl_out, res = L4_Schedule(spinner,
				~0ul, ~0ul, ~0ul, ~0ul, &timectl_out);
			uint64_t end = x86_rdtsc();
			if(res == L4_SCHEDRESULT_ERROR) n_fail++; else n_ok++;

			if(i < 3) continue;
			tally_add(t, (ssize_t)end - start);
		}

		xjoin_thread(spinner);
	}

	printf("%s: cycles_per_schedule=", __func__); print_tally(t); printf("\n");
	printf("  n_ok=%d, n_fail=%d\n", n_ok, n_fail);
	free(t);
}


static void bench_kernelinterface(void)
{
	const size_t n_iters = 1024;

	struct tally *t = tally_new(256);
	for(int i=0; i < n_iters; i++) {
		uint64_t start = x86_rdtsc();
		L4_Word_t apiver, apiflags, kernelid;
		void *kip_base = L4_KernelInterface(&apiver, &apiflags,
			&kernelid);
		uint64_t end = x86_rdtsc();
		assert(kip_base != NULL);	/* clears the warning */

		if(i < 8) continue;
		tally_add(t, (ssize_t)end - start);
	}

	printf("%s: cpi=", __func__); print_tally(t); printf("\n");
	free(t);
}


static void wait_and_quit_fn(void *param UNUSED)
{
	L4_ThreadId_t sender;
	L4_Wait(&sender);
	exit_thread("done");
}


static void bench_exregs(void)
{
	const size_t n_iters = 512;
	printf("%s:\n", __func__);

	/* first, bare ExchangeRegisters. no delivery, no modification. what this
	 * does is it changes the destination thread ID's localness, so we run the
	 * test twice.
	 */
	for(int round = 0; round < 2; round++) {
		struct tally *t = tally_new(256);
		L4_ThreadId_t test_tid = xstart_thread(&wait_and_quit_fn, NULL);
		test_tid = round == 0 ? L4_GlobalIdOf(test_tid) : L4_LocalIdOf(test_tid);
		for(int i=0; i < n_iters; i++) {
			uint64_t start = x86_rdtsc();
			L4_Word_t dummy;
			L4_ThreadId_t dummy_tid, result = L4_ExchangeRegisters(test_tid,
				0, 0, 0, 0, 0, L4_nilthread, &dummy, &dummy, &dummy, &dummy,
				&dummy, &dummy_tid);
			uint64_t end = x86_rdtsc();
			assert(round != 0 || L4_IsLocalId(result));
			assert(round == 0 || L4_IsGlobalId(result));

			if(i < 8) continue;
			tally_add(t, (ssize_t)end - start);
		}
		L4_LoadMR(0, 0); L4_Send(test_tid); xjoin_thread(test_tid);
		printf("  %s: cpi=", round == 0 ? "global-to-local" : "local-to-global");
		print_tally(t);
		printf("\n");
		free(t);
	}

	/* readouts. local TID only. */
	struct tally *t = tally_new(256);
	L4_ThreadId_t test_tid = xstart_thread(&wait_and_quit_fn, NULL);
	test_tid = L4_LocalIdOf(test_tid);
	for(int i=0; i < n_iters; i++) {
		uint64_t start = x86_rdtsc();
		L4_Word_t dummy;
		L4_ThreadId_t dummy_tid, result = L4_ExchangeRegisters(test_tid,
			0x200, 0, 0, 0, 0, L4_nilthread, &dummy, &dummy, &dummy, &dummy,
			&dummy, &dummy_tid);
		uint64_t end = x86_rdtsc();
		assert(!L4_IsNilThread(result));

		if(i < 8) continue;
		tally_add(t, (ssize_t)end - start);
	}
	L4_LoadMR(0, 0); L4_Send(test_tid); xjoin_thread(test_tid);
	printf("  readout: cpi="); print_tally(t); printf("\n");
	free(t);

	/* TODO: benchmark modification of thread state, one field at a time. */
}


/* fork @n_children times into a process that exits immediately, then wait for
 * all of these. measure clock cycles taken by the whole ordeal.
 */
void bench_fork_wait(const size_t n_children)
{
	const size_t n_iters = 200;
	printf("%s[n_children=%d]:\n", __func__, (int)n_children);

	struct tally *t = tally_new(256);
	for(size_t i=0; i < n_iters; i++) {
		int cpids[n_children];
		uint64_t start = x86_rdtsc();
		for(int j=0; j < n_children; j++) {
			int child = fork();
			if(child == 0) {
				exit(0);
			}
			cpids[j] = child;
		}
		int dead_pids[n_children];
		for(int j=0; j < n_children; j++) {
			int st = 0;
			dead_pids[j] = wait(&st);
		}
		uint64_t end = x86_rdtsc();

		/* verify that all children are dead. */
		int n_dead = 0;
		for(int j=0; j < n_children; j++) {
			for(int k=0; k < n_children; k++) {
				if(dead_pids[k] == cpids[j]) {
					n_dead++;
					break;
				}
			}
		}
		if(n_dead < n_children) {
			printf("ERROR: i=%d: n_dead=%d, n_children=%d\n",
				(int)i, n_dead, (int)n_children);
			break;
		}

		if(i < 8) continue;
		tally_add(t, (ssize_t)end - start);
	}

	printf("  fork-and-wait: "); print_tally(t); printf("\n");

	free(t);
}


static void tsc_child_fn(void *param_ptr)
{
	uint64_t start = x86_rdtsc();
	L4_LoadBR(0, 0);
	L4_ThreadId_t sender;
	L4_MsgTag_t tag = L4_Wait(&sender);
	if(L4_IpcFailed(tag)) {
		printf("%s: failed, ec=%#lx\n", __func__, L4_ErrorCode());
		exit_thread(NULL);
	}
	L4_LoadMR(0, (L4_MsgTag_t){ .X.u = 2 }.raw);
	L4_LoadMR(1, (uint32_t)start);
	L4_LoadMR(2, start >> 32);
	L4_Reply(sender);

	uint64_t *end_at = malloc(sizeof(*end_at));
	*end_at = start;
	L4_Sleep(L4_TimePeriod(2 * 1000));

	*end_at = x86_rdtsc();
	exit_thread(end_at);
}


void bench_thread_lifecycle(void)
{
	const size_t n_iters = 500;
	printf("%s[n_iters=%d]:\n", __func__, (int)n_iters);

	struct tally *t_create = tally_new(256),
		*t_join = tally_new(256);
	for(size_t i=0; i < n_iters; i++) {
		uint64_t start = x86_rdtsc();
		L4_ThreadId_t child_tid = start_thread(&tsc_child_fn, NULL);
		L4_LoadBR(0, 0);
		L4_LoadMR(0, 0);
		L4_MsgTag_t tag = L4_Call(child_tid);
		if(L4_IpcFailed(tag)) {
			printf("child call failed, ec=%#lx\n", L4_ErrorCode());
			kill_thread(child_tid);
			continue;
		}

		L4_Word_t lo; L4_StoreMR(1, &lo);
		L4_Word_t hi; L4_StoreMR(2, &hi);
		uint64_t child_start_at = (uint64_t)hi << 32 | lo;
		ssize_t s_create = (ssize_t)child_start_at - start;

		L4_Word_t ec = 0;
		void *ptr = join_thread_long(child_tid,
			L4_TimePeriod(25 * 1000), &ec);
		if(ptr == NULL && ec != 0) {
			printf("child join failed, ec=%#lx\n", L4_ErrorCode());
			kill_thread(child_tid);
			continue;
		}
		assert(ptr != NULL);
		ssize_t s_join = (ssize_t)x86_rdtsc() - *(uint64_t *)ptr;
		free(ptr);

		if(i >= 8) {
			tally_add(t_create, s_create);
			tally_add(t_join, s_join);
		}
	}

	printf("  create-to-start: "); print_tally(t_create); printf("\n");
	printf("  sync-to-join: "); print_tally(t_join); printf("\n");

	free(t_create);
	free(t_join);
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
	bench_idl_ipc("intra-space (L)", local_tid);
	bench_idl_ipc("inter-space", remote_tid);
	end_pair(child, local_tid, remote_tid);

	bench_threadswitch();
	bench_systemclock();
	bench_schedule();
	bench_kernelinterface();
	bench_exregs();

	/* application-ey benchmarks. */
	bench_thread_lifecycle();
	bench_fork_wait(1);
	bench_fork_wait(4);
	bench_fork_wait(8);
}
