
/* tests concerning the Ipc system call, with the exception of anything
 * related to string transfers which is broad enough to be tested in
 * string_suite.c instead.
 */

#define IPCHELPER_IMPL_SOURCE

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/wait.h>

#include <ccan/compiler/compiler.h>
#include <ccan/hash/hash.h>
#include <ccan/crc32c/crc32c.h>
#include <ccan/endian/endian.h>
#include <ccan/talloc/talloc.h>

#include <l4/types.h>
#include <l4/ipc.h>
#include <l4/kip.h>
#include <l4/space.h>
#include <l4/schedule.h>
#include <l4/syscall.h>

#include <ukernel/util.h>
#include <muidl.h>

#include "defs.h"
#include "test.h"
#include "forkserv-defs.h"
#include "ipc-suite-defs.h"


#define IS_LOCAL_TID(tid) (!L4_IsNilThread(L4_LocalIdOf((tid))))


struct helper_ctx
{
	int fault_delay_us;
	bool running;
};


/* TODO: this could very well come out one day, but it's complicated because
 * posix_memalign() was a huge fuckup.
 */
static void *alloc_aligned(void **base_p, size_t size, size_t alignment) {
	assert(POPCOUNT(alignment) == 1);
	*base_p = aligned_alloc(alignment, size);
	return *base_p;
}


static struct helper_ctx *helper_ctx(void)
{
	static int helper_ctx_key = -1;

	if(helper_ctx_key == -1) {
		tsd_key_create(&helper_ctx_key, &free);
	}
	struct helper_ctx *ctx = tsd_get(helper_ctx_key);
	if(ctx == NULL) {
		ctx = malloc(sizeof *ctx);
		*ctx = (struct helper_ctx){
			.running = true,
			.fault_delay_us = 0,
		};
		tsd_set(helper_ctx_key, ctx);
		assert(tsd_get(helper_ctx_key) == ctx);
	}

	return ctx;
}


static void helper_quit_impl(void) {
	helper_ctx()->running = false;
}


static void helper_yield_impl(void) {
	L4_ThreadSwitch(L4_nilthread);
}


static void helper_sleep_impl(
	int32_t us,
	bool spin,
	int32_t sleep_after_us)
{
#if 0
	diag("%s: us=%d, spin=%s, sleep_after_us=%d", __func__,
		us, btos(spin), sleep_after_us);
#endif
	if(us > 0) {
		if(spin) usleep(us); else L4_Sleep(L4_TimePeriod(us));
	}

	if(sleep_after_us > 0) {
		L4_Sleep(L4_TimePeriod(sleep_after_us));
	}

	helper_ctx()->fault_delay_us = MAX(int, us, 0);
}


static void helper_handle_fault_impl(
	L4_Word_t faddr,
	L4_Word_t fip,
	L4_MapItem_t *map)
{
	int sleep_us = helper_ctx()->fault_delay_us;
	diag("helper[pf]: faddr=%#lx, fip=%#lx, sleep_us=%d", faddr, fip, sleep_us);
	if(sleep_us > 0) {
		L4_Sleep(L4_TimePeriod(sleep_us));
	}

	/* pass it on and don't reply. this avoids a free() spinlock deadly
	 * embrace on reply fail.
	 */
	L4_MsgTag_t i_tag = muidl_get_tag(),
		tag = { .X.label = L4_Label(i_tag), .X.u = 2 };
	L4_Set_Propagation(&tag);
	L4_Set_VirtualSender(muidl_get_sender());
	L4_LoadBR(0, 0);
	L4_LoadMR(0, tag.raw);
	L4_LoadMR(1, faddr);
	L4_LoadMR(2, fip);
	tag = L4_Send(L4_Pager());
	if(!L4_IpcSucceeded(tag)) {
		diag("%s: ipc failed, ec %#lx", __func__, L4_ErrorCode());
	}
	muidl_raise_no_reply();
}


static int32_t helper_ipc_ping_impl(
	int32_t echoval,
	L4_Word_t *tag_ptr,
	L4_Word_t *sender_ptr,
	L4_Word_t *as_ptr)
{
	*tag_ptr = muidl_get_tag().raw;
	*sender_ptr = muidl_get_sender().raw;
	*as_ptr = L4_ActualSender().raw;
	return echoval;
}


static void munge_string_local(
	const char *input,
	char *output,
	int32_t rnd_seed)
{
	uint32_t str_seed = rnd_seed ^ hash_string(input);
	int length = strlen(input);
	char *buffer = malloc(length + 1);
	random_string(buffer, length, &str_seed);
	for(int i=0; i < length; i++) {
		output[i] = buffer[i] ^ input[i];
	}
	output[length] = '\0';
}


static const struct ipc_helper_vtable helper_vtab = {
	.quit = &helper_quit_impl,
	.yield = &helper_yield_impl,
	.sleep = &helper_sleep_impl,
	.handle_fault = &helper_handle_fault_impl,
	.munge_string = &munge_string_local,
	.ipc_ping = &helper_ipc_ping_impl,
};

IDL_FIXTURE(helper, ipc_helper, &helper_vtab, !helper_ctx()->running);
IDL_FIXTURE(other_helper, ipc_helper, &helper_vtab,
	!helper_ctx()->running);


struct sender_param {
	L4_ThreadId_t parent, vs;
	L4_Word_t payload;
	L4_Time_t delay;	/* ZeroTime or TimePeriod */
	bool use_lipc, call;
};

/* sender thread utilities. these attempt to do an IPC with the caller thread
 * either from another thread in this process, or a thread in a forked address
 * space.
 */
static void sender_thread_fn(void *param_ptr)
{
	struct sender_param *p = param_ptr;

	if(p->delay.raw != L4_ZeroTime.raw) {
		assert(p->delay.raw != L4_Never.raw);
		L4_Sleep(p->delay);
	}

	L4_MsgTag_t tag = (L4_MsgTag_t){ .X.u = 1, .X.label = 0xd00d };
	if(!L4_IsNilThread(p->vs)) {
		L4_Set_VirtualSender(p->vs);
		L4_Set_Propagation(&tag);
	}
	L4_LoadMR(0, tag.raw);
	L4_LoadMR(1, p->payload);
	L4_ThreadId_t dummy;
	if(p->call) {
		if(p->use_lipc) {
			tag = L4_Lipc(p->parent, p->parent,
				L4_Timeouts(L4_Never, L4_Never), &dummy);
		} else {
			tag = L4_Call(p->parent);
		}
	} else {
		if(p->use_lipc) {
			tag = L4_Lipc(p->parent, L4_nilthread,
				L4_Timeouts(L4_Never, L4_ZeroTime), &dummy);
		} else {
			tag = L4_Send(p->parent);
		}
	}
	if(L4_IpcFailed(tag)) {
		diag("%s[%lu:%lu]: %spc failed, ec=%#lx", __func__,
			L4_ThreadNo(L4_Myself()), L4_Version(L4_Myself()),
			p->use_lipc ? "Li" : "I", L4_ErrorCode());
	}

	free(p);
}


/* propagates from @vs when not nil. */
static L4_ThreadId_t ipc_to_from_thread(
	L4_ThreadId_t dest, L4_ThreadId_t vs, L4_Word_t payload, L4_Time_t delay,
	bool use_lipc, bool call)
{
	struct sender_param *param = malloc(sizeof(*param));
	fail_if(param == NULL);
	*param = (struct sender_param){
		.parent = dest, .vs = vs, .payload = payload, .delay = delay,
		.use_lipc = use_lipc, .call = call,
	};
	return xstart_thread(&sender_thread_fn, param);
}

#define send_from_thread(payload, delay) \
	ipc_to_from_thread(L4_MyGlobalId(), L4_nilthread, (payload), (delay), \
		false, false)


static L4_ThreadId_t send_from_fork(L4_Word_t payload, L4_Time_t delay)
{
	L4_ThreadId_t child_tid = L4_nilthread, parent_tid = L4_MyGlobalId();
	int pid = fork_tid(&child_tid);
	if(pid == 0) {
		struct sender_param *param = malloc(sizeof(*param));
		fail_if(param == NULL);
		*param = (struct sender_param){
			.parent = parent_tid, .payload = payload, .delay = delay,
		};
		sender_thread_fn(param);
		exit(0);
	}

	return child_tid;
}


static void close_sender(L4_ThreadId_t sender)
{
	if(IS_LOCAL_TID(sender)) {
		xjoin_thread(sender);
	} else {
		int status = 0, id = wait(&status);
		if(id < 0) diag("wait failed");
		else if(status != 0) diag("status from wait = %d", status);
	}
}


/* receive from a local sender specified by TID. both active and passive
 * receive, with a local and global FromSpec, and with a different sender
 * either before or after the proper one.
 */
START_LOOP_TEST(receive_from_local, iter, 0, 7)
{
	plan_tests(3);
	const bool send_passive = CHECK_FLAG(iter, 1),
		is_local_id = CHECK_FLAG(iter, 2),
		fake_after = CHECK_FLAG(iter, 4);
	diag("send_passive=%s, is_local_id=%s, fake_after=%s",
		btos(send_passive), btos(is_local_id), btos(fake_after));

	L4_ThreadId_t fake_tid;
	if(!fake_after) fake_tid = send_from_thread(0xbeefb055, L4_ZeroTime);
	L4_ThreadId_t sender_tid = send_from_thread(0xfaceb0a7,
		send_passive ? L4_ZeroTime : A_SHORT_NAP);
	if(fake_after) fake_tid = send_from_thread(0xcafeb055, L4_ZeroTime);
	L4_ThreadId_t recv_tid = is_local_id ? L4_LocalIdOf(sender_tid)
		: L4_GlobalIdOf(sender_tid);
	if(send_passive) L4_Sleep(A_SHORT_NAP);

	L4_LoadBR(0, 0);
	L4_MsgTag_t tag = L4_Receive_Timeout(recv_tid, TEST_IPC_DELAY);
	L4_Word_t payload; L4_StoreMR(1, &payload);
	ok1(L4_IpcSucceeded(tag));
	ok1(L4_Label(tag) == 0xd00d && L4_UntypedWords(tag) == 1);
	if(!ok1(payload == 0xfaceb0a7)) {
		diag("payload=%#lx", payload);
	}

	close_sender(sender_tid);
	L4_Receive_Timeout(fake_tid, TEST_IPC_DELAY);
	close_sender(fake_tid);
}
END_TEST


/* receive from a foreign sender specified by TID. both active and passive
 * receive, and with a different sender either before or after the proper one.
 */
START_LOOP_TEST(receive_from_foreign, iter, 0, 3)
{
	plan_tests(3);
	const bool send_passive = CHECK_FLAG(iter, 1),
		fake_after = CHECK_FLAG(iter, 2);
	diag("send_passive=%s, fake_after=%s",
		btos(send_passive), btos(fake_after));

	L4_ThreadId_t fake_tid = L4_nilthread;
	if(!fake_after) fake_tid = send_from_fork(0xabcadada, L4_ZeroTime);
	L4_ThreadId_t sender_tid = send_from_fork(0xdeafbabe,
		send_passive ? L4_ZeroTime : A_SHORT_NAP);
	assert(L4_IsGlobalId(sender_tid));
	if(fake_after) fake_tid = send_from_fork(0xdadad0d0, L4_ZeroTime);
	assert(!L4_IsNilThread(fake_tid));
	if(send_passive) L4_Sleep(A_SHORT_NAP);

	L4_LoadBR(0, 0);
	L4_MsgTag_t tag = L4_Receive_Timeout(sender_tid, TEST_IPC_DELAY);
	L4_Word_t payload; L4_StoreMR(1, &payload);
	L4_Word_t ec = L4_ErrorCode();
	ok1(L4_IpcSucceeded(tag));
	skip_start(L4_IpcFailed(tag), 2, "IPC failed (ec=%#lx)", ec) {
		ok1(L4_Label(tag) == 0xd00d && L4_UntypedWords(tag) == 1);
		if(!ok1(payload == 0xdeafbabe)) {
			diag("payload=%#lx", payload);
		}
	} skip_end;

	close_sender(sender_tid);
	L4_Receive_Timeout(fake_tid, TEST_IPC_DELAY);
	close_sender(fake_tid);
}
END_TEST


/* test four things about the L4_anylocalthread FromSpecifier:
 *   1) that when no thread at all is sending, L4_anylocalthread should cause
 *      a timeout (base case);
 *   2a) when only one thread is sending, but it is not local, should timeout;
 *   2b) same but one thread, local, should not timeout;
 *   3) two threads, one local & one foreign, should receive from local, and
 *      then timeout.
 *
 * test iteration varies between active send and active receive.
 */
START_LOOP_TEST(receive_from_anylocalthread, iter, 0, 1)
{
	plan_tests(5);

	const int delay_ms = 3;
	const L4_Time_t delay = L4_TimePeriod(delay_ms * 1000);
	const bool d_self = !CHECK_FLAG(iter, 1);	/* i.e. active receive */
	diag("d_self=%s", btos(d_self));

	const L4_Time_t d = d_self ? L4_ZeroTime : delay;
	diag("delay_ms=%d applies to %s", delay_ms,
		d_self ? "receiver" : "sender");

	/* part 1 */
	L4_ThreadId_t from;
	L4_MsgTag_t tag = L4_WaitLocal_Timeout(TEST_IPC_DELAY, &from);
	ok(L4_IpcFailed(tag) && (L4_ErrorCode() & 0xf) == 3,
		"recv timeout in no-sender");

	/* part 2a */
	L4_ThreadId_t sender = send_from_fork(0xdeadbeef, d);
	if(d_self) L4_Sleep(delay);
	tag = L4_WaitLocal_Timeout(TEST_IPC_DELAY, &from);
	/* (no diag(), receive phase timeout is expected) */
	ok(L4_IpcFailed(tag) && (L4_ErrorCode() & 0xf) == 3,
		"recv timeout in foreign sender");
	/* (clear it, though.) */
	do {
		tag = L4_Wait(&from);
		fail_if(L4_IpcFailed(tag));
	} while(from.raw != sender.raw);
	close_sender(sender);

	/* part 2b */
	L4_ThreadId_t fork_sender = send_from_thread(0xf00bdead, d);
	if(d_self) L4_Sleep(delay);
	tag = L4_WaitLocal_Timeout(TEST_IPC_DELAY, &from);
	if(L4_IpcFailed(tag)) diag("ec %#lx", L4_ErrorCode());
	L4_Word_t payload;
	L4_StoreMR(1, &payload);
	ok(L4_IpcSucceeded(tag) && payload == 0xf00bdead,
		"recv success in local sender");
	close_sender(fork_sender);

	/* part 3 */
	fork_sender = send_from_fork(0xbaddcafe, d);
	sender = send_from_thread(0xb0a7face, d);
	if(d_self) L4_Sleep(delay);
	tag = L4_WaitLocal_Timeout(TEST_IPC_DELAY, &from);
	if(L4_IpcFailed(tag)) diag("ec %#lx", L4_ErrorCode());
	L4_StoreMR(1, &payload);
	ok(L4_IpcSucceeded(tag) && payload == 0xb0a7face,
		"received from thread, first");
	close_sender(sender);	/* must catch its death gurgle first */

	tag = L4_Wait_Timeout(TEST_IPC_DELAY, &from);
	if(L4_IpcFailed(tag)) diag("ec %#lx", L4_ErrorCode());
	L4_StoreMR(1, &payload);
	ok(L4_IpcSucceeded(tag) && payload == 0xbaddcafe,
		"received from fork, after");
	close_sender(fork_sender);
}
END_TEST


static void wait_and_return(void *param_ptr UNUSED) {
	L4_ThreadId_t dummy;
	L4_Wait_Timeout(TEST_IPC_DELAY, &dummy);
}


/* this tests whether Ipc's matching condition checks space first and LTID
 * second, instead of (let's say) ignoring the address space locality question
 * altogether.
 *
 * variables:
 *   - active receive: no/yes
 *   - receiver's source parameter: sender's local tid, or local-id pattern
 *   - burn some thread IDs before creating the sender: no/yes
 *   - use Ipc or Lipc syscall in receiver
 *     - (this isn't super useful as Lipc always slow-paths out when there's
 *       no send phase, but whatever.)
 *   - use Ipc/Lipc syscall in true sender
 *   - use Ipc/Lipc syscall in false sender
 *
 * TODO:
 *   - shorten the amount of wallclock time the test takes, or execute its
 *     iterations concurrently somehow.
 */
START_LOOP_TEST(receive_from_false_local, iter, 0, 63)
{
	const bool act_receive = CHECK_FLAG(iter, 1),
		recv_pattern = CHECK_FLAG(iter, 2),
		burn_threads = CHECK_FLAG(iter, 4),
		recv_lipc = CHECK_FLAG(iter, 8),
		send_true_lipc = CHECK_FLAG(iter, 16),
		send_false_lipc = CHECK_FLAG(iter, 32);
	diag("act_receive=%s, recv_pattern=%s, burn_threads=%s, lipc=%s/%s/%s",
		btos(act_receive), btos(recv_pattern), btos(burn_threads),
		btos(recv_lipc), btos(send_true_lipc), btos(send_false_lipc));
	const L4_Word_t good_val = 0x12345678, bad_val = 0xabcdef64;
	plan_tests(4);

	L4_ThreadId_t parent = L4_MyGlobalId();

	L4_ThreadId_t burners[7];
	for(int i=0; i < NUM_ELEMENTS(burners); i++) {
		burners[i] = burn_threads ? xstart_thread(&wait_and_return, NULL)
			: L4_nilthread;
	}

	/* start the real sender. it'll always wait a few ticks to let the false
	 * sender get to the post first.
	 */
	L4_ThreadId_t sender = L4_LocalIdOf(ipc_to_from_thread(
		L4_MyLocalId(), L4_nilthread, good_val, A_SHORT_NAP,
		send_true_lipc, true));
	//diag("sender's ltid=%#lx", sender.raw);

	/* fork and spawn enough threads to duplicate the sender's LTID. */
	L4_ThreadId_t child_tid;
	int child = fork_tid(&child_tid);
	if(child == 0) {
		diag("false-sender child's main TID is %lu:%lu",
			L4_ThreadNo(L4_Myself()), L4_Version(L4_Myself()));
		L4_ThreadId_t notsender[MAX_THREADS];
		int num;
		for(num = 0; num < MAX_THREADS; num++) {
			notsender[num] = L4_LocalIdOf(ipc_to_from_thread(
				parent, L4_nilthread, bad_val, L4_ZeroTime,
				send_false_lipc, true));
			if(notsender[num].raw == sender.raw) break;
		}
		/* communicate to the parent whether the right child was started. */
		L4_LoadMR(0, (L4_MsgTag_t){ .X.u = 1 }.raw);
		L4_LoadMR(1, num == MAX_THREADS ? 1 : 0);
		L4_MsgTag_t tag = L4_Send_Timeout(parent, TEST_IPC_DELAY);
		if(L4_IpcFailed(tag)) {
			diag("child couldn't deliver status to parent");
		}
		for(int i=0; i <= num; i++) xjoin_thread(notsender[i]);
		exit(0);
	}

	/* receive from the real sender. */
	if(act_receive) {
		/* fifty winks */
		L4_Sleep(A_SHORT_NAP);
		L4_Sleep(A_SHORT_NAP);
	}
	L4_MsgTag_t tag;
	L4_ThreadId_t send_from;
	if(recv_pattern) {
		if(recv_lipc) {
			tag = L4_Lipc(L4_nilthread, L4_anylocalthread,
				L4_Timeouts(L4_ZeroTime, TEST_IPC_DELAY), &send_from);
		} else {
			tag = L4_WaitLocal_Timeout(TEST_IPC_DELAY, &send_from);
		}
	} else {
		send_from = sender;
		if(recv_lipc) {
			L4_ThreadId_t dummy;
			tag = L4_Lipc(L4_nilthread, sender,
				L4_Timeouts(L4_ZeroTime, TEST_IPC_DELAY), &dummy);
		} else {
			tag = L4_Receive_Timeout(sender, TEST_IPC_DELAY);
		}
	}
	L4_Word_t ec = L4_ErrorCode();
	L4_Word_t payload; L4_StoreMR(1, &payload);
	if(!ok(L4_IpcSucceeded(tag) && L4_Label(tag) == 0xd00d, "got ipc")) {
		diag("tag=%#lx, ec=%#lx", tag.raw, ec);
	}
	if(!ok(L4_SameThreads(send_from, sender), "ipc was from local sender")) {
		diag("send_from=%#lx, ec=%#lx", send_from.raw, ec);
	}
	if(!ok1(payload == good_val)) diag("payload=%#lx", payload);
	if(L4_IpcSucceeded(tag)) {
		L4_LoadMR(0, 0);
		tag = L4_Reply(sender);	/* always reply by regular Ipc. */
		if(L4_IpcFailed(tag)) {
			diag("reply failed, ec=%#lx", L4_ErrorCode());
		}
	}

	/* get status from the fork-spawner thread to confirm that it was able to
	 * start a thread with the same local TID as the proper sender.
	 */
	tag = L4_Receive_Timeout(child_tid, TEST_IPC_DELAY);
	L4_Word_t c_status; L4_StoreMR(1, &c_status);
	ok(L4_IpcSucceeded(tag) && c_status == 0,
		"same-LTID sender was started in other space");

	/* pump the other child messages to have a cleaner output log. */
	do {
		L4_ThreadId_t tid;
		tag = L4_Wait_Timeout(TEST_IPC_DELAY, &tid);
		if(L4_IpcSucceeded(tag)) {
			L4_StoreMR(1, &payload);
			if(payload != bad_val) {
				diag("got payload=%#lx from tid=%lu:%lu", payload,
					L4_ThreadNo(tid), L4_Version(tid));
			}
			L4_LoadMR(0, 0);
			L4_Reply(tid);
		}
	} while(L4_IpcSucceeded(tag));

	int st, dead = wait(&st);
	fail_unless(dead == child, "dead=%d, child=%d (st=%d)", dead, child, st);

	for(int i=0; i < NUM_ELEMENTS(burners); i++) {
		if(L4_IsNilThread(burners[i])) continue;
		L4_LoadMR(0, 0);
		L4_Send_Timeout(burners[i], A_SHORT_NAP);
		xjoin_thread(burners[i]);
	}
	xjoin_thread(sender);
}
END_TEST


struct map_receiver_param {
	L4_ThreadId_t parent;
	size_t range_shift;
};


static void map_receiver_thread(void *param_ptr)
{
	const struct map_receiver_param *param = param_ptr;
	size_t acc_size = 1 << param->range_shift;
	void *acc_base = NULL,
		*acc_mem = alloc_aligned(&acc_base, acc_size, acc_size);
	fail_if(acc_mem == NULL);
	L4_Fpage_t acc_page = L4_FpageLog2((L4_Word_t)acc_mem,
		param->range_shift);
	diag("acc_page %#lx:%#lx, acc_base=%p",
		L4_Address(acc_page), L4_Size(acc_page), acc_base);
	L4_Acceptor_t acc = L4_MapGrantItems(acc_page);

	bool running = true;
	while(running) {
		L4_FlushFpage(acc_page);
		L4_Accept(acc);
		L4_MsgTag_t tag = L4_Receive(param->parent);
		if(L4_IpcFailed(tag)) {
			diag("%s: ipc fail, ec=%#lx", __func__, L4_ErrorCode());
			continue;
		} else if(L4_Label(tag) == QUIT_LABEL) {
			running = false;
		} else {
			L4_MapItem_t maps[32];
			L4_StoreMRs(L4_UntypedWords(tag) + 1, L4_TypedWords(tag),
				maps[0].raw);
			for(int i=0; i < L4_TypedWords(tag) / 2; i++) {
				L4_Fpage_t fp = L4_MapItemSndFpage(maps[i]);
				L4_Word_t offset = L4_SizeLog2(fp) < L4_SizeLog2(acc_page)
					? L4_MapItemSndBase(maps[i]) & ~(L4_Size(fp) - 1)
					: 0;
				diag("got map %d = %#lx:%#lx +%#lx (%#lx) -> %#lx", i,
					L4_Address(fp), L4_Size(fp),
					L4_MapItemSndBase(maps[i]), L4_Rights(fp),
					L4_Address(acc_page) + offset);
				if(CHECK_FLAG(L4_Rights(fp), L4_Writable)) {
					char *ptr = (char *)(L4_Address(acc_page) + offset);
					diag("ptr=%p", ptr);
					assert(ptr >= (char *)acc_mem);
					if(ptr < (char *)acc_mem + acc_size) *ptr = 123;
				}
			}
		}

		L4_LoadMR(0, 0);
		L4_Reply(param->parent);
	}

	L4_FlushFpage(acc_page);
	free(acc_base);
}


static L4_MapItem_t mapgrantitem(
	L4_Fpage_t fp,
	int rights,
	L4_Word_t sendbase,
	bool is_grant)
{
	L4_Set_Rights(&fp, rights & L4_FullyAccessible);
	union {
		L4_MapItem_t mi;
		L4_GrantItem_t gi;
	} item;
	if(!is_grant) item.mi = L4_MapItem(fp, sendbase);
	else item.gi = L4_GrantItem(fp, sendbase);

	return item.mi;
}


/* sending MapItems and GrantItems into a sufficiently large acceptor */
START_LOOP_TEST(map_into_large_acceptor, iter, 0, 1)
{
	plan_tests(6);
	const bool is_grant = CHECK_FLAG(iter, 1);
	diag("is_grant=%s", btos(is_grant));

	struct map_receiver_param *param = malloc(sizeof(*param));
	*param = (struct map_receiver_param){
		.parent = L4_Myself(),
		.range_shift = 15,
	};
	L4_ThreadId_t other_tid;
	int child = fork_tid(&other_tid);
	if(child == 0) {
		map_receiver_thread(param);
		exit(0);
	}

	/* base case: SndBase 0. */
	const size_t two_pages = PAGE_SIZE * 2;
	void *mem_base = NULL;
	char *mem = alloc_aligned(&mem_base, two_pages, two_pages);
	diag("mem_base=%p, mem=%p", mem_base, mem);
	memset(mem, 0, two_pages);
	L4_Fpage_t fp = L4_Fpage((L4_Word_t)mem, two_pages);
	L4_Set_Rights(&fp, L4_FullyAccessible);
	diag("fp=%#lx:%#lx", L4_Address(fp), L4_Size(fp));
	L4_MapItem_t mi = mapgrantitem(fp, L4_FullyAccessible, 0, is_grant);
	L4_LoadMR(0, (L4_MsgTag_t){ .X.t = 2 }.raw);
	L4_LoadMRs(1, 2, mi.raw);
	L4_MsgTag_t tag = L4_Call(other_tid);
	ok(L4_IpcSucceeded(tag), "base ipc ok");
	ok(mem[0] != 0, "mem was modified");

	/* experiment 1: set SndBase to two_pages * 1. */
	memset(mem, 0, two_pages);
	mi = mapgrantitem(fp, L4_FullyAccessible, two_pages, is_grant);
	L4_LoadMR(0, (L4_MsgTag_t){ .X.t = 2 }.raw);
	L4_LoadMRs(1, 2, mi.raw);
	tag = L4_Call(other_tid);
	ok(L4_IpcSucceeded(tag), "offset ipc ok");
	ok(mem[0] != 0, "mem was modified despite offset");

	/* experiment 2: set SndBase to a spot that's outside the receive
	 * window.
	 */
	memset(mem, 0, two_pages);
	mi = mapgrantitem(fp, L4_FullyAccessible,
		(1 << param->range_shift) + two_pages, is_grant);
	L4_LoadMR(0, (L4_MsgTag_t){ .X.t = 2 }.raw);
	L4_LoadMRs(1, 2, mi.raw);
	tag = L4_Call(other_tid);
	ok(L4_IpcSucceeded(tag), "out-of-window ipc ok");
	bool all_zero = true;
	for(int i=0; i < two_pages; i++) {
		if(mem[i] != 0) {
			all_zero = false;
			break;
		}
	}
	ok(all_zero, "no change to sent page");

	send_quit(other_tid);
	int st = -1, dead = wait(&st);
	if(dead != child) {
		diag("odd wait result: st=%d, dead=%d (child=%d)", st, dead, child);
	}

	free(param);
	free(mem_base);
}
END_TEST


/* sending MapItems and GrantItems into a smaller acceptor */
START_LOOP_TEST(map_into_small_acceptor, iter, 0, 1)
{
	plan_tests(6);
	const bool is_grant = CHECK_FLAG(iter, 1);
	diag("is_grant=%s", btos(is_grant));

	struct map_receiver_param *param = malloc(sizeof(*param));
	*param = (struct map_receiver_param){
		.parent = L4_Myself(),
		.range_shift = 13,		/* a paltry 8k. */
	};
	L4_ThreadId_t other_tid;
	int child = fork_tid(&other_tid);
	if(child == 0) {
		map_receiver_thread(param);
		exit(0);
	}

	/* base case: SndBase 0. */
	const size_t big = (1 << param->range_shift) * 4;
	void *mem_base = NULL;
	char *mem = alloc_aligned(&mem_base, big, big);
	memset(mem, 0, big);
	L4_Fpage_t fp = L4_Fpage((L4_Word_t)mem, big);
	L4_Set_Rights(&fp, L4_FullyAccessible);
	diag("fp=%#lx:%#lx", L4_Address(fp), L4_Size(fp));
	L4_MapItem_t mi = mapgrantitem(fp, L4_FullyAccessible, 0, is_grant);
	L4_LoadMR(0, (L4_MsgTag_t){ .X.t = 2 }.raw);
	L4_LoadMRs(1, 2, mi.raw);
	L4_MsgTag_t tag = L4_Call(other_tid);
	ok(L4_IpcSucceeded(tag), "base ipc ok");
	ok(mem[0] != 0, "mem was modified");

	/* experiment 1: set SndBase to (1 << range_shift) * 2. */
	const size_t offset = (1 << param->range_shift) * 2;
	diag("offset=%#lx", (unsigned long)offset);
	assert(offset < big);
	memset(mem, 0, big);
	mi = mapgrantitem(fp, L4_FullyAccessible, offset, is_grant);
	L4_LoadMR(0, (L4_MsgTag_t){ .X.t = 2 }.raw);
	L4_LoadMRs(1, 2, mi.raw);
	tag = L4_Call(other_tid);
	ok(L4_IpcSucceeded(tag), "offset ipc ok");
	ok(mem[offset] != 0, "mem was modified at offset");

	/* experiment 2: set SndBase to big * 2. */
	memset(mem, 0, big);
	mi = mapgrantitem(fp, L4_FullyAccessible, big * 2, is_grant);
	L4_LoadMR(0, (L4_MsgTag_t){ .X.t = 2 }.raw);
	L4_LoadMRs(1, 2, mi.raw);
	tag = L4_Call(other_tid);
	ok(L4_IpcSucceeded(tag), "too-big sndbase ok");
	bool all_zero = true;
	for(int i=0; i < big; i++) {
		if(mem[i] != 0) {
			all_zero = false;
			break;
		}
	}
	ok(all_zero, "no change to sent page");

	send_quit(other_tid);
	int st = -1, dead = wait(&st);
	if(dead != child) {
		diag("odd wait result: st=%d, dead=%d (child=%d)", st, dead, child);
	}

	free(param);
	free(mem_base);
}
END_TEST


/* IPC sleep with a TimePoint value. passes when it not only doesn't panic the
 * microkernel, but also when it drops out of the sleep at the correct time.
 */
START_LOOP_TEST(point_ipc_timeouts, iter, 0, 15)
{
	const unsigned pre_us = 800 * iter, sleep_us = 20 * 1000;
	assert(pre_us < sleep_us);

	plan_tests(2);

	L4_Clock_t pt = { .raw = L4_SystemClock().raw + sleep_us };
	L4_Time_t t = L4_TimePoint(pt);
	fail_unless(L4_IsTimePoint_NP(t));

	/* part 1: a timeout on a time point should happen at the specified time
	 * regardless of interim sleeps or the like.
	 */
	if(pre_us > 0) L4_Sleep(L4_TimePeriod(pre_us));
	L4_Sleep(t);
	L4_Clock_t wake_at = L4_SystemClock();
	ok(wake_at.raw - pt.raw < 200, "woke up at t + %u µs", sleep_us);

	/* part 2: an invalid (expired) timepoit should be handled like
	 * L4_ZeroTime.
	 */
	L4_Sleep(L4_TimePeriod(1000));	/* put it definitely in the swamp */

	L4_Clock_t pre_sleep = L4_SystemClock();
	fail_if(pre_sleep.raw <= pt.raw);
	L4_Sleep(t);
	wake_at = L4_SystemClock();
	ok(wake_at.raw - pre_sleep.raw < 200,
		"didn't sleep with expired point");
}
END_TEST


/* call IpcHelper::munge_string() with buffers that're known not to be mapped.
 * compare result with locally-computed equivalent via crc32c().
 */
static int munge_case(
	void *memctx,
	L4_ThreadId_t partner_tid,
	size_t munge_size,
	uint32_t munge_seed)
{
	char *munge_in = talloc_size(memctx, munge_size + 1),
		*munge_out = talloc_size(memctx, munge_size + 1);
	uint32_t rnd_seed = 0xcabb1e23;
	random_string(munge_in, munge_size, &rnd_seed);
	munge_string_local(munge_in, munge_out, munge_seed);
	uint32_t local_crc = crc32c(0, munge_out, munge_size);

	flush_byte_range((uintptr_t)munge_in, munge_size, L4_FullyAccessible);
	flush_byte_range((uintptr_t)munge_out, munge_size, L4_FullyAccessible);

	int n = __ipchelper_munge_string(partner_tid,
		munge_in, munge_out, munge_seed);
	if(n == 0) {
		uint32_t remote_crc = crc32c(0, munge_out, munge_size);
		if(remote_crc != local_crc) {
			diag("remote_crc=%#x, local_crc=%#x", __func__,
				(unsigned)remote_crc, (unsigned)local_crc);
		}
	}

	return n;
}


/* same, but for xfer timeouts. this involves a string transfer helper thread,
 * and a delaying pager.
 */
START_TEST(point_xfer_timeouts)
{
	const size_t munge_size = 11 * 1024 + 77;
	const uint32_t munge_seed = 0x715517da;
	const int to_us = 25 * 1000, pg_delay_us = 5 * 1000;
	void *memctx = talloc_new(NULL);

	plan_tests(4);

	L4_Set_Pager(helper_tid);
	int n = __ipchelper_sleep(helper_tid, 0, false, 0);
	fail_unless(n == 0, "init sleep failed, n %#x", (unsigned)n);

	/* base case: no timeout when none is given. */
	L4_Set_XferTimeouts(L4_Timeouts(L4_Never, L4_Never));
	n = munge_case(memctx, other_helper_tid, munge_size, munge_seed);
	if(!ok1(n == 0)) diag("n=%d", n);

	/* part #1: no timeout when timeout is given, but no delay. */
	L4_Clock_t base = L4_SystemClock();
	L4_Time_t to_pt = L4_TimePoint2_NP(base,
		(L4_Clock_t){ .raw = base.raw + to_us });
	L4_Set_XferTimeouts(L4_Timeouts(to_pt, to_pt));
	n = munge_case(memctx, other_helper_tid, munge_size, munge_seed);
	if(!ok1(n == 0)) diag("n=%d", n);

	/* part #2: timeout should occur when delay is set. */
	L4_Set_XferTimeouts(L4_Timeouts(L4_Never, L4_Never));
	n = __ipchelper_sleep(helper_tid, pg_delay_us, false, 0);
	fail_unless(n == 0, "delay-setting sleep failed, n %#x", (unsigned)n);

	base = L4_SystemClock();
	to_pt = L4_TimePoint2_NP(base, (L4_Clock_t){ .raw = base.raw + to_us });
	L4_Sleep(L4_TimePeriod(to_us - pg_delay_us));
	if(!pt_is_valid(L4_SystemClock(), to_pt)) {
		diag("base=%#lx, to_pt={e=%d, m=%#x, c=%d} -> @%#lx",
			(L4_Word_t)base.raw, to_pt.point.e, to_pt.point.m, to_pt.point.c,
			(L4_Word_t)L4_PointClock_NP(base, to_pt).raw);
	}
	fail_unless(pt_is_valid(L4_SystemClock(), to_pt));

	L4_Set_XferTimeouts(L4_Timeouts(to_pt, to_pt));
	n = munge_case(memctx, other_helper_tid, munge_size, munge_seed);
	L4_Clock_t after = L4_SystemClock();
	diag("after=%#lx", (L4_Word_t)after.raw);
	int code = (n >> 1) & 0x7;
	if(!ok(code == 5 || code == 6, "hit xfer timeout")) diag("n=%d", n);
	int64_t end_diff = (int64_t)after.raw - base.raw - to_us;
	if(!ok1(end_diff > 0)) diag("end_diff=%d", (int)end_diff);

	talloc_free(memctx);
}
END_TEST


static void recv_spin_fn(void *param_ptr)
{
	const L4_Word_t *ps = param_ptr;
	int sleep_us = ps[0];

	L4_ThreadId_t from;
	L4_MsgTag_t tag = L4_Wait(&from);
	if(L4_IpcFailed(tag)) {
		printf("%s: ipc failed, ec %#lx\n", __func__, L4_ErrorCode());
		goto end;
	}

	usleep(sleep_us);

end:
	free(param_ptr);
}


/* tests that a send from a lower to higher priority thread causes a
 * scheduling preemption. also tests that a send to a lower-priority thread
 * causes no preemption. (sending to a same-priority thread may cause the IPC
 * to be active-received by the peer, which correctly doesn't pre-empt it; the
 * test's experiment would be inconclusive.)
 *
 * FIXME: see comment at r_recv_timeout_case; same applies here
 */
START_LOOP_TEST(send_preempt, iter, 0, 15)
{
	plan_tests(4);

	const bool p_preempt = CHECK_FLAG(iter, 1), p_delay = CHECK_FLAG(iter, 2),
		p_highsens = CHECK_FLAG(iter, 4), p_lowmax = CHECK_FLAG(iter, 8);
	diag("p_preempt=%s, p_delay=%s, p_highsens=%s, p_lowmax=%s",
		btos(p_preempt), btos(p_delay), btos(p_highsens), btos(p_lowmax));

	const bool full_delay = p_delay && p_highsens && !p_lowmax,
		short_delay = p_delay && p_highsens && p_lowmax;
	const int start_pri = find_own_priority(),
		spin_us = 5000;	/* 5 ms */
	diag("full_delay=%s, short_delay=%s, start_pri=%d, spin_us=%d",
		btos(full_delay), btos(short_delay), start_pri, spin_us);
	fail_unless(start_pri >= 12,
		"need start_pri at least 12, got %d", start_pri);

	L4_Word_t *param = malloc(sizeof(L4_Word_t));
	param[0] = spin_us;
	L4_ThreadId_t other = xstart_thread(&recv_spin_fn, param);
	L4_ThreadSwitch(other);

	L4_Word_t ret = L4_Set_PreemptionDelay(L4_Myself(),
		p_highsens ? start_pri : start_pri - 2,
		p_lowmax ? spin_us - 2000 : spin_us * 2);
	fail_if(ret == 0, "set_ped: ret=%lu, ec=%#lx", ret, L4_ErrorCode());
	if(p_delay) L4_DisablePreemption();

	ret = L4_Set_Priority(p_preempt ? L4_Myself() : other, start_pri - 11);
	fail_if(ret == 0, "ret %lu, ec %#lx", ret, L4_ErrorCode());

	next_tick();
	L4_Clock_t start = L4_SystemClock();
	L4_LoadMR(0, 0);
	L4_MsgTag_t tag = L4_Send_Timeout(other, TEST_IPC_DELAY);
	fail_if(L4_IpcFailed(tag), "ec %#lx", L4_ErrorCode());
	L4_Clock_t end = L4_SystemClock();
	usleep(spin_us);
	bool was_pending = L4_PreemptionPending(),
		was_disabled = L4_DisablePreemption();
	if(p_delay) {
		if(was_pending) L4_ThreadSwitch(L4_nilthread);
	}
	uint64_t diff_us = end.raw - start.raw;

	diag("diff_us=%lu, was_pending=%s, was_disabled=%s",
		(unsigned long)diff_us, btos(was_pending), btos(was_disabled));

	if(diff_us == spin_us - 1000) {
		/* to counter measurement jitter due to shitty timers and whatnot */
		diag("FUDGING: diff_us += 1000");
		diff_us += 1000;
	}

	/* check that return from the send-only Ipc happens immediately when
	 * preemption doesn't occur or is delayed, and after the other thread
	 * completes spinning otherwise.
	 */
	imply_ok1(!p_preempt || (p_delay && p_highsens), diff_us < 2000);
	imply_ok1(p_preempt && (!p_delay || !p_highsens), diff_us >= spin_us);

	/* see comment in recv_preempt */
	imply_ok1(p_preempt && short_delay, !was_pending && !was_disabled);
	imply_ok1(p_preempt && full_delay, was_pending && was_disabled);

	xjoin_thread(other);
}
END_TEST


/* helper of recv_preempt */
static void send_spin_fn(void *param_ptr)
{
	const L4_Word_t *ps = param_ptr;
	unsigned int sleep_us = ps[0];
	bool p_sendwait = ps[1];
	L4_ThreadId_t dest = { .raw = ps[2] };
	free(param_ptr);

	L4_ThreadId_t from;
	L4_MsgTag_t tag;
	L4_LoadMR(0, 0);
	if(p_sendwait) {
		tag = L4_Ipc(dest, L4_anythread,
			L4_Timeouts(TEST_IPC_DELAY, TEST_IPC_DELAY), &from);
	} else {
		tag = L4_Send_Timeout(dest, TEST_IPC_DELAY);
		from = L4_nilthread;
	}
	if(L4_IpcFailed(tag)) {
		diag("%s: ipc failed, ec %#lx", __func__, L4_ErrorCode());
		return;
	}

	if(sleep_us > 0) usleep(sleep_us);
}


/* basically the same thing as send_preempt, but for preemptions due to active
 * receive, and covers also preemptions due to immediately subsequent active
 * receive by the peer.
 */
START_LOOP_TEST(recv_preempt, iter, 0, 31)
{
	plan_tests(4);

	const bool p_preempt = CHECK_FLAG(iter, 1), p_delay = CHECK_FLAG(iter, 2),
		p_highsens = CHECK_FLAG(iter, 4), p_lowmax = CHECK_FLAG(iter, 8),
		p_sendwait = CHECK_FLAG(iter, 16);
	diag("p_preempt=%s, p_delay=%s, p_highsens=%s, p_lowmax=%s, p_sendwait=%s",
		btos(p_preempt), btos(p_delay), btos(p_highsens), btos(p_lowmax),
		btos(p_sendwait));

	const bool full_delay = p_delay && p_highsens && !p_lowmax,
		short_delay = p_delay && p_highsens && p_lowmax;
	const int start_pri = find_own_priority(),
		spin_us = 5000;	/* 5 ms */
	diag("full_delay=%s, short_delay=%s, start_pri=%d, spin_us=%d",
		btos(full_delay), btos(short_delay), start_pri, spin_us);
	fail_unless(start_pri >= 12,
		"need start_pri at least 12, got %d", start_pri);

	L4_Word_t *param = malloc(sizeof(L4_Word_t) * 3);
	param[0] = spin_us;
	param[1] = p_sendwait;
	param[2] = L4_Myself().raw;
	L4_ThreadId_t other = start_thread_long(&send_spin_fn, param,
		-1, L4_TimePeriod(50 * 1000), L4_Never);
	fail_if(L4_IsNilThread(other));

	L4_ThreadId_t other2 = L4_nilthread;
	if(p_sendwait) {
		param = malloc(sizeof(L4_Word_t) * 3);
		param[0] = 0;
		param[1] = false;
		param[2] = other.raw;
		other2 = start_thread_long(&send_spin_fn, param, -1,
			L4_TimePeriod(20 * 1000), L4_Never);
		fail_if(L4_IsNilThread(other2));
		L4_ThreadSwitch(other2);
	}

	L4_ThreadSwitch(other);

	L4_Word_t ret = L4_Set_PreemptionDelay(L4_Myself(),
		p_highsens ? start_pri : start_pri - 2,
		p_lowmax ? spin_us - 2000 : spin_us * 2);
	fail_if(ret == 0, "set_ped: ret=%lu, ec=%#lx", ret, L4_ErrorCode());
	if(p_delay) L4_DisablePreemption();

	ret = L4_Set_Priority(p_preempt ? L4_Myself() : other, start_pri - 11);
	fail_if(ret == 0, "ret %lu, ec %#lx", ret, L4_ErrorCode());
#if 0
	/* NOTE: this segment makes sure that other2 won't preempt the testing
	 * thread. it's uncertain whether this breaks the test, or if the kernel's
	 * lack of delay handling in active-receive preempts does that instead.
	 */
	if(!L4_IsNilThread(other2) && p_preempt) {
		ret = L4_Set_Priority(other2, start_pri - 11);
		fail_if(ret == 0, "ret %lu, ec %#lx", ret, L4_ErrorCode());
	}
#endif

	next_tick();
	L4_Clock_t start = L4_SystemClock();
	L4_LoadBR(0, 0);
	L4_MsgTag_t tag = L4_Receive_Timeout(other, TEST_IPC_DELAY);
	fail_if(L4_IpcFailed(tag), "ec %#lx", L4_ErrorCode());
	L4_Clock_t end = L4_SystemClock();
	usleep(spin_us);
	bool was_pending = L4_PreemptionPending(),
		was_disabled = L4_DisablePreemption();
	if(p_delay) {
		if(was_pending) L4_ThreadSwitch(L4_nilthread);
	}
	uint64_t diff_us = end.raw - start.raw;

	diag("diff_us=%lu, was_pending=%s, was_disabled=%s",
		(unsigned long)diff_us, btos(was_pending), btos(was_disabled));

	if(diff_us == spin_us - 1000) {
		/* to counter measurement jitter due to shitty timers and whatnot */
		diag("FUDGING: diff_us += 1000");
		diff_us += 1000;
	}

	/* check that return from the recv-only Ipc happens immediately when
	 * preemption doesn't occur or is delayed, and after the other thread
	 * completes spinning otherwise.
	 */
	imply_ok1(!p_preempt || (p_delay && p_highsens), diff_us < 2000);
	imply_ok1(p_preempt && (!p_delay || !p_highsens), diff_us >= spin_us);

	/* preemption due to max delay running out should cause the "preemption
	 * pending" bit to read clear after our spin. its absence should cause it
	 * to read set.
	 *
	 * furthermore, a preemption of the delay segment should clear the delay
	 * flag.
	 */
	imply_ok1(p_preempt && short_delay, !was_pending && !was_disabled);
	imply_ok1(p_preempt && full_delay, was_pending && was_disabled);

	xjoin_thread(other);
	if(!L4_IsNilThread(other2)) xjoin_thread(other2);
}
END_TEST


START_LOOP_TEST(recv_timeout_from_send, iter, 0, 3)
{
	plan_tests(6);

	const bool p_spin = CHECK_FLAG(iter, 1),
		p_preempt = CHECK_FLAG(iter, 2);
	const unsigned int sleep_ms = 70, timeo_ms = sleep_ms / 2;
	diag("p_spin=%s, p_preempt=%s", btos(p_spin), btos(p_preempt));
	diag("sleep_ms=%u, timeo_ms=%u", sleep_ms, timeo_ms);

	if(p_preempt) {
		/* variant: with the helper at a higher priority, receive timeout
		 * should start from send-phase completion, and still work properly.
		 */
		L4_Word_t ret = L4_Set_Priority(L4_Myself(),
			find_own_priority() - 10);
		fail_if(ret == 0, "set_priority failed: ret=%lu", ret);
	}

	/* part 1: shouldn't timeout without a timeout. */
	int n = __ipchelper_yield(helper_tid);
	fail_unless(n == 0, "n=%d", n);
	n = __ipchelper_sleep_timeout(helper_tid, sleep_ms * 1000, p_spin,
		0, L4_Never, L4_Never);
	if(n != 0) diag("n=%d", n);
	ok(n == 0, "base (no timeout)");

	/* part 2: should timeout with a timeout.
	 *
	 * when the partner is IPC-sleeping, the timeout should happen on
	 * schedule. when it's spinning, timeout shouldn't happen sooner than when
	 * the helper's quantum is exhausted (!p_preempt), or at sleep completion
	 * (otherwise).
	 */
	n = __ipchelper_yield(helper_tid);
	fail_unless(n == 0, "n=%d", n);
	L4_Clock_t start = L4_SystemClock();
	n = __ipchelper_sleep_timeout(helper_tid, sleep_ms * 1000, p_spin,
		0, L4_Never, L4_TimePeriod(timeo_ms * 1000));
	L4_Clock_t end = L4_SystemClock();
	const int code = (n >> 1) & 0x7;
	const bool send_phase = !CHECK_FLAG(n, 1);
	diag("n=%d, code=%d, send_phase=%s", n, code, btos(send_phase));
	ok1(!send_phase);
	ok(code == 1, "is timeout");
	int diff_us = end.raw - start.raw,
		diff_ms = diff_us / 1000;
	diag("diff_us=%d, diff_ms=%d", diff_us, diff_ms);
	ok1(p_spin || diff_ms <= timeo_ms + 2);

	/* p_spin && !p_preempt ==> diff_ms = 50 (within error).
	 * (50 ms comes from start_thread()'s default quantum.)
	 */
	ok1(!p_spin || p_preempt || (diff_ms >= 50 && diff_ms < 55));

	/* p_spin && p_preempt ==> diff_ms >= sleep_ms. */
	ok1(!p_spin || !p_preempt || diff_ms >= sleep_ms);
}
END_TEST


/* synopsis: test that receive timeout is applied from send-phase completion
 * even under instant preemption. this reflects the (potentially late) R_RECV
 * -> RECV_WAIT transition by the scheduler, which should retain the timeout
 * set in the previous transition to R_RECV.
 *
 * plan: call IpcHelper::sleep() from a lower-priority thread. spin for a
 * shorter time than the timeout, but sleep for long enough after to pop the
 * timeout -- or not, if it wasn't retained from R_RECV.
 */
START_TEST(recv_timeout_from_preempt)
{
	plan_tests(2);
	const unsigned timeo_ms = 50, spin_ms = 35, sleep_ms = 25;

	L4_Word_t ret = L4_Set_Priority(L4_Myself(), find_own_priority() - 10);
	fail_if(ret == 0, "setpri failed, ret=%lu", ret);

	int n = __ipchelper_yield(helper_tid);
	fail_unless(n == 0, "n=%d", n);
	n = __ipchelper_sleep_timeout(helper_tid, spin_ms * 1000, true,
		sleep_ms * 1000, L4_Never, L4_TimePeriod(timeo_ms * 1000));
	const int code = (n >> 1) & 0x7;
	const bool send_phase = !CHECK_FLAG(n, 1);
	diag("n=%d, code=%d, send_phase=%s", n, code, btos(send_phase));
	ok1(!send_phase);
	ok(code == 1, "is timeout");
}
END_TEST


/* test case for the correct ipc error result in threads that time out in the
 * TS_R_RECV state.
 *
 * two cases: with an incoming message, and without. the former tests whether
 * the TS_R_RECV timeout is handled correctly in the receive override case,
 * which isn't implemented as of 2012-05-06.
 *
 * FIXME: this should be verified properly.
 * TODO: and combined into an iterative test rather than four calls to
 * r_recv_timeout_case().
 */
static void r_recv_timeout_fn(void *param_ptr)
{
	/* in-parameters: [0] = partner TID, [1] = timeout (L4_Time_t) */
	L4_Word_t *param = param_ptr;
	L4_ThreadId_t partner = { .raw = param[0] };
	L4_Time_t timeout = { .raw = param[1] };
#if 0
	diag("in helper %lu:%lu; partner=%lu:%lu, timeout=%u µs",
		L4_ThreadNo(L4_Myself()), L4_Version(L4_Myself()),
		L4_ThreadNo(partner), L4_Version(partner),
		(unsigned)L4_PeriodUs_NP(timeout));
#endif

	L4_LoadMR(0, (L4_MsgTag_t){ .X.u = 1 }.raw);
	L4_LoadMR(1, 0xfedcba98);
	L4_ThreadId_t dummy;
	L4_MsgTag_t tag = L4_Ipc(partner, partner,
		L4_Timeouts(L4_Never, timeout), &dummy);
	if(L4_IpcSucceeded(tag)) param[0] = 0; else param[0] = L4_ErrorCode();

	L4_LoadMR(0, 0);
	L4_Send(partner);
}


/* FIXME: change thread.c, forkserv to set scheduler for forked processes'
 * threads like it is in the root task's threads, i.e. that the creator can
 * schedule its own threads.
 *
 * for now the test runs without fork.
 */
static L4_Word_t r_recv_timeout_case(int priority, bool spin, bool send)
{
	const int timeout_ms = 20;

	L4_Word_t *param = malloc(sizeof(*param) * 2);
	fail_if(param == NULL);
	param[0] = L4_Myself().raw;
	param[1] = L4_TimePeriod(timeout_ms * 1000).raw;
	L4_ThreadId_t helper = xstart_thread(&r_recv_timeout_fn, param);

	L4_Word_t res = L4_Set_Priority(helper, priority);
	fail_unless((res & 0xff) != 0, "res=%#lx, ec=%#lx",
		res, L4_ErrorCode());

	L4_MsgTag_t tag = L4_Receive(helper);
	fail_unless(L4_IpcSucceeded(tag), "ec=%#lx", L4_ErrorCode());

	if(spin) {
		L4_Clock_t start = L4_SystemClock();
		do {
			delay_loop(iters_per_tick / 100);
		} while(start.raw == L4_SystemClock().raw);

		start = L4_SystemClock();
		do {
			/* ishygddt */
			delay_loop(iters_per_tick);
		} while(start.raw + timeout_ms * 1000 > L4_SystemClock().raw);
	}

	if(send) {
		L4_LoadMR(0, 0);
		L4_Reply(helper);
	}

	/* get sync */
	tag = L4_Receive_Timeout(helper, L4_TimePeriod(150 * 1000));
	IPC_FAIL(tag);

	xjoin_thread(helper);
	L4_Word_t ret = param[0];
	free(param);

	return ret;
}


START_TEST(r_recv_timeout_test)
{
	plan_tests(4);
	const int own_pri = find_own_priority(), test_pri = own_pri - 2;
	diag("test_pri=%d, own_pri=%d", test_pri, own_pri);

	ok(r_recv_timeout_case(test_pri, false, false) == 0x3,
		"timeout in immediate nosend");
	ok(r_recv_timeout_case(test_pri, false, true) == 0,
		"successful immediate send");
	ok(r_recv_timeout_case(test_pri, true, false) == 0x3,
		"timeout in spin, nosend");
	ok(r_recv_timeout_case(test_pri, true, true) == 0x3,
		"timeout in spin, send");
}
END_TEST


/* cancel_sender's IPC partner which never picks up. */
static void do_nothing_fn(void *param UNUSED) {
	for(;;) L4_Sleep(L4_Never);
}


struct sar_param {
	L4_ThreadId_t dest_tid;
	L4_ThreadId_t prop_as;		/* propagatee, or nil */
	L4_ThreadId_t parent_tid;	/* report target */
};


/* cancel_sender's test half. */
static void send_and_report_fn(void *param_ptr)
{
	struct sar_param *p = param_ptr;
	diag("%s: self=%lu:%lu, parent=%lu:%lu", __func__,
		L4_ThreadNo(L4_MyGlobalId()), L4_Version(L4_MyGlobalId()),
		L4_ThreadNo(p->parent_tid), L4_Version(p->parent_tid));
	L4_MsgTag_t tag = { };
	if(!L4_IsNilThread(p->prop_as)) {
		L4_Set_VirtualSender(p->prop_as);
		L4_Set_Propagation(&tag);
	}
	L4_LoadMR(0, tag.raw);
	tag = L4_Send_Timeout(p->dest_tid, TEST_IPC_DELAY);
	L4_Word_t ec = L4_IpcSucceeded(tag) ? 0 : L4_ErrorCode();

	/* report back. */
	L4_LoadMR(0, (L4_MsgTag_t){ .X.label = 0xfeed, .X.u = 2 }.raw);
	L4_LoadMR(1, tag.raw);
	L4_LoadMR(2, ec);
	L4_Send_Timeout(p->parent_tid, TEST_IPC_DELAY);

	free(p);
}


static L4_ThreadId_t start_sar_long(
	L4_ThreadId_t dest_tid,
	L4_ThreadId_t prop_as,
	L4_ThreadId_t parent_tid)
{
	struct sar_param *p = malloc(sizeof(*p));
	*p = (struct sar_param){
		.prop_as = prop_as,
		.parent_tid = parent_tid,
		.dest_tid = dest_tid,
	};
	return xstart_thread(&send_and_report_fn, p);
}


#define start_sar(dest, vs) start_sar_long((dest), (vs), L4_MyGlobalId())


/* tests for correct error reporting when a passive send's destination is
 * removed before timeout. covers both ordinary and propagated sends.
 *
 * NOTE: this test requires roottask privilege to rewrite the destination's
 * version bits.
 *
 * TODO: add tests to validate send_and_report_fn() in its success and timeout
 * parts.
 */
START_LOOP_TEST(cancel_sender, iter, 0, 7)
{
	plan_tests(3);
	const bool is_local = CHECK_FLAG(iter, 1), is_propa = CHECK_FLAG(iter, 2),
		is_rewrite = CHECK_FLAG(iter, 4);
	diag("is_local=%s, is_propa=%s, is_rewrite=%s",
		btos(is_local), btos(is_propa), btos(is_rewrite));

	L4_ThreadId_t recv_tid = xstart_thread(&do_nothing_fn, NULL),
		sink_tid = L4_nilthread, prop_tid = L4_nilthread;
	if(is_propa) {
		sink_tid = xstart_thread(&do_nothing_fn, NULL);
		prop_tid = start_sar(sink_tid, L4_nilthread);
	}
	L4_ThreadId_t send_tid = start_sar(is_local ? L4_LocalIdOf(recv_tid)
		: L4_GlobalIdOf(recv_tid), prop_tid);

	L4_MsgTag_t tag = L4_Receive_Timeout(send_tid, A_SHORT_NAP);
	ok(L4_IpcFailed(tag) && L4_ErrorCode() == 3, "sender didn't instafail");
	if(is_rewrite) {
		assert(L4_IsGlobalId(recv_tid));
		L4_ThreadId_t new_tid = L4_GlobalId(L4_ThreadNo(recv_tid),
			(L4_Version(recv_tid) * 2) | 3);
		L4_Word_t ret = L4_ThreadControl(new_tid, L4_Myself(),
			L4_nilthread, L4_nilthread, (void *)-1);
		fail_if(ret != 1, "threadctl failed, ec=%#lx", L4_ErrorCode());
	} else {
		/* straight-up murder */
		kill_thread(recv_tid);
	}
	tag = L4_Receive_Timeout(send_tid, TEST_IPC_DELAY);
	IPC_FAIL(tag);
	L4_StoreMR(1, &tag.raw);
	L4_Word_t ec; L4_StoreMR(2, &ec);
	if(!ok(L4_IpcFailed(tag) && ec == 4, "sender reports partner gone")) {
		diag("tag=%#lx, ec=%#lx\n", tag.raw, ec);
	}

	/* analyze the propagatee also. it should return a send timeout. */
	if(is_propa) {
		tag = L4_Receive_Timeout(prop_tid, TEST_IPC_DELAY);
		IPC_FAIL(tag);
		L4_StoreMR(1, &tag.raw);
		L4_StoreMR(2, &ec);
	}
	imply_ok(is_propa, L4_IpcFailed(tag) && ec == 2,
		"virtual sender's own ipc wasn't cancelled");

	/* cleanup */
	xjoin_thread(send_tid);
	if(is_propa) {
		kill_thread(sink_tid);
		xjoin_thread(prop_tid);
	}
	if(is_rewrite) {
		/* restore the version bits first */
		L4_Word_t ret = L4_ThreadControl(recv_tid, L4_Myself(),
			L4_nilthread, L4_nilthread, (void *)-1);
		fail_if(ret != 1, "threadctl failed, ec=%#lx", L4_ErrorCode());
		kill_thread(recv_tid);
	}
}
END_TEST


/* see start_vs_pair() */
struct vs_pair {
	L4_ThreadId_t vs_tid, sender_tid;
	int pid;				/* when forked, otherwise 0 */
	L4_ThreadId_t ctl_tid;	/* when forked, otherwise nil */
};


static void end_vs_pair(struct vs_pair *p)
{
	if(p->pid == 0) {
		kill_thread(p->vs_tid);
		xjoin_thread(p->sender_tid);
	} else {
#if 0
		diag("%s: syncing with ctl=%lu:%lu", __func__,
			L4_ThreadNo(p->ctl_tid), L4_Version(p->ctl_tid));
#endif
		L4_LoadMR(0, 0);
		L4_MsgTag_t tag = L4_Send(p->ctl_tid);
		IPC_FAIL(tag);

		int st = 0, pid = wait(&st);
		if(pid != p->pid) {
			diag("%s: unexpected dead child (got=%d, expected=%d)", __func__,
				pid, p->pid);
		}
	}
}


/* part of cancel_virtualsender and propagation_in_active_receive.
 *
 * starts two threads: one that's propagated on behalf of that does nothing
 * until killed, and another that propagates on its behalf. the threads are
 * created in the same address space, which is a forked process (PID returned,
 * or zero) if @is_fork is true.
 *
 * if @is_vs_local is true, the virtual sender will be given to the propagator
 * as a local ID and global otherwise; if @is_local && !@is_fork, the
 * destination will be given as a local ID and global otherwise. (so @is_local
 * is meaningless when @is_fork == true.)
 *
 * stores vs_tid, sender_tid into @ret. the pair should be shut down with
 * end_vs_pair().
 */
static void start_vs_pair(
	struct vs_pair *ret,
	bool is_local,
	bool is_vs_local,
	bool is_fork)
{
	L4_ThreadId_t parent_tid = L4_nilthread;
	if(is_fork) {
		ret->pid = fork_tid(&ret->ctl_tid);
		fail_if(ret->pid < 0);
		if(ret->pid > 0) {
			/* parent side */
			L4_LoadMR(0, 0);
			L4_MsgTag_t tag = L4_Call(ret->ctl_tid);
			IPC_FAIL(tag);
			L4_StoreMR(1, &ret->vs_tid.raw);
			L4_StoreMR(2, &ret->sender_tid.raw);
			return;
		} else {
			/* child side */
			L4_MsgTag_t tag = L4_Wait(&parent_tid);
			if(L4_IpcFailed(tag)) {
				diag("%s: forked child: wait failed, ec=%#lx", __func__,
					L4_ErrorCode());
				exit(1);
			}
		}
	} else {
		ret->pid = 0;
		ret->ctl_tid = L4_nilthread;
	}

	ret->vs_tid = xstart_thread(&do_nothing_fn, NULL);
	ret->sender_tid = start_sar_long(
		is_fork ? parent_tid
			: (is_local ? L4_MyLocalId() : L4_MyGlobalId()),
		is_vs_local ? L4_LocalIdOf(ret->vs_tid) : L4_GlobalIdOf(ret->vs_tid),
		is_fork ? parent_tid : L4_MyGlobalId());

	if(is_fork) {
		assert(ret->pid == 0);
		/* communicate vs_tid, sender_tid to parent */
		ret->vs_tid = L4_GlobalIdOf(ret->vs_tid);
		ret->sender_tid = L4_GlobalIdOf(ret->sender_tid);
		L4_LoadMR(0, (L4_MsgTag_t){ .X.u = 2 }.raw);
		L4_LoadMR(1, ret->vs_tid.raw);
		L4_LoadMR(2, ret->sender_tid.raw);
		L4_MsgTag_t tag = L4_Reply(parent_tid);
		if(L4_IpcFailed(tag)) {
			diag("%s: child parent reply failed, ec=%#lx", __func__,
				L4_ErrorCode());
			exit(1);
		}

		/* sync once more, then dispose of children and exit. */
#if 0
		diag("pre-sync parent=%lu:%lu, ctl[self]=%lu:%lu",
			L4_ThreadNo(parent_tid), L4_Version(parent_tid),
			L4_ThreadNo(L4_MyGlobalId()), L4_Version(L4_MyGlobalId()));
#endif
		tag = L4_Receive(parent_tid);
		if(L4_IpcFailed(tag)) {
			diag("%s: final sync failed, ec=%#lx", __func__, L4_ErrorCode());
		}
		assert(ret->pid == 0);
		end_vs_pair(ret);
		exit(0);
	}
}


static const char *fromspec_name[] = {
	"local", "anylocal", "global", "any",
};


/* see fromspec_name[] */
static L4_ThreadId_t fromspec_class(int n, L4_ThreadId_t tid)
{
	switch(n) {
		case 0: return L4_LocalIdOf(tid);
		case 1: return L4_anylocalthread;
		case 2: return L4_GlobalIdOf(tid);
		case 3: return L4_anythread;
		default: assert(false); return L4_nilthread;
	}
}


/* tests for correct error propagation and ActualSender reporting when
 * VirtualSender is removed before timeout.
 *
 * NOTE: this test's positive case (i.e. where propagation succeeds under the
 * given (is_fork, is_local, is_rewrite) triple) is covered by
 * propagation_in_active_receive.
 *
 * NOTE: also see cancel_sender's notes
 */
START_LOOP_TEST(cancel_virtualsender, iter, 0, 15)
{
	plan_tests(4);
	const bool is_fork = CHECK_FLAG(iter, 1),
		is_local = CHECK_FLAG(iter, 2),
		is_vs_local = CHECK_FLAG(iter, 4),
		is_rewrite = CHECK_FLAG(iter, 8);
	diag("is_fork=%s, is_local=%s, is_vs_local=%s, is_rewrite=%s",
		btos(is_fork), btos(is_local), btos(is_vs_local), btos(is_rewrite));

	/* experiment: propagation shouldn't happen when vs_tid is rendered
	 * invalid after passive send, but before active receive. @i varies the
	 * FromSpec class.
	 *
	 * TODO: particularly interesting is the iteration when is_rewrite &&
	 * !is_fork, on the definite local ID iteration. it's not specified what
	 * should happen with regard to whether propagation succeeds; i.e. whether
	 * the virtual sender's thread pointer is saved at IPC rendezvous or at
	 * the actual sender's Ipc syscall.
	 *
	 * the difference is that in the former reading, it depends on whether the
	 * sender specifies the virtualsender as a local or global thread ID
	 * (local succeeding after version rewrite, but global not), and in the
	 * latter the recipient's LTID identifies the right thread regardless.
	 *
	 * TODO: tests for either behaviour should be implemented in another test
	 * after further analysis.
	 */
	for(int i=0; i < 4; i++) {
		if(i < 2 && is_fork) {
			skip(1, "mode `%s' not applicable when is_fork=%s",
				fromspec_name[i], btos(is_fork));
			continue;
		}

		subtest_start("FromSpec=%s", fromspec_name[i]);
		plan_tests(8);
		const bool is_wild = (i == 1 || i == 3);
		diag("is_wild=%s", btos(is_wild));

		struct vs_pair p;
		start_vs_pair(&p, is_local, is_vs_local, is_fork);
		L4_ThreadId_t vs_tid = p.vs_tid, sender_tid = p.sender_tid;
		L4_ThreadId_t fromspec = fromspec_class(i, vs_tid), from;

		L4_Sleep(A_SHORT_NAP);

		if(is_rewrite) {
			assert(L4_IsGlobalId(vs_tid));
			L4_ThreadId_t new_tid = L4_GlobalId(L4_ThreadNo(vs_tid),
				(L4_Version(vs_tid) * 2) | 3);
			L4_Word_t ret = L4_ThreadControl(new_tid, sender_tid,
				L4_nilthread, L4_nilthread, (void *)-1);
			fail_if(ret != 1, "threadctl failed, ec=%#lx", L4_ErrorCode());
		} else if(!is_fork) {
			/* straight-up murder */
			kill_thread(vs_tid);
		} else {
			/* it's what we call a global killer. the end of mankind. */
			assert(L4_IsGlobalId(vs_tid));
			L4_Word_t ret = L4_ThreadControl(vs_tid, L4_nilthread,
				L4_nilthread, L4_nilthread, (void *)-1);
			fail_if(ret != 1, "threadctl failed, ec=%#lx", L4_ErrorCode());
		}

		L4_MsgTag_t tag = L4_Ipc(L4_nilthread, fromspec,
			L4_Timeouts(L4_Never, L4_ZeroTime), &from);
		L4_Word_t ec = L4_IpcFailed(tag) ? L4_ErrorCode() : 0;
		if(L4_IpcFailed(tag)) diag("ec=%#lx", ec);

		iff_ok1(is_wild, L4_IpcSucceeded(tag));
		imply_ok1(!is_wild && is_fork, ec == 5);
		/* (it can be found with the LTID, but doesn't transmit.) */
		imply_ok1(!is_wild && L4_IsLocalId(vs_tid), ec == 2);
		skip_start(L4_IpcFailed(tag), 4, "Ipc receive failed (ec=%#lx)", ec) {
			ok1(!L4_IpcPropagated(tag));
			ok1(L4_SameThreads(from, sender_tid));
			ok1(!L4_SameThreads(from, vs_tid));
			iff_ok1(!is_fork, L4_IsLocalId(from));
		} skip_end;

		tag = L4_Receive_Timeout(sender_tid, TEST_IPC_DELAY);
		IPC_FAIL(tag);
		L4_StoreMR(1, &tag.raw);
		L4_StoreMR(2, &ec);
		if(!ok(L4_IpcSucceeded(tag), "sender ipc ok")) {
			diag("tag=%#lx, ec=%#lx\n", tag.raw, ec);
		}

		if(is_rewrite) {
			/* restore the version bits first */
			L4_Word_t ret = L4_ThreadControl(vs_tid, sender_tid,
				L4_nilthread, L4_nilthread, (void *)-1);
			fail_if(ret != 1, "threadctl failed, ec=%#lx", L4_ErrorCode());
		}

		end_vs_pair(&p);
		subtest_end();
	}
}
END_TEST


/* tests presence and absence of the closed_propagation feature without a
 * redirector, i.e. whether the microkernel permits or forbids propagation
 * on the basis of closed wait alone rather than requiring the propagator to
 * be in the same address space as either the virtual sender or recipient.
 *
 * variables:
 *   - active/passive send in the propagation step
 */
START_LOOP_TEST(propagation_on_closed_wait, iter, 0, 1)
{
	const bool closed_prop = has_feature("closed_propagation"),
		active_send = !!(iter & 1);
	diag("feature: closed_propagation=%s", btos(closed_prop));
	diag("variable: active_send=%s", btos(active_send));
	plan_tests(10);
	if(closed_prop) todo_start("feature reported but not implemented");

	L4_ThreadId_t parent = L4_Myself(), orig, prop;
	int c_prop = fork_subtest_start("propagator") {
		L4_LoadMR(0, 0); L4_Send_Timeout(parent, TEST_IPC_DELAY);
		plan_tests(2);
		/* receive from anythread, attempt to propagate to parent */
		L4_ThreadId_t sender;
		L4_MsgTag_t tag = L4_Wait_Timeout(TEST_IPC_DELAY, &sender);
		L4_Word_t ec = L4_ErrorCode();
		skip_start(!ok(L4_IpcSucceeded(tag), "got message"), 1,
			"no message, ec=%lu", ec)
		{
			if(active_send) L4_Sleep(A_SHORT_NAP);
			tag = (L4_MsgTag_t){ };
			L4_Set_Propagation(&tag);
			L4_Set_VirtualSender(sender);
			L4_LoadMR(0, tag.raw);
			tag = L4_Send_Timeout(parent, TEST_IPC_DELAY);
			ec = L4_ErrorCode();
			if(!ok1(L4_IpcSucceeded(tag))) {
				diag("ec=%lu", ec);
			}
		} skip_end;
	} fork_subtest_end;
	L4_MsgTag_t tag = L4_Wait_Timeout(TEST_IPC_DELAY, &prop);
	fail_if(L4_IpcFailed(tag), "prop sync failed, ec=%lu", L4_ErrorCode());

	int c_orig = fork_subtest_start("originator") {
		L4_LoadMR(0, 0); L4_Send_Timeout(parent, TEST_IPC_DELAY);
		plan_tests(1);
		L4_Set_VirtualSender(L4_nilthread);
		L4_LoadMR(0, 0);
		L4_MsgTag_t tag = L4_Call_Timeouts(prop, L4_Never, TEST_IPC_DELAY);
		/* this one is not in the L4.X2 spec; we'll fill in ActualSender even
		 * if the returned tag doesn't indicate propagation when a
		 * propagation-turned receive phase was matched.
		 */
		L4_ThreadId_t actual = L4_ActualSender();
		iff_ok1(L4_IpcSucceeded(tag) && L4_SameThreads(actual, parent),
			closed_prop);
	} fork_subtest_end;
	tag = L4_Wait_Timeout(TEST_IPC_DELAY, &orig);
	fail_if(L4_IpcFailed(tag), "orig sync failed, ec=%lu", L4_ErrorCode());

	if(!active_send) L4_Sleep(A_SHORT_NAP);
	L4_Set_VirtualSender(L4_nilthread);
	L4_ThreadId_t sender;
	tag = L4_Wait_Timeout(TEST_IPC_DELAY, &sender);
	L4_ThreadId_t actual = L4_ActualSender();
	ok1(L4_IpcSucceeded(tag));
	iff_ok1(L4_IpcPropagated(tag), closed_prop);
	imply_ok1(L4_IpcPropagated(tag), L4_SameThreads(actual, prop));
	imply_ok1(closed_prop, L4_SameThreads(sender, orig));
	imply_ok1(!closed_prop, L4_SameThreads(sender, prop));

	/* send a reply; the propagator won't wait for a reply, but the originator
	 * will, and furthermore its closed wait will have been turned.
	 */
	L4_LoadMR(0, 0);
	tag = L4_Reply(sender);
	imply_ok1(closed_prop, L4_IpcSucceeded(tag));
	imply_ok1(!closed_prop, L4_IpcFailed(tag) && L4_ErrorCode() == 2);

	fork_subtest_ok1(c_prop);
	fork_subtest_ok1(c_orig);
}
END_TEST


/* API tests wrt the Ipc system call. */

#define MUTATE_NUM_WAYS 2

static L4_ThreadId_t mutate_tid(L4_ThreadId_t tid, int way)
{
	assert(way < MUTATE_NUM_WAYS && way >= 0);
	L4_ThreadId_t wrong_tid = tid;
	if(L4_IsLocalId(wrong_tid)) {
		switch(way) {
			default: abort();
			case 0:
				/* local TIDs have the lowest six bits, i.e. 0x3f, set to
				 * zero. so flipping the seventh bit ought to produce an
				 * invalid local ID rather than refer to a neighbouring
				 * thread.
				 */
				wrong_tid.raw ^= 0x40;
				break;
			case 1:
				/* this puts the local TID entirely outside the space's UTCB
				 * segment.
				 */
				wrong_tid.raw ^= 0x94123000;
				break;
		}
		assert(L4_IsLocalId(wrong_tid));
	} else {
		switch(way) {
			default: abort();
			case 0:
				/* perturb the version somewhat. */
				wrong_tid.global.X.version ^= 0x29;
				break;
			case 1:
				/* WHEEEEEEE! */
				wrong_tid.raw ^= 0xf4a28007;
				break;
		}
		wrong_tid.global.X.version |= 2;
		assert(L4_IsGlobalId(wrong_tid));
	}
	assert(wrong_tid.raw != tid.raw);
	return wrong_tid;
}


/* test that L4_anythread, L4_anylocalthread, and the kernel ThreadNo range
 * aren't valid in Ipc's @to.
 */
START_TEST(tid_spec_to_fail)
{
	L4_KernelInterfacePage_t *kip = L4_GetKernelInterface();
	const int last_kern = kip->ThreadInfo.X.UserBase - 1;

	const struct {
		L4_ThreadId_t tid;
		const char *name;
	} invalid_cases[] = {
		{ L4_anythread, "anythread" },
		{ L4_anylocalthread, "anylocalthread" },
		/* the interrupt range is tested in interrupt_suite.c, (TODO: or
		 * should be, anyway)
		 */
	};
	plan_tests(2 * NUM_ELEMENTS(invalid_cases) + 1);

	/* part 1: invalid @to. */
	for(int i=0; i < NUM_ELEMENTS(invalid_cases); i++) {
		L4_ThreadId_t tid = invalid_cases[i].tid;
		const char *name = invalid_cases[i].name;
		diag("tid=%#lx", tid.raw);
		L4_LoadMR(0, 0);
		L4_MsgTag_t tag = L4_Reply(tid);
		L4_Word_t ec = L4_ErrorCode(), code = ec >> 1;
		if(!ok(L4_IpcFailed(tag), "%s shouldn't be valid Ipc.to", name)) {
			diag("tag.raw=%#lx", tag.raw);
		}
		ok1(code == 2);
	}

	/* part 2: the whole kernel range, version hardwired to 1 because it'd
	 * take a while to cover all 14 bits.
	 */
	bool all_ok = true;
	for(int i = kip->ThreadInfo.X.SystemBase; i <= last_kern; i++) {
		L4_ThreadId_t tid = L4_GlobalId(i, 1);
		L4_LoadMR(0, 0);
		L4_MsgTag_t tag = L4_Reply(tid);
		L4_Word_t ec = L4_ErrorCode(), code = ec >> 1;
		if(L4_IpcSucceeded(tag) || code != 2) {
			all_ok = false;
			diag("tid=%#lx: tag=%#lx, ec=%#lx", tid.raw, tag.raw, ec);
		}
	}
	ok(all_ok, "kernel range wasn't IPCable [version=1]");
}
END_TEST


static void receive_once(void *param UNUSED) {
	L4_ThreadId_t sender;
	L4_Wait(&sender);
}


/* local and global ID validity as Ipc.to .
 * 
 * i.e. that global and local thread IDs are valid if and only if the peer
 * exists, and that wrong thread version numbers and mangled local TIDs are
 * rejected.
 */
START_LOOP_TEST(tid_spec_to_ok, iter, 0, MUTATE_NUM_WAYS * 2 - 1)
{
	const bool local = CHECK_FLAG(iter, 1);
	const char *kind = local ? "local" : "global";
	const int mut_way = iter >> 1;
	diag("local=%s, kind=%s, mut_way=%d", btos(local), kind, mut_way);
	plan_tests(7);

	L4_ThreadId_t gtid = xstart_thread(&receive_once, NULL),
		test_tid = local ? L4_LocalIdOf(gtid) : gtid,
		wrong_tid = mutate_tid(test_tid, mut_way);

	L4_LoadMR(0, 0);
	L4_MsgTag_t tag = L4_Send_Timeout(test_tid, TEST_IPC_DELAY);
	if(!ok(L4_IpcSucceeded(tag), "send to extant %s TID", kind)) {
		 diag("ec=%#lx", L4_ErrorCode());
	}

	L4_LoadMR(0, 0);
	tag = L4_Send_Timeout(wrong_tid, TEST_IPC_DELAY);
	L4_Word_t ec = L4_ErrorCode();
	ok(L4_IpcFailed(tag), "send to wrong %s TID", kind);
	if(!ok1(ec == 4)) diag("ec=%#lx", ec);

	xjoin_thread(gtid);

	L4_LoadMR(0, 0);
	tag = L4_Send_Timeout(test_tid, TEST_IPC_DELAY);
	ec = L4_ErrorCode();
	ok(L4_IpcFailed(tag), "send to joined %s TID", kind);
	ok1(ec == 4);

	L4_LoadMR(0, 0);
	tag = L4_Send_Timeout(wrong_tid, TEST_IPC_DELAY);
	ec = L4_ErrorCode();
	ok(L4_IpcFailed(tag), "send to wrong %s TID post-join", kind);
	ok1(ec == 4);
}
END_TEST


/* test that the kernel ThreadNo range isn't valid in Ipc's @from.
 * (NOTE: this is not specified in the L4.X2 API document.)
 *
 * the interrupt range is tested in interrupt_suite.c, (TODO: or should be,
 * anyway)
 */
START_TEST(tid_spec_from_fail)
{
	plan_tests(1);

	L4_KernelInterfacePage_t *kip = L4_GetKernelInterface();

	bool all_ok = true;
	for(int i = kip->ThreadInfo.X.SystemBase;
		i < kip->ThreadInfo.X.UserBase;
		i++)
	{
		/* version hardwired to 1, because mung does that for actual kernel
		 * threads also.
		 */
		L4_ThreadId_t tid = L4_GlobalId(i, 1);
		L4_LoadMR(0, 0);
		L4_MsgTag_t tag = L4_Receive_Timeout(tid, L4_ZeroTime);
		L4_Word_t ec = L4_ErrorCode();
		if(L4_IpcSucceeded(tag) || ec != 5) {
			diag("tid=%#lx, tag=%#lx, ec=%#lx", tid.raw, tag.raw, ec);
			all_ok = false;
		}
	}
	ok(all_ok, "can't give system range in FromSpec");
}
END_TEST


static void send_once_fn(void *param)
{
	L4_ThreadId_t to = { .raw = (uintptr_t)param };
	L4_LoadMR(0, 0);
	L4_Send_Timeout(to, TEST_IPC_DELAY);
}


/* test that global and local thread IDs are valid if and only if the peer
 * exists, and that wrong thread version numbers and mangled local TIDs are
 * rejected.
 */
START_LOOP_TEST(tid_spec_from_ok, iter, 0, MUTATE_NUM_WAYS * 4 - 1)
{
	plan_tests(7);
	const bool local = CHECK_FLAG(iter, 1),
		pass_local = CHECK_FLAG(iter, 2);
	const int mut_way = iter >> 2;
	assert(mut_way < MUTATE_NUM_WAYS);
	diag("local=%s, pass_local=%s, mut_way=%d", btos(local),
		btos(pass_local), mut_way);

	const char *kind = local ? "local" : "global";

	L4_ThreadId_t parent_id = pass_local ? L4_MyLocalId() : L4_MyGlobalId(),
		gtid = xstart_thread(&send_once_fn, (void *)(uintptr_t)parent_id.raw),
		test_tid = local ? L4_LocalIdOf(gtid) : gtid,
		wrong_tid = mutate_tid(test_tid, mut_way);
	diag("parent_id=%#lx, test_tid=%#lx, wrong_tid=%#lx",
		parent_id.raw, test_tid.raw, wrong_tid.raw);

	L4_MsgTag_t tag = L4_Receive_Timeout(test_tid, TEST_IPC_DELAY);
	if(!ok(L4_IpcSucceeded(tag), "recv from extant %s TID", kind)) {
		diag("ec=%#lx", L4_ErrorCode());
	}

	tag = L4_Receive_Timeout(wrong_tid, TEST_IPC_DELAY);
	L4_Word_t ec = L4_ErrorCode();
	ok(L4_IpcFailed(tag), "recv from wrong %s TID", kind);
	if(!ok1(ec == 5)) diag("ec=%#lx", ec);

	xjoin_thread(gtid);

	tag = L4_Receive_Timeout(test_tid, TEST_IPC_DELAY);
	ec = L4_ErrorCode();
	ok(L4_IpcFailed(tag), "recv from joined %s TID", kind);
	ok1(ec == 5);

	tag = L4_Receive_Timeout(wrong_tid, TEST_IPC_DELAY);
	ec = L4_ErrorCode();
	ok(L4_IpcFailed(tag), "recv from wrong %s TID post-join", kind);
	ok1(ec == 5);
}
END_TEST


struct prop_param {
	L4_ThreadId_t dest;
	bool p_bit, use_ltid;
};


static void prop_peer_fn(void *param)
{
	struct prop_param *p = param;
	L4_ThreadId_t orig = xstart_thread(&exit_thread, NULL);
	if(p->use_ltid) orig = L4_LocalIdOf(orig);
	else orig = L4_GlobalIdOf(orig);

	L4_MsgTag_t tag = { .X.u = 1, .X.label = 0x1234 };
	if(p->p_bit) {
		L4_Set_Propagation(&tag);
		L4_Set_VirtualSender(orig);
	} else {
		L4_Set_VirtualSender(L4_nilthread);
	}
	L4_LoadMR(0, tag.raw);
	L4_LoadMR(1, orig.raw);
	tag = L4_Call(p->dest);
	p->dest.raw = L4_IpcFailed(tag) ? L4_ErrorCode() : 0;

	xjoin_thread(orig);
}


START_LOOP_TEST(propagation, iter, 0, 15)
{
	plan_tests(7);
	const bool same_as = CHECK_FLAG(iter, 1), p_bit = CHECK_FLAG(iter, 2),
		use_ltid = CHECK_FLAG(iter, 4), passive = CHECK_FLAG(iter, 8);
	diag("same_as=%s, p_bit=%s, use_ltid=%s, passive=%s",
		btos(same_as), btos(p_bit), btos(use_ltid), btos(passive));

	/* start peer. */
	struct prop_param *param = malloc(sizeof(*param));
	param->p_bit = p_bit;
	param->use_ltid = use_ltid;
	param->dest = L4_MyGlobalId();
	int child = -1;
	L4_ThreadId_t peer_tid = L4_nilthread;
	if(same_as) {
		peer_tid = xstart_thread(&prop_peer_fn, param);
	} else {
		child = fork_tid(&peer_tid);
		if(child == 0) {
			prop_peer_fn(param);
			exit(0);
		}
	}
	if(!passive) {
		/* hackiest thing ever. */
		L4_Sleep(A_SHORT_NAP);
	}

	/* receive and examine the entrails. */
	L4_ThreadId_t sender;
	L4_MsgTag_t tag = L4_Wait_Timeout(TEST_IPC_DELAY, &sender);
	fail_unless(L4_IpcSucceeded(tag), "ec=%#lx", L4_ErrorCode());
	L4_ThreadId_t orig_tid; L4_StoreMR(1, &orig_tid.raw);
	L4_ThreadId_t as_tid = L4_ActualSender();
	diag("sender=%lu:%lu, orig_tid=%lu:%lu, as_tid=%lu:%lu",
		L4_ThreadNo(sender), L4_Version(sender),
		L4_ThreadNo(orig_tid), L4_Version(orig_tid),
		L4_ThreadNo(as_tid), L4_Version(as_tid));
	fail_unless(use_ltid == L4_IsLocalId(orig_tid));

	iff_ok1(p_bit, L4_IpcPropagated(tag));
	imply_ok1(L4_SameThreads(sender, orig_tid), p_bit);
	imply_ok1(p_bit, L4_SameThreads(as_tid, peer_tid));
	imply_ok1(p_bit && same_as, L4_IsLocalId(as_tid));
	imply_ok1(p_bit && !same_as, L4_IsGlobalId(as_tid));

	iff_ok1(!p_bit, L4_SameThreads(sender, peer_tid));
	iff_ok1(same_as, L4_IsLocalId(sender));

	/* cleanup. */
	L4_LoadMR(0, 0);
	tag = L4_Reply(peer_tid);
	if(L4_IpcFailed(tag)) {
		diag("peer reply failed, ec=%#lx", L4_ErrorCode());
	}

	if(same_as) {
		xjoin_thread(peer_tid);
	} else {
		assert(child >= 0);
		int st, dead = wait(&st);
		fail_unless(dead == child, "dead=%d, child=%d", dead, child);
	}
	free(param);
}
END_TEST


/* test the part about propagation where it turns a closed waiter's FromSpec.
 * to wit, test_serv_fn should only be able to reply to call_serv_fn when the
 * call is propagated.
 *
 * - variables:
 *   - use propagation or not
 *   - active or passive send
 */
struct turn_fromspec_param {
	L4_ThreadId_t sender, as, parent;
	L4_MsgTag_t reply_tag;
	L4_Word_t ec;
	bool active_send;
};


static void test_serv_fn(void *);
static void call_serv_fn(void *);


START_LOOP_TEST(propagation_turn_fromspec, iter, 0, 3)
{
	plan_tests(7);
	const bool p_bit = !!(iter & 1), active_send = !!(iter & 2);
	diag("p_bit=%s, active_send=%s", btos(p_bit), btos(active_send));

	struct turn_fromspec_param *t = malloc(sizeof *t);
	*t = (struct turn_fromspec_param){
		.active_send = active_send, .parent = L4_MyGlobalId(),
	};
	L4_ThreadId_t serv_tid = xstart_thread(&test_serv_fn, t),
		call_tid = xstart_thread(&call_serv_fn, t);

	L4_ThreadId_t sender;
	if(!active_send) L4_Sleep(A_SHORT_NAP);
	L4_Accept(L4_UntypedWordsAcceptor);
	L4_MsgTag_t tag = L4_Wait_Timeout(TEST_IPC_DELAY, &sender);
	L4_Word_t ec = L4_ErrorCode();
	skip_start(!ok(L4_IpcSucceeded(tag), "got call"), 1,
		"didn't get call, ec=%lu", ec)
	{
		tag.X.flags = 0;
		if(p_bit) L4_Set_Propagation(&tag);
		L4_Set_VirtualSender(sender);
		L4_LoadMR(0, tag.raw);
		tag = L4_Send_Timeout(serv_tid, TEST_IPC_DELAY);
		ec = L4_ErrorCode();
		if(!ok(L4_IpcSucceeded(tag), "send to test_serv_fn")) {
			diag("ec=%lu", ec);
		}
	} skip_end;

	const char *serv_status = xjoin_thread(serv_tid);
	ok1(streq(serv_status, "ok!"));
	if(L4_IpcFailed(t->reply_tag)) {
		L4_LoadMR(0, 0);
		L4_Reply(sender);
	}
	const char *call_status = xjoin_thread(call_tid);
	ok1(streq(call_status, "ok!"));

	if(!iff_ok1(p_bit, L4_IpcSucceeded(t->reply_tag))) {
		diag("t->ec=%#lx", t->ec);
	}
	if(!imply_ok1(!p_bit, t->ec == 2)) diag("t->ec=%#lx", t->ec);
	if(!imply_ok1(p_bit, L4_SameThreads(t->as, L4_Myself()))) {
		diag("t->as=%lu:%lu", L4_ThreadNo(t->as), L4_Version(t->as));
	}

	free(t);
}
END_TEST


/* accepts one IPC call, records where it came from (incl. propagation), then
 * replies to the apparent sender address and records what happened.
 */
static void test_serv_fn(void *param_ptr)
{
	struct turn_fromspec_param *p = param_ptr;

	L4_Accept(L4_UntypedWordsAcceptor);
	L4_MsgTag_t tag = L4_Wait_Timeout(TEST_IPC_DELAY, &p->sender);
	if(L4_IpcFailed(tag)) {
		diag("%s: wait failed, ec=%lu", __func__, L4_ErrorCode());
		exit_thread("fail");
	}
	p->as = L4_IpcPropagated(tag) ? L4_ActualSender() : L4_nilthread;

	tag.X.flags = 0;	/* don't re-propagate. */
	L4_LoadMR(0, tag.raw);
	p->reply_tag = L4_Reply(p->sender);
	p->ec = L4_IpcFailed(p->reply_tag) ? L4_ErrorCode() : 0;

	exit_thread("ok!");
}


/* does one IPC call to a service, then exits. */
static void call_serv_fn(void *param)
{
	const struct turn_fromspec_param *p = param;
	if(p->active_send) L4_Sleep(A_SHORT_NAP);
	L4_LoadMR(0, 0);
	L4_MsgTag_t tag = L4_Call_Timeouts(p->parent, L4_Never, TEST_IPC_DELAY);
	exit_thread(L4_IpcSucceeded(tag) ? "ok!" : "fail");
}


/* test that propagation does not turn the virtual sender's FromSpec when
 * propagation criteria are present but the IPC send phase fails. the main
 * thread calls to a forwarder thread, which tries to propagate to a sleeper
 * thread; when that propagation fails, it returns the error code to the main
 * thread which examines the entrails.
 */
static void noturn_propagator(void *param);
static void noturn_sleeper(void *param);


START_TEST(propagation_no_turn_fromspec)
{
	plan_tests(5);

	L4_ThreadId_t sleeper = xstart_thread(&noturn_sleeper, NULL),
		propagator = xstart_thread(&noturn_propagator, (void *)sleeper.raw);
	L4_Accept(L4_UntypedWordsAcceptor);
	L4_Set_VirtualSender(L4_nilthread);
	L4_LoadMR(0, 0);
	L4_MsgTag_t tag = L4_Call_Timeouts(propagator, L4_Never, TEST_IPC_DELAY);
	L4_Word_t ec = L4_ErrorCode();
	L4_ThreadId_t actual = L4_ActualSender();
	L4_Word_t prop_ec; L4_StoreMR(1, &prop_ec);
	if(L4_IpcFailed(tag)) diag("call ec=%lu", ec);
	skip_start(!ok1(L4_IpcSucceeded(tag)), 3, "IPC failed") {
		ok1(!L4_IpcPropagated(tag));
		ok1(L4_IsNilThread(actual));
		ok1(prop_ec == 2);	/* send-phase timeout ErrorCode */
	} skip_end;

	char *prop_ret = xjoin_thread(propagator);
	if(!ok(prop_ret == NULL, "propagator")) {
		diag("status: %s", prop_ret);
	}
	xjoin_thread(sleeper);
}
END_TEST


static void noturn_propagator(void *param)
{
	char *failmsg, *what_failed;
	L4_Word_t ec;

	L4_ThreadId_t sleeper = { .raw = (L4_Word_t)param }, sender;
	L4_MsgTag_t tag = L4_WaitLocal_Timeout(TEST_IPC_DELAY, &sender);
	if(L4_IpcFailed(tag)) {
		ec = L4_ErrorCode();
		what_failed = "wait";
		goto fail;
	}

	sender = L4_GlobalIdOf(sender);
	diag("propagating for %lu:%lu", L4_ThreadNo(sender), L4_Version(sender));
	L4_Set_VirtualSender(sender);
	tag = (L4_MsgTag_t){ .X.u = 1 };
	L4_Set_Propagation(&tag);
	L4_LoadMR(0, tag.raw);
	L4_LoadMR(1, 12345);
	tag = L4_Send_Timeout(sleeper, A_SHORT_NAP);
	ec = L4_ErrorCode();
	if(L4_IpcSucceeded(tag)) {
		what_failed = "send (succeeded)";
		goto fail;
	}

	/* return ec to main thread */
	L4_LoadMR(0, (L4_MsgTag_t){ .X.u = 1 }.raw);
	L4_LoadMR(1, ec);
	tag = L4_Reply(sender);
	if(L4_IpcSucceeded(tag)) exit_thread(NULL);
	else {
		ec = L4_ErrorCode();
		what_failed = "reply";
		goto fail;
	}

	assert(false);

fail:
	failmsg = malloc(100);
	snprintf(failmsg, 100, "%s returned ec=%lu", what_failed, ec);
	exit_thread(failmsg);
}


static void noturn_sleeper(void *param) {
	L4_Sleep(TEST_IPC_DELAY);
	L4_Sleep(A_SHORT_NAP);
}


/* test that propagated passive sends can be received with all applicable
 * forms of FromSpec.
 */
START_LOOP_TEST(propagation_in_active_receive, iter, 0, 31)
{
	const int fsix = iter & 3;	/* fromspec index */
	iter >>= 2;
	const bool is_fork = CHECK_FLAG(iter, 1),
		is_local = CHECK_FLAG(iter, 2),
		is_vs_local = CHECK_FLAG(iter, 4);
	diag("FromSpec=%s, is_fork=%s, is_local=%s, is_vs_local=%s",
		fromspec_name[fsix], btos(is_fork), btos(is_local), btos(is_vs_local));
	if(fsix < 2 && is_fork) {
		plan_skip_all("skipping local FromSpec under is_fork");
		diag("mode `%s' not applicable when is_fork=%s",
			fromspec_name[fsix], btos(is_fork));
		return;
	}

	plan_tests(7);

	struct vs_pair p;
	start_vs_pair(&p, is_local, is_vs_local, is_fork);
	L4_ThreadId_t vs_tid = p.vs_tid, sender_tid = p.sender_tid;
	L4_Sleep(A_SHORT_NAP);

	L4_ThreadId_t from = fromspec_class(fsix, vs_tid);
	L4_MsgTag_t tag = L4_Ipc(L4_nilthread, from,
		L4_Timeouts(L4_Never, L4_ZeroTime), &from);
	L4_Word_t ec = L4_ErrorCode();
	if(!ok1(L4_IpcSucceeded(tag))) {
		diag("ec=%#lx", ec);
	}
	L4_ThreadId_t actual = L4_ActualSender();
	ok1(L4_IpcPropagated(tag));
	ok1(L4_SameThreads(actual, sender_tid));
	ok1(L4_SameThreads(from, vs_tid));
	iff_ok1(!is_fork, L4_IsLocalId(from));
	iff_ok1(!is_fork, L4_IsLocalId(actual));

	tag = L4_Receive_Timeout(sender_tid, TEST_IPC_DELAY);
	IPC_FAIL(tag);
	L4_StoreMR(1, &tag.raw);
	L4_StoreMR(2, &ec);
	if(!ok(L4_IpcSucceeded(tag) && ec == 0, "sender ok")) {
		diag("tag=%#lx, ec=%#lx", tag.raw, ec);
	}

	end_vs_pair(&p);
}
END_TEST


static void redir_tester(
	L4_ThreadId_t serv_tid,
	L4_ThreadId_t parent_tid,
	L4_Word_t magic)
{
	int32_t rv;
	L4_MsgTag_t tag;
	L4_ThreadId_t s, as;
	int n = __ipchelper_ipc_ping_timeout(serv_tid, &rv, magic, &tag.raw,
		&s.raw, &as.raw, TEST_IPC_DELAY);
	diag("%s: n=%d, rv=%#x", __func__, n, (unsigned)rv);

	L4_LoadMR(0, (L4_MsgTag_t){ .X.u = 5 }.raw);
	L4_LoadMR(1, rv);
	L4_LoadMR(2, tag.raw);
	L4_LoadMR(3, s.raw);
	L4_LoadMR(4, as.raw);
	L4_LoadMR(5, n);
	tag = L4_Send_Timeout(parent_tid, TEST_IPC_DELAY);
	if(L4_IpcFailed(tag)) {
		diag("%s: reply failed, ec=%#lx", __func__, L4_ErrorCode());
	}
}


static void redir_asst_fn(void *param)
{
	L4_Word_t val_to_set = (L4_Word_t)param;

	L4_ThreadId_t sender;
	/* (anylocalthread won't do, because under propagation the sender is no
	 * longer in the same space.)
	 */
	L4_MsgTag_t tag = L4_Wait(&sender);
	if(L4_IpcFailed(tag)) {
		diag("%s: ipc failed, ec=%#lx", __func__, L4_ErrorCode());
		return;
	} else if(!L4_IpcPropagated(tag)) {
		diag("%s: no propagation?", __func__);
		return;
	}
	L4_ThreadId_t as = L4_ActualSender();

	L4_LoadMR(0, (L4_MsgTag_t){ .X.u = 4 }.raw);
	L4_LoadMR(1, val_to_set);
	L4_LoadMR(2, 0);
	L4_LoadMR(3, sender.raw);
	L4_LoadMR(4, as.raw);
	tag = L4_Reply(sender);
	if(L4_IpcFailed(tag)) {
		diag("%s: reply failed, ec=%#lx", __func__, L4_ErrorCode());
	}
}


#define HOLD_LABEL 0x4096		/* eat mah BCD! */
#define FAKE_LABEL 0x4f7a
#define OTHER_LABEL 0xa420		/* rehhhh */
#define REPLY_LABEL 0x2008		/* iä iä */

#define OTHER_VALUE 0xbadface0
#define REPLY_VALUE 0xcafeb00b

static void redir_do(
	L4_MsgTag_t tag,
	L4_ThreadId_t sender,
	L4_Word_t xor_value,
	L4_Word_t fuck_label)
{
	L4_ThreadId_t ir = L4_IntendedReceiver();
	diag("redir label=%#x, ir=%lu:%lu, sender=%lu:%lu",
		tag.X.label, L4_ThreadNo(ir), L4_Version(ir),
		L4_ThreadNo(sender), L4_Version(sender));

	assert(tag.X.t == 0);
	tag.X.flags = 0;
	L4_Set_Propagation(&tag);
	L4_Set_VirtualSender(sender);

	if(tag.X.label == 0x1236 && fuck_label == HOLD_LABEL) {
		/* hold retransmission back for TEST_IPC_DELAY + 1 ms, to time it
		 * out
		 */
		L4_Word_t v; L4_StoreMR(1, &v);
		diag("HOLDING TEH GOBLINS");
		L4_Sleep(L4_TimePeriod(L4_PeriodUs_NP(TEST_IPC_DELAY) + 2000));

		L4_LoadMR(0, tag.raw);
		L4_LoadMR(1, v);
		tag = L4_Send_Timeout(ir, TEST_IPC_DELAY);
	} else if(tag.X.label == 0x1236 && fuck_label == FAKE_LABEL) {
		/* faking shit is hard!
		 *   -- IPC barbie
		 */
		diag("FAKING IT LIEK AN CHAMP");
		L4_Word_t v; L4_StoreMR(1, &v);
		v ^= xor_value;		/* munge the ping question. */

		L4_LoadMR(0, tag.raw);
		L4_LoadMR(1, v);
		tag = L4_Send_Timeout(ir, TEST_IPC_DELAY);
	} else if(tag.X.label == 0x1236 && fuck_label == REPLY_LABEL) {
		/* falsify results to IpcHelper::ipc_ping.
		 *
		 * note that this doesn't use propagation: that already happened as
		 * part of IPC redirection.
		 */
		L4_Word_t v; L4_StoreMR(1, &v);
		diag("IMMEDIATE SHOWDOWN");
		tag = (L4_MsgTag_t){ .X.u = 4 };
		L4_LoadMR(0, tag.raw);
		L4_LoadMR(1, REPLY_VALUE);
		L4_LoadMR(2, 0);
		L4_LoadMR(3, sender.raw);
		L4_LoadMR(4, L4_MyGlobalId().raw);
		tag = L4_Reply(sender);
	} else if(tag.X.label == 0x1236 && fuck_label == OTHER_LABEL) {
		/* delegate the IPC to a different thread from the one given in
		 * IntendedReceiver.
		 */
		L4_Word_t v; L4_StoreMR(1, &v);
		L4_ThreadId_t asst = xstart_thread(&redir_asst_fn,
			(void *)OTHER_VALUE);
		diag("ASSISTANT %lu:%lu EES ZE VERY BOTHERED YOU SEE",
			L4_ThreadNo(asst), L4_Version(asst));

		L4_LoadMR(0, tag.raw);
		L4_LoadMR(1, v);
		tag = L4_Send(asst);

		xjoin_thread(asst);
	} else {
		/* a plain redirection. */
		L4_LoadMR(0, tag.raw);
		tag = L4_Send_Timeout(ir, TEST_IPC_DELAY);
	}

	if(L4_IpcFailed(tag)) {
		diag("redir failed, ec=%#lx", L4_ErrorCode());
	}
}


static void redir_alter_echo(L4_Word_t xor_value)
{
	bool running = true;
	L4_Word_t fuck_label = 0;
	while(running) {
		L4_ThreadId_t sender;
		L4_MsgTag_t tag = L4_Wait(&sender);
		
		for(;;) {
			if(L4_IpcFailed(tag)) {
				diag("%s: failed IPC; ec=%#lx", __func__, L4_ErrorCode());
				break;
			} else if(L4_IpcRedirected(tag)) {
				redir_do(tag, sender, xor_value, fuck_label);
				if(tag.X.label == 0x1236) fuck_label = 0;
				break;
			}

			switch(tag.X.label) {
				case QUIT_LABEL: running = false; break;
				case HOLD_LABEL:
				case FAKE_LABEL:
				case OTHER_LABEL:
				case REPLY_LABEL:
					diag("setting fuck_label=%#lx", tag.X.label);
					fuck_label = tag.X.label;
					L4_LoadMR(0, 0);
					break;

				default:
					diag("%s: unknown label %#lx", __func__, tag.X.label);
					tag = L4_Wait(&sender);
					continue;
			}
			if(!running) break;

			tag = L4_ReplyWait(sender, &sender);
		}
	}
}


/* tests the following properties of redirectors:
 *
 * - they receive IPCs addressed to spaces other than that of the sender's,
 *   and the redirector's.
 * - that a redirector may force a timeout on IPC it doesn't like (the hold
 *   fuck),
 * - ... or reply to it directly by using propagation (reply),
 * - ... or pass it on to a different receiver (other),
 * - ... or pass it unaltered to where it was intended (none).
 *
 * variables:
 * - whether the redirector was set, or not.
 * - whether a fuckery mode (none, hold, fake, reply, or other) is to be
 *   applied
 *
 * TODO: !r_bit disables the effect of whichever fuckmode is being applied. so
 * change that to the 0 iter, and add PASS_LABEL separately to remove 4
 * redundant iterations.
 */
START_LOOP_TEST(basic_redir, iter, 0, 9)
{
	plan_tests(8);
	assert(getpid() == 1);
	const int32_t magic = 0xb0a7cafe, xor_val = 0x12345678;
	const bool r_bit = CHECK_FLAG(iter, 1);
	L4_Word_t fuck_mode;
	switch(iter >> 1) {
		case 0: fuck_mode = 0; break;
		case 1: fuck_mode = HOLD_LABEL; break;
		case 2: fuck_mode = FAKE_LABEL; break;
		case 3: fuck_mode = OTHER_LABEL; break;
		case 4: fuck_mode = REPLY_LABEL; break;
		default:
			fail_if(true, "iter=%d, which is bad", iter);
			return;
	}
	diag("r_bit=%s, fuck_mode=%#lx", btos(r_bit), fuck_mode);

	L4_ThreadId_t c_tid, parent_tid = L4_Myself();
	int child = fork_tid(&c_tid);
	if(child == 0) {
		/* sync before proceeding, so that the redirector and other modes will
		 * have been set
		 */
		L4_MsgTag_t tag = L4_Receive_Timeout(parent_tid, TEST_IPC_DELAY);
		fail_if(L4_IpcFailed(tag), "ec=%#lx", L4_ErrorCode());
		redir_tester(helper_tid, parent_tid, magic);
		exit(0);
	}
	L4_ThreadId_t r_tid;
	int redir = fork_tid(&r_tid);
	if(redir == 0) {
		redir_alter_echo(xor_val);
		exit(0);
	}
	diag("child tid=%lu:%lu, redir tid=%lu:%lu",
		L4_ThreadNo(c_tid), L4_Version(c_tid),
		L4_ThreadNo(r_tid), L4_Version(r_tid));

	/* set redirector (or not) */
	L4_Word_t dummy, res = L4_SpaceControl(c_tid, 0,
		L4_Nilpage, L4_Nilpage, r_bit ? r_tid : L4_anythread, &dummy);
	fail_unless(res == 1, "SpaceControl failed, ec=%#lx",
		L4_ErrorCode());

	if(fuck_mode != 0) {
		L4_LoadMR(0, (L4_MsgTag_t){ .X.label = fuck_mode }.raw);
		L4_MsgTag_t tag = L4_Call(r_tid);
		fail_if(L4_IpcFailed(tag), "ec=%#lx", L4_ErrorCode());
	}

	const bool fuck = fuck_mode != 0,
		fake = fuck_mode == FAKE_LABEL,
		hold = fuck_mode == HOLD_LABEL,
		reply = fuck_mode == REPLY_LABEL,
		other = fuck_mode == OTHER_LABEL;

	L4_LoadMR(0, 0);
	L4_MsgTag_t tag = L4_Call(c_tid);
	fail_if(L4_IpcFailed(tag), "ec=%#lx", L4_ErrorCode());

	L4_Word_t rv = 0, ec = ~0ul;
	L4_ThreadId_t s, as;
	L4_StoreMR(1, &rv);			/* ping reply value */
	L4_StoreMR(2, &tag.raw);	/* ipc_ping tag */
	L4_StoreMR(3, &s.raw);		/* ipc_ping sender */
	L4_StoreMR(4, &as.raw);		/* ipc_ping actualsender */
	L4_StoreMR(5, &ec);			/* ec from ipc_ping call */
	diag("rv=%#lx, tag=%#lx, s=%lu:%lu, as=%lu:%lu, ec=%#lx",
		rv, tag.raw, L4_ThreadNo(s), L4_Version(s),
		L4_ThreadNo(as), L4_Version(as), ec);

	imply_ok1(ec == 0, L4_SameThreads(s, c_tid));

	/* base case: no redirection, or simple pass to intended receiver. */
	imply_ok1(!r_bit || !fuck, rv == magic);
	imply_ok(!hold, ec == 0, "ok when not held");

	/* part 1: redirected but OK, and not sent by way of another thread: the
	 * helper should observe the redirector as propagator.
	 */
	imply_ok1(r_bit && !other && ec == 0, L4_SameThreads(as, r_tid));

	/* part 2 */
	imply_ok(r_bit && hold, ec == 3, "times out when held");

	/* the other fuck modes */
	iff_ok1(r_bit && fake, rv == (magic ^ xor_val));
	iff_ok1(r_bit && reply, rv == REPLY_VALUE);
	iff_ok1(r_bit && other, rv == OTHER_VALUE);

	/* cleanup */
	int st, dead = wait(&st);
	fail_if(dead != child, "failed to wait for %d (dead=%d)", child, dead);

	send_quit(r_tid);
	dead = wait(&st);
	fail_if(dead != redir, "failed to wait for %d (dead=%d)", redir, dead);
}
END_TEST


/* this runs in a separate process, so that map and grant items may be
 * tested.
 */
static void send_typed_items(void)
{
	const size_t mem_size = 4096;
	const char *test_string = "if you dont repost this comment on 10 other "
		"pages i will fly into your kitchen tonight and make a mess of "
		"your pots and pans";

	L4_ThreadId_t sender;
	L4_MsgTag_t tag = L4_Wait(&sender);
	IPC_FAIL(tag);

	void *memory = aligned_alloc(PAGE_SIZE, mem_size);
	memset(memory, 1, mem_size);
	diag("sender's memory is at %p", memory);

	L4_StringItem_t si = L4_StringItem(strlen(test_string) + 1,
		(char *)test_string);
	L4_Fpage_t map_page = L4_FpageLog2((L4_Word_t)memory, 12);
	L4_Set_Rights(&map_page, L4_Readable);
	L4_MapItem_t mi = L4_MapItem(map_page, 0);
	L4_LoadMR(0, (L4_MsgTag_t){ .X.t = 4 }.raw);
	L4_LoadMRs(1, 2, si.raw);
	L4_LoadMRs(3, 2, mi.raw);
	tag = L4_Reply(sender);
	IPC_FAIL(tag);

	diag("replied to sender.");

	/* get an additional sync before exiting the address space. */
	tag = L4_Receive(sender);
	IPC_FAIL(tag);
	L4_LoadMR(0, 0);
	tag = L4_Reply(sender);
	IPC_FAIL(tag);

	free(memory);
}


/* TODO: this test should be much more comprehensive:
 *   - the ordering of string and map items should vary
 *   - the number of string and map items should vary (0, 1, 2)
 *   - whether string and map items appear consecutively
 *
 * per the spec, the final typed item should have C == 0, and the ones before
 * should have C == 1.
 */
START_TEST(c_bit_in_typed_words)
{
	plan_tests(6);

	L4_ThreadId_t oth;
	int oth_pid = fork_tid(&oth);
	if(oth_pid == 0) {
		send_typed_items();
		exit(0);
	}

	const size_t wnd_size = 4096;
	uint8_t *memory = aligned_alloc(PAGE_SIZE, wnd_size),
		*buffer = malloc(16 * 1024);
	memset(memory, 0, wnd_size);
	memset(buffer, 0, 16 * 1024);
	L4_StringItem_t buf_si = L4_StringItem(16 * 1024, buffer);
	buf_si.X.C = 0;
	L4_Accept(L4_AddAcceptor(L4_StringItemsAcceptor,
		L4_MapGrantItems(L4_FpageLog2((L4_Word_t)memory, 12))));
	L4_LoadBRs(1, 2, buf_si.raw);

	L4_LoadMR(0, 0);
	L4_MsgTag_t tag = L4_Call(oth);
	IPC_FAIL(tag);
	L4_Word_t words[64];
	int u = L4_UntypedWords(tag), t = L4_TypedWords(tag);
	L4_StoreMRs(1, u + t, words);

	/* examine the contents of the typed transfer. */
	if(!ok(memory[0] != 0, "received map")) {
		diag("memory[0] = %#x", memory[0]);
	}
	buffer[1024] = '\0';
	ok(strstr((char *)buffer, "repost this") != NULL,
		"received string");

	/* and its form. */
	L4_StringItem_t si;
	memcpy(si.raw, &words[0], sizeof(L4_Word_t) * 2);
	ok1(L4_IsStringItem(&si));
	if(!ok1(si.X.C == 1)) {
		diag("string words: 0=%#lx, 1=%#lx", si.raw[0], si.raw[1]);
	}
	L4_MapItem_t mi;
	memcpy(mi.raw, &words[2], sizeof(L4_Word_t) * 2);
	ok1(L4_IsMapItem(mi));
	if(!ok1(mi.X.C == 0)) {
		diag("map words: 0=%#lx, 1=%#lx", mi.raw[0], mi.raw[1]);
	}

	/* kick the helper. */
	L4_Accept(L4_UntypedWordsAcceptor);
	L4_LoadMR(0, 0);
	tag = L4_Call(oth);
	IPC_FAIL(tag);
	int st, dead = wait(&st);
	fail_if(dead != oth_pid, "dead=%d, st=%d", dead, st);

	free(memory);
	free(buffer);
}
END_TEST


/* tcase "lipc" */
static const L4_Word_t lipc_inputs[] = {
	0xdeadbeef, 0xcafebabe, 0xab00b1e5, 0xd00dc0de,
	0x98765432, 0x12345678, 0x7055face, 0xb00760d5,
};


struct lipc_receiver_param {
	bool reply_with_lipc;
};


static void lipc_receiver_fn(void *param_ptr)
{
	struct lipc_receiver_param *p = param_ptr;
	void *arena = aligned_alloc(PAGE_SIZE, 1 << 12);
	L4_Fpage_t a_page = L4_FpageLog2((L4_Word_t)arena, 12);
	L4_Set_Rights(&a_page, L4_FullyAccessible);
	L4_Accept(L4_MapGrantItems(a_page));

	L4_ThreadId_t sender;
	L4_MsgTag_t tag = L4_Wait_Timeout(TEST_IPC_DELAY, &sender);
	IPC_FAIL(tag);
	const int n_words = L4_UntypedWords(tag) + L4_TypedWords(tag);

	L4_Word_t words[64];
	L4_StoreMRs(1, n_words, words);
	diag("%s: got u=%lu, t=%lu, label=%#lx; from=%#lx", __func__,
		L4_UntypedWords(tag), L4_TypedWords(tag), L4_Label(tag), sender.raw);
	/* byteswap the untyped words to show that we were here. */
	for(int i=0; i < n_words; i++) words[i] = bswap_32(words[i]);
	L4_LoadMR(0, (L4_MsgTag_t){ .X.label = tag.X.label,
		.X.u = n_words }.raw);
	L4_LoadMRs(1, n_words, words);
	if(p->reply_with_lipc && L4_IsLocalId(sender)) {
		/* our little synthetic L4_Lreply() */
		L4_ThreadId_t dummy;
		tag = L4_Lipc(sender, L4_nilthread,
			L4_Timeouts(L4_ZeroTime, L4_Never), &dummy);
	} else {
		tag = L4_Reply(sender);
	}
	IPC_FAIL(tag);

	L4_Set_Rights(&a_page, L4_FullyAccessible);
	L4_FlushFpage(a_page);
	free(arena);
}


static bool verify_lipc_output(L4_MsgTag_t tag, const L4_Word_t *mr)
{
	const int n_inputs = MIN(int, L4_UntypedWords(tag),
		NUM_ELEMENTS(lipc_inputs));
	for(int i=0; i < n_inputs; i++) {
		L4_Word_t inp = lipc_inputs[i];
		if(mr[i] != bswap_32(inp)) {
			diag("MR%d=%#lx, expected %#lx", i + 1, mr[i], bswap_32(inp));
			return false;
		}
	}
	return true;
}


/* test a basic successful Lipc call flow with a small number of untyped
 * words, no typed items, and no timeouts.
 *
 * TODO: this should get moved into a basic_untyped_call test in the "api"
 * tcase, instead.
 *
 * variables:
 *   - number of input words
 *   - whether receiver uses Lipc
 *   - whether sender uses Lipc
 *   - [TODO] whether receiver thread is in local space or forked child
 *
 * TODO: add "has no receive phase" test based on this, once
 * lipc_receiver_fn() reports back instead of IPC_FAIL()ing on its reply
 * result.
 */
START_LOOP_TEST(basic_lipc, iter, 0, 7)
{
	plan_tests(2);
	const bool low_inputs = CHECK_FLAG(iter, 1),
		reply_with_lipc = CHECK_FLAG(iter, 2),
		send_with_lipc = CHECK_FLAG(iter, 4);
	const int n_inputs = low_inputs ? 2 : NUM_ELEMENTS(lipc_inputs);
	diag("low_inputs=%s, reply_with_lipc=%s, send_with_lipc=%s, n_inputs=%d",
		btos(low_inputs), btos(reply_with_lipc), btos(send_with_lipc),
		n_inputs);

	struct lipc_receiver_param *rparam = malloc(sizeof(*rparam));
	*rparam = (struct lipc_receiver_param){
		.reply_with_lipc = reply_with_lipc,
	};
	L4_ThreadId_t oth = xstart_thread(&lipc_receiver_fn, rparam);
	L4_Sleep(L4_TimePeriod(5 * 1000));
	diag("calling from %lu:%lu (lid=%#lx)",
		L4_ThreadNo(L4_Myself()), L4_Version(L4_Myself()),
		L4_MyLocalId().raw);

	L4_Word_t mr[64];
	memset(mr, 0, sizeof(mr));
	L4_ThreadId_t dst_tid = send_with_lipc ? L4_LocalIdOf(oth) : oth;
	L4_LoadMR(0, (L4_MsgTag_t){ .X.label = 0xbeef, .X.u = n_inputs }.raw);
	L4_LoadMRs(1, n_inputs, lipc_inputs);
	L4_MsgTag_t tag;
	if(send_with_lipc) tag = L4_Lcall(dst_tid); else tag = L4_Call(oth);
	IPC_FAIL(tag);
	L4_StoreMRs(1, L4_UntypedWords(tag) + L4_TypedWords(tag), mr);
	if(!ok(tag.X.u == n_inputs && tag.X.t == 0, "tag OK")) {
		diag("tag=%#lx", tag.raw);
	}
	ok(verify_lipc_output(tag, mr), "got correct output");

	xjoin_thread(oth);
	free(rparam);
}
END_TEST


/* tries out the various Lipc fallback cases:
 *   - non-local destination TID
 *   - non-Never receive timeout
 *   - typed words (MapItem)
 *
 * TODO:
 *   - different hw thread [v2, needs SMP support]
 *   - dest doesn't exist (needs post-fault recovery)
 *   - has no receive phase (incompatible with not_never)
 */
START_LOOP_TEST(basic_invalid_lipc, iter, 0, 7)
{
	plan_tests(2);
	const int n_inputs = NUM_ELEMENTS(lipc_inputs);
	const bool not_ltid = CHECK_FLAG(iter, 1),
		not_never = CHECK_FLAG(iter, 2),
		has_typed = CHECK_FLAG(iter, 4);
	diag("n_inputs=%d, not_ltid=%s, not_never=%s, has_typed=%s",
		n_inputs, btos(not_ltid), btos(not_never), btos(has_typed));

	void *memory = aligned_alloc(PAGE_SIZE, 1 << 12);
	struct lipc_receiver_param *rparam = malloc(sizeof(*rparam));
	*rparam = (struct lipc_receiver_param){
		.reply_with_lipc = false,
	};
	L4_ThreadId_t oth = xstart_thread(&lipc_receiver_fn, rparam),
		dest_tid = not_ltid ? L4_GlobalIdOf(oth) : L4_LocalIdOf(oth);
	L4_Sleep(L4_TimePeriod(5 * 1000));
	diag("doing Lipc from %lu:%lu", L4_ThreadNo(L4_Myself()),
		L4_Version(L4_Myself()));

	L4_MapItem_t mi;
	L4_Word_t mr[64];
	memset(mr, 0, sizeof(mr));
	if(has_typed) {
		memset(memory, 1, 1 << 12);
		L4_Fpage_t p = L4_FpageLog2((L4_Word_t)memory, 12);
		L4_Set_Rights(&p, L4_ReadWriteOnly);
		mi = L4_MapItem(p, 0);
	}
	L4_LoadMR(0, (L4_MsgTag_t){ .X.label = 0xbeef,
		.X.u = n_inputs, .X.t = has_typed ? 2 : 0 }.raw);
	L4_LoadMRs(1, n_inputs, lipc_inputs);
	if(has_typed) L4_LoadMRs(n_inputs + 1, 2, mi.raw);
	L4_MsgTag_t tag;
	if(not_never) {
		L4_ThreadId_t dummy;
		tag = L4_Lipc(dest_tid, dest_tid,
			L4_Timeouts(L4_Never, TEST_IPC_DELAY), &dummy);
	} else {
		tag = L4_Lcall(dest_tid);
	}
	IPC_FAIL(tag);
	L4_StoreMRs(1, L4_UntypedWords(tag) + L4_TypedWords(tag), mr);
	if(!ok(tag.X.u == n_inputs + (has_typed ? 2 : 0) && tag.X.t == 0, "tag OK")) {
		diag("tag=%#lx", tag.raw);
	}
	ok(verify_lipc_output(tag, mr), "got correct output");

	xjoin_thread(oth);
	free(memory);
	free(rparam);
}
END_TEST


/* verify that when map/grant items are transferred, the SndPage field's
 * address part is rewritten to match the position in RcvWindow.
 *
 * variables:
 *   - whether the item is a map or a grant;
 *   - position within RcvWindow (zero, nonzero);
 *   - size of the sent page (4k, >4k) [TODO]
 */
START_LOOP_TEST(mapgrant_address, iter, 0, 3)
{
	plan_tests(5);
	const bool is_grant = CHECK_FLAG(iter, 1);
	const size_t accept_size = PAGE_SIZE * 16,
		snd_offset = CHECK_FLAG(iter, 2) ? PAGE_SIZE * 3 : 0;
	diag("is_grant=%s, snd_offset=%u",
		btos(is_grant), (unsigned)snd_offset);
	assert(accept_size >= snd_offset + PAGE_SIZE);

	union {
		L4_MapItem_t mi;
		L4_GrantItem_t gi;
		L4_Word_t raw[2];
	} t;

	uint8_t *map_mem = aligned_alloc(PAGE_SIZE, PAGE_SIZE);
	memset(map_mem, 1, PAGE_SIZE);
	L4_ThreadId_t child_tid, parent_tid = L4_Myself();
	int child = fork_tid(&child_tid);
	if(child == 0) {
		memset(map_mem, 2, PAGE_SIZE);
		L4_Fpage_t map_page = L4_Fpage((L4_Word_t)map_mem, PAGE_SIZE);
		L4_Set_Rights(&map_page, L4_FullyAccessible);
		if(is_grant) t.gi = L4_GrantItem(map_page, snd_offset);
		else t.mi = L4_MapItem(map_page, snd_offset);
		L4_Accept(L4_UntypedWordsAcceptor);
		L4_LoadMR(0, (L4_MsgTag_t){ .X.t = 2 }.raw);
		L4_LoadMRs(1, 2, t.raw);
		L4_Call(parent_tid);
		exit(0);
	} else {
		free(map_mem);
	}

	void *accept_base = NULL;
	uint8_t *accept_mem = alloc_aligned(&accept_base, accept_size,
		accept_size);
	diag("accept_base=%p, accept_mem=%p", accept_base, accept_mem);
	memset(accept_mem, 0xff, accept_size);
	assert(accept_mem[0] == 0xff);
	L4_Accept(L4_MapGrantItems(
		L4_Fpage((L4_Word_t)accept_mem, accept_size)));
	L4_MsgTag_t tag = L4_Receive_Timeout(child_tid, TEST_IPC_DELAY);
	L4_StoreMRs(1, 2, t.raw);
	ok(L4_IpcSucceeded(tag) && L4_TypedWords(tag) == 2
		&& L4_UntypedWords(tag) == 0, "ipc ok");
	size_t map_pos = L4_MapItemSndBase(t.mi);
	if(!ok1(map_pos == snd_offset)) {
		diag("map_pos=%u, snd_offset=%u",
			(unsigned)map_pos, (unsigned)snd_offset);
	}
	if(!ok(accept_mem[map_pos] != 0xff, "page was mapped")) {
		diag("accept_mem[map_pos]=%#02x", accept_mem[map_pos]);
	}
	iff_ok1(L4_IsGrantItem(t.gi), is_grant);
	L4_Fpage_t m_page = L4_MapItemSndFpage(t.mi);
	if(!ok1(L4_Address(m_page) == (L4_Word_t)accept_mem + map_pos)) {
		diag("m_page.address=%#lx, (accept_mem + map_pos)=%p",
			L4_Address(m_page), accept_mem + map_pos);
	}

	L4_LoadMR(0, 0);
	L4_Reply(child_tid);
	int st, dead = wait(&st);
	fail_if(dead != child, "dead=%d, child=%d", dead, child);

	free(accept_base);
}
END_TEST


static bool send_mapgrant_to(
	L4_ThreadId_t parent_tid,
	bool send_nilpage,
	bool send_grant)
{
	void *send_mem = aligned_alloc(PAGE_SIZE, PAGE_SIZE);
	assert(((uintptr_t)send_mem & (PAGE_SIZE - 1)) == 0);
	memset(send_mem, 0x5f, PAGE_SIZE);

	/* handshake. */
	L4_Accept(L4_UntypedWordsAcceptor);
	L4_MsgTag_t tag = L4_Receive_Timeout(parent_tid, TEST_IPC_DELAY);
	if(L4_IpcFailed(tag)) {
		diag("%s: receive failed, ec=%#lx", __func__, L4_ErrorCode());
		return false;
	}

	L4_Fpage_t map_page;
	if(send_nilpage) map_page = L4_Nilpage;
	else {
		map_page = L4_Fpage((uintptr_t)send_mem, PAGE_SIZE);
		L4_Set_Rights(&map_page, L4_ReadWriteOnly);
	}
	L4_MapItem_t mi;
	if(send_grant) {
		*(L4_GrantItem_t *)&mi.raw[0] = L4_GrantItem(map_page, 0);
	} else {
		mi = L4_MapItem(map_page, 0);
	}
	L4_LoadMR(0, (L4_MsgTag_t){ .X.t = 2, .X.label = 0xbeef }.raw);
	L4_LoadMRs(1, 2, mi.raw);
	tag = L4_Reply(parent_tid);
	if(L4_IpcFailed(tag)) {
		diag("%s: reply failed, ec=%#lx", __func__, L4_ErrorCode());
	}

	/* sync to let the parent read the entrails. */
	L4_Accept(L4_UntypedWordsAcceptor);
	tag = L4_Receive_Timeout(parent_tid, TEST_IPC_DELAY);
	if(L4_IpcFailed(tag)) {
		diag("%s: sync receive failed, ec=%#lx", __func__, L4_ErrorCode());
	}

	free(send_mem);
	return L4_IpcSucceeded(tag);
}


struct smgt_param {
	L4_ThreadId_t parent_tid;
	bool is_nil, is_grant;
};


static void send_mapgrant_to_fn(void *param)
{
	struct smgt_param *p = param;
	send_mapgrant_to(p->parent_tid, p->is_nil, p->is_grant);
	free(p);
}


/* test that map/grant operations are only effective under the conditions
 * specified by receiving a map/grant item into a nil receivewindow, sending a
 * nil page, sending a map/grant item in the same address space, or a
 * combination thereof.
 *
 * it should only map when neither page is nil and the transfer occurs between
 * address spaces.
 *
 * this covers the parameter validation clause in apply_mapitem().
 */
START_LOOP_TEST(mapgrant_effect_clause, iter, 0, 15)
{
	const bool is_nil_map = CHECK_FLAG(iter, 1),
		is_nil_wnd = CHECK_FLAG(iter, 2),
		is_grant = CHECK_FLAG(iter, 4),
		is_thread = CHECK_FLAG(iter, 8);
	diag("is_nil_map=%s, is_nil_wnd=%s, is_grant=%s, is_thread=%s",
		btos(is_nil_map), btos(is_nil_wnd), btos(is_grant), btos(is_thread));

	plan_tests(4);

	L4_ThreadId_t child_tid, parent_tid = L4_Myself();
	int child = -1;
	if(is_thread) {
		struct smgt_param *p = malloc(sizeof(*p));
		*p = (struct smgt_param){ .parent_tid = parent_tid,
			.is_nil = is_nil_map, .is_grant = is_grant };
		child_tid = xstart_thread(&send_mapgrant_to_fn, p);
	} else {
		child = fork_tid(&child_tid);
		if(child == 0) {
			bool ok = send_mapgrant_to(parent_tid, is_nil_map, is_grant);
			exit(ok ? 0 : 1);
		}
	}

	volatile uint8_t *recv_mem = aligned_alloc(PAGE_SIZE, PAGE_SIZE);
	memset((void *)recv_mem, 0xaa, PAGE_SIZE);
	uint8_t old_value = recv_mem[0];
	L4_Fpage_t rwnd;
	if(is_nil_wnd) rwnd = L4_Nilpage;
	else {
		rwnd = L4_Fpage((uintptr_t)recv_mem, PAGE_SIZE);
		L4_Set_Rights(&rwnd, L4_FullyAccessible);
	}
	L4_Accept(L4_MapGrantItems(rwnd));
	L4_LoadMR(0, 0);
	L4_MsgTag_t tag = L4_Call_Timeouts(child_tid,
		TEST_IPC_DELAY, TEST_IPC_DELAY);
	L4_Word_t ec = L4_ErrorCode();
	if(!ok1(L4_IpcSucceeded(tag))) diag("ec=%#lx", ec);

	/* no map when ineffective. */
	imply_ok1(is_nil_map || is_nil_wnd || is_thread,
		recv_mem[0] == 0xaa && recv_mem[0x123] == 0xaa);
	/* map when effective. */
	imply_ok1(!is_nil_map && !is_nil_wnd && !is_thread,
		recv_mem[0] != 0xaa && recv_mem[0x123] != 0xaa);

	/* write should disappear on flush when effective. */
	recv_mem[0] ^= 0xff;
	L4_FlushFpage(rwnd);
	iff_ok1(recv_mem[0] == old_value,
		!is_nil_map && !is_nil_wnd && !is_thread);

	L4_LoadMR(0, 0);
	L4_Send_Timeout(child_tid, TEST_IPC_DELAY);
	if(is_thread) {
		xjoin_thread(child_tid);
	} else {
		int st, dead = wait(&st);
		fail_unless(dead == child);
	}
}
END_TEST


/* generate 2..16 pages in the parent and map them into the forked child as
 * one aligned chunk.
 *
 * TODO: test with grantitems also, i.e. check that they become unmapped in
 * the parent. (this may be a tall order.)
 */
START_LOOP_TEST(map_smaller_pages, iter, 0, 7)
{
	const int num_pages = !CHECK_FLAG(iter, 1) ? 2 : 16;
	const unsigned parent_sndbase =
		CHECK_FLAG(iter, 2) ? PAGE_SIZE * num_pages * 2 : 0;
	const bool large_grain = CHECK_FLAG(iter, 4);
	uint32_t rand_seed = L4_SystemClock().raw ^ 0xb00ba6e1;
	diag("num_pages=%d, parent_sndbase=%u, large_grain=%s, rand_seed=%#x",
		num_pages, parent_sndbase, btos(large_grain), rand_seed);

	plan_tests(1);

	L4_ThreadId_t parent_tid = L4_Myself(), child_tid;
	int child = fork_tid(&child_tid);
	if(child == 0) {
		/* receive a mapping into a zeroed buffer, and compute the crc32() of
		 * its contents.
		 */
		const unsigned area_size = 1 << size_to_shift(
			128 * 1024 + parent_sndbase);
		void *base, *buffer = alloc_aligned(&base, area_size, area_size);
		memset(buffer, 0, area_size);
		diag("child's buffer=%p (area_size=%#x)", buffer, area_size);
		L4_Fpage_t acc_range = L4_Fpage((L4_Word_t)buffer, area_size);
		L4_Set_Rights(&acc_range, L4_FullyAccessible);
		L4_FlushFpage(acc_range);
		L4_Accept(L4_MapGrantItems(acc_range));
		L4_MsgTag_t tag = L4_Receive_Timeout(parent_tid, TEST_IPC_DELAY);
		if(L4_IpcFailed(tag) || L4_TypedWords(tag) < 2) {
			diag("child ipc failed or too weird, ec=%#lx tag=%#lx",
				L4_ErrorCode(), tag.raw);
			exit(1);
		}
		L4_MapItem_t mi;
		L4_StoreMRs(1, 2, mi.raw);
		if(!L4_IsMapItem(mi)) {
			diag("child message wasn't a mapitem");
			exit(1);
		}
		L4_Fpage_t fp = L4_MapItemSndFpage(mi);
		L4_Word_t sndbase = L4_MapItemSndBase(mi);
		diag("child observes sndbase=%#lx, fp=%#lx:%#lx", sndbase,
			L4_Address(fp), L4_Size(fp));
		uint32_t child_crc = crc32c(0, buffer + sndbase,
			MIN(size_t, area_size, L4_Size(L4_MapItemSndFpage(mi))));
		L4_Accept(L4_UntypedWordsAcceptor);
		L4_LoadMR(0, (L4_MsgTag_t){ .X.u = 1 }.raw);
		L4_LoadMR(1, child_crc);
		tag = L4_Reply(parent_tid);
		if(L4_IpcFailed(tag)) {
			diag("child reply failed, ec=%#lx", L4_ErrorCode());
		}
		free(base);
		exit(L4_IpcSucceeded(tag) ? 0 : 1);
	}

	while(CHECK_FLAG_ANY((uintptr_t)sbrk(0) + PAGE_SIZE,
		PAGE_SIZE * num_pages - 1))
	{
		void *throwaway = sbrk(PAGE_SIZE);
		fail_unless(throwaway != NULL);
		diag("backed sbrk() to %p", throwaway);
	}

	char *testmem = sbrk(num_pages * PAGE_SIZE);
	diag("testmem=%p", testmem);
	fail_unless(!CHECK_FLAG_ANY((uintptr_t)testmem,
		PAGE_SIZE * num_pages - 1));

	L4_Fpage_t sndpage = L4_Fpage((L4_Word_t)testmem, num_pages * PAGE_SIZE);
	L4_Set_Rights(&sndpage, L4_Readable);
	int n = forkserv_discontiguate(L4_Pager(), sndpage,
		large_grain ? 0 : PAGE_BITS);
	fail_if(n != 0, "discontig: n=%d", n);

	random_string(testmem, num_pages * PAGE_SIZE, &rand_seed);
	L4_Word_t refcrc = crc32c(0, testmem, num_pages * PAGE_SIZE);

	L4_MapItem_t mi = L4_MapItem(sndpage, parent_sndbase);
	diag("parent sends sndbase=%#lx", L4_MapItemSndBase(mi));
	L4_LoadMR(0, (L4_MsgTag_t){ .X.t = 2 }.raw);
	L4_LoadMRs(1, 2, mi.raw);
	L4_MsgTag_t tag = L4_Call_Timeouts(child_tid,
		TEST_IPC_DELAY, TEST_IPC_DELAY);
	IPC_FAIL(tag);
	fail_if(L4_UntypedWords(tag) < 1, "tag=%#lx", tag.raw);
	L4_Word_t child_crc; L4_StoreMR(1, &child_crc);
	if(!ok1(child_crc == refcrc)) {
		diag("child_crc=%#lx, refcrc=%#lx", child_crc, refcrc);
	}

	int st = 0, dead = wait(&st);
	fail_if(dead != child, "unexpected dead=%d (wanted %d)", dead, child);
}
END_TEST


/* simple enough. fork, receive map in fork over initialized memory, check
 * if it still looks like that memory afterward, return observation.
 *
 * TODO: the is_large mode only attempts to map the forbidden page and
 * anything possibly next to it. another test could be written that'd map
 * actual data pages from a different forked child process next to the actual
 * forbidden page, where possible, and then map that whole shebang with a
 * larger fpage. the receiver should compute a checksum of all pages mapped,
 * and return it.
 */
START_LOOP_TEST(forbid_map_from_special, iter, 0, 7)
{
	L4_KernelInterfacePage_t *kip = L4_GetKernelInterface();
	const L4_Word_t bad_addrs[] = {
		(L4_Word_t)kip,
		L4_MyLocalId().raw,
	};

	plan_tests(1);
	const bool is_grant = CHECK_FLAG(iter, 1),
		is_large = CHECK_FLAG(iter, 2);
	iter >>= 2;
	assert(iter < NUM_ELEMENTS(bad_addrs));
	const L4_Word_t bad_addr = bad_addrs[iter];
	const size_t recv_max = is_large ? 64 * 1024 : PAGE_SIZE * 2;
	diag("is_grant=%s, is_large=%s, bad_addr=%#lx, recv_max=%#lx",
		btos(is_grant), btos(is_large), bad_addr, (L4_Word_t)recv_max);

	L4_ThreadId_t parent_tid = L4_MyGlobalId(), child_tid;
	int child = fork_tid(&child_tid);
	if(child == 0) {
		void *mem_base = NULL;
		uint8_t *mem = alloc_aligned(&mem_base, recv_max, recv_max);
		diag("mem_base=%p, mem=%p, recv_max=%lu", mem_base, mem,
			(unsigned long)recv_max);
		memset(mem, 0xf0, recv_max);
		L4_Fpage_t acc_range = L4_Fpage((L4_Word_t)mem, recv_max);
		L4_Set_Rights(&acc_range, L4_FullyAccessible);
		L4_Accept(L4_MapGrantItems(acc_range));
		L4_MsgTag_t tag = L4_Receive_Timeout(parent_tid, TEST_IPC_DELAY);
		IPC_FAIL(tag);
		if(tag.X.t >= 2) {
			L4_MapItem_t mi;
			L4_StoreMRs(tag.X.u + 1, 2, mi.raw);
			if(L4_IsMapItem(mi) || L4_IsGrantItem(*(L4_GrantItem_t *)mi.raw)) {
				L4_Fpage_t fp = L4_MapItemSndFpage(mi);
				diag("  got map/grant item: sndbase %#lx, fpage %#lx:%#lx",
					L4_MapItemSndBase(mi), L4_Address(fp),
					L4_Size(fp));
			}
		}
		diag("received ipc had u=%lu,t=%lu", tag.X.u, tag.X.t);
		bool changed = false;
		for(int j=0; j < recv_max; j++) {
			if(mem[j] != 0xf0) {
				diag("deviation at j=%d: value=%#02x", j, mem[j]);
				changed = true;
				break;
			}
		}
		L4_LoadMR(0, (L4_MsgTag_t){ .X.u = 1 }.raw);
		L4_LoadMR(1, changed ? 1 : 0);
		tag = L4_Reply(parent_tid);
		if(L4_IpcFailed(tag)) {
			diag("child reply failed, ec=%#lx", L4_ErrorCode());
		}
		L4_FlushFpage(acc_range);
		free(mem_base);
		exit(0);
	}

	L4_Fpage_t fp = L4_Fpage(bad_addr & ~PAGE_MASK,
		is_large ? 32 * 1024 : PAGE_SIZE);
	L4_Set_Rights(&fp, L4_FullyAccessible);
	diag("trying to map %#lx:%#lx", L4_Address(fp), L4_Size(fp));
	L4_LoadMR(0, (L4_MsgTag_t){ .X.t = 2 }.raw);
	if(is_grant) {
		L4_GrantItem_t gi = L4_GrantItem(fp, 0);
		L4_LoadMRs(1, 2, gi.raw);
	} else {
		L4_MapItem_t mi = L4_MapItem(fp, 0);
		L4_LoadMRs(1, 2, mi.raw);
	}
	L4_MsgTag_t tag = L4_Call_Timeouts(child_tid, TEST_IPC_DELAY,
		TEST_IPC_DELAY);
	IPC_FAIL(tag);
	L4_Word_t changed = ~0ul; L4_StoreMR(1, &changed);
	ok1(changed == 0);

	int st = 0, dead = wait(&st);
	fail_if(dead != child, "dead=%d, st=%d", dead, st);
}
END_TEST


/* fork a child process, and attempt to receive a mapping from the parent into
 * either the UTCB range or on top of the kernel interface page. both should
 * fail, but if the KIP mapping succeeds, the child will abort and a reply
 * handshake will fail.
 */
START_LOOP_TEST(forbid_map_over_special, iter, 0, 1)
{
	L4_KernelInterfacePage_t *kip = L4_GetKernelInterface();
	const L4_Word_t bad_addrs[] = {
		(L4_Word_t)kip,
		L4_MyLocalId().raw,
	};

	plan_tests(4);
	assert(iter < NUM_ELEMENTS(bad_addrs));
	const L4_Word_t bad_addr = bad_addrs[iter];
	diag("bad_addr=%#lx", bad_addr);

	L4_ThreadId_t parent_tid = L4_MyGlobalId(), child_tid;
	int child = fork_tid(&child_tid);
	if(child == 0) {
		uint8_t kip_copy[16];
		memcpy(kip_copy, kip, sizeof(kip_copy));
		L4_ThreadId_t self_before = L4_MyGlobalId();

		L4_Fpage_t acc_range = L4_FpageLog2(bad_addr, PAGE_BITS);
		L4_Set_Rights(&acc_range, L4_FullyAccessible);
		diag("acc_range=%#lx:%#lx",
			L4_Address(acc_range), L4_Size(acc_range));
		L4_Accept(L4_MapGrantItems(acc_range));
		L4_MsgTag_t tag = L4_Receive_Timeout(parent_tid, TEST_IPC_DELAY);
		IPC_FAIL(tag);
		bool tid_ok = L4_SameThreads(self_before, L4_MyGlobalId()),
			kip_ok = memcmp(kip_copy, kip, sizeof(kip_copy)) == 0;
		/* provoke segfaults on IPC (MR write, syscall entry in KIP) if
		 * the mapping succeeded and a flushable page was mapped.
		 */
		L4_FlushFpage(acc_range);
		L4_LoadMR(0, (L4_MsgTag_t){ .X.u = 2 }.raw);
		L4_LoadMR(1, tid_ok ? 1 : 0);
		L4_LoadMR(2, kip_ok ? 1 : 0);
		L4_Reply(parent_tid);
		if(L4_IpcFailed(tag)) {
			diag("child reply failed, ec=%#lx", L4_ErrorCode());
		}

		exit(tid_ok && kip_ok ? 0 : 1);
	}

	/* map a page of frothy piss. */
	char *mapz0r = aligned_alloc(PAGE_SIZE, PAGE_SIZE);
	uint32_t seed = 0xbeefc0de;
	random_string(mapz0r, PAGE_SIZE, &seed);
	L4_Fpage_t map_page = L4_FpageLog2((L4_Word_t)mapz0r, PAGE_BITS);
	L4_Set_Rights(&map_page, L4_FullyAccessible);
	L4_MapItem_t mi = L4_MapItem(map_page, 0);
	L4_LoadBR(0, 0);
	L4_LoadMR(0, (L4_MsgTag_t){ .X.t = 2 }.raw);
	L4_LoadMRs(1, 2, mi.raw);
	L4_MsgTag_t tag = L4_Call_Timeouts(child_tid,
		TEST_IPC_DELAY, TEST_IPC_DELAY);
	L4_Word_t ec = L4_ErrorCode();
	L4_Word_t tid_ok, kip_ok;
	L4_StoreMR(1, &tid_ok);
	L4_StoreMR(2, &kip_ok);
	ok1(L4_IpcSucceeded(tag));
	skip_start(L4_IpcFailed(tag), 2, "ec=%#lx", ec) {
		ok1(tid_ok);
		ok1(kip_ok);
	} skip_end;

	int st = 0, dead = wait(&st);
	fail_if(dead != child, "dead=%d, st=%d", dead, st);
	if(!ok((st & 15) != 7, "child didn't segfault")) {
		diag("addr & ~0xf = %#lx", (unsigned long)st & ~0xful);
	}

	L4_FlushFpage(map_page);
	free(mapz0r);
}
END_TEST


/* access memory at an address, causing a segfault where applicable. */
static void poke_addr_fn(void *addr)
{
	volatile uint8_t *p = addr;
	(*p) ^= 0xff;
	(*p) ^= 0xff;
	exit_thread(NULL);
}


static bool segfaults_at(uintptr_t addr)
{
	L4_ThreadId_t poke_tid = xstart_thread(&poke_addr_fn, (void *)addr);
	L4_Word_t ec = 0;
	void *result = join_thread_long(poke_tid, A_SHORT_NAP, &ec);
	bool fault = (uintptr_t)result == addr;
	diag("addr=%#lx: fault=%s (ec=%#lx, result=%p)", addr,
		btos(fault), ec, result);

	return fault;
}


/* tests mapping into empty UTCB ranges. this should cause a fault before and
 * after.
 */
START_TEST(forbid_map_into_utcb_range)
{
	/* scan the UTCB area to find a page that's not currently mapped. */
	L4_Fpage_t utcb_area = L4_Nilpage;
	int n = forkserv_get_utcb_area(L4_Pager(), &utcb_area);
	fail_if(n != 0, "forkserv call failed, n=%d", n);

	uintptr_t my_utcb_page = L4_MyLocalId().raw & ~PAGE_MASK,
		missing_page = 0;
	for(int i = L4_Size(utcb_area) / PAGE_SIZE - 1;
		i >= 0;
		i--)
	{
		uintptr_t addr = L4_Address(utcb_area) + i * PAGE_SIZE;
		if(addr == my_utcb_page) continue;

		/* probe it. there should be a segfault if the page isn't mapped. */
		uintptr_t fault_addr = addr | 127;
		if(segfaults_at(fault_addr)) {
			missing_page = addr;
			break;
		}
	}
	if(missing_page == 0) {
		plan_skip_all("can't find a missing UTCB area page");
		return;
	}

	plan_tests(4);
	diag("my_utcb_page=%#lx, missing_page=%#lx",
		my_utcb_page, missing_page);

	/* construct a page that the child will try to map in. */
	void *map_base;
	char *map_page = alloc_aligned(&map_base, PAGE_SIZE, PAGE_SIZE);
	uint32_t seed = 0x1bfa5438;
	random_string(map_page, PAGE_SIZE, &seed);

	L4_ThreadId_t child_tid = L4_nilthread, parent_tid = L4_MyGlobalId();
	int child = fork_tid(&child_tid);
	if(child == 0) {
		/* receive messages from parent_tid until QUIT_LABEL, reply with a
		 * mapitem of `map_page'.
		 */
		for(;;) {
			L4_LoadBR(0, 0);
			L4_MsgTag_t tag = L4_Receive(parent_tid);
			if(L4_IpcFailed(tag)) exit(1);
			if(L4_Label(tag) == QUIT_LABEL) break;
			*(volatile uint8_t *)map_page ^= 0xff;
			L4_Fpage_t fp = L4_FpageLog2((uintptr_t)map_page, PAGE_BITS);
			L4_Set_Rights(&fp, L4_Readable | L4_Writable);
			L4_MapItem_t mi = L4_MapItem(fp, 0);
			L4_LoadMR(0, (L4_MsgTag_t){ .X.t = 2 }.raw);
			L4_LoadMRs(1, 2, mi.raw);
			tag = L4_Reply(parent_tid);
			if(L4_IpcFailed(tag)) exit(2);
		}
		diag("child done");
		exit(0);
	}

	/* first, map the child's page somewhere to verify its mechanism. */
	void *wnd_base;
	uint8_t *wnd_mem = alloc_aligned(&wnd_base, PAGE_SIZE, PAGE_SIZE);
	memset(wnd_mem, 0, PAGE_SIZE);
	L4_Fpage_t wnd = L4_FpageLog2((uintptr_t)wnd_mem, PAGE_BITS);
	diag("wnd=%#lx:%#lx", L4_Address(wnd), L4_Size(wnd));
	L4_Accept(L4_MapGrantItems(wnd));
	L4_LoadMR(0, 0);
	L4_MsgTag_t tag = L4_Call_Timeouts(child_tid,
		TEST_IPC_DELAY, TEST_IPC_DELAY);
	IPC_FAIL(tag);
	ok(*wnd_mem == map_page[0] || *wnd_mem == (map_page[0] ^ 0xff),
		"page received into normal range OK");
	ok1(!segfaults_at((uintptr_t)wnd_mem | 0x7f));
	L4_Set_Rights(&wnd, L4_FullyAccessible);
	L4_FlushFpage(wnd);
	free(wnd_base);

	/* then, map it into a known hole in the UTCB range. */
	ok1(segfaults_at(missing_page | 0x7f));		/* for consistency */
	wnd = L4_FpageLog2(missing_page, PAGE_BITS);
	L4_Accept(L4_MapGrantItems(wnd));
	L4_LoadMR(0, 0);
	tag = L4_Call_Timeouts(child_tid, TEST_IPC_DELAY, TEST_IPC_DELAY);
	IPC_FAIL(tag);
	ok1(segfaults_at(missing_page | 0x7f));

	send_quit(child_tid);
	free(map_base);
	int st = 0, dead = wait(&st);
	fail_if(dead != child, "expected %d, got dead=%d, st=%d",
		child, dead, st);
}
END_TEST


/* the "bug" tcase */


struct big_reply_param {
	size_t n_trash;
	int n_faults;
};

/* pager thread that replies with a configurable number of trash MRs,
 * inflating the message and overwriting registers beyond that expected of a
 * minimal pager reply.
 */
static void big_reply_pager(void *param_ptr)
{
	struct big_reply_param *p = param_ptr;
	const size_t n_trash = p->n_trash;
	p->n_faults = 0;
	uint32_t r_state = 0xb337f00d;	/* not a balanced diet, though */
	bool running = true;
	while(running) {
		L4_ThreadId_t sender;
		L4_Accept(L4_UntypedWordsAcceptor);
		L4_MsgTag_t tag = L4_Wait(&sender);
		for(;;) {
			if(L4_IpcFailed(tag)) {
				diag("%s: IPC failed, ec=%#lx", __func__, L4_ErrorCode());
				break;
			}
			if(L4_Label(tag) == QUIT_LABEL) {
				running = false;
				break;
			} else if(L4_Label(tag) >> 4 != 0xffe) {
				diag("%s: unknown label %#lx", __func__, L4_Label(tag));
				break;
			}

			/* forward to own pager */
			L4_Word_t u[2];
			assert(L4_UntypedWords(tag) == 2);
			L4_StoreMRs(1, 2, u);
			p->n_faults++;
			diag("serving fault %#lx, %#lx", u[0], u[1]);
			L4_Accept(L4_MapGrantItems(L4_CompleteAddressSpace));
			L4_LoadMR(0, (L4_MsgTag_t){ .X.u = 2,
				.X.label = L4_Label(tag) }.raw);
			L4_LoadMRs(1, 2, u);
			tag = L4_Call(L4_Pager());
			if(L4_IpcFailed(tag)) {
				diag("%s: forward failed, ec=%#lx", __func__, L4_ErrorCode());
				break;
			}

			assert(L4_UntypedWords(tag) == 0);
			assert(L4_TypedWords(tag) == 2);
			L4_StoreMRs(1, 2, u);
			/* reply w/ trash */
			L4_LoadMR(0, (L4_MsgTag_t){ .X.u = n_trash, .X.t = 2 }.raw);
			for(int i=0; i < n_trash; i++) {
				L4_LoadMR(i + 1, rand32(&r_state));
			}
			L4_LoadMRs(n_trash + 1, 2, u);
			L4_Accept(L4_UntypedWordsAcceptor);
			tag = L4_ReplyWait(sender, &sender);
		}
	}

	diag("%s exiting", __func__);
}


/* tcase "halt" */

/* verify that passive senders and receivers do not rendezvous when the
 * passive party's H bit is set, regardless of propagation. instead a
 * potential peer will time out.
 */
START_LOOP_TEST(basic_halt_before_rendezvous, iter, 0, 3)
{
	const bool pasv_is_send = CHECK_FLAG(iter, 1),
		propagate = CHECK_FLAG(iter, 2);
	diag("pasv_is_send=%s, propagate=%s",
		btos(pasv_is_send), btos(propagate));

	plan_tests(9);

	const L4_Word_t payload = 0xb0a7face;
	L4_ThreadId_t prop_other = xstart_thread(&receive_and_exit, NULL),
		sender = ipc_to_from_thread(L4_Myself(),
			propagate ? prop_other : L4_nilthread,
			payload, L4_ZeroTime, false, true);
	diag("sender=%lu:%lu, prop_other=%lu:%lu",
		L4_ThreadNo(sender), L4_Version(sender),
		L4_ThreadNo(prop_other), L4_Version(prop_other));

	L4_ThreadId_t peer = propagate ? prop_other : sender;
	if(!pasv_is_send) {
		/* receive and cause other to fall into passive receive. */
		L4_Accept(L4_UntypedWordsAcceptor);
		L4_MsgTag_t tag = L4_Receive_Timeout(peer, TEST_IPC_DELAY);
		IPC_FAIL(tag);
		L4_Sleep(A_SHORT_NAP);
		ok1(thr_in_recv(sender));
		skip(1, "!pasv_is_send");	/* skip thr_in_send() */
	} else {
		/* padding to keep test iters in sync */
		L4_Sleep(A_SHORT_NAP);
		skip(1, "pasv_is_send");	/* skip thr_in_recv() */
		ok1(thr_in_send(sender));
	}

	ok(!set_h_bit(sender, true), "halt set OK");
	ok1(thr_is_halted(sender));

	/* check IPC non-rendezvous under halt. */
	L4_MsgTag_t tag;
	if(pasv_is_send) {
		L4_Accept(L4_UntypedWordsAcceptor);
		tag = L4_Receive_Timeout(peer, A_SHORT_NAP);
	} else {
		L4_LoadMR(0, 0);
		tag = L4_Send_Timeout(sender, A_SHORT_NAP);
	}
	L4_Word_t ec = L4_IpcSucceeded(tag) ? 0 : L4_ErrorCode();
	if(!ok(L4_IpcFailed(tag) && (ec >> 1) == 1, "Ipc should timeout")) {
		diag("tag=%#lx, ec=%#lx", tag.raw, ec);
	}

	/* then check it once halt is cleared. */
	ok(!set_h_bit(sender, false), "halt cleared OK");
	ok1(!thr_is_halted(sender));
	if(pasv_is_send) {
		L4_Accept(L4_UntypedWordsAcceptor);
		tag = L4_Receive_Timeout(peer, A_SHORT_NAP);
	} else {
		L4_LoadMR(0, 0);
		tag = L4_Send_Timeout(sender, A_SHORT_NAP);
	}
	ec = L4_IpcSucceeded(tag) ? 0 : L4_ErrorCode();
	if(!ok(L4_IpcSucceeded(tag), "Ipc should succeed")) {
		diag("tag=%#lx, ec=%#lx", tag.raw, ec);
	}
	imply_ok1(propagate && pasv_is_send, L4_IpcPropagated(tag));

	/* clean up */
	if(pasv_is_send) {
		/* execute economic slave */
		L4_LoadMR(0, 0);
		L4_Reply(sender);
	}
	close_sender(sender);
	send_quit(prop_other);
	xjoin_thread(prop_other);
}
END_TEST


/* Bug description: if a pagefault is replied to with more than 3 MRs (tag,
 * single MapItem), the reply's data overwrites that present in the
 * recipient's TCB. this can disrupt ongoing IPC by overwriting registers that
 * haven't been copied, and munge MRs in a pre-IPC condition when a pagefault
 * or exception is generated while the thread loads its MRs.
 * ---
 *
 * when successful, this test demonstrates that the pagefault IPC mechanism
 * avoids stomping the receiver's MRs regardless of reply length.
 */
START_LOOP_TEST(big_pf_stomp, iter, 0, 1)
{
	plan_tests(2);
	const size_t n_trash = CHECK_FLAG(iter, 1) ? 7 : 0;
	diag("n_trash=%u", (unsigned)n_trash);
	const uint32_t initial_seed = 0x498ba2c1;

	struct big_reply_param *param = malloc(sizeof(*param));
	*param = (struct big_reply_param){ .n_trash = n_trash };
	L4_ThreadId_t old_pager = L4_Pager(),
		helper = xstart_thread(&big_reply_pager, param);

	uint8_t *fault_mem = aligned_alloc(PAGE_SIZE, PAGE_SIZE);
	assert(((L4_Word_t)fault_mem & PAGE_MASK) == 0);
	memset(fault_mem, 1, PAGE_SIZE);
	uint32_t seed = initial_seed;
	for(int i=0; i < 63; i++) L4_LoadMR(i + 1, rand32(&seed));
	L4_Set_Pager(helper);
	diag("inits done, helper pager set");

	L4_Fpage_t p = L4_Fpage((L4_Word_t)fault_mem, PAGE_SIZE);
	L4_Set_Rights(&p, L4_FullyAccessible);
	L4_FlushFpage(p);
	memset(fault_mem, 2, PAGE_SIZE);
	L4_Set_Pager(old_pager);
	diag("fault done, pager reset");

	bool all_ok = true;
	seed = initial_seed;
	for(int i=0; i < 63; i++) {
		L4_Word_t w; L4_StoreMR(i + 1, &w);
		uint32_t expect = rand32(&seed);
		if(w != expect) {
			diag("index %d: w=%#lx, expect=%#x", i, w, expect);
			all_ok = false;
		}
	}
	ok(param->n_faults > 0, "faults occurred");
	ok(all_ok, "MRs weren't trashed");

	fail_if(!send_quit(helper));
	xjoin_thread(helper);
	free(fault_mem);
	free(param);
}
END_TEST


static Suite *ipc_suite(void)
{
	Suite *s = suite_create("ipc");

	{
		TCase *tc = tcase_create("api");
		tcase_add_test(tc, tid_spec_to_fail);
		tcase_add_test(tc, tid_spec_to_ok);
		tcase_add_test(tc, tid_spec_from_fail);
		tcase_add_test(tc, tid_spec_from_ok);
		tcase_add_test(tc, receive_from_local);
		tcase_add_test(tc, receive_from_foreign);
		tcase_add_test(tc, receive_from_anylocalthread);
		tcase_add_test(tc, receive_from_false_local);
		suite_add_tcase(s, tc);
	}

	{
		/* TODO: should also have a distinct test for VirtualSender values
		 * that don't exist.
		 */
		TCase *tc = tcase_create("propagate");
		tcase_set_fork(tc, false);
		tcase_add_test(tc, propagation);
		tcase_add_test(tc, propagation_turn_fromspec);
		tcase_add_test(tc, propagation_no_turn_fromspec);
		tcase_add_test(tc, propagation_in_active_receive);
		tcase_add_test(tc, cancel_virtualsender);
		tcase_add_test(tc, propagation_on_closed_wait);
		suite_add_tcase(s, tc);
	}

	/* TODO: merge this into redir_suite.c to lose ~350 lines, once the IPC
	 * helper thread has been exported or an equivalent written somewhere
	 * else.
	 */
	{
		TCase *tc = tcase_create("redir");
		tcase_set_fork(tc, false);	/* involves SpaceControl */
		ADD_IDL_FIXTURE_F(tc, helper);
		tcase_add_test(tc, basic_redir);
		suite_add_tcase(s, tc);
	}

	/* preemption tests.
	 *
	 * TODO: move these into sched_suite.c; preemption isn't so much related to
	 * the Ipc syscall that these should take up some of this suite's
	 * thousands of lines.
	 */
	{
		TCase *tc = tcase_create("preempt");
		tcase_set_fork(tc, false);
		tcase_add_test(tc, send_preempt);
		tcase_add_test(tc, recv_preempt);
		suite_add_tcase(s, tc);
	}

	/* error conditions reported for timeouts and cancellations. */
	{
		TCase *tc = tcase_create("error");
		/* (required by cancel_sender, maybe others [TODO: look them up]) */
		tcase_set_fork(tc, false);
		ADD_IDL_FIXTURE_U(tc, helper);
		ADD_IDL_FIXTURE_U(tc, other_helper);
		tcase_add_test(tc, recv_timeout_from_send);
		tcase_add_test(tc, recv_timeout_from_preempt);
		tcase_add_test(tc, point_ipc_timeouts);
		tcase_add_test(tc, point_xfer_timeouts);
		tcase_add_test(tc, r_recv_timeout_test);
		tcase_add_test(tc, cancel_sender);
		suite_add_tcase(s, tc);
	}

	/* typed transfer tests. */
	{
		TCase *tc = tcase_create("typed");
		tcase_add_test(tc, c_bit_in_typed_words);
		tcase_add_test(tc, mapgrant_address);
		tcase_add_test(tc, mapgrant_effect_clause);
		tcase_add_test(tc, map_smaller_pages);
		/* NOTE: these must be run from within a fork; otherwise there's a
		 * chance they screw with testbench's UTCB or KIP pages.
		 */
		tcase_add_test(tc, forbid_map_from_special);
		tcase_add_test(tc, forbid_map_over_special);
		tcase_add_test(tc, forbid_map_into_utcb_range);
		suite_add_tcase(s, tc);
	}

	/* tests about interactions between the IPC state machine and threads' H
	 * (halt) bit
	 */
	{
		TCase *tc = tcase_create("halt");
		tcase_set_fork(tc, false);
		tcase_add_test(tc, basic_halt_before_rendezvous);
		suite_add_tcase(s, tc);
	}

	/* bug-provocation tests */
	{
		TCase *tc = tcase_create("bug");
		tcase_add_test(tc, big_pf_stomp);
		suite_add_tcase(s, tc);
	}

	/* tests written to hit a panic() in ipc.c, which haven't been sorted
	 * elsewhere yet
	 */
	{
		TCase *tc = tcase_create("panic");
		tcase_add_test(tc, map_into_large_acceptor);
		tcase_add_test(tc, map_into_small_acceptor);
		suite_add_tcase(s, tc);
	}

	/* tests of the Lipc syscall. */
	{
		TCase *tc = tcase_create("lipc");
		tcase_add_test(tc, basic_lipc);
		tcase_add_test(tc, basic_invalid_lipc);
		suite_add_tcase(s, tc);
	}

	return s;
}


static const struct suite_spec s = { &ipc_suite, 94 };
AUTODATA(testsuites, &s);
