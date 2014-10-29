/* tests on L4.X2 redirection. */

#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <assert.h>
#include <ccan/talloc/talloc.h>
#include <ccan/darray/darray.h>

#include <l4/types.h>
#include <l4/ipc.h>

#include <ukernel/util.h>

#include "defs.h"
#include "test.h"
#include "forkserv-defs.h"


#define GET_MSGS_LABEL 0x674d


struct rf_ctx {
	bool running;
	darray(struct redir_fixture_msg_info *) msgs;
};


/* (typedef for ease of relocation to IDL, eventually.) */
typedef struct redir_fixture_msg_info {
	L4_Word_t tag, sender, ir;
	L4_Word_t mr[7];
} fixmsg_t;


static L4_ThreadId_t redir_fixture_tid;

static int __redir_fixture_get_msgs_timeouts(
	L4_ThreadId_t service_tid,
	uint16_t *n_got,
	fixmsg_t *buf,
	unsigned *buflen_p,
	L4_Time_t send_timeout,
	L4_Time_t recv_timeout);


/* an out-of-process receiver. this is used because IPC to the redirector's
 * space (i.e. the test process) occurs without redirection. calls parent with
 * ec, label and exits after the call finishes.
 */
static int fork_receiver(L4_ThreadId_t *tid_p)
{
	L4_ThreadId_t parent_tid = L4_MyGlobalId();
	int receiver = fork_tid(tid_p);
	if(receiver != 0) return receiver;

	L4_ThreadId_t sender;
	L4_MsgTag_t tag = L4_Wait_Timeout(TEST_IPC_DELAY, &sender);
	L4_Word_t ec = L4_IpcFailed(tag) ? L4_ErrorCode() : 0;
	L4_Word_t label = 0;
	if(L4_IpcFailed(tag)) {
		diag("receiver wait failed: ec=%#lx", L4_ErrorCode());
	} else {
		label = L4_Label(tag);
		L4_LoadMR(0, 0);
		tag = L4_Reply(sender);
		if(L4_IpcFailed(tag)) {
			diag("receiver reply failed: ec=%#lx", L4_ErrorCode());
		}
	}
	/* notify parent. */
	L4_LoadMR(0, (L4_MsgTag_t){ .X.u = 2 }.raw);
	L4_LoadMR(1, ec);
	L4_LoadMR(2, label);
	L4_Call_Timeouts(parent_tid, TEST_IPC_DELAY, TEST_IPC_DELAY);
	exit(0);
}


static L4_ThreadId_t set_fork_redir(L4_ThreadId_t redir_tid)
{
	L4_ThreadId_t prev_redir;
	int n = forkserv_set_fork_redir(L4_Pager(), &prev_redir.raw,
		redir_tid.raw);
	fail_if(n != 0, "redir setting failed, n=%d", n);

	if(!L4_IsNilThread(redir_tid)) {
		if(prev_redir.raw != L4_anythread.raw) {
			diag("prev_redir=%lu:%lu",
				L4_ThreadNo(prev_redir), L4_Version(prev_redir));
		} else {
			diag("prev_redir=any");
		}
	}

	return prev_redir;
}


/* simple child process. sends label to partner, then exits. */
static int fork_sender(L4_ThreadId_t *tid_p, L4_ThreadId_t partner, L4_Word_t label)
{
	int child = fork_tid(tid_p);
	if(child != 0) return child;

	L4_LoadMR(0, (L4_MsgTag_t){ .X.label = label, .X.u = 1 }.raw);
	L4_LoadMR(1, partner.raw);
	L4_MsgTag_t tag = L4_Call_Timeouts(partner, TEST_IPC_DELAY,
		TEST_IPC_DELAY);
	if(L4_IpcFailed(tag)) {
		diag("sender: ipc failed, ec=%#lx", L4_ErrorCode());
	}
	exit(0);
}


/* test that a child process can be started with redirection, and that the
 * redirected child can wake up enough to synchronize with a different process
 * and then quit.
 */
START_LOOP_TEST(fork_redirect_basic, iter, 0, 1)
{
	const L4_Word_t call_label = 0xbabe;
	const bool use_redir = CHECK_FLAG(iter, 1);

	plan_tests(5);
	diag("use_redir=%s", btos(use_redir));

	L4_ThreadId_t receiver_tid;
	int receiver = fork_receiver(&receiver_tid);

	if(use_redir) set_fork_redir(redir_fixture_tid);
	L4_ThreadId_t child_tid;
	int child = fork_sender(&child_tid, receiver_tid, call_label);

	diag("waiting for receiver=%d, child=%d", receiver, child);
	L4_MsgTag_t tag = L4_Receive_Timeout(receiver_tid, TEST_IPC_DELAY);
	L4_Word_t recv_ec; L4_StoreMR(1, &recv_ec);
	L4_Word_t recv_label; L4_StoreMR(2, &recv_label);
	ok1(L4_IpcSucceeded(tag) && recv_ec == 0);
	ok1(recv_label == call_label);
	L4_LoadMR(0, 0); L4_Reply(receiver_tid);
	for(int i=0; i < 2; i++) {
		int st, dead = wait(&st);
		if(dead != receiver && dead != child) {
			diag("unexpected dead=%d", dead);
		}
	}

	/* grab and examine the entrails.
	 * TODO: move this into a function.
	 */
	void *talctx = talloc_new(NULL);
	darray(struct redir_fixture_msg_info *) msgs;
	darray_init(msgs);
	uint16_t n_got;
	do {
		n_got = 0;
		struct redir_fixture_msg_info buf[6];
		unsigned blen = sizeof(buf);
		memset(buf, 0, sizeof(buf));
		int n = __redir_fixture_get_msgs_timeouts(redir_fixture_tid,
			&n_got, buf, &blen, TEST_IPC_DELAY, TEST_IPC_DELAY);
		if(n != 0) {
			diag("RedirFixture::get_msgs failed: n=%d", n);
			break;
		}
		for(int i=0; i < n_got; i++) {
			struct redir_fixture_msg_info *m = talloc_size(talctx,
				sizeof(*m));
			*m = buf[i];
			darray_push(msgs, m);
		}
	} while(n_got > 0);
	iff_ok1(msgs.size > 0, use_redir);

	bool found_call = false, found_pf = false;
	for(size_t i=0; i < msgs.size; i++) {
		L4_Word_t label = L4_Label((L4_MsgTag_t){ .raw = msgs.item[i]->tag });
		if(label == call_label) found_call = true;
		else if((label >> 4) == 0xffe) found_pf = true;
	}
	iff_ok1(found_call, use_redir);
	iff_ok1(found_pf, use_redir);

	/* cleanup */
	darray_free(msgs);
	talloc_free(talctx);
}
END_TEST


/* test for redirection from the child's child, which should have the same
 * redirector as its parent. the redirector should see messages according to
 * where redirectors are enabled.
 *
 * FIXME: remove all the debug diags once redirection starts to work without
 * blocking.
 */
START_LOOP_TEST(fork_redirect_deep, iter, 0, 3)
{
	const L4_Word_t call_label = 0xb0a7;
	const bool use_outer_redir = CHECK_FLAG(iter, 1),
		use_inner_redir = CHECK_FLAG(iter, 2);
	diag("call_label=%#lx, use_outer_redir=%s, use_inner_redir=%s",
		call_label, btos(use_outer_redir), btos(use_inner_redir));

	if(use_outer_redir && use_inner_redir) {
		/* FIXME: this case craps out and leaves a child process hanging in
		 * iter=3, which crashes forkserv. that's due to the redirection
		 * lock-up in the child's fork call causing it never to return;
		 * subsequently nothing kills the child nor parent, and forkserv winds
		 * up in untested space -- and assert-crashes out.
		 */
		plan_skip_all("FIXME: see comment for iter=3");
		return;
	}

	plan_tests(2);

	diag("redir_fixture_tid=%lu:%lu",
		L4_ThreadNo(redir_fixture_tid), L4_Version(redir_fixture_tid));

	L4_ThreadId_t receiver_tid, child_tid, parent_tid = L4_Myself();
	int receiver = fork_receiver(&receiver_tid);
	diag("receiver=%d (%lu:%lu)", receiver,
		L4_ThreadNo(receiver_tid), L4_Version(receiver_tid));
	if(use_outer_redir) set_fork_redir(redir_fixture_tid);

	int child = fork_tid(&child_tid);
	if(child == 0) {
		if(use_outer_redir != use_inner_redir) {
			set_fork_redir(use_inner_redir ? redir_fixture_tid : L4_anythread);
		}
		set_fork_redir(L4_nilthread);
		L4_ThreadId_t sender_tid;
		int sender = fork_sender(&sender_tid, receiver_tid, call_label);
		diag("sender=%d (%lu:%lu)", sender,
			L4_ThreadNo(sender_tid), L4_Version(sender_tid));

		int st, dead = wait(&st);
		if(dead != sender) diag("dead=%d, sender=%d", dead, sender);

		L4_LoadMR(0, (L4_MsgTag_t){ .X.u = 2 }.raw);
		L4_LoadMR(1, sender);
		L4_LoadMR(2, sender_tid.raw);
		L4_MsgTag_t tag = L4_Call_Timeouts(parent_tid,
			TEST_IPC_DELAY, TEST_IPC_DELAY);
		if(L4_IpcFailed(tag)) diag("child: ec=%#lx", L4_ErrorCode());
		exit(0);
	}
	diag("child=%d (%lu:%lu)", child,
		L4_ThreadNo(child_tid), L4_Version(child_tid));

	L4_MsgTag_t tag = L4_Receive_Timeout(child_tid, TEST_IPC_DELAY);
	L4_Word_t sender_pid;
	L4_ThreadId_t sender_tid;
	if(!ok(L4_IpcSucceeded(tag), "received from child")) {
		diag("ec=%#lx", L4_ErrorCode());
		sender_pid = 0;
		sender_tid = L4_nilthread;
	} else {
		L4_StoreMR(1, &sender_pid);
		L4_StoreMR(2, &sender_tid.raw);
		L4_LoadMR(0, 0);
		L4_Reply(child_tid);
		diag("sender_pid=%lu", sender_pid);
	}

	/* TODO: examine the entrails. check that call_label exists iff
	 * use_inner_redir
	 */
	pass("didn't crash!");

	for(int i=0; i < 2; i++) {
		int st, dead = wait(&st);
		if(dead != receiver && dead != child) {
			diag("unexpected dead=%d (ok are %d, %d)\n",
				dead, receiver, child);
		}
		diag("i=%d, dead=%d", i, dead);
	}
}
END_TEST


/* the "deadlk" tcase. */

/* fork two children A, B. A calls B, then communicates status to parent and
 * exits. B receives, replies, and exits.
 *
 * variables: whether A is being redirected, same for B, and another that says
 * whether the receiver should sleep before receiving.
 */
START_LOOP_TEST(no_deadlock, iter, 0, 7)
{
	const bool redir_rcv = CHECK_FLAG(iter, 1),
		redir_snd = CHECK_FLAG(iter, 2),
		rcv_sleep = CHECK_FLAG(iter, 4);
	diag("redir_rcv=%s, redir_snd=%s, rcv_sleep=%s",
		btos(redir_rcv), btos(redir_snd), btos(rcv_sleep));

	plan_tests(2);

	const L4_ThreadId_t parent_tid = L4_MyGlobalId();

	/* B, aka `rcv' */
	set_fork_redir(redir_rcv ? redir_fixture_tid : L4_anythread);
	L4_ThreadId_t rcv_tid;
	int rcv_child = fork_tid(&rcv_tid);
	if(rcv_child == 0) {
		if(rcv_sleep) {
			/* twice the forward timeout of redir_fixture_fn() */
			L4_Sleep(A_SHORT_NAP);
			L4_Sleep(A_SHORT_NAP);
		}
		L4_ThreadId_t sender;
		L4_MsgTag_t tag = L4_Wait_Timeout(TEST_IPC_DELAY, &sender);
		if(L4_IpcSucceeded(tag)) {
			L4_LoadMR(0, 0);
			L4_Reply(sender);
		} else {
			diag("rcv_child: ec=%#lx", L4_ErrorCode());
		}
		exit(L4_IpcFailed(tag) ? 1 : 0);
	}

	/* A, aka `snd' */
	set_fork_redir(redir_snd ? redir_fixture_tid : L4_anythread);
	L4_ThreadId_t snd_tid;
	int snd_child = fork_tid(&snd_tid);
	if(snd_child == 0) {
		L4_LoadMR(0, (L4_MsgTag_t){ .X.label = 0xbeef }.raw);
		L4_MsgTag_t tag = L4_Call_Timeouts(rcv_tid,
			TEST_IPC_DELAY, TEST_IPC_DELAY);
		L4_Word_t ec = L4_IpcFailed(tag) ? L4_ErrorCode() : 0;
		if(ec != 0) diag("snd_child: ec=%#lx", ec);

		/* report back. */
		L4_LoadMR(0, (L4_MsgTag_t){ .X.u = 1 }.raw);
		L4_LoadMR(1, ec);
		tag = L4_Call_Timeouts(parent_tid, TEST_IPC_DELAY, TEST_IPC_DELAY);
		if(L4_IpcFailed(tag)) {
			diag("snd_child: report call failed, ec=%#lx", L4_ErrorCode());
		}
		exit(L4_IpcFailed(tag) ? 1 : 0);
	}

	/* catch report from A. */
	L4_MsgTag_t tag = L4_Receive_Timeout(snd_tid,
		L4_TimePeriod(L4_PeriodUs_NP(TEST_IPC_DELAY) * 3));
	L4_Word_t snd_ec = 0; L4_StoreMR(1, &snd_ec);
	if(!ok(L4_IpcSucceeded(tag), "got report")) {
		diag("report ec=%#lx", L4_ErrorCode());
	}
	if(!ok1(snd_ec == 0)) diag("snd_ec=%#lx", snd_ec);
	if(L4_IpcSucceeded(tag)) {
		L4_LoadMR(0, 0);
		L4_Reply(snd_tid);
	}

	/* clean up. */
	diag("waiting for snd=%d, rcv=%d", snd_child, rcv_child);
	for(int i=0; i < 2; i++) {
		int st, dead = wait(&st);
		if(dead != snd_child && dead != rcv_child) {
			diag("unexpected dead=%d", dead);
		}
	}
}
END_TEST


/* fork two children A, B. both call to one another with a timeout, and report
 * the result back to parent. both should report a send-side timeout both with
 * and without redirection.
 */
START_LOOP_TEST(mutual_send_deadlock, iter, 0, 3)
{
	const bool redir_flag[2] = { CHECK_FLAG(iter, 1), CHECK_FLAG(iter, 2) };
	diag("redir_flag={%s, %s}", btos(redir_flag[0]), btos(redir_flag[1]));
	plan_tests(4);

	const L4_ThreadId_t parent_tid = L4_MyGlobalId();

	int child[2];
	L4_ThreadId_t child_tid[2];
	for(int i=0; i < 2; i++) {
		set_fork_redir(redir_flag[i] ? redir_fixture_tid : L4_anythread);
		child[i] = fork_tid(&child_tid[i]);
		if(child[i] == 0) {
			L4_ThreadId_t peer_tid;
			if(i == 1) peer_tid = child_tid[0];
			else {
				/* get peer from parent. */
				L4_MsgTag_t tag = L4_Receive_Timeout(parent_tid,
					TEST_IPC_DELAY);
				IPC_FAIL(tag);
				L4_StoreMR(1, &peer_tid.raw);
			}
			/* call. */
			L4_LoadMR(0, (L4_MsgTag_t){ .X.label = 0xb0a0 + i }.raw);
			L4_MsgTag_t tag = L4_Call_Timeouts(peer_tid,
				TEST_IPC_DELAY, TEST_IPC_DELAY);
			L4_Word_t ec = L4_ErrorCode();
			/* report back. */
			L4_LoadMR(0, (L4_MsgTag_t){ .X.u = 2 }.raw);
			L4_LoadMR(1, tag.raw);
			L4_LoadMR(2, ec);
			tag = L4_Send_Timeout(parent_tid, TEST_IPC_DELAY);
			exit(L4_IpcFailed(tag) ? 1 : 0);
		}
	}

	/* send peer info to the first child. */
	L4_LoadMR(0, (L4_MsgTag_t){ .X.u = 1 }.raw);
	L4_LoadMR(1, child_tid[1].raw);
	L4_MsgTag_t tag = L4_Send_Timeout(child_tid[0], A_SHORT_NAP);
	fail_if(L4_IpcFailed(tag), "can't send to child[0]=%d; ec=%#lx",
		child[0], L4_ErrorCode());

	/* get results. */
	L4_MsgTag_t child_tag[2];
	L4_Word_t child_ec[2];
	for(int i=0; i < 2; i++) {
		L4_MsgTag_t tag = L4_Receive_Timeout(child_tid[i],
			L4_TimePeriod(L4_PeriodUs_NP(TEST_IPC_DELAY) * 3));
		fail_if(L4_IpcFailed(tag),
			"can't get result from child[%d]=%d; ec=%#lx",
			i, child[i], L4_ErrorCode());
		L4_StoreMR(1, &child_tag[i].raw);
		L4_StoreMR(2, &child_ec[i]);
	}

	ok1(L4_IpcFailed(child_tag[0]) && L4_IpcFailed(child_tag[1]));
	ok1(L4_Label(child_tag[0]) >> 4 != 0xb0a);
	ok1(L4_Label(child_tag[1]) >> 4 != 0xb0a);
	ok1(child_ec[0] == 0x2 && child_ec[1] == 0x2);

	/* clean up */
	for(int i=0; i < 2; i++) {
		int st, dead = wait(&st);
		if(dead != child[0] && dead != child[1]) {
			diag("unexpected dead=%d", dead);
		}
	}
}
END_TEST


/* a simple redirector fixture. captures and forwards all messages besides
 * get_msgs() and quit().
 *
 * TODO: add support for string transfers
 * TODO: add support for always doing active receive, i.e. by making this the
 * very lowest-priority thread and using discrete reply and wait
 */
static void redir_fixture_fn(void *parameter)
{
	if(L4_IsNilThread(redir_fixture_tid)) {
		L4_Sleep(A_SHORT_NAP);
	}

	assert(L4_SameThreads(redir_fixture_tid, L4_Myself()));

	struct rf_ctx *ctx = parameter;

	while(ctx->running) {
		L4_ThreadId_t sender;
		L4_MsgTag_t tag = L4_Wait(&sender);
		for(;;) {
			if(L4_IpcFailed(tag)) {
				printf("%s: ipc failed, ec=%#lx\n", __func__, L4_ErrorCode());
				break;
			}

			if(L4_IpcRedirected(tag)) {
				/* store and forward. */
				L4_Word_t mrs[63], n_mrs = tag.X.u + tag.X.t;
				L4_StoreMRs(1, n_mrs, mrs);
				L4_ThreadId_t ir = L4_IntendedReceiver();
				/* NOTE: this assert could be in a test instead, but that'd be
				 * meaningless as redirection never happens when the target is
				 * in the redirector's own space; therefore a local ID will
				 * never appear.
				 */
				assert(L4_IsGlobalId(ir));
				sender = L4_GlobalIdOf(sender);
#if 0
				if(L4_Label(tag) >> 4 != 0xffe) {
					printf("redir ipc from=%lu:%lu, label=%#lx, ir=%lu:%lu\n",
						L4_ThreadNo(sender), L4_Version(sender), L4_Label(tag),
						L4_ThreadNo(ir), L4_Version(ir));
				} else {
					L4_Word_t fip = mrs[1], faddr = mrs[0];
					printf("redir #pf from=%lu:%lu, ip=%#lx, addr=%#lx, ir=%lu:%lu\n",
						L4_ThreadNo(sender), L4_Version(sender), fip, faddr,
						L4_ThreadNo(ir), L4_Version(ir));
				}
#endif
				struct redir_fixture_msg_info *msg = talloc_size(ctx,
					sizeof(*msg));
				msg->tag = tag.raw;
				msg->sender = sender.raw;
				msg->ir = ir.raw;
				memcpy(msg->mr, mrs, MIN(size_t, sizeof(msg->mr),
					n_mrs * sizeof(L4_Word_t)));
				darray_push(ctx->msgs, msg);

				tag = (L4_MsgTag_t){
					.X.label = L4_Label(tag),
					.X.u = L4_UntypedWords(tag),
					.X.t = L4_TypedWords(tag),
				};
				L4_Set_Propagation(&tag);
				L4_Set_VirtualSender(sender);
				sender = ir;
				L4_LoadMR(0, tag.raw);
				L4_LoadMRs(1, n_mrs, mrs);

				/* FIXME: remove from here up to "break;" once the scheduling
				 * artifact test criterion has been fulfilled.
				 */
				tag = L4_Send_Timeout(ir, A_SHORT_NAP);
				if(L4_IpcFailed(tag)) {
					printf("%s: forward failed, ec=%#lx\n", __func__,
						L4_ErrorCode());
				}
				break;
			} else if(L4_Label(tag) == QUIT_LABEL) {
				ctx->running = false;
				break;
			} else if(L4_Label(tag) == GET_MSGS_LABEL) {
				/* RedirFixture::get_msgs */
				int n_send = MIN(int, 6, ctx->msgs.size);
				struct redir_fixture_msg_info buf[6];
				for(int i=0; i < n_send; i++) {
					memcpy(&buf[i], ctx->msgs.item[i], sizeof(buf[0]));
				}
				L4_StringItem_t r_si = L4_StringItem(
					sizeof(buf[0]) * n_send, buf);
				L4_LoadMR(0, (L4_MsgTag_t){ .X.u = 1, .X.t = 2 }.raw);
				L4_LoadMR(1, n_send);
				L4_LoadMRs(2, 2, r_si.raw);
				tag = L4_Reply(sender);
				if(L4_IpcFailed(tag)) {
					printf("%s: get_msgs reply failed, ec=%#lx\n", __func__,
						L4_ErrorCode());
				} else {
					int new_size = ctx->msgs.size - n_send;
					for(int i=0; i < n_send; i++) {
						talloc_free(ctx->msgs.item[i]);
					}
					memmove(ctx->msgs.item, &ctx->msgs.item[n_send],
						sizeof(*ctx->msgs.item) * new_size);
					darray_resize(ctx->msgs, new_size);
				}
				break;
			} else {
				printf("%s: unrecognized ipc tag=%#lx from %lu:%lu\n",
					__func__, tag.raw, L4_ThreadNo(sender),
					L4_Version(sender));
				break;
			}

			tag = L4_ReplyWait(sender, &sender);
			if(L4_IpcFailed(tag)) {
				printf("%s: inner ipc failed, ec=%#lx\n", __func__,
					L4_ErrorCode());
			}
		}
	}

	darray_free(ctx->msgs);
	talloc_free(parameter);
}


static int __redir_fixture_get_msgs_timeouts(
	L4_ThreadId_t service_tid,
	uint16_t *n_got,
	fixmsg_t *buf,
	unsigned *buflen_p,
	L4_Time_t send_timeout,
	L4_Time_t recv_timeout)
{
	L4_StringItem_t r_si = L4_StringItem(*buflen_p, buf);
	L4_LoadBR(0, L4_StringItemsAcceptor.raw);
	L4_LoadBRs(1, 2, r_si.raw);
	L4_LoadMR(0, (L4_MsgTag_t){ .X.label = GET_MSGS_LABEL }.raw);
	L4_MsgTag_t tag = L4_Call_Timeouts(service_tid, send_timeout,
		recv_timeout);
	if(L4_IpcFailed(tag)) return L4_ErrorCode();
	L4_Word_t t; L4_StoreMR(1, &t); *n_got = t;
	if(tag.X.t == 0) {
		*buflen_p = 0;
	} else {
		union {
			L4_Word_t w[63];
			L4_StringItem_t si;
		} X;
		L4_StoreMRs(1 + tag.X.u, tag.X.t, X.w);
		*buflen_p = stritemlen(&X.si);
	}

	return 0;
}


static void fixture_start(void)
{
	struct rf_ctx *ctx = talloc(NULL, struct rf_ctx);
	ctx->running = true;
	darray_init(ctx->msgs);
	redir_fixture_tid = xstart_thread(&redir_fixture_fn, ctx);
}


static void fixture_teardown(void)
{
	/* stop giving this ID to new forks from this process. doesn't actually
	 * matter unless the fixture is used in an unchecked context.
	 */
	L4_ThreadId_t prev_redir = set_fork_redir(L4_nilthread);
	if(L4_SameThreads(prev_redir, redir_fixture_tid)) {
		set_fork_redir(L4_anythread);
	}

	send_quit(redir_fixture_tid);
	xjoin_thread(redir_fixture_tid);
}


Suite *redir_suite(void)
{
	Suite *s = suite_create("redir");

	/* tests of the runtime (i.e. forkserv). */
	{
		TCase *tc = tcase_create("rt");
		tcase_add_checked_fixture(tc, &fixture_start, &fixture_teardown);
		tcase_add_test(tc, fork_redirect_basic);
		tcase_add_test(tc, fork_redirect_deep);
		suite_add_tcase(s, tc);
	}

	/* tests on deadlock behaviour under redirection. */
	{
		TCase *tc = tcase_create("deadlk");
		tcase_add_checked_fixture(tc, &fixture_start, &fixture_teardown);
		tcase_add_test(tc, no_deadlock);
		tcase_add_test(tc, mutual_send_deadlock);
		suite_add_tcase(s, tc);
	}

	return s;
}
