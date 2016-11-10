
/* the L4.X2 IPC state machine, the Ipc system call, untyped transfers, and
 * kernel-to-user IPC operation.
 */

#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <errno.h>
#include <ccan/likely/likely.h>
#include <ccan/htable/htable.h>

#include <l4/types.h>
#include <l4/message.h>
#include <l4/vregs.h>
#include <l4/schedule.h>
#include <l4/ipc.h>

#include <ukernel/config.h>
#include <ukernel/misc.h>
#include <ukernel/trace.h>
#include <ukernel/slab.h>
#include <ukernel/thread.h>
#include <ukernel/sched.h>
#include <ukernel/space.h>
#include <ukernel/bug.h>
#include <ukernel/kip.h>
#include <ukernel/interrupt.h>
#include <ukernel/ipc.h>


#define TRACE(fmt, ...) TRACE_MSG(TRID_IPC, fmt, ##__VA_ARGS__)
#define TRACE_REDIR(fmt, ...) TRACE_MSG(TRID_IPC_REDIR, fmt, ##__VA_ARGS__)


static size_t hash_ipc_wait(const void *tid, void *priv);
static size_t hash_pasv_from(const void *tid, void *priv);
static size_t hash_waited_redir(const void *thread, void *priv);


/* sendwait_hash is a multiset keyed by thread->u2.ipc_wait.dest_tid, i.e. the
 * recipient's gTID. active receive scans these to find a passive sender.
 *
 * recvwait_hash is much the same, but by a passive receiver's ipc_from, which
 * is a global TID. cancel_ipc_to() scans this to generate "invalid thread ID"
 * errors on deleting ThreadControl.
 *
 * redir_wait is similar for redirection waits.
 */
static struct htable sendwait_hash = HTABLE_INITIALIZER(
		sendwait_hash, &hash_ipc_wait, NULL),
	recvwait_hash = HTABLE_INITIALIZER(recvwait_hash, &hash_pasv_from, NULL),
	redir_wait = HTABLE_INITIALIZER(redir_wait, &hash_waited_redir, NULL);


static size_t hash_ipc_wait(const void *threadptr, void *priv) {
	const struct thread *t = threadptr;
	return int_hash(t->u2.ipc_wait.dest_tid.raw);
}


static size_t hash_pasv_from(const void *threadptr, void *priv) {
	const struct thread *t = threadptr;
	assert(thread_is_valid(t));
	assert(t->status == TS_RECV_WAIT);
	assert(L4_IsGlobalId(t->ipc_from));	/* implies !IsNilThread */
	assert(t->ipc_from.raw != L4_anythread.raw);
	return int_hash(t->ipc_from.raw);
}


static size_t hash_waited_redir(const void *threadptr, void *priv) {
	const struct thread *t = threadptr;
	assert(!L4_IsNilThread(t->u1.waited_redir));
	assert(t->space != kernel_space);
	return int_hash(t->u1.waited_redir.raw);
}


/* TODO: move into <ukernel/util.h> or some such */
static inline bool is_wildcard(L4_ThreadId_t tid) {
	return tid.raw == L4_anythread.raw
		|| tid.raw == L4_anylocalthread.raw;
}


/* whether @t should be in recvwait_hash or not, by its ->status and
 * ->ipc_from fields. this is repeated all over the place.
 */
static bool in_recv_wait(struct thread *t) {
	return t->status == TS_RECV_WAIT
		&& !L4_IsNilThread(t->ipc_from)
		&& !is_wildcard(t->ipc_from);
}


#ifndef NDEBUG
static bool verify_recv_wait(struct thread *t)
{
	size_t hash = int_hash(t->ipc_from.raw);
	struct htable_iter it;
	for(struct thread *cand = htable_firstval(&recvwait_hash, &it, hash);
		cand != NULL;
		cand = htable_nextval(&recvwait_hash, &it, hash))
	{
		if(cand == t) return true;
	}
	return false;
}
#endif


/* module invariants. these mostly concern thread states within hash
 * tables, but there are some others that're useful as well.
 */
#ifndef DEBUG_ME_HARDER
#define check_ipc_module() true
#else
#include <ukernel/invariant.h>

static bool check_ipc_module(void)
{
	INV_CTX;

	/* for all threads, check correct membership in recvwait_hash and
	 * sendwait_hash.
	 */
	struct ra_iter rai;
	for(struct thread *t = ra_first(thread_ra, &rai);
		t != NULL;
		t = ra_next(thread_ra, &rai))
	{
		inv_push("t=%lu:%lu (%p), ->ipc_from=%lu:%lu, ->ipc_to=%lu:%lu",
			TID_THREADNUM(t->id), TID_VERSION(t->id), t,
			L4_ThreadNo(t->ipc_from), L4_Version(t->ipc_from),
			L4_ThreadNo(t->ipc_to), L4_Version(t->ipc_to));

		/* string transfers that're waiting for either the other thread's
		 * pagefaults, or for the transfer to proceed via scheduling.
		 */
		inv_push("[TS_XFER checks] t->status=%s, ->ipc=%p",
			sched_status_str(t), t->ipc);
		inv_imply1(t->status == TS_XFER, t->ipc != NULL);
		if(t->status == TS_XFER) {
			inv_log("ipc->from->id=%lu:%lu, ->to->id=%lu:%lu",
				TID_THREADNUM(t->ipc->from->id), TID_VERSION(t->ipc->from->id),
				TID_THREADNUM(t->ipc->to->id), TID_VERSION(t->ipc->to->id));
		}
		inv_imply1(t->status == TS_XFER,
			t->ipc->from == t || t->ipc->to == t);
		inv_imply1(t->status == TS_XFER && t->ipc->from == t,
			t->ipc->to != t);
		inv_imply1(t->status == TS_XFER && t->ipc->to == t,
			t->ipc->from != t);
		inv_pop();

		/* check how many times @t occurs in sendwait_hash and
		 * recvwait_hash.
		 */
		struct htable_iter it;
		int sendwait_count = 0;
		for(struct thread *cand = htable_first(&sendwait_hash, &it);
			cand != NULL;
			cand = htable_next(&sendwait_hash, &it))
		{
			if(cand == t) sendwait_count++;
		}
		inv_log("t->status=%s, sendwait_count=%d",
			sched_status_str(t), sendwait_count);
		inv_imply1(
			t->status == TS_SEND_WAIT && !CHECK_FLAG(t->flags, TF_HALT),
			sendwait_count == 1);
		inv_imply1(
			t->status != TS_SEND_WAIT || CHECK_FLAG(t->flags, TF_HALT),
			sendwait_count == 0);

		int recvwait_count = 0;
		for(struct thread *cand = htable_first(&recvwait_hash, &it);
			cand != NULL;
			cand = htable_next(&recvwait_hash, &it))
		{
			if(cand == t) recvwait_count++;
		}
		inv_log("in_recv_wait(t)=%s, recvwait_count=%d",
			btos(in_recv_wait(t)), recvwait_count);
		inv_imply1(in_recv_wait(t), recvwait_count == 1);
		inv_imply1(!in_recv_wait(t), recvwait_count == 0);

		inv_pop();
	}

	/* recvwait_hash per member. simple enough. */
	struct htable_iter it;
	for(struct thread *t = htable_first(&recvwait_hash, &it);
		t != NULL;
		t = htable_next(&recvwait_hash, &it))
	{
		inv_push("recvwait_hash: t=%lu:%lu (%p), ->status=%s, ->ipc_from=%lu:%lu",
			TID_THREADNUM(t->id), TID_VERSION(t->id), t, sched_status_str(t),
			L4_ThreadNo(t->ipc_from), L4_Version(t->ipc_from));

		inv_ok1(thread_is_valid(t));

		inv_ok1(t->status == TS_RECV_WAIT);	/* no R_RECV plz. */
		inv_ok1(!L4_IsNilThread(t->ipc_from));
		inv_ok1(L4_IsGlobalId(t->ipc_from));
		inv_ok1(resolve_tid_spec(t->space, t->ipc_from) != NULL);

		inv_ok1(in_recv_wait(t));		/* duh */
		inv_ok1(verify_recv_wait(t));	/* implied */

		inv_pop();
	}

	for(struct thread *t = htable_first(&sendwait_hash, &it);
		t != NULL;
		t = htable_next(&sendwait_hash, &it))
	{
		inv_push("sendwait_hash: t=%lu:%lu (%p), ->status=%s, ->ipc_from=%lu:%lu",
			TID_THREADNUM(t->id), TID_VERSION(t->id), t, sched_status_str(t),
			L4_ThreadNo(t->ipc_from), L4_Version(t->ipc_from));

		inv_ok1(thread_is_valid(t));

		/* TODO: other sendwait things */

		inv_pop();
	}

	for(struct thread *t = htable_first(&redir_wait, &it);
		t != NULL;
		t = htable_next(&redir_wait, &it))
	{
		inv_push("redir_wait: t=%lu:%lu (%p), ->status=%s, ->ipc_from=%lu:%lu",
			TID_THREADNUM(t->id), TID_VERSION(t->id), t, sched_status_str(t),
			L4_ThreadNo(t->ipc_from), L4_Version(t->ipc_from));

		inv_ok1(thread_is_valid(t));

		/* TODO: other redir things */

		inv_pop();
	}

	return true;

inv_fail:
	return false;
}

#endif


static inline void set_ipc_error(void *utcb, L4_Word_t ec)
{
	L4_VREG(utcb, L4_TCR_ERRORCODE) = ec;
	L4_VREG(utcb, L4_TCR_MR(0)) = ((L4_MsgTag_t){ .X.flags = 0x8 }).raw;
	L4_VREG(utcb, L4_TCR_MR(1)) = 0;
	L4_VREG(utcb, L4_TCR_MR(2)) = 0;
}


inline void set_ipc_return_regs(
	struct x86_regs *regs,
	struct thread *current,
	void *utcb)
{
	regs->eax = current->ipc_from.raw;
	regs->esi = L4_VREG(utcb, L4_TCR_MR(0));
	regs->ebx = L4_VREG(utcb, L4_TCR_MR(1));
	regs->ebp = L4_VREG(utcb, L4_TCR_MR(2));
}


static inline void set_ipc_return_thread(struct thread *t, void *utcb)
{
	set_ipc_return_regs(&t->ctx.r, t, utcb);
}


/* exported for sched.c */
void set_ipc_error_thread(struct thread *t, L4_Word_t ec)
{
	void *utcb = thread_get_utcb(t);
	set_ipc_error(utcb, ec);
	set_ipc_return_thread(t, utcb);
}


/* this is like resolve_tid_spec(), but in reverse. */
static L4_ThreadId_t tid_return(struct thread *self, struct thread *t)
{
	if(self->space == t->space) {
		return get_local_id(t);
	} else {
		return (L4_ThreadId_t){ .raw = t->id };
	}
}


void remove_send_wait(struct thread *t) {
	htable_del(&sendwait_hash, hash_ipc_wait(t, NULL), t);
}


bool insert_send_wait(struct thread *t) {
	return htable_add(&sendwait_hash, hash_ipc_wait(t, NULL), t);
}


static void remove_recv_wait(struct thread *t)
{
	assert(in_recv_wait(t));
	assert(verify_recv_wait(t));
	htable_del(&recvwait_hash, hash_pasv_from(t, NULL), t);
}


static bool insert_recv_wait(struct thread *t)
{
	assert(L4_IsGlobalId(t->ipc_from));		/* implies !IsNilThread */
	assert(!is_wildcard(t->ipc_from));
	return htable_add(&recvwait_hash, hash_pasv_from(t, NULL), t);
}


void remove_redir_wait(struct thread *t)
{
	if(likely(!L4_IsNilThread(t->u1.waited_redir))) {
		htable_del(&redir_wait, hash_waited_redir(t, NULL), t);
	}
}


void ipc_xfer_timeout(struct ipc_state *st)
{
	assert(st->from->ipc == st);
	assert(CHECK_FLAG(st->from->flags, TF_SENDER));
	assert(st->to->ipc == st);

	TRACE("%s: st->from{%lu:%lu} -> st->to{%lu:%lu}\n", __func__,
		TID_THREADNUM(st->from->id), TID_VERSION(st->from->id),
		TID_THREADNUM(st->to->id), TID_VERSION(st->to->id));

	/* must determine which peer's fault caused the timeout. if they were both
	 * waiting, we'll blame the sender.
	 */
	bool f_in_src;
	if(st->str_off < 0) {
		/* pre-xfer mode. decided by whether the sender has unserviced faults,
		 * or not. this isn't quite even, but regardless if the sender's pager
		 * services them quicker than the receiver's, then the error code
		 * indicates the receiver.
		 */
		struct fault_peer *f_src = &st->xfer.fault[0];
		assert(f_src->num > 0 || st->xfer.fault[1].num > 0);
		f_in_src = f_src->num > 0;
	} else {
		/* in-transfer mode. decided by whether the sender is waiting for the
		 * receiver's fault handling.
		 */
		f_in_src = (st->from->status != TS_XFER);
	}

	const L4_Word_t ec_offs = st->tot_offset << 4;
	set_ipc_error_thread(st->from, ec_offs | (f_in_src ? 5 : 6) << 1);
	set_ipc_error_thread(st->to, ec_offs | (!f_in_src ? 5 : 6) << 1 | 1);

	struct thread *from = st->from, *to = st->to;
	/* (note the use of bitwise rather than short-circuit or. this is
	 * intended.)
	 */
	bool dead_ipc = post_exn_fail(from) | post_exn_fail(to);
	if(!dead_ipc) {
		/* can happen when both were waiting for transfer start due to
		 * scheduling. otherwise, free() is called from prexfer_ipc_hook().
		 */
		st->from->ipc = NULL;
		st->to->ipc = NULL;
		free(st);
	}

	from->flags &= ~TF_SENDER;
	thread_ipc_fail(from);
	thread_ipc_fail(to);
}


/* this function is called from the scheduler when it finds that both sides of
 * a paused string transfer are ready to proceed. it should write both sides'
 * IPC return registers.
 */
bool ipc_resume(struct thread *t)
{
	assert(check_ipc_module());
	assert(t->status == TS_XFER);
	assert(t->ipc != NULL);
	struct ipc_state *st = t->ipc;
	struct thread *dest = st->to, *source = st->from;

	TRACE("%s: called on %lu:%lu -> %lu:%lu\n",
		__func__, TID_THREADNUM(source->id), TID_VERSION(source->id),
		TID_THREADNUM(dest->id), TID_VERSION(dest->id));

	void *s_utcb = thread_get_utcb(st->from),
		*d_utcb = thread_get_utcb(st->to);
	assert(source->ipc == st);
	assert(dest->ipc == st);

	/* resume mode. */
	L4_MsgTag_t tag = { .raw = L4_VREG(s_utcb, L4_TCR_MR(0)) };
	int n = do_typed_transfer(source, s_utcb, dest, d_utcb, tag);
	if(n < 0) {
		assert(n == -EFAULT);
		assert(check_ipc_module());
		return false;
	}
	assert(dest->ipc == NULL);
	assert(source->ipc == NULL);
	assert(!CHECK_FLAG(source->flags | dest->flags, TF_SENDER));

	if(n == 0) {
		/* success. */
		dest->ipc_from.raw = source->id;
	} else {
		/* overflows and xfer timeouts. */
		assert(n > 0);
		assert(((n >> 1) & 0xf) > 3);
		/* TODO: translate xfer timeout by partner/caller! */
		set_ipc_error(s_utcb, n & ~1u);
		set_ipc_error(d_utcb, n | 1);
		source->ipc_from = L4_nilthread;
	}

	if(L4_IsNilThread(source->ipc_from)) {
		TRACE("%s: source returns to userspace\n", __func__);
		source->status = TS_READY;
		source->wakeup_time = 0;
		set_ipc_return_thread(source, s_utcb);
	} else {
		source->status = TS_R_RECV;
		source->wakeup_time = wakeup_at(L4_Never);	/* FIXME: IPC timeout */
		TRACE("%s: source receives from %lu:%lu\n", __func__,
			L4_ThreadNo(source->ipc_from), L4_Version(source->ipc_from));
	}

	/* dest always wakes up. */
	dest->status = TS_READY;
	dest->wakeup_time = 0;
	set_ipc_return_thread(dest, d_utcb);

	sq_update_thread(source);
	sq_update_thread(dest);

	assert(check_ipc_module());
	return true;
}


struct thread *ipc_partner(struct thread *t)
{
	assert(t->status == TS_XFER || IS_IPC_WAIT(t->status));
	assert(t->ipc != NULL);
	assert(!CHECK_FLAG(t->flags, TF_SENDER) || t == t->ipc->from);
	assert(CHECK_FLAG(t->flags, TF_SENDER) || t == t->ipc->to);
	assert(check_ipc_module());

	struct thread *partner = CHECK_FLAG(t->flags, TF_SENDER) ? t->ipc->to : t->ipc->from;
	assert(IS_IPC(partner->status));
	assert(partner->ipc == t->ipc);
	return partner;
}


/* returns 0 on success, ErrorCode on error signal, or -EFAULT on ongoing
 * typed transfer.
 */
static int do_ipc_transfer(
	struct thread *source, void *s_utcb,
	struct thread *dest, void *d_utcb)
{
	L4_MsgTag_t tag = { .raw = L4_VREG(s_utcb, L4_TCR_MR(0)) };
	if(hook_empty(&dest->post_exn_call)) {
		L4_VREG(d_utcb, L4_TCR_MR(0)) = tag.raw;
		for(int i=0; i < tag.X.u; i++) {
			int reg = L4_TCR_MR(i + 1);
			L4_VREG(d_utcb, reg) = L4_VREG(s_utcb, reg);
		}
	}

	if(tag.X.t == 0) return 0;
	else {
		return do_typed_transfer(source, s_utcb, dest, d_utcb, tag);
	}
}


/* used by the deleting and overwriting modes of ThreadControl.
 *
 * TODO: this should signal preemption when it occurs: one of the aborted
 * would-be peers may have priority.
 */
void cancel_ipc_to(L4_ThreadId_t with_tid, L4_Word_t errcode)
{
	assert(L4_IsGlobalId(with_tid));
	/* invariant check only at bottom; this function fixes things up. */

	/* fail passive senders to @with_tid. */
	struct htable_iter it;
	size_t hash = int_hash(with_tid.raw);
	errcode &= ~(L4_Word_t)1;		/* send-phase errors. */
	for(struct thread *peer = htable_firstval(&sendwait_hash, &it, hash);
		peer != NULL;
		peer = htable_nextval(&sendwait_hash, &it, hash))
	{
		if(peer->u2.ipc_wait.dest_tid.raw != with_tid.raw) continue;

		assert(!L4_IsGlobalId(peer->ipc_to)
			|| L4_ThreadNo(peer->ipc_to) == L4_ThreadNo(with_tid));
		assert(!L4_IsLocalId(peer->ipc_to)
			|| resolve_tid_spec(peer->space, peer->ipc_to) == NULL
			|| TID_THREADNUM(resolve_tid_spec(peer->space, peer->ipc_to)->id)
				== L4_ThreadNo(with_tid));
		assert(peer->status == TS_SEND_WAIT || peer->status == TS_XFER
			|| peer->status == TS_STOPPED);
		TRACE("%s: cancelling sendwait peer=%lu:%lu\n", __func__,
			TID_THREADNUM(peer->id), TID_VERSION(peer->id));
		if(!post_exn_fail(peer)) {
			set_ipc_error_thread(peer, errcode);
			thread_wake(peer);
		}

		htable_delval(&sendwait_hash, &it);
	}

	/* fail passive receivers from @with_tid. */
	errcode |= 1;		/* receive-phase errors. */
	for(struct thread *peer = htable_firstval(&recvwait_hash, &it, hash);
		peer != NULL;
		peer = htable_nextval(&recvwait_hash, &it, hash))
	{
		if(peer->ipc_from.raw != with_tid.raw) continue;
		TRACE("%s: cancelling recvwait peer=%lu:%lu\n", __func__,
			TID_THREADNUM(peer->id), TID_VERSION(peer->id));
		if(!post_exn_fail(peer)) {
			set_ipc_error_thread(peer, errcode);
			thread_wake(peer);
		}

		htable_delval(&recvwait_hash, &it);
	}

	assert(check_ipc_module());
}


/* TODO: this function scales poorly: it does a brute-force loop over all the
 * passive sends in the system, not just those for the IPC peer.
 */
static void rewrite_passive_vs_from(struct thread *t)
{
	L4_ThreadId_t ltid = L4_nilthread;
	/* inactive threads may be propagated on behalf of. */
	if(likely(t->utcb_pos >= 0)) ltid = get_local_id(t);

	struct htable_iter it;
	for(struct thread *from = htable_first(&sendwait_hash, &it);
		from != NULL;
		from = htable_next(&sendwait_hash, &it))
	{
		if(from->u2.ipc_wait.send_tid.raw != t->id
			&& from->u2.ipc_wait.send_tid.raw != ltid.raw)
		{
			continue;
		}

		struct thread *dest = thread_get_fast(from->u2.ipc_wait.dest_tid);
		from->u2.ipc_wait.send_tid = tid_return(dest, from);
		L4_MsgTag_t *tag = (void *)&L4_VREG(
			thread_get_utcb(from), L4_TCR_MR(0));
		tag->X.flags &= ~0x1;
	}
}


/* called from thread_ipc_fail() and from the deleting/modifying
 * ThreadControl. takes care of the {send,recv}wait_hash entries and disables
 * passive propagated sends' propagation. leaves errorcode setting to caller's
 * caller.
 */
void cancel_ipc_from(struct thread *t)
{
	assert(verify_recv_wait(t) || !in_recv_wait(t));
	if(t->status == TS_SEND_WAIT) {
		remove_send_wait(t);
		sq_remove_thread(t);
		t->status = TS_READY;
		sq_insert_thread(t);
	} else if(t->status == TS_RECV_WAIT || t->status == TS_R_RECV) {
		struct thread *s = resolve_tid_spec(t->space, t->ipc_from);
		if(s != NULL && s->id == t->ipc_from.raw && s->u0.partner == t) {
			/* TODO: coördinate partnership breaking using a function of some
			 * kind, such as one that undoes scheduling effects.
			 */
			s->u0.partner = NULL;
		}
		if(in_recv_wait(t)) {
			remove_recv_wait(t);
			t->status = 0x42;	/* a dummy */
			assert(!in_recv_wait(t));
		}
	}
	rewrite_passive_vs_from(t);
	assert(check_ipc_module());
}


static inline bool is_interrupt(L4_ThreadId_t tid) {
	return L4_Version(tid) == 1
		&& L4_ThreadNo(tid) <= last_int_threadno();
}


static bool active_send_match(struct thread *sender, struct thread *dest)
{
	return dest->ipc_from.raw == L4_anythread.raw
		|| dest->ipc_from.raw == sender->id
		|| (dest->space == sender->space
			&& (dest->ipc_from.raw == L4_anylocalthread.raw
				|| dest->ipc_from.raw == get_local_id(sender).raw));
}


/* whether "tip" ends up redirecting for "base". */
static bool has_redir_chain(struct thread *base, struct thread *tip)
{
	assert(tip != NULL);
	do {
		if(!CHECK_FLAG(base->space->flags, SF_REDIRECT)) return false;
		struct thread *r = base->space->redirector;
		if(r == tip) return true;
		base = r;
	} while(base != NULL);
	return false;
}


/* whether @t's redirector chain is ready. this covers the whole chain.
 * returns the non-ready one in *holdup_p, or nilthread if there was an
 * invalid redirector along the chain.
 */
static bool is_redir_ready(L4_ThreadId_t *holdup_p, struct thread *t)
{
	assert(holdup_p != NULL);

	struct thread *prev;
	do {
		if(!CHECK_FLAG(t->space->flags, SF_REDIRECT)) return true;
		if(unlikely(t->space->redirector == NULL)) {
			/* invalid redirector. happens when the redirecting thread's ID
			 * has been deleted or its version bits overwritten, and a new
			 * redirector hasn't been set.
			 */
			*holdup_p = L4_nilthread;
			return false;
		}
		prev = t;
		t = t->space->redirector;
	} while((t->status == TS_R_RECV || t->status == TS_RECV_WAIT)
		&& active_send_match(prev, t));

	holdup_p->raw = t->id;
	return false;
}


/* returns true iff @t's IPC to @dst will be redirected. */
static inline bool will_redirect(struct thread *t, struct thread *dst)
{
	return CHECK_FLAG(t->space->flags, SF_REDIRECT)
		&& t->space != dst->space
		&& (unlikely(t->space->redirector == NULL)
			|| t->space->redirector->space != dst->space);
}


/* returns true for instant success, and false for error condition, or IPC in
 * progress (sleep, string transfer fault). afterward *@dest_p will point to
 * the actual destination, i.e. a redirector if that applied.
 *
 * precond: @self->status != TS_STOPPED && !CHECK_FLAG(@self->flags, TF_HALT)
 * postcond: !@retval -> @self->status \in {SEND_WAIT, XFER, READY, STOPPED}
 */
static bool ipc_send_half(
	struct thread *self,
	void *self_utcb,
	struct thread **dest_p)
{
	/* must look this alive to attempt active send */
	assert(!CHECK_FLAG(self->flags, TF_HALT));
	assert(self->status != TS_STOPPED);

	/* NOTE: this assert can blow under some curious timing circumstances.
	 * those are provoked by DEBUG_ME_HARDER, i.e. the super nasty invariant
	 * checks in mapdb.c .
	 */
	assert(!L4_IsNilThread(self->ipc_to));
	assert(!is_wildcard(self->ipc_to));

	int err_code = 0;
	L4_MsgTag_t *tag = (void *)&L4_VREG(self_utcb, L4_TCR_MR(0));
	tag->X.flags &= 0x1;	/* keep the propagate flag */

	assert(dest_p != NULL);
	if(*dest_p == NULL) {
		assert(CHECK_FLAG(self->flags, TF_INTR));
		assert(is_interrupt(self->ipc_to));
		assert(!CHECK_FLAG(self->space->flags, SF_REDIRECT));

		/* eat an interrupt reply. */
		err_code = int_clear(L4_ThreadNo(self->ipc_to), self);
		if(err_code == 0) return true; else goto error;
	}
	struct thread *dest = *dest_p;

	/* get matching variables, incl. propagation */
	L4_ThreadId_t self_id = { .raw = self->id }, self_lid = get_local_id(self);
	bool propagated = false;
	struct thread *vs = NULL, *sender = self;
	if(CHECK_FLAG(tag->X.flags, 0x1)) {
		/* propagation (sender fakery). */
		vs = get_tcr_thread(self, self_utcb, L4_TCR_VA_SENDER);
		/* FIXME: also check interrupt propagation */
		if(vs != NULL && (self->space == vs->space
			|| self->space == dest->space
			|| has_redir_chain(vs, self)))
		{
			/* redirect a closed IPC filter when applicable */
			if((vs->status == TS_R_RECV || vs->status == TS_RECV_WAIT)
				&& (vs->ipc_from.raw == self_id.raw
					|| (vs->ipc_from.raw == self_lid.raw
						&& vs->space == self->space)))
			{
				bool twiddle = in_recv_wait(vs);
				if(twiddle) remove_recv_wait(vs);
				vs->ipc_from.raw = dest->id;
				if(vs->u0.partner == self) vs->u0.partner = dest;
				if(twiddle) insert_recv_wait(vs);
			}

			sender = vs;
			self_id.raw = vs->id;
			self_lid = get_local_id(vs);
			propagated = true;
		} else {
			tag->X.flags &= ~0x1;		/* no propagation for you. */
		}
	}
	assert(!propagated || vs != NULL);

	/* NOTE: due to the way propagation can alter self_lid, this condition
	 * can't be replaced with active_send_match().
	 */
	const bool match_cond = dest->ipc_from.raw == L4_anythread.raw
		|| dest->ipc_from.raw == self_id.raw
		|| (dest->space == sender->space
			&& (dest->ipc_from.raw == L4_anylocalthread.raw
				|| dest->ipc_from.raw == self_lid.raw));
	uint64_t now_us = ksystemclock();

	/* override TS_R_RECV? */
	int status = dest->status;
	bool status_cond;
	if(match_cond && status == TS_R_RECV
		&& !CHECK_FLAG(dest->flags, TF_HALT))
	{
		/* FIXME: this peer-timeouting thing shouldn't be here. such timeouts
		 * should be handled by the scheduler; this code should just fail to
		 * send to a thread under its receive timeout.
		 *
		 * and that test could bear to be much higher up. it would also call
		 * passive_send() explicitly.
		 */
		if(now_us >= dest->wakeup_time) {
			/* nah, time the peer out instead. */
			dest->status = TS_RECV_WAIT;	/* required by thread_wake() */
			set_ipc_error_thread(dest, (1 << 1) | 1);
			thread_wake(dest);
			status = dest->status;		/* reload after thread_wake() */
			TRACE("%s: r_recv override timeout\n", __func__);
			status_cond = false;
		} else {
			/* yep */
			TRACE("%s: override r_recv\n", __func__);
			status = TS_RECV_WAIT;
			status_cond = true;
		}
	} else {
		status_cond = (status == TS_RECV_WAIT);
	}

	if(status_cond
		&& match_cond
		&& !CHECK_FLAG(dest->flags, TF_HALT)
		&& (dest->wakeup_time == ~(uint64_t)0u
			|| dest->wakeup_time > now_us))
	{
		/* active send */
		TRACE("%s: active send to %lu:%lu (from %lu:%lu, actual %lu:%lu)\n", __func__,
			TID_THREADNUM(dest->id), TID_VERSION(dest->id),
			TID_THREADNUM(self_id.raw), TID_VERSION(self_id.raw),
			TID_THREADNUM(self->id), TID_VERSION(self->id));

		/* check and apply redirection. */
		bool redirected = false;
		struct thread *saved_dest = NULL;
		if(self->space != dest->space
			&& CHECK_FLAG(self->space->flags, SF_REDIRECT))
		{
			assert(!CHECK_FLAG(self->flags, TF_REDIR_WAIT));
			if(!is_redir_ready(&self->u1.waited_redir, self)) {
				/* a redirector in the chain was either invalid, or not ready
				 * to receive. hold this IPC until ready.
				 */
				TRACE_REDIR("IPC: send-side held by %lu:%lu\n",
					L4_ThreadNo(self->u1.waited_redir),
					L4_Version(self->u1.waited_redir));
				if(self->send_timeout.raw == L4_ZeroTime.raw) {
					TRACE_REDIR("IPC: immediate send timeout on redir wait\n");
					goto send_timeout;
				}
				self->flags |= TF_REDIR_WAIT;
				self->status = TS_SEND_WAIT;
				thread_sleep(self, self->send_timeout);
				if(!L4_IsNilThread(self->u1.waited_redir)) {
					/* TODO: handle or conceal OOM */
					htable_add(&redir_wait,
						hash_waited_redir(self, NULL), self);
				}
				return false;
			}

			struct thread *red = self->space->redirector;
			assert(red != NULL);	/* ensured by is_redir_ready() */
			if(dest->space != red->space) {
				TRACE_REDIR("IPC: redirecting from=%lu:%lu, to=%lu:%lu -> red=%lu:%lu\n",
					TID_THREADNUM(self->id), TID_VERSION(self->id),
					TID_THREADNUM(dest->id), TID_VERSION(dest->id),
					TID_THREADNUM(red->id), TID_VERSION(red->id));
				tag->X.flags |= 0x2;		/* set redirect bit */
				redirected = true;
				saved_dest = dest;
				*dest_p = dest = red;
				/* redirect a closed IPC's receive phase. this'll be
				 * re-redirected to saved_dest if the redirector passes the
				 * IPC as-is, or replied to if the redirector returns a
				 * rejection.
				 *
				 * NOTE: this is not explicitly specified by L4.X2, but the
				 * wording also doesn't forbid it; and this seems like the
				 * most reasonable approach in any case. (it also fits the
				 * "redirector chain" pattern; if not, those'd only work for
				 * chaining pass/don't policies together -- and that's
				 * bizarre.)
				 *
				 * rerererere.
				 */
				if(self->ipc_from.raw == saved_dest->id) {
					self->ipc_from.raw = red->id;
					TRACE_REDIR("IPC: closed wait on %lu:%lu changed to %lu:%lu\n",
						TID_THREADNUM(saved_dest->id), TID_VERSION(saved_dest->id),
						TID_THREADNUM(red->id), TID_VERSION(red->id));
				}
			}
		}
		assert(!redirected || saved_dest != NULL);

		if(in_recv_wait(dest)) {
			remove_recv_wait(dest);
			dest->status = TS_R_RECV;	/* a more curiouser dummy */
			assert(!in_recv_wait(dest));
		}

		void *dest_utcb = thread_get_utcb(dest);
		int n = do_ipc_transfer(self, self_utcb, dest, dest_utcb);
		if(n > 0) {
			TRACE("%s: do_ipc_transfer failed, n=%d\n", __func__, n);
			const L4_Word_t error = n;
			int code = (error & 0xe) >> 1;
			if(code >= 4) {
				/* mutual error; signal to partner also. */
				if(likely(!post_exn_fail(dest))) {
					set_ipc_error_thread(dest, error | 1);
				}
				thread_wake(dest);
			}
			set_ipc_error(self_utcb, error & ~1ul);
			assert(self->status == TS_RUNNING);
			self->status = TS_READY;
			return false;
		} else if(n < 0) {
			assert(n == -EFAULT);
			/* (may be in send_wait, to pager; recv_wait and r_recv, from
			 * pager; and xfer, waiting for partner's pager thing.)
			 */
			assert(IS_IPC(self->status));
			return false;
		}

		/* wake the receiver up, joining with the overridden status. this
		 * satisfies thread_wake()'s precondition both in ordinary and
		 * kernel-originated IPC.
		 */
		assert(dest->status == TS_RECV_WAIT || dest->status == TS_R_RECV);
		dest->status = status;
		if(!post_exn_ok(dest, self)) {
			/* receiver was in Ipc system call. set return values & wake it up
			 * from Ipc.
			 */
			if(propagated) {
				assert(L4_IpcPropagated(*tag));
				L4_VREG(dest_utcb, L4_TCR_VA_SENDER) = tid_return(dest,
					self).raw;
			}
			if(redirected) {
				assert(L4_IpcRedirected(*tag));
				L4_VREG(dest_utcb, L4_TCR_INTENDEDRECEIVER) = tid_return(
					dest, saved_dest).raw;
			}
			dest->ipc_from = tid_return(dest, sender);
			set_ipc_return_thread(dest, dest_utcb);
			thread_wake(dest);
		} else {
			assert(dest->ipc != NULL
				|| IS_READY(dest->status)
				|| (CHECK_FLAG(dest->flags, TF_HALT)
					&& dest->status == TS_STOPPED));
			/* timeout should've happened in post_exn_ok() already */
			assert(dest->ipc == NULL
				|| dest->ipc->xferto_at == 0
				|| dest->ipc->xferto_at > ksystemclock());
		}

		return true;
	} else if(self->send_timeout.raw != L4_ZeroTime.raw) {
		/* passive send */
		/* FIXME: check return value from insert_send_wait(). (really, instead
		 * of sendwait_hash use a linked list in the destination thread,
		 * through ipc_wait. there's already one mechanism that turns thread
		 * IDs into hashtable entries.)
		 */
		TRACE("%s: passive send to %lu:%lu (from %lu:%lu, actual %lu:%lu)\n",
			__func__,
			TID_THREADNUM(dest->id), TID_VERSION(dest->id),
			TID_THREADNUM(self_id.raw), TID_VERSION(self_id.raw),
			TID_THREADNUM(self->id), TID_VERSION(self->id));

		self->u2.ipc_wait.dest_tid.raw = dest->id;
		self->u2.ipc_wait.send_tid = tid_return(dest, propagated ? vs : self);
		insert_send_wait(self);
		assert(!L4_IsGlobalId(self->ipc_to)
			|| L4_ThreadNo(self->u2.ipc_wait.dest_tid) == L4_ThreadNo(self->ipc_to));

		self->status = TS_SEND_WAIT;
		thread_sleep(self, self->send_timeout);

		return false;
	} else {
send_timeout:
		/* instant timeout. */
		if(hook_empty(&self->post_exn_call)) {
			set_ipc_error(self_utcb, (1 << 1) | 0);
			/* TODO: is this required? instant return would indicate "no". */
			set_ipc_return_thread(self, self_utcb);
			self->status = TS_READY;
		}
		return false;
	}
	assert(false);

error:
	set_ipc_error(self_utcb, (err_code << 1) | 0);
	self->status = TS_READY;
	return false;
}


bool redo_ipc_send_half(struct thread *t)
{
	assert(t->status == TS_SEND_WAIT);
	assert(!CHECK_FLAG(t->flags, TF_HALT));
	assert(!CHECK_FLAG(t->flags, TF_REDIR_WAIT));
	assert(check_ipc_module());

	/* drop a previous ipc_wait & cancel propagation chaining. */
	remove_send_wait(t);
	rewrite_passive_vs_from(t);

	if(t->wakeup_time == ~(uint64_t)0) t->send_timeout = L4_Never;
	else {
		L4_Clock_t base = { .raw = ksystemclock() };
		/* it's OK to use a timeperiod here because the passive send will
		 * resolve immediately anyway.
		 */
		t->send_timeout = base.raw < t->wakeup_time
			? L4_TimePeriod(t->wakeup_time - base.raw)
			: L4_ZeroTime;
	}
	thread_wake(t);

	void *utcb = thread_get_utcb(t);
	struct thread *dest = resolve_tid_spec(t->space, t->ipc_to);
	if(unlikely(dest == NULL)) {
		set_ipc_error(utcb, 4);	/* no partner, send phase */
		might_preempt(t);
		assert(check_ipc_module());
		return true;
	}

	struct thread *target = dest;
	bool done = ipc_send_half(t, utcb, &target);
	if(done && t->status == TS_READY && !L4_IsNilThread(t->ipc_from)) {
		sq_remove_thread(t);
		t->wakeup_time = wakeup_at(t->recv_timeout);
		t->status = TS_R_RECV;
		sq_insert_thread(t);
	} else if(done && !IS_IPC(t->status)) {
		might_preempt(t);
	}

	assert(check_ipc_module());
	return done;
}


/* fast-path kernel-to-user IPC, first half. uses a slow path for passive
 * send, propagation, and non-instant redirection.
 */
void *ipc_user(
	L4_MsgTag_t tag,
	struct thread *from, void *from_utcb,
	struct thread **to_p,
	int n_regs)
{
	assert(check_ipc_module());

	/* this must be a global ID so that cancel_ipc_to()'s receiver search
	 * will find it.
	 */
	from->ipc_to.raw = (*to_p)->id;
	from->ipc_from.raw = (*to_p)->id;
	from->send_timeout = L4_Never;
	from->recv_timeout = L4_Never;

	struct thread *dest = *to_p;

	/* basic fastpath filter. */
	if((dest->status != TS_R_RECV && dest->status != TS_RECV_WAIT)
		|| !active_send_match(from, dest)
		|| (dest->wakeup_time != ~(uint64_t)0
			&& ksystemclock() >= dest->wakeup_time)
		|| unlikely(tag.X.flags != 0 || tag.X.t != 0)
		|| unlikely(!hook_empty(&dest->post_exn_call)))
	{
		/* any flags enabled (weird for an in-kernel message so not handled
		 * here), typed transfers specified, recipient's ipc_from doesn't
		 * match @from, recipient's state doesn't permit active send, or
		 * recipient's receive phase has already timed out.
		 *
		 * or a more esoteric reason that still needs accounting for.
		 */
		TRACE("%s: first filter hit\n", __func__);
		goto slow;
	}

	/* redirection. */
	struct thread *saved_dest = NULL;
	if(CHECK_FLAG(from->space->flags, SF_REDIRECT)
		&& from->space != dest->space)
	{
		L4_ThreadId_t dummy;
		if(!is_redir_ready(&dummy, from)) {
			TRACE("%s: second filter hit\n", __func__);
			goto slow;
		}
		struct thread *red = from->space->redirector;
		assert(red != NULL);	/* ensured by is_redir_ready() */
		if(dest->space != red->space) {
			tag.X.flags |= 0x2;
			saved_dest = dest;
			*to_p = dest = red;
			from->ipc_from.raw = red->id;
		}
	}

	if(in_recv_wait(dest)) {
		assert(verify_recv_wait(dest));
		remove_recv_wait(dest);
	}
	assert(!verify_recv_wait(dest));
	dest->status = 0x42;	/* a dummy */
	assert(!in_recv_wait(dest));

	void *dst_utcb = thread_get_utcb(dest);
	L4_VREG(dst_utcb, L4_TCR_MR(0)) = tag.raw;
	if(L4_IpcRedirected(tag)) {
		assert(saved_dest != NULL);
		L4_VREG(dst_utcb, L4_TCR_INTENDEDRECEIVER) = tid_return(
			dest, saved_dest).raw;
	}
	dest->ipc_from = tid_return(dest, from);
	TRACE("%s: did fastpath setup; from=%lu:%lu, dest=%lu:%lu\n", __func__,
		TID_THREADNUM(from->id), TID_VERSION(from->id),
		TID_THREADNUM(dest->id), TID_VERSION(dest->id));
	TRACE("%s: ... dest->status=%s\n", __func__, sched_status_str(dest));
	assert(check_ipc_module());
	return dst_utcb;

slow:
	save_ipc_regs(from, from_utcb, n_regs);
	L4_VREG(from_utcb, L4_TCR_MR(0)) = tag.raw;
	assert(check_ipc_module());
	return from_utcb;
}


/* fast-path second half when not exiting kernel. */
bool ipc_user_complete(
	struct thread *from,
	void *msg_utcb,
	struct thread **to_p)
{
	assert(check_ipc_module());

	void *from_utcb = thread_get_utcb(from);
	if(msg_utcb != from_utcb) {
		/* wheeee */
		assert(msg_utcb == thread_get_utcb(*to_p));
		set_ipc_return_thread(*to_p, msg_utcb);
		assert(hook_empty(&(*to_p)->post_exn_call));
		assert((*to_p)->status == 0x42);	/* check the dummy. */
		(*to_p)->status = TS_RECV_WAIT;
		thread_wake(*to_p);
		goto fast;
	}

	assert(from->ipc_to.raw == (*to_p)->id);
	assert(from->ipc_from.raw == (*to_p)->id);
	assert(from->send_timeout.raw == L4_Never.raw);
	assert(from->recv_timeout.raw == L4_Never.raw);

	if(!ipc_send_half(from, from_utcb, to_p)) {
		assert(check_ipc_module());
		return false;
	} else {
		/* this needs R_RECV because of the possibility of a propagated
		 * passive send, which should succeed at that point, despite being
		 * quite weird a case indeed.
		 */
fast:
		from->status = TS_R_RECV;
		from->wakeup_time = ~0ull;
		sq_update_thread(from);
		assert(check_ipc_module());
		return true;
	}
}


bool ipc_user_complete_oneway(
	struct thread *from, void *msg_utcb,
	struct thread **to_p)
{
	from->ipc_from.raw = L4_nilthread.raw;
	from->recv_timeout = L4_ZeroTime;

	bool done = false;	/* wtf, gcc */
	void *from_utcb = thread_get_utcb(from);
	if(msg_utcb != from_utcb) {
		set_ipc_return_thread(*to_p, msg_utcb);
		thread_wake(*to_p);
		post_exn_ok(from, NULL);
		done = true;
	} else if(from->send_timeout.raw != L4_ZeroTime.raw) {
		/* the long way around to passive send. or immediate success. that may
		 * happen.
		 */
		bool done = ipc_send_half(from, from_utcb, to_p);
		if(done) post_exn_ok(from, NULL);
	} else {
		/* instant timeout. */
		post_exn_fail(from);
		done = false;
	}

	assert(check_ipc_module());
	return done;
}


/* add `t' to redir_hash under `holdup', then remove `t' from sendwait_hash.
 * this makes sure that a held passive send can be restarted through the
 * `holdup' redirector's active receive when the ultimate recipient goes into
 * passive receive.
 *
 * TODO: this function is quite similar to what other code that handles
 * redir_wait does. one of them might be removed.
 */
static void convert_to_redirwait(struct thread *t, L4_ThreadId_t holdup)
{
	assert(!CHECK_FLAG(t->flags, TF_REDIR_WAIT));
	assert(t->status == TS_SEND_WAIT);

	t->u1.waited_redir = holdup;
	t->flags |= TF_REDIR_WAIT;
	if(likely(!L4_IsNilThread(holdup))) {
		/* TODO: handle malloc fail */
		htable_add(&redir_wait, hash_waited_redir(t, NULL), t);
	}
	remove_send_wait(t);
}


/* removes retval from sendwait_hash. */
static struct thread *find_global_sender(bool *valid_p, struct thread *self)
{
	assert(L4_IsGlobalId(self->ipc_from));

	size_t hash = int_hash(self->id);
	struct htable_iter it;
	struct thread *cand;
	*valid_p = true;
	if(self->ipc_from.raw != L4_anythread.raw) {
		/* LTID conversion & validity test. */
		struct thread *ft = thread_get(self->ipc_from);
		if(ft == NULL) {
			*valid_p = false;
			return NULL;
		}
		L4_ThreadId_t ltid = tid_return(self, ft);

		/* find the IPC waiter. it may be from @ft, or a propagator. */
		for(cand = htable_firstval(&sendwait_hash, &it, hash);
			cand != NULL;
			cand = htable_nextval(&sendwait_hash, &it, hash))
		{
			if(cand->u2.ipc_wait.dest_tid.raw == self->id
				&& (cand->u2.ipc_wait.send_tid.raw == ft->id
					|| cand->u2.ipc_wait.send_tid.raw == ltid.raw))
			{
				break;
			}
		}

		L4_ThreadId_t holdup;
		if(cand != NULL
			&& will_redirect(cand, self)
			&& !is_redir_ready(&holdup, cand))
		{
			TRACE_REDIR("IPC: targeted receive held by %lu:%lu\n",
				L4_ThreadNo(holdup), L4_Version(holdup));
			/* the caller might go to sleep on passive receive, so the passive
			 * sender should be made resumable by a redirector's action.
			 */
			convert_to_redirwait(cand, holdup);
			cand = NULL;
		}
	} else {
		/* anythread case. */
		for(cand = htable_firstval(&sendwait_hash, &it, hash);
			cand != NULL;
			cand = htable_nextval(&sendwait_hash, &it, hash))
		{
			if(cand->u2.ipc_wait.dest_tid.raw != self->id) continue;

			L4_ThreadId_t holdup;
			if(will_redirect(cand, self) && !is_redir_ready(&holdup, cand)) {
				TRACE_REDIR("IPC: global anythread receive held by %lu:%lu\n",
					L4_ThreadNo(holdup), L4_Version(holdup));
				convert_to_redirwait(cand, holdup);
			} else {
				/* got one. (TODO: collect them instead and pick the one with
				 * highest priority?)
				 */
				break;
			}
		}
	}

	if(cand != NULL) htable_delval(&sendwait_hash, &it);
	return cand;
}


/* find a redirected IPC sender for @redir, or return NULL.
 *
 * TODO: doesn't remove the thread on success, but should; this'd avoid
 * recomputing the hash value.
 */
static struct thread *find_redir_sender(struct thread *redir)
{
	struct thread *ret;

	/* fool is_redir_ready() */
	int old_status = redir->status;
	redir->status = TS_R_RECV;

	struct htable_iter it;
	size_t hash = int_hash(redir->id);
	for(struct thread *cand = htable_firstval(&redir_wait, &it, hash);
		cand != NULL;
		cand = htable_nextval(&redir_wait, &it, hash))
	{
		if(cand->u1.waited_redir.raw != redir->id) continue;

		/* TODO: move these into an invariant check over @redir_wait's
		 * items
		 */
		assert(cand->status == TS_SEND_WAIT);
		assert(CHECK_FLAG(cand->flags, TF_REDIR_WAIT));
		assert(cand->space->redirector != NULL);

		if(redir->ipc_from.raw != L4_anythread.raw
			&& redir->ipc_from.raw != cand->id)
		{
			continue;
		}

		L4_ThreadId_t holdup;
		if(!is_redir_ready(&holdup, cand)) {
			assert(holdup.raw != redir->id);
			/* move redir_wait to a different thread. */
			htable_del(&redir_wait, hash, cand);
			cand->u1.waited_redir = holdup;
			/* TODO: handle malloc failure */
			htable_add(&redir_wait, hash_waited_redir(cand, NULL), cand);
			continue;
		}

		if(cand->space->redirector == redir) {
			/* it's the immediate redirector. short-circuit re-send processing
			 * into active receive.
			 */

			/* FIXME: this duplicates the inter-space active send condition.
			 * it should be moved out of this, ipc_send_wait(), and whatever
			 * else uses it.
			 */
			assert(L4_IsGlobalId(cand->ipc_to));
			struct thread *dest = thread_get_fast(cand->ipc_to);
			if((dest->status == TS_R_RECV || dest->status == TS_RECV_WAIT)
				&& (dest->ipc_from.raw == L4_anythread.raw
					|| dest->ipc_from.raw == cand->id))
			{
				ret = cand;
				goto end;
			}
		} else {
			/* TODO: this re-runs ipc_send_half() entirely, which may be very
			 * expensive. it should instead cause it to be run through
			 * scheduling, so that the highest-priority ready sender gets to
			 * execute.
			 */
			htable_del(&redir_wait, hash, cand);
			cand->flags &= ~TF_REDIR_WAIT;
			cand->u1.waited_redir = L4_nilthread;
			redo_ipc_send_half(cand);
		}
	}
	ret = NULL;

end:
	redir->status = old_status;
	return ret;
}


/* always sets *from_tid_p to a local TID. on propagation, ActualSender will
 * be set from retval->id. removes retval from sendwait_hash. rewrites
 * @self->ipc_from when return value is not NULL, or *valid_p is returned
 * true.
 */
static struct thread *find_local_sender(bool *valid_p, struct thread *self)
{
	L4_ThreadId_t fromspec = self->ipc_from;
	assert(!L4_IsNilThread(self->ipc_from));
	assert(L4_IsLocalId(self->ipc_from));

	struct thread *cand;
	size_t hash = int_hash(self->id);
	*valid_p = true;
	if(fromspec.raw != L4_anylocalthread.raw) {
		/* a particular LTID either exists, or doesn't. */
		cand = space_find_local_thread(self->space, fromspec.local);
		if(cand == NULL) {
			*valid_p = false;
			return NULL;
		}
		self->ipc_from.raw = cand->id;

		/* check sendwait match. if it's in the hashtable as well, accept
		 * immediately.
		 */
		if(cand->u2.ipc_wait.dest_tid.raw != self->id
			|| !htable_del(&sendwait_hash, hash, cand))
		{
			/* otherwise, find propagated sends on @fromspec's behalf. */
			struct htable_iter it;
			for(cand = htable_firstval(&sendwait_hash, &it, hash);
				cand != NULL;
				cand = htable_nextval(&sendwait_hash, &it, hash))
			{
				if(cand->u2.ipc_wait.dest_tid.raw != self->id) continue;
				if(cand->u2.ipc_wait.send_tid.raw == fromspec.raw) {
					/* re-check propagation predicate.
					 *
					 * FIXME: do the other things besides "does virtualsender
					 * still exist" also
					 */
					struct thread *vs = resolve_tid_spec(self->space,
						cand->u2.ipc_wait.send_tid);
					if(vs != NULL) break;
				}
			}
			if(cand != NULL) htable_delval(&sendwait_hash, &it);
		}
	} else {
		/* anylocalthread side. */
		/* (TODO: use a per-space list of IPC senders, or something.) */
		struct htable_iter it;
		for(cand = htable_firstval(&sendwait_hash, &it, hash);
			cand != NULL;
			cand = htable_nextval(&sendwait_hash, &it, hash))
		{
			/* it only has to look like a local sender. propagation is
			 * fine, too.
			 */
			if(cand->u2.ipc_wait.dest_tid.raw == self->id
				&& L4_IsLocalId(cand->u2.ipc_wait.send_tid))
			{
				break;
			}
		}

		if(cand != NULL) htable_delval(&sendwait_hash, &it);
	}

	assert(cand == NULL || L4_IsLocalId(cand->u2.ipc_wait.send_tid));
	return cand;
}


/* postcond: !@retval || @self->status \in {READY, R_RECV, STOPPED} */
bool ipc_recv_half(struct thread *self, void *self_utcb)
{
	assert(check_ipc_module());

	/* poll for interrupts where applicable. */
	if(CHECK_FLAG(self->flags, TF_INTR)
		&& (self->ipc_from.raw == L4_anythread.raw
			|| is_interrupt(self->ipc_from)))
	{
		int found = int_poll(self,
			self->ipc_from.raw == L4_anythread.raw ? -1 : L4_ThreadNo(self->ipc_from));
		if(found >= 0) {
			/* eat the IPC, eat the IPC, eat the IPC, eat the IPC
			 * die in awful pain, die in awful pain, awful pain
			 */
			void *utcb = thread_get_utcb(self);
			L4_VREG(utcb, L4_TCR_MR(0)) = (L4_MsgTag_t){
				.X.label = 0xfff0 }.raw;
			self->ipc_from = L4_GlobalId(found, 1);
			set_ipc_return_regs(&self->ctx.r, self, utcb);
			if(self->status == TS_RUNNING) self->status = TS_READY;
			assert(IS_READY(self->status));
			assert(check_ipc_module());
			return true;
		}
	}

	/* find waiting IPC. */
	struct thread *from, *r_sender = NULL;
	L4_ThreadId_t send_tid = L4_nilthread;
	if(L4_IsLocalId(self->ipc_from)) {
		assert(self->ipc_from.raw != L4_anythread.raw);
		bool valid;
		from = find_local_sender(&valid, self);
		if(!valid) goto err_no_partner;
		if(from != NULL) send_tid = from->u2.ipc_wait.send_tid;
		assert(L4_IsGlobalId(self->ipc_from)
			|| self->ipc_from.raw == L4_anylocalthread.raw);
	} else if(CHECK_FLAG(self->flags, TF_REDIR)
		&& (r_sender = find_redir_sender(self)) != NULL)
	{
		TRACE_REDIR("IPC: found redir_sender(%lu:%lu) in %lu:%lu\n",
			TID_THREADNUM(r_sender->id), TID_VERSION(r_sender->id),
			TID_THREADNUM(self->id), TID_VERSION(self->id));
		htable_del(&redir_wait, hash_waited_redir(r_sender, NULL), r_sender);
		send_tid = tid_return(self, r_sender);
		from = r_sender;
	} else {
		assert(self->ipc_from.raw != L4_anylocalthread.raw);
		bool valid;
		from = find_global_sender(&valid, self);
		if(!valid && !is_interrupt(self->ipc_from)) {
			/* some things IPC can't relay. for everything else, there's
			 * passive receive.
			 */
			assert(self->ipc_from.raw != L4_anythread.raw);
			goto err_no_partner;
		}
		if(from != NULL) send_tid = from->u2.ipc_wait.send_tid;
	}
	assert(from == NULL
		|| (!CHECK_FLAG(from->space->flags, SF_REDIRECT)
			|| from->space->redirector != NULL));

	if(from == NULL) {
		TRACE("%s: passive receive to %lu:%lu (waiting on %lu:%lu)\n", __func__,
			TID_THREADNUM(self->id), TID_VERSION(self->id),
			TID_THREADNUM(self->ipc_from.raw), TID_VERSION(self->ipc_from.raw));
		bool old_wakeup = self->status == TS_R_RECV;
		self->status = TS_RECV_WAIT;

		if(self->ipc != NULL && self->ipc->xferto_at > 0) {
			if(ksystemclock() >= self->ipc->xferto_at) {
				/* this happens when @self is a xfer pagefault sender and that
				 * IPC succeeded passively, putting @self in TS_R_RECV,
				 * causing the scheduler to call ipc_recv_half() on it after
				 * xferto_at has passed.
				 */
				ipc_xfer_timeout(self->ipc);
				assert(check_ipc_module());
				return true;
			} else {
				/* avoid L4_Time_t back-and-forth in thread_sleep() */
				self->wakeup_time = self->ipc->xferto_at;
				sq_update_thread(self);
			}
		} else if(old_wakeup) {
			/* came from R_RECV, therefore wakeup_time is set from the
			 * send-phase completion.
			 */
			sq_update_thread(self);		/* FIXME: likely redundant */
			assert(self->wakeup_time == ~(uint64_t)0
				|| self->wakeup_time == 0
				|| self->wakeup_time > ksystemclock());
			if(self->wakeup_time == 0) {
				/* timeout. */
				set_ipc_error_thread(self, (1 << 1) | 1);
				self->status = TS_READY;
				assert(check_ipc_module());
				return false;
			}
		} else {
			/* receive-only IPC, such as L4_Wait(), gets its timeout here. */
			thread_sleep(self, self->recv_timeout);
			if(self->status == TS_READY) {
				/* instant timeout. */
				assert(self->recv_timeout.raw == L4_ZeroTime.raw);
				set_ipc_error_thread(self, (1 << 1) | 1);
				assert(check_ipc_module());
				return false;
			}
		}

		/* don't pile up on the wildcards. */
		if(!L4_IsNilThread(self->ipc_from)
			&& !is_wildcard(self->ipc_from)
			&& !insert_recv_wait(self))
		{
			/* TODO: hold them horses until memory comes back again */
			panic("ran out of memory in passive receive!");
		}

		assert(check_ipc_module());
		return false;
	} else if(CHECK_FLAG(from->space->flags, SF_REDIRECT)
		&& self->space != from->space
		&& self->space != from->space->redirector->space)
	{
		/* this thread was the current redirection-wait target for w->thread,
		 * but wasn't its immediate redirector. we'll push the redirection
		 * forward one step.
		 */
		TRACE_REDIR("IPC: stepping redir chain forward for %lu:%lu\n",
			TID_THREADNUM(from->id), TID_VERSION(from->id));

		L4_ThreadId_t dummy;
		assert(is_redir_ready(&dummy, from));

		/* put current thread into R_RECV so that the final redirector may
		 * communicate, that the scheduler will pick up the next partner if
		 * the redirectors go to perpetual call-sleep instead, and that
		 * timeouts occur as they should.
		 */
		if(self->status == TS_RUNNING || IS_READY(self->status)) {
			sq_remove_thread(self);
		}
		self->status = TS_R_RECV;
		self->wakeup_time = wakeup_at(self->recv_timeout);
		sq_insert_thread(self);

		/* re-execute @from's send half. */
		if(redo_ipc_send_half(from)) {
			/* go back to scheduler; our own IPC didn't complete yet. */
			/* TODO: switch to the redirector through the IPC partnering
			 * mechanism, yet to be written.
			 */
			TRACE_REDIR("IPC: active receive was redirected, status=%s\n",
				sched_status_str(self));
			/* (instead leave it to the scheduler.) */
		} else if(!check_preempt()) {
			/* IPC didn't complete, and no pre-emption occurs. try again. */
			sq_remove_thread(self);
			self->status = TS_RUNNING;
			return ipc_recv_half(self, self_utcb);
		}

		/* pre-empted. stay in R_RECV and don't complete. */
		assert(self->status == TS_R_RECV);
		assert(check_ipc_module());
		return false;
	} else {
		/* active receive */
		TRACE("%s: active receive from %lu:%lu actual %lu:%lu (to %lu:%lu)\n",
			__func__,
			TID_THREADNUM(send_tid.raw), TID_VERSION(send_tid.raw),
			TID_THREADNUM(from->id), TID_VERSION(from->id),
			TID_THREADNUM(self->id), TID_VERSION(self->id));
		assert(from->status == TS_SEND_WAIT || from->status == TS_XFER);

		void *from_utcb = thread_get_utcb(from);
		int n = do_ipc_transfer(from, from_utcb, self, self_utcb);
		if(unlikely(n > 0)) {
			const L4_Word_t error = n;
			TRACE("%s: active receive caused errorcode %#lx\n",
				__func__, error);
			int code = (error & 0xe) >> 1;
			if(code >= 4) {
				/* mutual error; notify sender also (i.e. break its exception
				 * receive phase, if any)
				 */
				if(!post_exn_fail(from)) {
					set_ipc_error(from_utcb, error & ~1ul);
					set_ipc_return_thread(from, from_utcb);
				}
				thread_wake(from);
			} else {
				/* FIXME: cause `w' to be freed (when r_sender != NULL) or reinserted
				 * into sendwait_hash.
				 */
			}
			set_ipc_error(self_utcb, error | 1);
			assert(self->status == TS_RUNNING);
			self->status = TS_READY;	/* failed active receive -> READY. */
			assert(check_ipc_module());
			return false;
		} else if(n < 0) {
			TRACE("%s: transfer caused pagefault\n", __func__);
			assert(n == -EFAULT);
			assert(IS_IPC(self->status));
			/* FIXME: reinsert or free `w' */
			assert(check_ipc_module());
			return false;
		}

		/* successful active receive -> READY. */
		self->status = TS_READY;
		if(self->wakeup_time > 0) {
			self->wakeup_time = 0;
			sq_update_thread(self);
		}

		/* whodunnit */
		self->ipc_from = send_tid;
		L4_MsgTag_t *tag = (L4_MsgTag_t *)&L4_VREG(self_utcb, L4_TCR_MR(0));
		if(L4_IpcPropagated(*tag)) {
			TRACE("%s: propagated to %lu:%lu from %lu:%lu as %lu:%lu\n",
				__func__, TID_THREADNUM(self->id), TID_VERSION(self->id),
				TID_THREADNUM(from->id), TID_VERSION(from->id),
				L4_ThreadNo(self->ipc_from), L4_Version(self->ipc_from));
			L4_VREG(self_utcb, L4_TCR_VA_SENDER) = tid_return(self, from).raw;
		}
		if(r_sender != NULL) {
			tag->X.flags |= 0x2;	/* redirection */
			L4_VREG(self_utcb, L4_TCR_INTENDEDRECEIVER) = r_sender->ipc_to.raw;
			r_sender->flags &= ~TF_REDIR_WAIT;

			/* (local destinations aren't subject to redirection.) */
			assert(L4_IsNilThread(r_sender->ipc_to)
				|| !L4_IsLocalId(r_sender->ipc_to));
			if(r_sender->ipc_from.raw == r_sender->ipc_to.raw) {
				r_sender->ipc_from.raw = self->id;
			}
		}

		/* userspace IPC state transition to the receive phase. */
		if(L4_IsNilThread(from->ipc_from)) {
			/* no receive phase. */
			TRACE("%s: ... sender becomes ready.\n", __func__);
			thread_wake(from);
			if(!post_exn_ok(from, NULL)) {	/* clear total_quantum RPC */
				/* only set regs in syscall-generated IPC */
				set_ipc_return_thread(from, from_utcb);
			}
		} else {
			/* go to a deferred receive half */
			TRACE("%s: ... sender waits to receive.\n", __func__);
			from->status = TS_R_RECV;
			from->wakeup_time = wakeup_at(from->recv_timeout);
			sq_update_thread(from);
			might_preempt(from);
		}
		if(!post_exn_ok(self, from)) {
			/* post-IPC exception hooks may start another IPC operation right
			 * away, so check this only in the ordinary path.
			 */
			assert(IS_READY(from->status)
				|| (CHECK_FLAG(from->flags, TF_HALT)
					&& from->status == TS_STOPPED));
			set_ipc_return_regs(&self->ctx.r, self, self_utcb);
			TRACE("%s: ... receiver was in userspace\n", __func__);
		} else {
			/* don't load Ipc/Lipc output registers. */
			TRACE("%s: ... receiver was a hook\n", __func__);
		}

		assert(check_ipc_module());
		return true;
	}

	assert(false);

err_no_partner:
	if(self == get_current_thread() || !post_exn_fail(self)) {
		/* mosey on back to userspace */
		set_ipc_error(self_utcb, 5);
		assert(self->status == TS_RUNNING || self->status == TS_R_RECV);
		self->status = TS_READY;	/* failed active receive -> READY. */
	}
	assert(check_ipc_module());
	return false;
}


SYSCALL L4_Word_t sys_ipc(
	L4_ThreadId_t to,
	L4_ThreadId_t from,
	L4_Word_t timeouts,
	void *utcb,
	L4_Word_t mr0)
{
	assert(x86_irq_is_enabled());
	assert(check_ipc_module());

	struct thread *current = get_current_thread();
	TRACE("%s: current=%lu:%lu, to=%#lx, fromspec=%#lx, timeouts=%#lx, mr0=%#lx\n",
		__func__, TID_THREADNUM(current->id), TID_VERSION(current->id),
		to.raw, from.raw, timeouts, mr0);
	/* TODO: carry mr0 into ipc_send_half() explicitly */
	L4_VREG(utcb, L4_TCR_MR(0)) = mr0;

	/* parameter validation. */
	if(!L4_IsNilThread(to)
		&& unlikely(is_wildcard(to)
			|| (!L4_IsLocalId(to)
				&& L4_ThreadNo(to) < first_user_threadno()
				&& L4_ThreadNo(to) > last_int_threadno())))
	{
		set_ipc_error(utcb, 4);		/* non-existing partner, send phase */
		return L4_nilthread.raw;
	}
	if(unlikely(L4_IsGlobalId(from)
		&& L4_ThreadNo(from) > last_int_threadno()
		&& L4_ThreadNo(from) < first_user_threadno()))
	{
		set_ipc_error(utcb, 5);		/* non-existing partner, receive phase */
		return L4_nilthread.raw;
	}

	L4_Clock_t now = { .raw = ksystemclock() };
	current->ipc_to = to;
	current->ipc_from = from;
	current->send_timeout.raw = timeouts >> 16;
	current->recv_timeout.raw = timeouts & 0xffff;
	/* FIXME: this way of handling the receive timeout is basically wrong; a
	 * time period counts from the start of the receive phase but a timepoint
	 * is rooted in @now, resolved at sys_ipc() invocation.
	 *
	 * for SndTimeout though they're exactly the same thing.
	 */
	if(L4_IsTimePoint_NP(current->recv_timeout)) {
		current->recv_timeout = point_to_period(now, current->recv_timeout);
	}
	/* SndTimeout is converted after the send phase test. */

	struct thread *dest = NULL;
	if(!L4_IsNilThread(to)
		&& (!CHECK_FLAG(current->flags, TF_INTR)
			|| !is_interrupt(to)
			/* redirected tasks can never reply to interrupts. */
			|| CHECK_FLAG(current->space->flags, SF_REDIRECT)))
	{
		/* resolve to an actual thread (not an interrupt). or fail if not
		 * associated with an interrupt.
		 */
		dest = resolve_tid_spec(current->space, to);
		if(unlikely(dest == NULL)) {
			TRACE("%s: can't find %s peer %lu:%lu\n", __func__,
				L4_IsGlobalId(to) ? "global" : "local",
				L4_ThreadNo(to), L4_Version(to));
			set_ipc_error(utcb, 4);	/* no partner, send phase */
			assert(current->status == TS_RUNNING
				|| current->status == TS_READY);
			current->status = TS_RUNNING;
			assert(check_ipc_module());
			return L4_nilthread.raw;
		}
	}
	if(!L4_IsNilThread(to)) {
		/* send phase. */
		TRACE("%s: IPC send phase.\n", __func__);
		if(L4_IsTimePoint_NP(current->send_timeout)) {
			current->send_timeout = point_to_period(now,
				current->send_timeout);
		}
		if(!ipc_send_half(current, utcb, &dest)) {
			/* didn't complete. either READY, SEND_WAIT, STOPPED, or XFER. */
			assert(current->status != TS_STOPPED);	/* (would be weird.) */
			if(IS_IPC(current->status)) {
				/* ongoing IPC. */
				assert(check_ipc_module());
				return_to_scheduler();
			} else {
				/* error condition in send phase. */
				goto err_exit;
			}
		} else if(current->u0.partner == dest && dest != NULL) {
			/* reschedule previous IPC partner. */
			TRACE("%s: returning to partner %lu:%lu\n", __func__,
				TID_THREADNUM(dest->id), TID_VERSION(dest->id));
			if(L4_IsNilThread(from)) {
				current->status = TS_READY;
				current->wakeup_time = 0;
				L4_VREG(utcb, L4_TCR_MR(0)) = 0;
				set_ipc_return_thread(current, utcb);
			} else {
				current->status = TS_R_RECV;
				current->wakeup_time = wakeup_at(current->recv_timeout);
			}
			assert(check_ipc_module());
			return_to_partner();
		} else if(check_preempt()) {
			assert(IS_READY(current->status) || current->status == TS_RUNNING);
			if(!L4_IsNilThread(from)) {
				/* must step off due to preemption. indicate a need for active
				 * receive through the scheduler.
				 */
				current->status = TS_R_RECV;
				current->wakeup_time = wakeup_at(current->recv_timeout);
				sq_update_thread(current);
			} else {
				/* preemption before Ipc exit. */
				current->status = TS_READY;
				set_ipc_return_regs(&current->ctx.r, current, utcb);
			}
			assert(check_ipc_module());
			return_to_scheduler();
		}
	}

	if(!L4_IsNilThread(from) && current->status != TS_SEND_WAIT) {
		/* receive phase. */
		TRACE("%s: IPC receive phase.\n", __func__);
		if(ipc_recv_half(current, utcb)) {
			if(check_preempt() && IS_READY(current->status)) {
				set_ipc_return_regs(&current->ctx.r, current, utcb);
				assert(check_ipc_module());
				return_to_scheduler();
			}

			/* TODO: alter ipc_recv_half() to not change status to TS_READY
			 * when about to return true
			 */
			current->status = TS_RUNNING;
		} else if(IS_IPC(current->status)) {
			/* ongoing IPC. */
			assert(check_ipc_module());
			return_to_scheduler();
		} else {
			/* error condition in receive phase. */
			goto err_exit;
		}
	}

	/* successful exit. */
	assert(current->status == TS_RUNNING);
	assert(check_ipc_module());
	TRACE("%s: successful exit, from=%#lx\n", __func__,
		current->ipc_from.raw);
	return current->ipc_from.raw;

err_exit:
	assert(CHECK_FLAG(L4_VREG(utcb, L4_TCR_MR(0)), 0x8000));
	assert(L4_VREG(utcb, L4_TCR_ERRORCODE) != 0);
	current->status = TS_RUNNING;
	assert(check_ipc_module());
	return L4_nilthread.raw;
}


SYSCALL L4_Word_t sys_lipc(
	L4_ThreadId_t to,
	L4_ThreadId_t fromspec,
	L4_Word_t timeouts,
	void *utcb_ptr,
	L4_Word_t mr0, L4_Word_t mr1, L4_Word_t mr2)
{
	assert(check_ipc_module());
	L4_MsgTag_t tag = { .raw = mr0 };

	struct thread *sender = get_current_thread(), *dest = NULL;
	L4_ThreadId_t sender_ltid = get_local_id(sender);
	L4_Word_t kip_base = L4_Address(sender->space->kip_area);

	TRACE("%s: called in %lu:%lu, to=%#lx, fromspec=%#lx, timeouts=%#lx\n",
		__func__, TID_THREADNUM(sender->id), TID_VERSION(sender->id),
		to.raw, fromspec.raw, timeouts);

	if(USE_SYSENTER) {
		/* set up delayed return into the Ipc epilog. this happens on
		 * fallback-to-Ipc and reply-via-Ipc; reply-via-Lipc will replace it
		 * anyway.
		 */
		sender->ctx.eip = kip_base + sysexit_epilogs.fast;
		/* pop mr1, mr2 pushed in the syscall stub */
		sender->ctx.r.esp += 8;
	}

	/* TODO: the l4.x2 spec seems to imply that Lipc might do propagation.
	 * this tests to exclude that (and doesn't inspect the VirtualSender TCR).
	 * same for string transfers; this chucks typed transfers in general.
	 *
	 * NOTE: similarly the spec says that Lipc.RcvTimeout should be Never.
	 * mung's partner scheduling should become strong enough not to give a
	 * fuck about that though, so we'll skip that clause even while it's not
	 * quite there yet.
	 *
	 * TODO: likewise nothing here cares about ProcessorNo. SMP isn't here yet
	 * so it doesn't have to, either.
	 */
	L4_Word_t failmask = (to.raw & 0x3f)	/* local ID bits */
		| (tag.raw & 0xffc0);				/* flags, t */
	bool pass = failmask == 0
		&& (dest = space_find_local_thread(sender->space, to.local),
			dest != NULL)
		&& (dest->status == TS_RECV_WAIT || dest->status == TS_R_RECV)
		&& likely(hook_empty(&dest->post_exn_call))
		&& (is_wildcard(dest->ipc_from)
			|| dest->ipc_from.raw == sender_ltid.raw
			|| dest->ipc_from.raw == sender->id);
	if(unlikely(!pass)) {
		/* fall back to regular ipc. */
		TRACE("%s: fallback\n", __func__);
		L4_VREG(utcb_ptr, L4_TCR_MR(1)) = mr1;
		L4_VREG(utcb_ptr, L4_TCR_MR(2)) = mr2;
		return sys_ipc(to, fromspec, timeouts, utcb_ptr, mr0);
	}

	/* effect IPC state transition & switch threads without the return_to_*()
	 * family.
	 */
	if(in_recv_wait(dest)) remove_recv_wait(dest);
	sender->recv_timeout = (L4_Time_t){ .raw = timeouts & 0xffff };
	assert(is_wildcard(fromspec)
		|| fromspec.raw == dest->id
		|| fromspec.raw == get_local_id(dest).raw);
	sender->ipc_from.raw = !is_wildcard(fromspec) ? dest->id : fromspec.raw;
	sender->status = TS_R_RECV;		/* avoids hotpath recvwait_hash insert */
	/* don't accidentally become partner by way of reply! */
	bool dest_was_partner = sender->u0.partner == dest;
	if(likely((timeouts & 0xffff) == L4_Never.raw)) {
		sched_ipc_handoff_quick(sender, dest);
	} else {
		sched_ipc_handoff_timeout(sender, dest, sender->recv_timeout);
	}
	assert(dest->status == TS_RUNNING);
	assert(!in_recv_wait(dest));
	assert(sender->u0.partner != dest);
	if(to.raw == fromspec.raw && likely(!dest_was_partner)) {
		assert(get_local_id(dest).raw == to.raw);
		/* FIXME: when dest->u0.partner != NULL, break the existing scheduling
		 * partnership. (it may be enough to just set the pointer.) that
		 * should happen with a sched_set_partner() call or some such.
		 */
		dest->u0.partner = sender;
	}
	/* set the receiver's epilogue frame up. */
	dest->ctx.r.edi = to.raw;
	dest->ctx.r.eax = sender_ltid.raw;
	tag.X.flags = 0;
	dest->ctx.r.esi = tag.raw;
	dest->ctx.r.ebx = mr1;
	dest->ctx.r.ebp = mr2;
	dest->ctx.eip = kip_base + lipc_epilog_offset;

	assert(check_ipc_module());
	return_from_exn();
	exit_to_thread(dest);
}
