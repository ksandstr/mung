
/* the L4.X2 IPC state machine, the Ipc system call, untyped transfers, and
 * kernel-to-user IPC operation.
 */

#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <errno.h>
#include <ccan/likely/likely.h>
#include <ccan/compiler/compiler.h>
#include <ccan/alignof/alignof.h>
#include <ccan/htable/htable.h>

#include <l4/types.h>
#include <l4/message.h>
#include <l4/vregs.h>
#include <l4/schedule.h>
#include <l4/ipc.h>

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


/* these are kept in sendwait_hash in a multiset way, i.e. use
 * htable_firstval() and so forth to scan.
 */
struct ipc_wait
{
	L4_ThreadId_t dest_tid;		/* global ID. key, ptr in sendwait_hash */
	L4_ThreadId_t send_tid;		/* thread->id or vs, may be local */
	struct thread *thread;		/* (actual) sender thread */
};


static size_t hash_threadid(const void *tid, void *priv);
static size_t hash_waited_redir(const void *thread, void *priv);


static struct kmem_cache *ipc_wait_slab = NULL;
static struct htable sendwait_hash = HTABLE_INITIALIZER(
		sendwait_hash, &hash_threadid, NULL),
	redir_wait = HTABLE_INITIALIZER(redir_wait, &hash_waited_redir, NULL);


static size_t hash_threadid(const void *tid, void *priv) {
	const L4_ThreadId_t *p = tid;
	return int_hash(p->raw);
}


static size_t hash_waited_redir(const void *threadptr, void *priv) {
	const struct thread *t = threadptr;
	assert(!L4_IsNilThread(t->u1.waited_redir));
	assert(t->space != kernel_space);
	return int_hash(t->u1.waited_redir.raw);
}


COLD void init_ipc(void)
{
	ipc_wait_slab = kmem_cache_create("ipc_wait_slab",
		sizeof(struct ipc_wait), ALIGNOF(struct ipc_wait),
		0, NULL, NULL);
}


static inline void set_ipc_error(void *utcb, L4_Word_t ec)
{
	L4_VREG(utcb, L4_TCR_ERRORCODE) = ec;
	L4_VREG(utcb, L4_TCR_MR(0)) = ((L4_MsgTag_t){ .X.flags = 0x8 }).raw;
	L4_VREG(utcb, L4_TCR_MR(1)) = 0;
	L4_VREG(utcb, L4_TCR_MR(2)) = 0;
}


inline void set_ipc_return_regs(
	struct x86_exregs *regs,
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
	assert(!IS_KERNEL_THREAD(t));
	set_ipc_return_regs(&t->ctx, t, utcb);
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
		/* in-transfer mode. decided by whether the sender is waiting for
		 * pager service.
		 */
		f_in_src = IS_IPC_WAIT(st->from->status)
			|| st->from->status == TS_R_RECV;
		assert(!f_in_src
			|| (IS_IPC_WAIT(st->to->status) || st->to->status == TS_R_RECV));
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
	assert(t->status == TS_XFER);
	assert(t->ipc != NULL);
	struct ipc_state *st = t->ipc;
	struct thread *dest = st->to, *source = st->from;

	TRACE("%s: called on %lu:%lu -> %lu:%lu\n",
		__func__, TID_THREADNUM(source->id), TID_VERSION(source->id),
		TID_THREADNUM(dest->id), TID_VERSION(dest->id));

	/* resumption applies only to string transfers. kernel threads are
	 * forbidden from doing string transfers.
	 */
	assert(!IS_KERNEL_THREAD(dest));
	assert(!IS_KERNEL_THREAD(source));

	void *s_utcb = thread_get_utcb(st->from),
		*d_utcb = thread_get_utcb(st->to);
	assert(source->ipc == st);
	assert(dest->ipc == st);

	/* resume mode. */
	L4_MsgTag_t tag = { .raw = L4_VREG(s_utcb, L4_TCR_MR(0)) };
	int n = do_typed_transfer(source, s_utcb, dest, d_utcb, tag);
	if(n < 0) {
		assert(n == -EFAULT);
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

	return true;
}


struct thread *ipc_partner(struct thread *t)
{
	assert(t->status == TS_XFER || IS_IPC_WAIT(t->status));
	assert(t->ipc != NULL);
	assert(!CHECK_FLAG(t->flags, TF_SENDER) || t == t->ipc->from);
	assert(CHECK_FLAG(t->flags, TF_SENDER) || t == t->ipc->to);

	struct thread *partner = CHECK_FLAG(t->flags, TF_SENDER) ? t->ipc->to : t->ipc->from;
	assert(IS_IPC(partner->status));
	assert(partner->ipc == t->ipc);
	return partner;
}


/* returns 0 on success, ErrorCode on error signal, or -EFAULT on ongoing
 * typed transfer.
 */
static inline int do_ipc_transfer(
	struct thread *source,
	void *s_utcb,
	struct thread *dest,
	void *d_utcb)
{
	L4_MsgTag_t tag = { .raw = L4_VREG(s_utcb, L4_TCR_MR(0)) };
	if(unlikely(!hook_empty(&dest->post_exn_call))) {
		/* (likelihood reflects exceptions' nature.) */
		save_ipc_regs(dest, d_utcb,
			L4_UntypedWords(tag) + L4_TypedWords(tag) + 1);
	}
	L4_VREG(d_utcb, L4_TCR_MR(0)) = tag.raw;
	for(int i=0; i < tag.X.u; i++) {
		int reg = L4_TCR_MR(i + 1);
		L4_VREG(d_utcb, reg) = L4_VREG(s_utcb, reg);
	}

	if(tag.X.t == 0) return 0;
	else {
		return do_typed_transfer(source, s_utcb, dest, d_utcb, tag);
	}
}


/* used by the deleting and overwriting modes of ThreadControl.
 *
 * TODO: this should signal preemption when it occurs: one of the aborted
 * senders may have priority.
 */
void cancel_ipc_to(L4_ThreadId_t with_tid, L4_Word_t errcode)
{
	/* fail those IPC operations that're waiting to send to this one. */
	assert(L4_IsGlobalId(with_tid));

	struct htable_iter it;
	size_t hash = int_hash(with_tid.raw);
	errcode &= ~(L4_Word_t)1;		/* send-phase errors. */
	for(void *ptr = htable_firstval(&sendwait_hash, &it, hash);
		ptr != NULL;
		ptr = htable_nextval(&sendwait_hash, &it, hash))
	{
		struct ipc_wait *w = container_of(ptr, struct ipc_wait, dest_tid);
		if(w->dest_tid.raw != with_tid.raw) continue;

		struct thread *peer = w->thread;
		assert(!L4_IsGlobalId(peer->ipc_to)
			|| L4_ThreadNo(peer->ipc_to) == L4_ThreadNo(with_tid));
		assert(!L4_IsLocalId(peer->ipc_to)
			|| resolve_tid_spec(peer->space, peer->ipc_to) == NULL
			|| TID_THREADNUM(resolve_tid_spec(peer->space, peer->ipc_to)->id)
				== L4_ThreadNo(with_tid));
		assert(peer->status == TS_SEND_WAIT || peer->status == TS_XFER
			|| peer->status == TS_STOPPED);
		if(!post_exn_fail(peer)) {
			/* ordinary non-exception IPC. for exceptions, a silent return via
			 * the callback
			 */
			set_ipc_error_thread(peer, errcode);
			thread_wake(peer);
		}

		htable_delval(&sendwait_hash, &it);
		kmem_cache_free(ipc_wait_slab, w);
	}

	/* NOTE: out-of-module access to thread_hash!
	 * NOTE: also, this should be done with a recvwait_hash type thing to
	 * avoid brute force. (luckily the CCAN htable module serves as multiset
	 * just as well.)
	 */
	errcode |= 1;		/* receive-phase errors. */
	for(struct thread *other = htable_first(&thread_hash, &it);
		other != NULL;
		other = htable_next(&thread_hash, &it))
	{
		if((other->status != TS_RECV_WAIT && other->status != TS_R_RECV)
			|| other->ipc_from.raw != with_tid.raw)
		{
			continue;
		}

		if(!post_exn_fail(other)) {
			set_ipc_error_thread(other, errcode);
			thread_wake(other);
		}
	}
}


static void cancel_passive_send(struct thread *t)
{
	assert(t->status == TS_SEND_WAIT || t->status == TS_STOPPED);

	L4_ThreadId_t partner_tid = t->ipc_to;
	if(!L4_IsNilThread(partner_tid) && L4_IsLocalId(partner_tid)) {
		struct thread *partner = space_find_local_thread(t->space,
			partner_tid.local);
		BUG_ON(partner == NULL, "invalid partner LTID!");
		partner_tid.raw = partner->id;
	} else if(L4_IsNilThread(partner_tid)) {
		return;
	}
	assert(L4_IsGlobalId(partner_tid));

	size_t dst_hash = int_hash(partner_tid.raw);
	assert(offsetof(struct ipc_wait, dest_tid) == 0);
	struct htable_iter it;
	for(struct ipc_wait *w = htable_firstval(&sendwait_hash, &it, dst_hash);
		w != NULL;
		w = htable_nextval(&sendwait_hash, &it, dst_hash))
	{
		if(w->thread == t) {
			assert(w->dest_tid.raw == partner_tid.raw);
			htable_delval(&sendwait_hash, &it);
			kmem_cache_free(ipc_wait_slab, w);
			break;
		}
	}
}


/* TODO: this function scales poorly. unlike cancel_passive_send() it does a
 * brute-force loop over all the passive sends in the system, not just those
 * for the IPC peer.
 */
static void rewrite_passive_vs_from(struct thread *t)
{
	L4_ThreadId_t ltid = L4_nilthread;
	/* inactive threads may be propagated on behalf of. */
	if(likely(t->utcb_pos >= 0)) ltid = get_local_id(t);

	struct htable_iter it;
	for(struct ipc_wait *w = htable_first(&sendwait_hash, &it);
		w != NULL;
		w = htable_next(&sendwait_hash, &it))
	{
		if(w->send_tid.raw != t->id && w->send_tid.raw != ltid.raw) continue;

		struct thread *dest = thread_find(w->dest_tid.raw);
		assert(dest != NULL);
		w->send_tid = tid_return(dest, w->thread);
		L4_MsgTag_t *tag = (void *)&L4_VREG(
			thread_get_utcb(w->thread), L4_TCR_MR(0));
		tag->X.flags &= ~0x1;
	}
}


/* called from thread_ipc_fail() and from the deleting/modifying
 * ThreadControl. takes care of the sendwait_hash entry and disables passive
 * propagated sends' propagation. leaves errorcode setting to caller's caller.
 */
void cancel_ipc_from(struct thread *t)
{
	if(t->status == TS_SEND_WAIT || t->status == TS_STOPPED) {
		cancel_passive_send(t);
	}
	rewrite_passive_vs_from(t);
}


static inline bool is_interrupt(L4_ThreadId_t tid) {
	return L4_Version(tid) == 1
		&& L4_ThreadNo(tid) <= last_int_threadno();
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

	bool rdy;
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
		struct thread *prev = t;
		t = t->space->redirector;
		rdy = (t->status == TS_R_RECV || t->status == TS_RECV_WAIT)
			&& (t->ipc_from.raw == L4_anythread.raw
				|| t->ipc_from.raw == prev->id
				|| (prev->space == t->space
					&& (t->ipc_from.raw == L4_anylocalthread.raw
						|| t->ipc_from.raw == tid_return(t, prev).raw)));
	} while(rdy);

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
 * progress (sleep, string transfer fault).
 *
 * precond: @self->status != TS_STOPPED && !CHECK_FLAG(@self->flags, TF_HALT)
 * postcond: @retval -> @self->status \in {READY, R_RECV, STOPPED}
 *           !@retval -> @self->status \in {SEND_WAIT, XFER, READY, STOPPED}
 */
static bool ipc_send_half(
	struct thread *self,
	void *self_utcb,
	struct thread *dest)
{
	/* must look this alive to attempt active send */
	assert(!CHECK_FLAG(self->flags, TF_HALT));
	assert(self->status != TS_STOPPED);

	/* NOTE: this assert can blow under some curious timing circumstances.
	 * those are provoked by DEBUG_ME_HARDER, i.e. the super nasty invariant
	 * checks in mapdb.c .
	 */
	assert(!L4_IsNilThread(self->ipc_to)
		&& self->ipc_to.raw != L4_anylocalthread.raw
		&& self->ipc_to.raw != L4_anythread.raw);

	int err_code = 0;
	L4_MsgTag_t *tag = (void *)&L4_VREG(self_utcb, L4_TCR_MR(0));
	tag->X.flags &= 0x1;	/* keep the propagate flag */

	if(dest == NULL) {
		assert(CHECK_FLAG(self->flags, TF_INTR));
		assert(is_interrupt(self->ipc_to));
		assert(!CHECK_FLAG(self->space->flags, SF_REDIRECT));

		/* eat an interrupt reply. */
		err_code = int_clear(L4_ThreadNo(self->ipc_to), self);
		if(err_code == 0) return true; else goto error;
	}

	/* get matching variables, incl. propagation */
	L4_ThreadId_t self_id = { .raw = self->id },
		self_lid = tid_return(dest, self);
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
				vs->ipc_from = tid_return(vs, dest);
			}

			sender = vs;
			self_id.raw = vs->id;
			self_lid = tid_return(dest, vs);
			propagated = true;
		} else {
			tag->X.flags &= ~0x1;		/* no propagation for you. */
		}
	}
	assert(!propagated || vs != NULL);

	uint64_t now_us = ksystemclock();
	const bool match_cond = dest->ipc_from.raw == L4_anythread.raw
		|| dest->ipc_from.raw == self_id.raw
		|| dest->ipc_from.raw == self_lid.raw
		|| (dest->ipc_from.raw == L4_anylocalthread.raw
			&& dest->space == sender->space);

	/* override TS_R_RECV? */
	int status = dest->status;
	bool status_cond;
	if(match_cond && status == TS_R_RECV) {
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
				TRACE_REDIR("IPC: redirecting orig[to=%lu:%lu] -> red=%lu:%lu\n",
					TID_THREADNUM(dest->id), TID_VERSION(dest->id),
					TID_THREADNUM(red->id), TID_VERSION(red->id));
				tag->X.flags |= 0x2;		/* set redirect bit */
				redirected = true;
				saved_dest = dest;
				dest = red;
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
				}
			}
		}
		assert(!redirected || saved_dest != NULL);

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
		if(!post_exn_ok(dest)) {
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
			if(likely(!IS_KERNEL_THREAD(dest))) {
				set_ipc_return_thread(dest, dest_utcb);
			}
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
		/* FIXME: check return values from kmem_cache_alloc(), htable_add() --
		 * both may have allocator failure. (really, instead of sendwait_hash
		 * use a linked list in the destination thread, through ipc_wait.
		 * there's already one mechanism that turns thread IDs into hashtable
		 * entries.)
		 */
		TRACE("%s: passive send to %lu:%lu (from %lu:%lu, actual %lu:%lu)\n",
			__func__,
			TID_THREADNUM(dest->id), TID_VERSION(dest->id),
			TID_THREADNUM(self_id.raw), TID_VERSION(self_id.raw),
			TID_THREADNUM(self->id), TID_VERSION(self->id));

		struct ipc_wait *w = kmem_cache_alloc(ipc_wait_slab);
		*w = (struct ipc_wait){
			.dest_tid = { .raw = dest->id },
			.send_tid = tid_return(dest, propagated ? vs : self),
			.thread = self,
		};
		htable_add(&sendwait_hash, int_hash(w->dest_tid.raw), w);
		assert(!L4_IsGlobalId(self->ipc_to)
			|| L4_ThreadNo(w->dest_tid) == L4_ThreadNo(self->ipc_to));

		self->status = TS_SEND_WAIT;
		thread_sleep(self, self->send_timeout);

		return false;
	} else {
		/* instant timeout. */
		set_ipc_error(self_utcb, (1 << 1) | 0);
		/* TODO: is this required? instant return would indicate "no". */
		set_ipc_return_thread(self, self_utcb);
		self->status = TS_READY;
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

	/* drop a previous ipc_wait & cancel propagation chaining. */
	cancel_ipc_from(t);

	if(t->wakeup_time == ~(uint64_t)0) t->send_timeout = L4_Never;
	else {
		L4_Clock_t base = { .raw = ksystemclock() };
		t->send_timeout = base.raw < t->wakeup_time
			? L4_TimePoint2_NP(base, (L4_Clock_t){ .raw = t->wakeup_time })
			: L4_ZeroTime;
	}
	thread_wake(t);

	void *utcb = thread_get_utcb(t);
	struct thread *dest = resolve_tid_spec(t->space, t->ipc_to);
	if(unlikely(dest == NULL)) {
		set_ipc_error(utcb, 4);	/* no partner, send phase */
		might_preempt(t);
		return true;
	}

	bool done = ipc_send_half(t, utcb, dest);
	if(done && t->status == TS_READY && !L4_IsNilThread(t->ipc_from)) {
		sq_remove_thread(t);
		t->wakeup_time = wakeup_at(t->recv_timeout);
		t->status = TS_R_RECV;
		sq_insert_thread(t);
	} else if(done && !IS_IPC(t->status)) {
		might_preempt(t);
	}
	return done;
}


void ipc_user(
	struct thread *from,
	struct thread *to,
	uint64_t xferto_at)
{
	/* this must be a global ID so that cancel_ipc_to()'s receiver search
	 * will find it.
	 */
	from->ipc_to.raw = to->id;
	from->ipc_from.raw = to->id;
	from->send_timeout = L4_Never;
	from->recv_timeout = L4_Never;

	void *from_utcb = thread_get_utcb(from);
	if(ipc_send_half(from, from_utcb, to)) {
		/* TODO: this can only succeed with a propagated passive send, so put
		 * @from in R_RECV and return to partner instead.
		 */
		ipc_recv_half(from, from_utcb);
	}

	if(xferto_at > 0 && IS_IPC(from->status)) {
		from->wakeup_time = xferto_at;
		sq_update_thread(from);
	}
}


/* add `w' to redir_hash under `holdup', then remove `w' from sendwait_hash
 * and free it. this makes sure that a held passive send can be restarted
 * through the `holdup' redirector's active receive when the ultimate
 * recipient goes to passive receive.
 *
 * TODO: this function is quite similar to what other code that handles
 * redir_wait does.
 */
static void convert_to_redirwait(struct ipc_wait *w, L4_ThreadId_t holdup)
{
	assert(!CHECK_FLAG(w->thread->flags, TF_REDIR_WAIT));
	assert(w->thread->status == TS_SEND_WAIT);

	w->thread->u1.waited_redir = holdup;
	w->thread->flags |= TF_REDIR_WAIT;
	if(likely(!L4_IsNilThread(holdup))) {
		/* TODO: handle malloc fail */
		htable_add(&redir_wait, hash_waited_redir(w->thread, NULL),
			w->thread);
	}
	htable_del(&sendwait_hash, int_hash(w->dest_tid.raw), w);
	kmem_cache_free(ipc_wait_slab, w);
}


static struct ipc_wait *find_global_sender(bool *valid_p, struct thread *self)
{
	assert(L4_IsGlobalId(self->ipc_from));

	size_t hash = int_hash(self->id);
	struct htable_iter it;
	struct ipc_wait *w;
	*valid_p = true;
	if(self->ipc_from.raw != L4_anythread.raw) {
		/* LTID conversion & validity test. */
		struct thread *ft = thread_find(self->ipc_from.raw);
		if(ft == NULL || ft->id != self->ipc_from.raw) {
			*valid_p = false;
			return NULL;
		}
		L4_ThreadId_t ltid = tid_return(self, ft);

		/* find the IPC waiter. it may be from @ft, or a propagator. */
		for(w = htable_firstval(&sendwait_hash, &it, hash);
			w != NULL;
			w = htable_nextval(&sendwait_hash, &it, hash))
		{
			if(w->dest_tid.raw == self->id
				&& (w->send_tid.raw == ft->id
					|| w->send_tid.raw == ltid.raw))
			{
				break;
			}
		}

		L4_ThreadId_t holdup;
		if(w != NULL
			&& will_redirect(w->thread, self)
			&& !is_redir_ready(&holdup, w->thread))
		{
			TRACE_REDIR("IPC: targeted receive held by %lu:%lu\n",
				L4_ThreadNo(holdup), L4_Version(holdup));
			/* the caller might go to sleep on passive receive, so the passive
			 * sender should be made resumable by a redirector's action.
			 */
			convert_to_redirwait(w, holdup);
			w = NULL;
		}
	} else {
		for(w = htable_firstval(&sendwait_hash, &it, hash);
			w != NULL;
			w = htable_nextval(&sendwait_hash, &it, hash))
		{
			if(w->dest_tid.raw != self->id) continue;

			L4_ThreadId_t holdup;
			if(will_redirect(w->thread, self)
				&& !is_redir_ready(&holdup, w->thread))
			{
				TRACE_REDIR("IPC: global anythread receive held by %lu:%lu\n",
					L4_ThreadNo(holdup), L4_Version(holdup));
				convert_to_redirwait(w, holdup);
			} else {
				/* got one. (TODO: collect them instead and pick the one with
				 * highest priority?)
				 */
				break;
			}
		}
	}

	if(w != NULL) htable_delval(&sendwait_hash, &it);
	return w;
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
			struct thread *dest = thread_find(cand->ipc_to.raw);
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
 * be set from retval->id.
 */
static struct ipc_wait *find_local_sender(bool *valid_p, struct thread *self)
{
	assert(!L4_IsNilThread(self->ipc_from));
	assert(L4_IsLocalId(self->ipc_from));

	struct ipc_wait *w;
	struct htable_iter it;
	size_t hash = int_hash(self->id);
	*valid_p = true;
	if(self->ipc_from.raw != L4_anylocalthread.raw) {
		/* a particular LTID either exists, or doesn't. */
		struct thread *loc = space_find_local_thread(self->space,
			self->ipc_from.local);
		if(loc == NULL) {
			*valid_p = false;
			return NULL;
		}

		/* ... still need to find & remove the ipc_wait, though.
		 * (FIXME: this is inelegant. fix it by finding the right thread using
		 * a per-space thing.)
		 */
		L4_ThreadId_t loc_tid = get_local_id(loc);
		for(w = htable_firstval(&sendwait_hash, &it, hash);
			w != NULL;
			w = htable_nextval(&sendwait_hash, &it, hash))
		{
			if(w->dest_tid.raw != self->id) {
				/* hash miss. */
				continue;
			}

			if(w->thread == loc && w->send_tid.raw == loc_tid.raw) {
				/* nonpropagated passive send. accept immediately. */
				break;
			}

			if(w->send_tid.raw == self->ipc_from.raw) {
				struct thread *vs = resolve_tid_spec(self->space,
					w->send_tid);
				if(vs != NULL) {
					/* FIXME: check that the propagation predicate still
					 * holds
					 */
					break;
				}
			}
		}
	} else {
		/* anylocalthread side. */
		/* (TODO: use a per-space list of IPC senders, or something.) */
		for(w = htable_firstval(&sendwait_hash, &it, hash);
			w != NULL;
			w = htable_nextval(&sendwait_hash, &it, hash))
		{
			/* it only has to look like a local sender. propagation is
			 * fine, too.
			 */
			if(w->dest_tid.raw == self->id && L4_IsLocalId(w->send_tid)) {
				break;
			}
		}
	}

	if(w != NULL) htable_delval(&sendwait_hash, &it);
	assert(w == NULL || L4_IsLocalId(w->send_tid));
	return w;
}


/* postcond: !@retval || @self->status \in {READY, R_RECV, STOPPED} */
bool ipc_recv_half(struct thread *self, void *self_utcb)
{
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
			set_ipc_return_regs(&self->ctx, self, utcb);
			if(self->status == TS_RUNNING) self->status = TS_READY;
			assert(IS_READY(self->status));
			return true;
		}
	}

	/* find waiting IPC. */
	struct ipc_wait *w;
	struct thread *r_sender = NULL;
	if(L4_IsLocalId(self->ipc_from)) {
		assert(self->ipc_from.raw != L4_anythread.raw);
		bool valid;
		w = find_local_sender(&valid, self);
		if(!valid) goto err_no_partner;
	} else if(CHECK_FLAG(self->flags, TF_REDIR)
		&& (r_sender = find_redir_sender(self)) != NULL)
	{
		TRACE_REDIR("IPC: found redir_sender(%lu:%lu) in %lu:%lu\n",
			TID_THREADNUM(r_sender->id), TID_VERSION(r_sender->id),
			TID_THREADNUM(self->id), TID_VERSION(self->id));
		htable_del(&redir_wait, hash_waited_redir(r_sender, NULL), r_sender);
		/* synthesize ipc_wait. (slab allocation is fast enough for this.) */
		w = kmem_cache_alloc(ipc_wait_slab);
		*w = (struct ipc_wait){
			.dest_tid.raw = self->id,
			.send_tid = tid_return(self, r_sender),
			.thread = r_sender,
		};
	} else {
		assert(self->ipc_from.raw != L4_anylocalthread.raw);
		bool valid;
		w = find_global_sender(&valid, self);
		if(!valid && !is_interrupt(self->ipc_from)) {
			/* some things IPC can't relay. for everything else, there's
			 * passive receive.
			 */
			assert(self->ipc_from.raw != L4_anythread.raw);
			goto err_no_partner;
		}
	}
	assert(w == NULL
		|| (!CHECK_FLAG(w->thread->space->flags, SF_REDIRECT)
			|| w->thread->space->redirector != NULL));

	if(w == NULL) {
		TRACE("%s: passive receive to %lu:%lu (waiting on %lu:%lu)\n", __func__,
			TID_THREADNUM(self->id), TID_VERSION(self->id),
			TID_THREADNUM(self->ipc_from.raw), TID_VERSION(self->ipc_from.raw));
		bool old_wakeup = self->status == TS_R_RECV;
		self->status = TS_RECV_WAIT;

		if(self->ipc != NULL && self->ipc->xferto_at > 0) {
			if(ksystemclock() >= self->ipc->xferto_at) {
				/* this happens when @self is a xfer pagefault sender and that
				 * IPC succeeded passively, putting @self in TS_R_RECV,
				 * causing schedule() to call ipc_recv_half() on it after
				 * xferto_at has passed.
				 */
				ipc_xfer_timeout(self->ipc);
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
			sq_update_thread(self);
			assert(self->wakeup_time == wakeup_at(L4_Never)
				|| self->wakeup_time > ksystemclock());
			if(self->wakeup_time == 0) {
				/* timeout. */
				set_ipc_error_thread(self, (1 << 1) | 1);
			}
		} else {
			/* receive-only IPC, such as L4_Wait(), gets its timeout here. */
			thread_sleep(self, self->recv_timeout);
			if(self->status == TS_READY) {
				/* instant timeout. */
				assert(self->recv_timeout.raw == L4_ZeroTime.raw);
				set_ipc_error_thread(self, (1 << 1) | 1);
			}
		}
		return false;
	} else if(CHECK_FLAG(w->thread->space->flags, SF_REDIRECT)
		&& self->space != w->thread->space
		&& self->space != w->thread->space->redirector->space)
	{
		/* active receive through redirector. */
		struct thread *from = w->thread;
		TRACE_REDIR("IPC: redirecting active receive from %lu:%lu\n",
			TID_THREADNUM(from->id), TID_VERSION(from->id));
		kmem_cache_free(ipc_wait_slab, w);

		L4_ThreadId_t dummy;
		assert(is_redir_ready(&dummy, w->thread));

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
		} else if(!kernel_preempt_pending) {
			/* IPC didn't complete, and no pre-emption occurs. try again. */
			sq_remove_thread(self);
			self->status = TS_RUNNING;
			return ipc_recv_half(self, self_utcb);
		}

		/* pre-empted. stay in R_RECV and don't complete. */
		assert(self->status == TS_R_RECV);
		return false;
	} else {
		/* active receive */
		struct thread *from = w->thread;

		TRACE("%s: active receive from %lu:%lu actual %lu:%lu (to %lu:%lu)\n",
			__func__,
			TID_THREADNUM(w->send_tid.raw), TID_VERSION(w->send_tid.raw),
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
			return false;
		} else if(n < 0) {
			assert(n == -EFAULT);
			assert(IS_IPC(self->status));
			/* FIXME: reinsert or free `w' */
			return false;
		}

		/* successful active receive -> READY. */
		self->status = TS_READY;
		if(self->wakeup_time > 0) {
			self->wakeup_time = 0;
			sq_update_thread(self);
		}

		/* whodunnit */
		self->ipc_from = w->send_tid;
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
		}
		kmem_cache_free(ipc_wait_slab, w);

		if(unlikely(IS_KERNEL_THREAD(from))) {
			/* kernel threads do the send/receive phases as control flow. */
			thread_wake(from);
		} else {
			/* userspace threads operate via a state machine. */
			if(L4_IsNilThread(from->ipc_from)) {
				/* no receive phase. */
				set_ipc_return_thread(from, from_utcb);
				thread_wake(from);
			} else {
				/* (the only special thread state transition in this
				 * module.)
				 */
				from->status = TS_R_RECV;
				from->wakeup_time = wakeup_at(from->recv_timeout);
				sq_update_thread(from);
				might_preempt(from);
			}
		}
		if(!post_exn_ok(self)) {
			/* post-IPC exception hooks may start another IPC operation right
			 * away, so check this only in the ordinary path.
			 */
			assert(IS_READY(from->status)
				|| (CHECK_FLAG(from->flags, TF_HALT)
					&& from->status == TS_STOPPED));
		}

		return true;
	}

	assert(false);

err_no_partner:
	set_ipc_error(self_utcb, 5);
	assert(self->status == TS_RUNNING);
	self->status = TS_READY;	/* failed active receive -> READY. */
	return false;
}


/* IPC in a kernel thread. string transfers are forbidden, enforced by
 * checking that BR0.s == 0 .
 *
 * TODO: move this into kth.c; it uses a kernel stack instead of the L4.X2 IPC
 * state machine.
 */
L4_MsgTag_t kipc(
	L4_ThreadId_t to,
	L4_ThreadId_t *from_p,
	L4_Word_t timeouts)
{
	struct thread *current = get_current_thread();
	void *utcb = thread_get_utcb(current);
	assert(!CHECK_FLAG(L4_VREG(utcb, L4_TCR_BR(0)), 0x2));

	TRACE("%s: entered in %lu:%lu (to %lu:%lu)\n", __func__,
		TID_THREADNUM(current->id), TID_VERSION(current->id),
		TID_THREADNUM(to.raw), TID_VERSION(to.raw));

	current->ipc_from = *from_p;
	current->ipc_to = to;
	current->send_timeout.raw = timeouts >> 16;
	current->recv_timeout.raw = timeouts & 0xffff;

	L4_MsgTag_t tag = { .raw = 0 };		/* "no error" */
	if(likely(!L4_IsNilThread(to))) {
		struct thread *dest = resolve_tid_spec(current->space, to);
		if(dest == NULL) {
			set_ipc_error(utcb, 4);		/* send: no partner */
			tag.raw = L4_VREG(utcb, L4_TCR_MR(0));
			return tag;
		}

		if(!ipc_send_half(current, utcb, dest)) {
			if(current->status == TS_SEND_WAIT) {
				/* passive send. */
				thread_sleep(current, current->send_timeout);
				TRACE("%s: passive send, scheduling\n", __func__);
				schedule();
			}
			tag.raw = L4_VREG(utcb, L4_TCR_MR(0));
			if(L4_IpcFailed(tag)) {
				assert(current->status == TS_RUNNING);
				TRACE("%s: error %#lx\n",
					__func__, L4_VREG(utcb, L4_TCR_ERRORCODE));
				return tag;
			}
		}
	}
	/* TODO: check kth preemption? */
	assert(current->status != TS_XFER);

	if(likely(!L4_IsNilThread(current->ipc_from))) {
		if(!ipc_recv_half(current, utcb)) {
			if(current->status == TS_RECV_WAIT) {
				/* passive receive.
				 *
				 * TODO: implement switching right into the other thread.
				 */
				thread_sleep(current, current->recv_timeout);
				TRACE("%s: passive receive, scheduling\n", __func__);
				schedule();
			}
		}
		/* TODO: check kth preemption? */
		tag.raw = L4_VREG(utcb, L4_TCR_MR(0));
		if(likely(L4_IpcSucceeded(tag))) *from_p = current->ipc_from;
	}
	assert(current->status != TS_XFER);
	assert(!IS_READY(current->status));

	TRACE("%s: returning\n", __func__);

	return tag;
}


SYSCALL L4_Word_t sys_ipc(
	void *utcb,
	L4_ThreadId_t to,
	L4_ThreadId_t from,
	L4_Word_t timeouts)
{
	struct thread *current = get_current_thread();
	TRACE("%s: called in %lu:%lu; to %#lx, from %#lx, timeouts %#lx\n",
		__func__, TID_THREADNUM(current->id), TID_VERSION(current->id),
		to.raw, from.raw, timeouts);

	/* parameter validation. */
	if(!L4_IsNilThread(to)
#ifdef ENABLE_SELFTEST
		/* special permission for the root task for kernel self-tests. */
		&& (!CHECK_FLAG(current->space->flags, SF_PRIVILEGE)
			|| to.raw != s0_pager->id)
#endif
		&& unlikely(to.raw == L4_anythread.raw
			|| to.raw == L4_anylocalthread.raw
			/* only sigma0 may IPC to the kth range, and then only to its pager
			 * thread.
			 */
			|| (!L4_IsLocalId(to)
				&& L4_ThreadNo(to) < first_user_threadno()
				&& L4_ThreadNo(to) > last_int_threadno()
				&& likely(current->space != sigma0_space
					|| to.raw != s0_pager->id))))
	{
		set_ipc_error(utcb, 4);		/* non-existing partner, send phase */
		return L4_nilthread.raw;
	}
	if(unlikely(L4_IsGlobalId(from)
		&& L4_ThreadNo(from) > last_int_threadno()
		&& L4_ThreadNo(from) < first_user_threadno()
		&& likely(current->space != sigma0_space
			|| from.raw != s0_pager->id)))
	{
		set_ipc_error(utcb, 5);		/* non-existing partner, receive phase */
		return L4_nilthread.raw;
	}

	current->ipc_to = to;
	current->ipc_from = from;
	current->send_timeout.raw = timeouts >> 16;
	current->recv_timeout.raw = timeouts & 0xffff;

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
			return L4_nilthread.raw;
		}
	}
	if(!L4_IsNilThread(to)) {
		/* send phase. */
		TRACE("%s: IPC send phase.\n", __func__);
		if(!ipc_send_half(current, utcb, dest)) {
			/* didn't complete. either READY, SEND_WAIT, STOPPED, or XFER. */
			assert(current->status != TS_STOPPED);	/* (would be weird.) */
			if(IS_IPC(current->status)) {
				/* ongoing IPC. */
				return_to_scheduler();
			} else {
				/* error condition in send phase. */
				goto err_exit;
			}
		} else if(check_preempt()) {
			/* TODO: do partner scheduling before preemption checks. */
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
				set_ipc_return_regs(&current->ctx, current, utcb);
			}
			return_to_scheduler();
		}
	}

	if(!L4_IsNilThread(from) && current->status != TS_SEND_WAIT) {
		/* receive phase. */
		TRACE("%s: IPC receive phase.\n", __func__);
		if(ipc_recv_half(current, utcb)) {
			if(check_preempt() && IS_READY(current->status)) {
				set_ipc_return_regs(&current->ctx, current, utcb);
				return_to_scheduler();
			}

			/* TODO: alter ipc_recv_half() to not change status to TS_READY
			 * when about to return true
			 */
			current->status = TS_RUNNING;
		} else if(IS_IPC(current->status)) {
			/* ongoing IPC. */
			return_to_scheduler();
		} else {
			/* error condition in receive phase. */
			goto err_exit;
		}
	}

	/* successful exit. */
	assert(current->status == TS_RUNNING);
	assert(!IS_KERNEL_THREAD(current));
	return current->ipc_from.raw;

err_exit:
	assert(CHECK_FLAG(L4_VREG(utcb, L4_TCR_MR(0)), 0x8000));
	assert(L4_VREG(utcb, L4_TCR_ERRORCODE) != 0);
	current->status = TS_RUNNING;
	return L4_nilthread.raw;
}
