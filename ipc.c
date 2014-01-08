
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


/* these are kept in sendwait_hash in a multiset way, i.e. use
 * htable_firstval() and so forth to scan.
 */
struct ipc_wait
{
	L4_ThreadId_t dest_tid;		/* global ID. key, ptr in sendwait_hash */
	L4_ThreadId_t send_tid;		/* thread->id or vs, may be local */
	struct thread *thread;		/* (actual) sender thread */
	L4_ThreadId_t ir_tid;		/* IntendedReceiver, or nil when not */
};


static size_t hash_threadid(const void *tid, void *priv);


static struct kmem_cache *ipc_wait_slab = NULL;
static struct htable sendwait_hash = HTABLE_INITIALIZER(
	sendwait_hash, &hash_threadid, NULL);


static size_t hash_threadid(const void *tid, void *priv) {
	const L4_ThreadId_t *p = tid;
	return int_hash(p->raw);
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
	assert(IS_READY(from->status));
	thread_ipc_fail(to);
	assert(IS_READY(to->status));
}


/* this function is called from the scheduler when it finds that both sides of
 * a paused string transfer are ready to proceed. it should write both sides'
 * IPC return registers.
 */
bool ipc_resume(struct thread *t, bool *preempt_p)
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
		*preempt_p = false;
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

	*preempt_p = false;		/* FIXME */
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
		assert(peer->ipc_to.raw == with_tid.raw);
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


/* called from thread_ipc_fail() and from the deleting ThreadControl. takes
 * care of the sendwait_hash entry. leaves errorcode setting to caller's
 * caller.
 */
void cancel_ipc_from(struct thread *t)
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


/* this is sort of like resolve_tid_spec(), but in reverse. */
static L4_ThreadId_t tid_return(struct thread *self, struct thread *t)
{
	if(self->space == t->space) {
		return get_local_id(t);
	} else {
		return (L4_ThreadId_t){ .raw = t->id };
	}
}


static inline bool is_interrupt(L4_ThreadId_t tid) {
	return L4_Version(tid) == 1
		&& L4_ThreadNo(tid) <= last_int_threadno();
}


/* whether "tip" ends up redirecting for "base". */
static bool has_redir_chain(struct thread *base, struct thread *tip)
{
	do {
		L4_ThreadId_t r = base->space->redirector;
		if(r.raw == L4_anythread.raw) return false;
		if(r.raw == tip->id) return true;
		base = thread_find(r.raw);
		if(base == NULL || base->id != r.raw) return false;
	} while(base != NULL);
	return false;
}


/* postcond: !@retval || @self->status \in {READY, R_RECV, STOPPED} */
static bool ipc_send_half(
	struct thread *self,
	void *self_utcb,
	bool *preempt_p)
{
	assert(preempt_p != NULL);
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

	if(tag->raw == 0
		&& CHECK_FLAG(self->flags, TF_INTR)
		&& is_interrupt(self->ipc_to)
		/* redirected tasks can never reply to interrupts. */
		&& self->space->redirector.raw == L4_anythread.raw)
	{
		/* eat an interrupt reply. */
		err_code = int_clear(L4_ThreadNo(self->ipc_to), self);
		if(err_code != 0) goto error;
		else {
			*preempt_p = false;
			return true;
		}
	}

	struct thread *dest = resolve_tid_spec(self->space, self->ipc_to);
	if(dest == NULL) {
		TRACE("%s: can't find peer %lu:%lu\n", __func__,
			TID_THREADNUM(self->ipc_to.raw), TID_VERSION(self->ipc_to.raw));
		self->status = TS_READY;
		goto no_partner;
	}

	bool redirected = false;
	struct thread *saved_dest = NULL;
	tag->X.flags &= 0x1;	/* keep the propagate flag */
	if(self->space != dest->space
		&& self->space->redirector.raw != L4_anythread.raw)
	{
		assert(L4_IsGlobalId(self->space->redirector));
		/* TODO: this entails a thread lookup every time that a redirected
		 * task needs to do IPC with a non-local thread. that's sort of bad.
		 * instead there should be a redir_thread pointer, and the thread
		 * marked with TF_REDIR; the pointer is reset when that thread is
		 * deleted. (this scales linearly wrt all threads in the system.)
		 */
		struct thread *red = thread_find(self->space->redirector.raw);
		if(red == NULL || red->id != self->space->redirector.raw) {
			/* FIXME: this should put the sender into TF_HALT | TF_RWAIT,
			 * which should be resumed from in sys_spacecontrol() when a valid
			 * redirector is reinstated.
			 */
			panic("invalid redirector!");
		}

		if(dest->space != red->space) {
			tag->X.flags |= 0x2;		/* set redirect bit */
			redirected = true;
			saved_dest = dest;
			dest = red;
			/* redirect a closed IPC's receive phase. this'll be re-redirected
			 * to saved_dest if the redirector passes the IPC as-is, or
			 * replied to if the redirector returns a rejection.
			 *
			 * NOTE: this is not explicitly specified by L4.X2, but the
			 * wording also doesn't forbid it; and this seems like the most
			 * reasonable approach in any case. (it also fits the "redirector
			 * chain" pattern; if not, those'd only work for chaining
			 * pass/don't policies together -- and that's bizarre.)
			 *
			 * rerererere.
			 */
			if(self->ipc_from.raw == saved_dest->id) {
				self->ipc_from.raw = red->id;
			}
		}
	}
	assert(!redirected || saved_dest != NULL);

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
	const bool active_cond = dest->ipc_from.raw == L4_anythread.raw
		|| dest->ipc_from.raw == self_id.raw
		|| dest->ipc_from.raw == self_lid.raw
		|| (dest->ipc_from.raw == L4_anylocalthread.raw
			&& dest->space == sender->space);

	/* override TS_R_RECV? */
	int status = dest->status;
	if(active_cond && status == TS_R_RECV) {
		if(now_us >= dest->wakeup_time) {
			/* nah, time the peer out instead. */
			dest->status = TS_RECV_WAIT;	/* required by thread_wake() */
			set_ipc_error_thread(dest, (1 << 1) | 1);
			thread_wake(dest);
			status = dest->status;		/* reload after thread_wake() */
			TRACE("%s: r_recv override timeout\n", __func__);
		} else {
			/* yep */
			TRACE("%s: override r_recv\n", __func__);
			status = TS_RECV_WAIT;
		}
	}

	if(status == TS_RECV_WAIT
		&& active_cond
		&& (dest->wakeup_time == ~(uint64_t)0u
			|| dest->wakeup_time > now_us))
	{
		/* active send */
		TRACE("%s: active send to %lu:%lu (from %lu:%lu, actual %lu:%lu)\n", __func__,
			TID_THREADNUM(dest->id), TID_VERSION(dest->id),
			TID_THREADNUM(self_id.raw), TID_VERSION(self_id.raw),
			TID_THREADNUM(self->id), TID_VERSION(self->id));

		void *dest_utcb = thread_get_utcb(dest);
		int n = do_ipc_transfer(self, self_utcb, dest, dest_utcb);
		if(n > 0) {
			const L4_Word_t error = n;
			int code = (error & 0xe) >> 1;
			if(code >= 4) {
				/* mutual error; signal to partner also. */
				if(likely(!post_exn_fail(dest))) {
					set_ipc_error_thread(dest, error | 1);
				}
				thread_wake(dest);
				*preempt_p = dest->pri > self->pri;
			} else {
				*preempt_p = false;
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
			*preempt_p = false;
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
			assert(dest->ipc != NULL || IS_READY(dest->status));
			/* timeout should've happened in post_exn_ok() already */
			assert(dest->ipc == NULL
				|| dest->ipc->xferto_at == 0
				|| dest->ipc->xferto_at > ksystemclock());
		}
		*preempt_p = dest->pri > self->pri && IS_READY(dest->status);
		TRACE("%s: preempt=%s; d %d/%s, s %d/%s\n", __func__,
			btos(*preempt_p), dest->pri, sched_status_str(dest),
			self->pri, sched_status_str(self));

		if(L4_IsNilThread(self->ipc_from)) {
			/* send-only, and done. */
			self->status = TS_READY;
		} else {
			/* indicate active receive. set wakeup time from right now. */
			self->status = TS_R_RECV;
			self->wakeup_time = wakeup_at(self->recv_timeout);
			sq_update_thread(self);
		}
		assert(IS_READY(self->status));
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
			.ir_tid = redirected
				? tid_return(dest, saved_dest)
				: L4_nilthread,
		};
		htable_add(&sendwait_hash, int_hash(w->dest_tid.raw), w);

		self->status = TS_SEND_WAIT;
		thread_sleep(self, self->send_timeout);

		return false;
	} else {
		/* instant timeout. */
		set_ipc_error(self_utcb, (1 << 1) | 0);
		set_ipc_return_thread(self, self_utcb);
		self->status = TS_READY;
		return false;
	}
	assert(false);

no_partner:
	err_code = 2;

error:
	set_ipc_error(self_utcb, (err_code << 1) | 0);
	self->status = TS_READY;
	*preempt_p = false;
	return false;
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
	bool preempt = false;
	if(ipc_send_half(from, from_utcb, &preempt)
		&& !preempt
		&& from->status == TS_R_RECV)
	{
		ipc_recv_half(from, from_utcb, &preempt);
		/* NOTE: preempt is discarded. if @from is the currently running
		 * thread, this can have the effect of failing to pre-empt @from when
		 * the IPC from @to is satisfied by a propagated passive send.
		 */
	}

	if(xferto_at > 0 && IS_IPC(from->status)) {
		from->wakeup_time = xferto_at;
		sq_update_thread(from);
	}
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

		/* find the IPC waiter. it may be from the @ft, or a propagator. */
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
	} else {
		for(w = htable_firstval(&sendwait_hash, &it, hash);
			w != NULL;
			w = htable_nextval(&sendwait_hash, &it, hash))
		{
			if(w->dest_tid.raw == self->id) break;
		}
	}

	if(w != NULL) htable_delval(&sendwait_hash, &it);
	return w;
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
		for(w = htable_firstval(&sendwait_hash, &it, hash);
			w != NULL;
			w = htable_nextval(&sendwait_hash, &it, hash))
		{
			if(w->thread == loc && w->dest_tid.raw == self->id) {
				break;
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
bool ipc_recv_half(
	struct thread *self,
	void *self_utcb,
	bool *preempt_p)
{
	assert(preempt_p != NULL);

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
			*preempt_p = false;
			return true;
		}
	}

	/* find waiting IPC. */
	struct ipc_wait *w;
	if(L4_IsLocalId(self->ipc_from)) {
		assert(self->ipc_from.raw != L4_anythread.raw);
		bool valid;
		w = find_local_sender(&valid, self);
		if(!valid) goto err_no_partner;
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
				assert(IS_READY(self->status));
				*preempt_p = false;
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
			}
			set_ipc_error(self_utcb, error | 1);
			assert(self->status == TS_RUNNING);
			self->status = TS_READY;	/* failed active receive -> READY. */
			return false;
		} else if(n < 0) {
			assert(n == -EFAULT);
			assert(IS_IPC(self->status));
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
		L4_MsgTag_t tag = { .raw = L4_VREG(self_utcb, L4_TCR_MR(0)) };
		if(L4_IpcPropagated(tag)) {
			L4_VREG(self_utcb, L4_TCR_VA_SENDER) = tid_return(self, from).raw;
		}
		if(L4_IpcRedirected(tag)) {
			L4_VREG(self_utcb, L4_TCR_INTENDEDRECEIVER) = w->ir_tid.raw;
		}
		kmem_cache_free(ipc_wait_slab, w);

		if(unlikely(IS_KERNEL_THREAD(from))) {
			/* kernel threads do the send/receive phases as control flow. */
			thread_wake(from);
		} else {
			/* userspace threads operate via a state machine. */
			if(L4_IsNilThread(from->ipc_from)) {
				/* no receive phase. */
				thread_wake(from);
			} else {
				/* (the only special thread state transition in this
				 * module.)
				 */
				from->status = TS_R_RECV;
				from->wakeup_time = wakeup_at(from->recv_timeout);
				sq_update_thread(from);
			}
		}
		if(!post_exn_ok(self)) {
			/* post-IPC exception hooks may start another IPC operation right
			 * away, so check this only in the ordinary path.
			 */
			assert(IS_READY(from->status));
		}
		*preempt_p = self == get_current_thread()
			&& preempted_by(self, task_switch_time * 1000, from);
		if(*preempt_p) {
			assert(self->status == TS_RUNNING || self->status == TS_R_RECV
				|| self->status == TS_READY);
			self->status = TS_READY;
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
	bool preempt = false;
	if(likely(!L4_IsNilThread(to)) && !ipc_send_half(current, utcb, &preempt)) {
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
	assert(!preempt);		/* kthreads aren't preemptable (TODO) */
	assert(current->status != TS_XFER);

	if(likely(!L4_IsNilThread(current->ipc_from)) && !preempt) {
		if(!ipc_recv_half(current, utcb, &preempt)) {
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
		/* TODO: kernel threads aren't preemptable quite yet. */
		assert(!preempt);
		tag.raw = L4_VREG(utcb, L4_TCR_MR(0));
		if(likely(L4_IpcSucceeded(tag))) *from_p = current->ipc_from;
	}
	assert(current->status != TS_XFER);
	assert(!IS_READY(current->status));

	TRACE("%s: returning\n", __func__);

	return tag;
}


static void ipc(struct thread *current, void *utcb, bool *preempt_p)
{
	assert(utcb != NULL);
	assert(preempt_p != NULL);

	*preempt_p = false;

	/* send phase. */
	if(!L4_IsNilThread(current->ipc_to)) {
		TRACE("%s: IPC send phase.\n", __func__);
		if(!ipc_send_half(current, utcb, preempt_p)) {
			/* error case. */
			goto end;
		}
	}
	if(!L4_IsNilThread(current->ipc_from)
		&& current->status != TS_SEND_WAIT
		&& !*preempt_p)
	{
		/* receive phase. */
		TRACE("%s: IPC receive phase.\n", __func__);
		ipc_recv_half(current, utcb, preempt_p);
		assert(current->status == TS_READY
			|| current->status == TS_RECV_WAIT);
	}

end:
	if(current->status == TS_READY && !*preempt_p) {
		current->status = TS_RUNNING;
	}
}


SYSCALL L4_ThreadId_t sys_ipc(
	void *utcb,
	L4_ThreadId_t to,
	L4_ThreadId_t from,
	L4_Word_t timeouts)
{
	struct thread *current = get_current_thread();
	TRACE("%s: called in %lu:%lu; to %#lx, from %#lx, timeouts %#lx\n",
		__func__, TID_THREADNUM(current->id), TID_VERSION(current->id),
		to.raw, from.raw, timeouts);

	/* parameter validation.
	 *
	 * TODO: catch attempts to IPC to a kernel thread from within a
	 * non-privileged space.
	 */
	if(unlikely(to.raw == L4_anythread.raw
		|| to.raw == L4_anylocalthread.raw))
	{
		set_ipc_error(utcb, 4);		/* non-existing partner, send phase */
		return L4_nilthread;
	}
	if(unlikely(L4_IsGlobalId(from)
		&& L4_ThreadNo(from) > last_int_threadno()
		&& L4_ThreadNo(from) < first_user_threadno()))
	{
		set_ipc_error(utcb, 5);		/* non-existing partner, receive phase */
		return L4_nilthread;
	}

	bool preempt = false;
	current->ipc_to = to;
	current->ipc_from = from;
	current->send_timeout.raw = timeouts >> 16;
	current->recv_timeout.raw = timeouts & 0xffff;
	ipc(current, utcb, &preempt);
	if(preempt && IS_READY(current->status)) {
		/* would return, but was pre-empted */
		assert(!IS_KERNEL_THREAD(current));		/* >implying */
		TRACE("%s: scheduling (pre-empted)\n", __func__);
		set_ipc_return_regs(&current->ctx, current, utcb);
		return_to_scheduler();
		assert(false);
	} else if(IS_IPC(current->status)) {
		/* IPC ongoing. */
		TRACE("%s: scheduling (ongoing IPC)\n", __func__);
		/* TODO: schedule the waitee */
		return_to_scheduler();
		assert(false);
	} else {
		/* return from IPC at once. */
		TRACE("%s: returning to caller\n", __func__);
		assert(current->status == TS_RUNNING);
		assert(!IS_KERNEL_THREAD(current));
	}

	return current->ipc_from;
}
