
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
#include <l4/ipc.h>

#include <ukernel/misc.h>
#include <ukernel/trace.h>
#include <ukernel/slab.h>
#include <ukernel/thread.h>
#include <ukernel/sched.h>
#include <ukernel/space.h>
#include <ukernel/ipc.h>


/* for "active/passive send/receive" prints */
#define TRACE(fmt, ...) TRACE_MSG(TRID_IPC, fmt, __VA_ARGS__)


/* these are kept in sendwait_hash in a multiset way, i.e. use
 * htable_firstval() and so forth to scan.
 */
struct ipc_wait
{
	L4_ThreadId_t dest_tid;		/* key, ptr offset in sendwait_hash */
	L4_ThreadId_t send_tid;		/* vs when propagated */
	struct thread *thread;		/* sender thread (avoids lookup) */
};


static struct kmem_cache *ipc_wait_slab = NULL;
static struct htable sendwait_hash;


static inline void set_ipc_error(void *utcb, L4_Word_t ec)
{
	L4_VREG(utcb, L4_TCR_ERRORCODE) = ec;
	L4_VREG(utcb, L4_TCR_MR(0)) = ((L4_MsgTag_t){ .X.flags = 0x8 }).raw;
	L4_VREG(utcb, L4_TCR_MR(1)) = 0;
	L4_VREG(utcb, L4_TCR_MR(2)) = 0;
}


/* NOTE: this treats I/O ports as inclusive. this is good enough; exclusive
 * (i.e. refusing to pass ranges that aren't all present) would just be
 * another hassle.
 *
 * TODO: this doesn't fail atomically.
 * ALSO TODO: nor does it handle grantitems.
 */
static int apply_io_mapitem(
	struct thread *source,
	const void *s_base,
	struct thread *dest,
	void *d_base,
	L4_MapItem_t m,
	L4_Fpage_t recvwnd)
{
	if(source->space->tss == NULL) return 0;	/* no-op */

	assert(L4_IsIoFpage(m.X.snd_fpage));
	const uint8_t *src_bm = (void *)&source->space->tss[1];
	int run = 0;
	L4_Word_t start = 0, first_port = L4_IoFpagePort(recvwnd),
		last_port = first_port + L4_IoFpageSize(recvwnd) - 1,
		max_port = (source->space->tss_len - sizeof(struct tss)) * 8 - 1;
	for(L4_Word_t port = L4_IoFpagePort(m.X.snd_fpage),
				  end = MIN(L4_Word_t, max_port,
					port + L4_IoFpageSize(m.X.snd_fpage));
		port < end;
		port++)
	{
		int byte = port >> 3, bit = port & 0x7;
		bool skip = CHECK_FLAG(src_bm[byte], 1 << bit)
			|| port < first_port || port > last_port;
		if(skip && run > 0) {
			if(!space_add_ioperm(dest->space, start, run)) {
				return -ENOMEM;
			}
			run = 0;
		} else if(!skip) {
			if(run == 0) start = port;
			run++;
		}
	}
	if(run > 0 && !space_add_ioperm(dest->space, start, run)) {
		return -ENOMEM;
	}

	return 0;
}


/* FIXME: this function should indicate to the caller that active TLBs for the
 * destination space may need to be flushed, or at least invalidated in the
 * regions given in a buffer of some kind (if provided by the caller; it may
 * flip into "all-flushing" mode at some point).
 *
 * the fourth bit in the return value, perhaps? and a size_t * & a L4_Fpage_t
 * buffer for 10 items (up to a megabyte). the fifth bit as a "better off
 * flushing it all" hint.
 *
 * fuck, this interface is getting complicated just because of the snd_base
 * and rcvwindow computation. maybe it'd be better off in a function of its
 * own.
 *
 *
 * anyway, on failure this returns negative errno.
 */
static int apply_mapitem(
	struct thread *source,
	const void *s_base,
	struct thread *dest,
	void *d_base,
	L4_MapItem_t m)
{
	L4_Fpage_t map_page = L4_MapItemSndFpage(m);
	bool is_grant;
	switch(m.X.__type) {
		case 0x5: is_grant = true; break;
		case 0x4: is_grant = false; break;
		default:
			/* neither mapitem or grantitem. skip it. */
			return 0;
	}

	/* no-ops */
	if(source->space == dest->space) return 0;
	if(L4_IsNilFpage(map_page)) return 0;

	L4_Fpage_t wnd = { .raw = L4_VREG(d_base, L4_TCR_BR(0)) };
	if(unlikely(L4_IsNilFpage(wnd))) {
		/* TODO: isn't this an error condition? */
		return 0;
	}

	if(unlikely(L4_IsIoFpage(map_page))) {
		if(!L4_IsIoFpage(wnd)) return 0;	/* again, error? */
		else return apply_io_mapitem(source, s_base, dest, d_base, m, wnd);
	}

	TRACE("mapping 0x%lx:0x%lx, sndbase 0x%lx, rcvwindow %#lx:%#lx (%s)\n",
		L4_Address(map_page), L4_Size(map_page),
		L4_MapItemSndBase(m), L4_Address(wnd), L4_Size(wnd),
		wnd.raw == L4_CompleteAddressSpace.raw ? "CompleteAddressSpace" : "<- that");

	if(wnd.raw == L4_CompleteAddressSpace.raw
		|| L4_Size(wnd) >= L4_MapItemSndBase(m) + L4_Size(map_page))
	{
		int given = mapdb_map_pages(&source->space->mapdb,
			&dest->space->mapdb, map_page,
			L4_Address(wnd) + L4_MapItemSndBase(m));
		if(is_grant && given != 0) {
			L4_Set_Rights(&map_page, given);
			mapdb_unmap_fpage(&source->space->mapdb, map_page, true, false,
				false);
			/* TODO: this revokes all access and destroys access bits.
			 * fortunately the latter will have been saved for child mappings
			 * by mapdb_unmap_fpage(), but for this one that's a problem.
			 *
			 * also there'll be a few invisible page faults when this bit is
			 * accessed again, which is silly.
			 */
			for(L4_Word_t addr = L4_Address(map_page);
				addr < L4_Address(map_page) + L4_Size(map_page);
				addr += PAGE_SIZE)
			{
				space_put_page(source->space, addr, 0, 0);
			}
			/* FIXME: move this out of the "one map item" function. */
			space_commit(source->space);
		}
		return given;
	} else {
		/* TODO */
		panic("apply_mapitem() can't handle trunc rcvwindow cases");
		return 0;
	}
}


static L4_Word_t do_typed_transfer(
	struct thread *source,
	const void *s_base,
	struct thread *dest,
	void *d_base,
	L4_MsgTag_t tag)
{
	int pos = tag.X.u + 1, last = tag.X.u + tag.X.t;
	while(pos <= last) {
		L4_Word_t w0 = L4_VREG(s_base, L4_TCR_MR(pos));
		switch(w0 & 0xe) {
			case 0x8:
			case 0xa: {
				/* MapItem (0x8), GrantItem (0xa) */
				if(unlikely(pos + 1 > last)) goto too_short;
				L4_MapItem_t m = {
					.raw = { w0, L4_VREG(s_base, pos + 1) },
				};
				int given = apply_mapitem(source, s_base, dest, d_base, m);
				if(unlikely(given == -ENOMEM)) {
					/* code = message overflow
					 * offset = MR# of mg/ item's first word
					 */
					return ((L4_Word_t)pos << 5) | (4ul << 1);
				}
				m.X.snd_fpage.X.b = 0;
				m.X.snd_fpage.X.rwx = given;
				m.X.C = (pos + 2 <= last);
				L4_VREG(d_base, pos) = m.raw[0];
				L4_VREG(d_base, pos + 1) = m.raw[1];
				pos += 2;
				break;
			}
			case 0xc:
				panic("ctrlxferitems not supported");
				break;
			case 0xe:
				/* FIXME: return an IPC error instead */
				panic("reserved map type");
				break;
			default:
				panic("can't handle string transfers yet");
				break;
		}
	}

	return 0;

too_short:
	/* FIXME: return a proper error */
	panic("not enough typed message words");
	return 0;
}


/* returns ErrorCode value, or 0 for success. */
static L4_Word_t do_ipc_transfer(
	struct thread *source,
	struct thread *dest)
{
	const void *s_base = thread_get_utcb(source);
	void *d_base = thread_get_utcb(dest);
	L4_MsgTag_t tag = { .raw = L4_VREG(s_base, L4_TCR_MR(0)) };
	L4_VREG(d_base, L4_TCR_MR(0)) = tag.raw;
	for(int i=0; i < tag.X.u; i++) {
		int reg = L4_TCR_MR(i + 1);
		L4_VREG(d_base, reg) = L4_VREG(s_base, reg);
	}

	if(tag.X.t == 0) return 0;
	else {
		return do_typed_transfer(source, s_base, dest, d_base, tag);
	}
}


static void set_ipc_return_regs(
	struct x86_exregs *regs,
	struct thread *current,
	void *utcb)
{
	assert(utcb == thread_get_utcb(current));
	regs->eax = current->ipc_from.raw;
	regs->esi = L4_VREG(utcb, L4_TCR_MR(0));
	regs->ebx = L4_VREG(utcb, L4_TCR_MR(1));
	regs->ebp = L4_VREG(utcb, L4_TCR_MR(2));
}


static void set_ipc_return_thread(struct thread *t)
{
	TRACE("%s: called for %skernel thread %lu:%lu\n", __func__,
		!IS_KERNEL_THREAD(t) ? "non-" : "", TID_THREADNUM(t->id),
		TID_VERSION(t->id));
	if(likely(!IS_KERNEL_THREAD(t))) {
		set_ipc_return_regs(&t->ctx, t, thread_get_utcb(t));
	}
}


void set_ipc_error_thread(struct thread *t, L4_Word_t ec)
{
	set_ipc_error(thread_get_utcb(t), ec);
	set_ipc_return_thread(t);
}


/* one thing that thread_ipc_fail() doesn't do. used by the deleting mode
 * of ThreadControl.
 *
 * FIXME: untested!
 */
void abort_waiting_ipc(struct thread *t, L4_Word_t errcode)
{
	/* fail those IPC operations that're waiting to send to this one. */
	struct htable_iter it;
	size_t hash = int_hash(t->id);
	errcode &= ~(L4_Word_t)1;		/* send-phase errors. */
	for(void *ptr = htable_firstval(&sendwait_hash, &it, hash);
		ptr != NULL;
		ptr = htable_nextval(&sendwait_hash, &it, hash))
	{
		struct ipc_wait *w = container_of(ptr, struct ipc_wait, dest_tid);
		if(w->dest_tid.raw != t->id) continue;

		assert(w->thread->status == TS_SEND_WAIT);
		assert(w->thread->ipc_to.raw == t->id);
		if(!post_exn_fail(w->thread)) {
			/* ordinary non-exception IPC. for exceptions, a silent return via
			 * the callback
			 */
			set_ipc_error_thread(w->thread, errcode);
		}
		thread_wake(w->thread);

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
		if(other->status != TS_RECV_WAIT || other->ipc_from.raw != t->id) {
			continue;
		}

		if(!post_exn_fail(other)) {
			set_ipc_error_thread(other, errcode);
		}
		thread_wake(other);
	}
}


/* returns true when the send phase has completed succesfully. false
 * otherwise; if the send phase doesn't immediately succeed, status changes to
 * TS_SEND_WAIT if the thread should sleep, and sets ErrorCode on error.
 *
 * TODO: this function is rather sub-optimal wrt thread_get_utcb(). the
 * pointers should really be grabbed once and passed as parameters to
 * do_ipc_transfer() etc.
 */
bool ipc_send_half(struct thread *self)
{
	assert(!L4_IsNilThread(self->ipc_to)
		&& self->ipc_to.raw != L4_anylocalthread.raw
		&& self->ipc_to.raw != L4_anythread.raw);

	struct thread *dest = thread_find(self->ipc_to.raw);
	if(unlikely(dest == NULL)) {
		TRACE("%s: can't find peer %lu:%lu\n", __func__,
			TID_THREADNUM(self->ipc_to.raw), TID_VERSION(self->ipc_to.raw));
		set_ipc_error(thread_get_utcb(self), 2 << 1 | 0);
		return false;
	}

	/* TODO: quicker tag access? maybe keep it next to ipc_to? */
	void *self_utcb = thread_get_utcb(self);
	L4_MsgTag_t tag = { .raw = L4_VREG(self_utcb, L4_TCR_MR(0)) };
	L4_ThreadId_t self_id = { .raw = self->id };
	bool propagated = false;
	if(CHECK_FLAG(tag.X.flags, 0x1)) {
		/* propagation (sender fakery). */
		L4_ThreadId_t vs_tid = {
			.raw = L4_VREG(self_utcb, L4_TCR_VA_SENDER),
		};
		struct thread *vs;
		if(L4_IsLocalId(vs_tid)) {
			vs = space_find_local_thread(self->space, vs_tid.local);
		} else {
			vs = thread_find(vs_tid.raw);
		}
		/* FIXME: also check interrupt propagation, redirector chain */
		if(vs != NULL && (self->space == vs->space
			|| self->space == dest->space))
		{
			self_id.raw = vs->id;
			propagated = true;
		}
	}

	/* override TS_R_RECV? */
	int status = dest->status;
	uint64_t now_us = read_global_timer() * 1000;
	if(status == TS_R_RECV && dest->ipc_from.raw == self_id.raw) {
		if(now_us >= dest->wakeup_time) {
			/* nah, time the peer out instead. */
			dest->status = TS_RECV_WAIT;	/* required by thread_wake() */
			set_ipc_error_thread(dest, (1 << 1) | 1);
			thread_wake(dest);
			status = dest->status;
		} else {
			/* yep */
			status = TS_RECV_WAIT;
		}
	}

	if(status == TS_RECV_WAIT
		&& (dest->ipc_from.raw == L4_anythread.raw
			|| dest->ipc_from.raw == self_id.raw)
		&& (dest->wakeup_time == ~(uint64_t)0u
			|| dest->wakeup_time > now_us))
	{
		/* active send */
		TRACE("%s: active send to %lu:%lu (from %lu:%lu, actual %lu:%lu)\n", __func__,
			TID_THREADNUM(dest->id), TID_VERSION(dest->id),
			TID_THREADNUM(self_id.raw), TID_VERSION(self_id.raw),
			TID_THREADNUM(self->id), TID_VERSION(self->id));

		L4_Word_t error = do_ipc_transfer(self, dest);
		if(unlikely(error != 0)) {
			TRACE("%s: active send caused errorcode %#lx\n", __func__, error);
			int code = (error & 0xe) >> 1;
			if(code >= 4) {
				/* mutual error; signal to partner also. */
				dest->ipc_from = L4_nilthread;
				if(likely(!post_exn_fail(dest))) {
					set_ipc_error(thread_get_utcb(dest), error | 1);
					set_ipc_return_thread(dest);
				}
				thread_wake(dest);
			}
			set_ipc_error(thread_get_utcb(self), error & ~1ul);
			assert(self->status == TS_RUNNING);
			return false;
		}

		/* receiver's IPC return */
		dest->ipc_from = self_id;
		if(likely(!post_exn_ok(dest))) {
			/* ordinary IPC; set up the IPC return registers. */
			set_ipc_return_thread(dest);
			if(propagated) {
				void *dest_utcb = thread_get_utcb(dest);
				assert(CHECK_FLAG(L4_VREG(dest_utcb, L4_TCR_MR(0)),
					0x1000));
				L4_VREG(dest_utcb, L4_TCR_VA_SENDER) = self->id;
			}
		}
		/* join with the overridden status. this satisfies thread_wake()'s
		 * precondition.
		 */
		assert(dest->status == TS_RECV_WAIT || dest->status == TS_R_RECV);
		dest->status = status;
		thread_wake(dest);

		if(L4_IsNilThread(self->ipc_from)) {
			/* send-only, and done. */
			assert(self->status == TS_RUNNING);
			return true;
		} else {
			/* indicate active receive. */
			TRACE("%s: setting status to R_RECV\n", __func__);
			self->status = TS_R_RECV;
			if(self->wakeup_time > 0) {
				self->wakeup_time = 0;
				sq_update_thread(self);
			}
			return true;
		}
	} else if(self->send_timeout.raw != L4_ZeroTime.raw) {
		/* passive send */
		TRACE("%s: passive send to %lu:%lu (from %lu:%lu, actual %lu:%lu)\n",
			__func__,
			TID_THREADNUM(dest->id), TID_VERSION(dest->id),
			TID_THREADNUM(self_id.raw), TID_VERSION(self_id.raw),
			TID_THREADNUM(self->id), TID_VERSION(self->id));
		struct ipc_wait *w = kmem_cache_alloc(ipc_wait_slab);
		w->dest_tid = self->ipc_to;
		w->send_tid = self_id;
		w->thread = self;

		htable_add(&sendwait_hash, int_hash(w->dest_tid.raw), w);

		self->status = TS_SEND_WAIT;
		thread_sleep(self, self->send_timeout);
		assert(self->status != TS_READY);

		return false;
	} else {
		/* instant timeout. */
		set_ipc_error_thread(self, (1 << 1) | 0);
		return false;
	}
}


/* simple IPC is used by e.g. pagefaults and exceptions, simple messages
 * produced by kernel features.
 */
void ipc_simple(struct thread *dest)
{
	struct thread *current = get_current_thread();
	assert(current->saved_mrs != 0 || current->saved_brs != 0);

	ipc_user(current, dest);
}


void ipc_user(struct thread *from, struct thread *to)
{
	from->ipc_to.raw = to->id;
	from->ipc_from.raw = to->id;
	from->send_timeout = L4_Never;
	from->recv_timeout = L4_Never;

	if(ipc_send_half(from) && from->status == TS_R_RECV) {
		/* TODO: use preempt somewhere */
		bool preempt = false;
		ipc_recv_half(from, &preempt);
	}
}


bool ipc_recv_half(struct thread *self, bool *preempt_p)
{
	assert(preempt_p != NULL);

	/* find sender. */
	struct thread *from = NULL;
	L4_ThreadId_t from_tid = L4_nilthread;
	size_t hash = int_hash(self->id);
	if(self->ipc_from.raw != L4_anylocalthread.raw) {
		struct htable_iter it;
		for(struct ipc_wait *w = htable_firstval(&sendwait_hash, &it, hash);
			w != NULL;
			w = htable_nextval(&sendwait_hash, &it, hash))
		{
			if(w->dest_tid.raw == self->id
				&& (self->ipc_from.raw == L4_anythread.raw
					|| self->ipc_from.raw == w->send_tid.raw))
			{
				from = w->thread;
				from_tid = w->send_tid;
				htable_delval(&sendwait_hash, &it);
				kmem_cache_free(ipc_wait_slab, w);
				break;
			}
		}
	} else {
		/* TODO: handle those, also */
		panic("anylocalthread not handled");
	}

	if(from == NULL) {
		TRACE("%s: passive receive to %lu:%lu (waiting on %lu:%lu)\n", __func__,
			TID_THREADNUM(self->id), TID_VERSION(self->id),
			TID_THREADNUM(self->ipc_from.raw), TID_VERSION(self->ipc_from.raw));
		self->status = TS_RECV_WAIT;
		thread_sleep(self, self->recv_timeout);
		if(self->status == TS_READY) {
			/* instant timeout. */
			set_ipc_error_thread(self, (1 << 1) | 1);
		}
		return false;
	} else {
		/* active receive */
		TRACE("%s: active receive from %lu:%lu actual %lu:%lu (to %lu:%lu)\n",
			__func__,
			TID_THREADNUM(from_tid.raw), TID_VERSION(from_tid.raw),
			TID_THREADNUM(from->id), TID_VERSION(from->id),
			TID_THREADNUM(self->id), TID_VERSION(self->id));
		assert(from->status == TS_SEND_WAIT);

		L4_Word_t error = do_ipc_transfer(from, self);
		if(unlikely(error != 0)) {
			TRACE("%s: active receive caused errorcode %#lx\n",
				__func__, error);
			int code = (error & 0xe) >> 1;
			if(code >= 4) {
				/* mutual error; notify sender also (i.e. break its exception
				 * receive phase, if any)
				 */
				if(likely(!post_exn_fail(from))) {
					set_ipc_error(thread_get_utcb(from), error & ~1ul);
					set_ipc_return_thread(from);
				}
				thread_wake(from);
			}
			set_ipc_error(thread_get_utcb(self), error | 1);
			assert(self->status == TS_RUNNING);
			return false;
		}

		/* successful active receive -> READY. */
		self->status = TS_READY;
		if(self->wakeup_time > 0) {
			self->wakeup_time = 0;
			sq_update_thread(self);
		}

		self->ipc_from.raw = from_tid.raw;
		if(from_tid.raw != from->id) {
			/* propagation parameter delivery */
			/* TODO: get the utcb ptr somewhere else... */
			void *self_utcb = thread_get_utcb(self);
			assert(CHECK_FLAG(L4_VREG(self_utcb, L4_TCR_MR(0)), 0x1000));
			L4_VREG(self_utcb, L4_TCR_VA_SENDER) = from->id;
		}

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
		post_exn_ok(self);

		assert(IS_READY(from->status));
		*preempt_p = self == get_current_thread()
			&& preempted_by(self, task_switch_time * 1000, from);
		if(*preempt_p) {
#if 0
			printf("%s: thread %u:%u (pri %d) was preempted by partner %u:%u (pri %d)\n",
				__func__, TID_THREADNUM(self->id), TID_VERSION(self->id),
				(int)self->pri, TID_THREADNUM(from->id), TID_VERSION(from->id),
				(int)from->pri);
#endif
			assert(self->status == TS_RUNNING || self->status == TS_R_RECV
				|| self->status == TS_READY);
			self->status = TS_READY;
		}

		return true;
	}
}


/* IPC in a kernel thread. */
L4_MsgTag_t kipc(
	L4_ThreadId_t to,
	L4_ThreadId_t *from_p,
	L4_Word_t timeouts)
{
	struct thread *current = get_current_thread();
	void *utcb = thread_get_utcb(current);

	current->ipc_from = *from_p;
	current->ipc_to = to;
	current->send_timeout.raw = timeouts >> 16;
	current->recv_timeout.raw = timeouts & 0xffff;

	L4_MsgTag_t tag = { .raw = 0 };		/* "no error" */
	if(likely(!L4_IsNilThread(to)) && !ipc_send_half(current)) {
		if(current->status == TS_SEND_WAIT) {
			/* passive send. */
			thread_sleep(current, current->send_timeout);
			schedule();
		}
		tag.raw = L4_VREG(utcb, L4_TCR_MR(0));
		if(L4_IpcFailed(tag)) {
			assert(current->status == TS_RUNNING);
			return tag;
		}
	}

	if(likely(!L4_IsNilThread(current->ipc_from))) {
		bool preempt = false;
		if(!ipc_recv_half(current, &preempt)) {
			if(current->status == TS_RECV_WAIT) {
				/* passive receive.
				 *
				 * TODO: implement switching right into the other thread.
				 */
				thread_sleep(current, current->recv_timeout);
				schedule();
			}
		}
		if(preempt) {
			/* NB: kernel threads aren't preemptable quite yet. */
		}
		tag.raw = L4_VREG(utcb, L4_TCR_MR(0));
		if(likely(L4_IpcSucceeded(tag))) *from_p = current->ipc_from;
	}

	return tag;
}


static void ipc(struct thread *current, void *utcb, bool *preempt_p)
{
	assert(utcb == thread_get_utcb(current));
	assert(preempt_p != NULL);

	*preempt_p = false;

	/* send phase. */
	if(!L4_IsNilThread(current->ipc_to)) {
		TRACE("%s: IPC send phase.\n", __func__);
		if(!ipc_send_half(current)) {
			/* error case. */
			goto end;
		}
	}
	if(!L4_IsNilThread(current->ipc_from)
		&& current->status != TS_SEND_WAIT)
	{
		/* receive phase. */
		TRACE("%s: IPC receive phase.\n", __func__);
		ipc_recv_half(current, preempt_p);
		assert(current->status == TS_READY
			|| current->status == TS_RECV_WAIT);
	}

end:
	if(current->status == TS_READY && !*preempt_p) {
		current->status = TS_RUNNING;
	}

	TRACE("%s: IPC returning with status %d.\n", __func__,
		(int)current->status);
}


void sys_ipc(struct x86_exregs *regs)
{
	struct thread *current = get_current_thread();
	current->ipc_to.raw = regs->eax;
	current->ipc_from.raw = regs->edx;
	L4_Word_t timeouts = regs->ecx;
	TRACE("%s: ipc_to %#lx, ipc_from %#lx, timeouts %#lx\n", __func__,
		regs->eax, regs->edx, regs->ecx);
	current->send_timeout.raw = timeouts >> 16;
	current->recv_timeout.raw = timeouts & 0xffff;

	// L4_Word_t utcb_addr = regs->edi;
	/* TODO: could translate "utcb_addr" into a user-space pointer,
	 * verify that it points to the current thread's UTCB, and if not,
	 * do this slower thing.
	 */
	void *utcb = thread_get_utcb(current);
	L4_VREG(utcb, L4_TCR_MR(0)) = regs->esi;

	bool preempt = false;
	ipc(current, utcb, &preempt);
	if(current->status == TS_SEND_WAIT || current->status == TS_RECV_WAIT) {
		TRACE("%s: returning to scheduler\n", __func__);
		thread_save_ctx(current, regs);
		/* TODO: schedule the waitee */
		return_to_scheduler();
		assert(false);
	} else if(current->status == TS_READY && preempt) {
		/* IPC successful, but pre-empted by the receive-phase partner */
		TRACE("%s: returning to partner\n", __func__);
		thread_save_ctx(current, regs);
		set_ipc_return_regs(&current->ctx, current, utcb);
		return_to_scheduler();
		assert(false);
	} else {
		/* return from IPC at once. */
		TRACE("%s: returning to caller\n", __func__);
		assert(current->status == TS_RUNNING);
		set_ipc_return_regs(regs, current, utcb);
	}
}


static size_t hash_threadid(const void *tid, void *priv) {
	const L4_ThreadId_t *p = tid;
	return int_hash(p->raw);
}


COLD void init_ipc(void)
{
	ipc_wait_slab = kmem_cache_create("ipc_wait_slab",
		sizeof(struct ipc_wait), ALIGNOF(struct ipc_wait),
		0, NULL, NULL);

	htable_init(&sendwait_hash, &hash_threadid, NULL);
}
