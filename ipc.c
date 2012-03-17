
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>

#include <ccan/likely/likely.h>
#include <ccan/compiler/compiler.h>
#include <ccan/alignof/alignof.h>
#include <ccan/htable/htable.h>

#include <l4/types.h>
#include <l4/message.h>
#include <l4/vregs.h>
#include <l4/ipc.h>

#include <ukernel/misc.h>
#include <ukernel/slab.h>
#include <ukernel/thread.h>
#include <ukernel/space.h>
#include <ukernel/ipc.h>


/* these are kept in sendwait_hash in a multiset way, i.e. use
 * htable_firstval() and so forth to scan.
 */
struct ipc_wait
{
	L4_ThreadId_t dest_tid;		/* copied for rehash */
	struct thread *thread;
};


static struct kmem_cache *ipc_wait_slab = NULL;
static struct htable sendwait_hash;


static void do_ipc_transfer(
	struct thread *source,
	struct thread *dest)
{
	const void *s_base = thread_get_utcb(source);
	void *d_base = thread_get_utcb(dest);
	L4_Word_t tag = L4_VREG(s_base, L4_TCR_MR(0));
	L4_VREG(d_base, L4_TCR_MR(0)) = tag;
	int u = tag & 0x3f, t = (tag >> 6) & 0x3f;
	for(int i=0; i < u; i++) {
		int reg = L4_TCR_MR(i + 1);
		L4_VREG(d_base, reg) = L4_VREG(s_base, reg);
	}
	if(t > 0) {
		panic("do_ipc_transfer: can't handle typed words");
	}
}


bool ipc_send_half(struct thread *self)
{
	assert(!L4_IsNilThread(self->ipc_to));
	assert(self->ipc_to.raw != L4_anylocalthread.raw
		&& self->ipc_to.raw != L4_anythread.raw);

	struct thread *dest = thread_find(self->ipc_to.raw);
	if(unlikely(dest == NULL)) {
		/* FIXME: produce an IPC error instead */
		panic("dest thread not found in ipc_send_half()");
	}

	/* TODO: override TS_R_RECV when peer's ipc_from == self->id . */
	if(likely(dest->status == TS_RECV_WAIT)) {
		/* active send */
		printf("%s: active send to %d:%d\n", __func__,
			TID_THREADNUM(dest->id), TID_VERSION(dest->id));

		do_ipc_transfer(self, dest);

		if(L4_IsNilThread(self->ipc_from)) {
			assert(self->status == TS_RUNNING);
			return true;
		} else {
			/* try active receive, just in case. */
			if(ipc_recv_half(self)) {
				assert(self->status == TS_RUNNING);
				return true;
			} else {
				assert(self->status == TS_RECV_WAIT);
				return false;
			}
		}
	} else {
		/* passive send */
		printf("%s: passive send from %d:%d\n", __func__,
			TID_THREADNUM(self->id), TID_VERSION(self->id));
		struct ipc_wait *w = kmem_cache_alloc(ipc_wait_slab);
		w->dest_tid = self->ipc_to;
		w->thread = self;
		htable_add(&sendwait_hash, int_hash(w->dest_tid.raw), w);

		self->status = TS_SEND_WAIT;

		return false;
	}
}


/* simple IPC is used by e.g. pagefaults and exceptions, simple messages
 * produced by kernel features.
 */
void ipc_simple(struct thread *dest)
{
	struct thread *current = get_current_thread();

	current->ipc_to.raw = dest->id;
	current->ipc_from.raw = dest->id;
	current->send_timeout = L4_Never;
	current->recv_timeout = L4_Never;

	ipc_send_half(current);
}


/* TODO: timeouts (i.e. on zerotime, go to TS_READY instead of any
 * TS_*_WAIT)
 */
bool ipc_recv_half(struct thread *self)
{
	/* find sender. */
	struct thread *from = NULL;
	size_t hash = int_hash(self->id);
	if(self->ipc_from.raw != L4_anylocalthread.raw) {
		struct htable_iter it;
		for(struct ipc_wait *w = htable_firstval(&sendwait_hash, &it, hash);
			w != NULL;
			w = htable_nextval(&sendwait_hash, &it, hash))
		{
			if(w->dest_tid.raw == self->id
				&& (self->ipc_from.raw == L4_anythread.raw
					|| self->ipc_from.raw == w->thread->id))
			{
				from = w->thread;
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
		printf("%s: passive receive in %d:%d\n", __func__,
			TID_THREADNUM(self->id), TID_VERSION(self->id));
		self->status = TS_RECV_WAIT;
		return false;
	} else {
		/* active receive */
		printf("%s: active receive from %d:%d\n", __func__,
			TID_THREADNUM(from->id), TID_VERSION(from->id));
		assert(from->status == TS_SEND_WAIT);

		do_ipc_transfer(from, self);

		self->status = TS_READY;
		self->ipc_from.raw = from->id;
		if(unlikely(IS_KERNEL_THREAD(from))) {
			/* kernel threads do the send/receive phases as control flow. */
			from->status = TS_READY;
		} else {
			/* userspace threads operate via a state machine. */
			from->status = L4_IsNilThread(from->ipc_from) ? TS_READY : TS_R_RECV;
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
		/* passive send. */
		assert(current->status == TS_SEND_WAIT);
		schedule();
		tag.raw = L4_VREG(utcb, L4_TCR_MR(0));
		if(unlikely(L4_IpcFailed(tag))) return tag;
	}

	if(likely(!L4_IsNilThread(current->ipc_from))) {
		if(!ipc_recv_half(current)) {
			/* passive receive.
			 *
			 * TODO: implement switching right into the other thread.
			 */
			assert(current->status == TS_RECV_WAIT);
			schedule();
		}
		tag.raw = L4_VREG(utcb, L4_TCR_MR(0));
		*from_p = current->ipc_from;
	}

	return tag;
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
