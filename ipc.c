
#include <stdlib.h>

#include <ccan/likely/likely.h>
#include <ccan/compiler/compiler.h>
#include <ccan/alignof/alignof.h>
#include <ccan/htable/htable.h>

#include <l4/types.h>
#include <l4/vregs.h>

#include <ukernel/misc.h>
#include <ukernel/slab.h>
#include <ukernel/thread.h>
#include <ukernel/ipc.h>


/* these are kept in sendwait_hash in a multiset way, i.e. use
 * htable_firstval() and so forth to scan.
 */
struct ipc_wait
{
	L4_ThreadId_t dest_tid;
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


void ipc_simple(struct thread *dest)
{
	struct thread *current = get_current_thread();

	/* fastpath switching to a recv-waiting thread to optimize for
	 * "call" IPC.
	 */
	if(unlikely(dest->status != TS_RECV_WAIT)) {
		/* passive send. */
		struct ipc_wait *w = kmem_cache_alloc(ipc_wait_slab);
		w->dest_tid.raw = dest->id;
		w->thread = current;
		htable_add(&sendwait_hash, int_hash(w->dest_tid.raw), w);

		current->status = TS_SEND_WAIT;
	} else {
		/* active send. */
		do_ipc_transfer(current, dest);

		dest->status = TS_READY;
		current->status = TS_RECV_WAIT;
	}
}


/* receive in a kernel context. */
void kipc_recv(struct thread **from_p)
{
	struct thread *current = get_current_thread();

	struct htable_iter it;
	size_t hash = int_hash(current->id);
	struct thread *from = NULL;
	for(struct ipc_wait *w = htable_firstval(&sendwait_hash, &it, hash);
		w != NULL;
		w = htable_nextval(&sendwait_hash, &it, hash))
	{
		if(w->dest_tid.raw == current->id) {
			from = w->thread;
			htable_delval(&sendwait_hash, &it);
			kmem_cache_free(ipc_wait_slab, w);
			break;
		}
	}

	if(unlikely(from == NULL)) {
		current->status = TS_RECV_WAIT;
		schedule();
	} else {
		/* active receive */
		*from_p = from;
		assert(from->status == TS_SEND_WAIT);

		do_ipc_transfer(from, current);

		/* FIXME */
		from->status = /* has_recv_phase(from) ? TS_RECV_WAIT : */ TS_READY;
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
