
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
};


struct stritem_iter
{
	L4_Word_t *words;
	int hdr, sub;

	uintptr_t ptr;
	L4_Word_t len;
};


struct fault_peer
{
	L4_Fpage_t *faults;
	uint16_t num, pos;	/* at most 4M per strxfer = 1024 pages /peer */
};


/* in-progress typed transfer. at most one per two threads. */
struct ipc_state
{
	/* non-derived parameters to do_typed_transfer(). (UTCB pointers aren't
	 * retained in sleep, to avoid kernel address space pinning.)
	 */
	struct thread *from, *to;
	L4_MsgTag_t tag;

	/* do_typed_transfer() outer state */
	L4_Word_t str_offset;
	int pos, last, dbr_pos;

	/* xfer timeout tracking */
	uint64_t xferto_at;		/* µs, 0 when not applicable */

	/* stringitem copy state */
	int src_off, dst_off;
	struct stritem_iter src_iter, dst_iter;

	/* pre-transfer fault state */
	struct fault_peer f_src, f_dst;

	/* first f_src.num are for the source thread, next f_dst.num are for dest,
	 * after that are words for dst_iter's contents.
	 */
	L4_Fpage_t faults[];	/* sizelog2 = PAGE_BITS, access = ro/rw */
};


/* an error result from the copy_*_stritem() family. tagged by error code,
 * supplied out of band.
 */
struct copy_err
{
	L4_Fpage_t fault;	/* [EFAULT] src/dst determined by read/write */
};


static void send_xfer_fault(
	struct thread *t,
	L4_Fpage_t fault,
	L4_Word_t ip,
	uint64_t xfto_abs);

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


static void stritem_first(L4_StringItem_t *si, struct stritem_iter *it)
{
	assert(L4_IsStringItem(si));

	it->words = (L4_Word_t *)si;
	it->hdr = 0;
	it->sub = 1;
	it->ptr = (uintptr_t)si->X.str.substring_ptr[0];
	it->len = si->X.string_length;
}


static bool stritem_next(struct stritem_iter *it)
{
	L4_StringItem_t *si = (L4_StringItem_t *)&it->words[it->hdr];
	assert(L4_IsStringItem(si));
	if(it->sub >= L4_Substrings(si) && !L4_CompoundString(si)) {
		/* simple string items are the most common. */
		return false;
	} else if(it->sub < L4_Substrings(si)) {
		it->ptr = (uintptr_t)si->X.str.substring_ptr[it->sub++];
		assert(it->len == si->X.string_length);
		return true;
	} else if(L4_CompoundString(si)) {
		L4_Word_t *next = (L4_Word_t *)&si->X.str.substring_ptr[it->sub + 1];
		assert(L4_IsStringItem((L4_StringItem_t *)next));
		it->hdr = next - it->words;
		it->sub = -1;
		return stritem_next(it);
	} else {
		/* end of compound string. */
		return false;
	}
}


/* NOTE: this treats I/O ports as inclusive. this is good enough; exclusive
 * (i.e. refusing to pass ranges that aren't all present) would just be
 * another hassle.
 *
 * TODO: this doesn't fail atomically.
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
		/* FIXME: the correct acceptor has s' == 16, p == 0. reject others
		 * quietly.
		 */
		if(!L4_IsIoFpage(wnd)) return 0;	/* again, error? */
		else return apply_io_mapitem(source, s_base, dest, d_base, m, wnd);
	}

	bool is_cas = wnd.raw == L4_CompleteAddressSpace.raw;
	TRACE("mapping 0x%lx:0x%lx, sndbase 0x%lx, rcvwindow %#lx:%#lx (%s)\n",
		L4_Address(map_page), L4_Size(map_page),
		L4_MapItemSndBase(m), is_cas ? 0 : L4_Address(wnd),
		is_cas ? ~0ul : L4_Size(wnd),
		is_cas ? "CompleteAddressSpace" : "<- that");

	if(is_cas || L4_Size(wnd) >= L4_MapItemSndBase(m) + L4_Size(map_page)) {
		const L4_Word_t wnd_base = is_cas ? 0 : L4_Address(wnd);
		int given = mapdb_map_pages(&source->space->mapdb,
			&dest->space->mapdb, map_page,
			wnd_base + L4_MapItemSndBase(m));
		if(is_grant && given != 0) {
			L4_Set_Rights(&map_page, given);
			mapdb_unmap_fpage(&source->space->mapdb, map_page, true, false,
				false);
		}
		return given;
	} else {
		/* TODO */
		panic("apply_mapitem() can't handle trunc rcvwindow cases");
		return 0;
	}
}


static int stritemlen(L4_StringItem_t *si)
{
	int len = 0;
	L4_StringItem_t *prev;
	do {
		prev = si;
		len += si->X.string_length * L4_Substrings(si);
		L4_Word_t *wp = (L4_Word_t *)si;
		si = (L4_StringItem_t *)&wp[L4_Substrings(si) + 1];
	} while(L4_CompoundString(prev));
	return len;
}


static int stritem_faults(
	L4_Fpage_t *faults,
	size_t faults_len,
	struct space *sp,
	L4_StringItem_t *si,
	int access,
	int max_fail)
{
	int n_faults = 0;
	struct stritem_iter it;
	stritem_first(si, &it);
	/* this loop works by brute fucking force. the groups-and-entries format
	 * is too much of a hassle to do right the first time around.
	 */
	L4_Word_t addr = it.ptr & ~PAGE_MASK;
	while(addr < it.ptr + it.len) {
		struct map_entry *e = mapdb_probe(&sp->mapdb, addr);
		if(e == NULL || !CHECK_FLAG_ALL(L4_Rights(e->range), access)) {
			if(n_faults < faults_len) {
				L4_Fpage_t f = L4_FpageLog2(addr, PAGE_BITS);
				/* (TODO: if e != NULL, set access to the bits that weren't
				 * present in e->range's rights
				 */
				L4_Set_Rights(&f, access);
				faults[n_faults] = f;
			}
			n_faults++;
			if(max_fail >= 0 && n_faults > max_fail) return n_faults;

			addr += PAGE_SIZE;
		} else {
			addr = L4_Address(e->range) + L4_Size(e->range);
		}

		if(addr >= it.ptr + it.len && stritem_next(&it)) {
			/* next segment. */
			addr = it.ptr;
		}
	}

	return n_faults;
}


void ipc_xfer_timeout(struct ipc_state *st)
{
	assert(st->from->ipc == st);
	assert(st->to->ipc == st);

	TRACE("%s: st->from{%lu:%lu} -> st->to{%lu:%lu}\n", __func__,
		TID_THREADNUM(st->from->id), TID_VERSION(st->from->id),
		TID_THREADNUM(st->to->id), TID_VERSION(st->to->id));

	assert(st->f_src.num > 0 || st->f_dst.num > 0);
	const bool f_in_src = st->f_src.num > 0;
	const L4_Word_t ec_offs = st->str_offset << 4;
	set_ipc_error_thread(st->from, ec_offs | (f_in_src ? 5 : 6) << 1);
	set_ipc_error_thread(st->to, ec_offs | (!f_in_src ? 5 : 6) << 1 | 1);

	struct thread *from = st->from, *to = st->to;
	bool dead_ipc = post_exn_fail(from) | post_exn_fail(to);
	if(!dead_ipc) {
		/* can happen when both were waiting for transfer start due to
		 * scheduling.
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


static void prexfer_ipc_hook(struct hook *hook, uintptr_t code, void *dataptr)
{
	struct thread *t = container_of(hook, struct thread, post_exn_call);
	assert(&t->post_exn_call == hook);

	hook_detach(hook);

	if(code != 0) {
		/* xfer fault loop was aborted. this happens for three reasons:
		 *   1) scheduler selected the current thread, which was in R_RECV,
		 *      and would've gone into passive receive but the transfer's xfer
		 *      timeout was hit;
		 *   2) an active send to @t completed, but not before the xfer
		 *      timeout was hit;
		 *   3) IPC abort from ExchangeRegisters, on either peer.
		 *
		 * if t->ipc isn't NULL, we'll clean it up on both sides.
		 */
		if(t->ipc != NULL) {
			struct ipc_state *st = t->ipc;
			st->from->ipc = NULL;
			st->to->ipc = NULL;
			assert(t->ipc == NULL);
			free(st);
		}

		assert(t->saved_mrs == 0 && t->saved_brs == 0);
		return;
	}

	assert(t->ipc != NULL);

	t->ipc_from.raw = t->saved_regs[12];
	t->ipc_to.raw = t->saved_regs[13];

	/* FIXME: restore send, recv timeouts (needed for other IPC stage) */

	bool is_sender = CHECK_FLAG(t->flags, TF_SENDER);
	struct fault_peer *p_self = &t->ipc->f_src, *p_other = &t->ipc->f_dst;
	if(!is_sender) {
		assert(t == t->ipc->to);
		SWAP(struct fault_peer *, p_self, p_other);
		SWAP(L4_ThreadId_t, t->ipc_from, t->ipc_to);
	} else {
		assert(t == t->ipc->from);
	}

	if(t->ipc->xferto_at > 0 && ksystemclock() >= t->ipc->xferto_at) {
		/* transfer timed out under the pager transaction, and wasn't killed
		 * by the scheduler before that.
		 *
		 * FIXME: hit this in a test, first
		 */
		printf("%s: post-fault xfer timeout in %lu:%lu\n", __func__,
			TID_THREADNUM(t->id), TID_VERSION(t->id));
		panic("should xfertimeout in post-fault");
	} else if(++p_self->pos < p_self->num) {
		/* next fault. */
		TRACE("%s: sending next fault (index %u out of %u) (in %lu:%lu)\n",
			__func__, p_self->pos, p_self->num, TID_THREADNUM(t->id),
			TID_VERSION(t->id));
		send_xfer_fault(t, p_self->faults[p_self->pos], t->ctx.eip,
			t->ipc->xferto_at);
		TRACE("%s: next fault sent, state now %s\n", __func__,
			sched_status_str(t));
	} else if(p_other->pos < p_other->num) {
		/* wait for partner. */
		TRACE("%s: waiting for partner in %s thread %lu:%lu\n", __func__,
			CHECK_FLAG(t->flags, TF_SENDER) ? "sender" : "receiver",
			TID_THREADNUM(t->id), TID_VERSION(t->id));
		t->status = is_sender ? TS_SEND_WAIT : TS_RECV_WAIT;
		thread_sleep(t, L4_Never);
	} else {
		TRACE("%s: partner %lu:%lu ready, scheduling\n", __func__,
			TID_THREADNUM(t->id), TID_VERSION(t->id));
		t->status = TS_XFER;
		thread_wake(t);
		ipc_partner(t)->status = TS_XFER;
		thread_wake(ipc_partner(t));
	}
}


static void send_xfer_fault(
	struct thread *t,
	L4_Fpage_t fault,
	L4_Word_t ip,
	uint64_t xferto_abs)
{
	TRACE("ipc: xfer fault %#lx (ip %#lx) in %lu:%lu (xfto %lu)\n",
		L4_Address(fault), ip, TID_THREADNUM(t->id), TID_VERSION(t->id),
		(unsigned long)xferto_abs);

	void *utcb = thread_get_utcb(t);
	struct thread *pager = thread_get_pager(t, utcb);
	t->saved_regs[12] = t->ipc_from.raw;
	t->saved_regs[13] = t->ipc_to.raw;
	set_pf_msg(t, utcb, L4_Address(fault), ip, L4_Rights(fault));
	/* (this can cause calls to ipc_send_half() to nest. due to the way IPC
	 * works in L4.X2, that's completely safe.)
	 */
	hook_push_back(&t->post_exn_call, &prexfer_ipc_hook, NULL);
	ipc_user(t, pager, xferto_abs);
	assert(xferto_abs == 0 || t->wakeup_time == xferto_abs);
	assert(IS_IPC(t->status));
}


/* starts the pre-transfer fault process. sets thread states and reallocates
 * the saved IPC context to fit the fault pages and relevant receive-side
 * string item. returns true on success and false on out-of-memory.
 *
 * @recv_si must be a pointer to the string item in the buffer registers in
 * the _correct_ order, i.e. reverse from the BR order.
 *
 * @xferto_us is the number of microseconds until one of the participants
 * aborts due to xfer timeout, or 0 for never.
 */
static bool set_prexfer_fault_state(
	L4_MsgTag_t tag,
	uint64_t xferto_us,
	struct thread *source,
	L4_StringItem_t *send_si,
	int nf_send,
	struct thread *dest,
	L4_StringItem_t *recv_si,
	int recv_si_words,
	int nf_recv)
{
	TRACE("%s: entered. source %lu:%lu, dest %lu:%lu, xferto_us %lu\n", __func__,
		TID_THREADNUM(source->id), TID_VERSION(source->id),
		TID_THREADNUM(dest->id), TID_VERSION(dest->id),
		(unsigned long)xferto_us);

	assert(nf_send + nf_recv >= 0);
	assert(source->ipc == dest->ipc);
	size_t is_size = sizeof(struct ipc_state)
		+ (nf_send + nf_recv) * sizeof(L4_Fpage_t)
		+ recv_si_words * sizeof(L4_Word_t);
	struct ipc_state *st = realloc(source->ipc, is_size);
	if(unlikely(st == NULL)) {
		free(source->ipc);
		source->ipc = NULL;
		dest->ipc = NULL;
		st = malloc(is_size);
		if(unlikely(st == NULL)) return false;
	}
	*st = (struct ipc_state){
		.from = source, .to = dest, .tag = tag,
		.f_src = { .num = nf_send, .faults = &st->faults[0] },
		.f_dst = { .num = nf_recv, .faults = &st->faults[nf_send] },
		.xferto_at = xferto_us == 0 ? 0 : ksystemclock() + xferto_us,
	};
	memcpy(&st->faults[nf_send + nf_recv], recv_si->raw,
		recv_si_words * sizeof(L4_Word_t));
	stritem_first(send_si, &st->src_iter);
	stritem_first((L4_StringItem_t *)&st->faults[nf_send + nf_recv],
		&st->dst_iter);

	source->ipc = st;
	dest->ipc = st;
	source->flags |= TF_SENDER;
	assert(!CHECK_FLAG(dest->flags, TF_SENDER));

	if(nf_send > 0) {
		stritem_faults(&st->faults[0], nf_send, source->space, send_si,
			L4_Readable, -1);
		send_xfer_fault(source, st->faults[0], source->ctx.eip,
			st->xferto_at);
	} else {
		source->status = TS_SEND_WAIT;
		thread_sleep(source, L4_Never);
	}

	if(nf_recv > 0) {
		stritem_faults(&st->faults[nf_send], nf_recv, dest->space, recv_si,
			L4_Writable | L4_Readable, -1);
		send_xfer_fault(dest, st->faults[nf_send], dest->ctx.eip,
			st->xferto_at);
	} else {
		dest->status = TS_RECV_WAIT;
		thread_sleep(dest, L4_Never);
	}

	return true;
}


/* sends a pagefault on behalf of the source or destination, and puts the
 * other thread in the appropriate wait state.
 */
static void set_xfer_fault_state(struct ipc_state *st, L4_Fpage_t fault)
{
	/* neither is relevant in this flow */
	st->f_src = (struct fault_peer){ .faults = NULL };
	st->f_dst = (struct fault_peer){ .faults = NULL };
	struct thread *ft = st->from, *oth = st->to;
	if(CHECK_FLAG(L4_Rights(fault), L4_Writable)) {
		/* faulted in the receiver (write side). */
		SWAP(struct thread *, ft, oth);
	}

	/* the other side waits, appearing ready to the faulting side when it gets
	 * to the post-fault hook
	 */
	assert(oth->status == TS_XFER);
	oth->status = oth == st->to ? TS_RECV_WAIT : TS_SEND_WAIT;
	thread_sleep(oth, L4_Never);

	/* the faulter does an IPC in the scheduled transfer peer's (or IPC
	 * caller's, outside of the ipc_resume() path) context.
	 */
	send_xfer_fault(ft, fault, ft->ctx.eip, st->xferto_at);
}


/* a møøse once bit my sister... */
static int copy_interspace_stritem(struct copy_err *err_p, struct ipc_state *st)
{
	int rc;
	uintptr_t copy_dst = reserve_heap_page();
	uint32_t copy_page = 0;
	/* (old function parameters, now in `st') */
	int s_off = st->src_off, d_off = st->dst_off;
	struct stritem_iter *src_iter = &st->src_iter, *dst_iter = &st->dst_iter;
	struct space *dest_space = st->to->space, *src_space = st->from->space;
	do {
#if 0
		printf("start of loop; s_off %d, d_off %d\n", s_off, d_off);
		printf("  src len %d, ptr %#lx\n", (int)src_iter->len,
			(L4_Word_t)src_iter->ptr);
		printf("  dst len %d, ptr %#lx\n", (int)dst_iter->len,
			(L4_Word_t)dst_iter->ptr);
#endif

		/* TODO: avoid repeated probe */
		uintptr_t dest_page = (dst_iter->ptr + d_off) & ~PAGE_MASK;
		struct map_entry *e = mapdb_probe(&dest_space->mapdb, dest_page);
		if(e == NULL || !CHECK_FLAG(L4_Rights(e->range), L4_Writable)) {
			/* pop a write fault */
			err_p->fault = L4_FpageLog2(dest_page, PAGE_BITS);
			TRACE("ipc: write fault at %#lx:%#lx\n", L4_Address(err_p->fault),
				L4_Size(err_p->fault));
			L4_Set_Rights(&err_p->fault, L4_Readable | L4_Writable);
			goto fault;
		}
		uint32_t d_page = mapdb_page_id_in_entry(e, dest_page);
		if(d_page != copy_page) {
			put_supervisor_page(copy_dst, d_page);
			copy_page = d_page;
			x86_flush_tlbs();		/* FIXME: just invalidate copy_dst */
		}

		int seg = MIN(int, PAGE_SIZE - ((dst_iter->ptr + d_off) & PAGE_MASK),
				MIN(int, src_iter->len - s_off, dst_iter->len - d_off));

		int d_pos = ((dst_iter->ptr & PAGE_MASK) + d_off) & PAGE_MASK;
		assert(d_pos >= 0 && seg > 0);
		assert(d_pos + seg <= PAGE_SIZE);

		size_t n = space_memcpy_from(src_space, (void *)(copy_dst + d_pos),
			src_iter->ptr + s_off, seg);
		if(n < seg) {
			/* pop a read fault */
			TRACE("ipc: read fault after %u bytes (seg %d)\n",
				(unsigned)n, seg);
			s_off += n;
			d_off += n;
			err_p->fault = L4_FpageLog2((src_iter->ptr + s_off) & ~PAGE_MASK,
				PAGE_BITS);
			L4_Set_Rights(&err_p->fault, L4_Readable);
			goto fault;
		}

		s_off += seg;
		d_off += seg;
		if(d_off == dst_iter->len) {
			bool ok UNNEEDED = stritem_next(dst_iter);
			assert(ok);
			d_off = 0;
		}
	} while(s_off < src_iter->len || (s_off = 0, stritem_next(src_iter)));

	rc = 0;

end:
	put_supervisor_page(copy_dst, 0);
	x86_flush_tlbs();
	free_heap_page(copy_dst);

	return rc;

fault:
	st->src_off = s_off;
	st->dst_off = d_off;
	rc = EFAULT;
	goto end;
}


/* expects the following fields valid in @st: from, to, src_off, dst_off,
 * src_iter, dst_iter
 */
static int do_string_transfer(struct copy_err *err_p, struct ipc_state *st)
{
	/* TODO: add special case for intra-space transfers (with the appropriate
	 * segment etc. hax)
	 */
	return copy_interspace_stritem(err_p, st);
}


static bool eval_xfer_timeout(
	uint64_t *xferto_us_p,
	L4_Time_t snd_to,
	L4_Time_t rcv_to)
{
	/* NOTE: TimePoint values in {snd,rcv}_to can cause a timeout in two ways:
	 * by being reached, and by having been reached before the call to
	 * eval_xfer_timeout(). the former is handled as an IPC timeout via a
	 * TimePeriod conversion, and the latter is detected with pt_is_valid()
	 * from a latent call to do_typed_transfer().
	 *
	 * this code could unify the clock conversion bits in a nicer way. as it
	 * stands TimePoints are slightly slower to process.
	 */
	if(L4_IsTimePoint_NP(snd_to)) {
		L4_Clock_t base = { .raw = ksystemclock() };
		snd_to = pt_is_valid(base, snd_to)
			? L4_TimePeriod(L4_PointClock_NP(base, snd_to).raw - base.raw)
			: L4_ZeroTime;
	}
	if(L4_IsTimePoint_NP(rcv_to)) {
		L4_Clock_t base = { .raw = ksystemclock() };
		rcv_to = pt_is_valid(base, rcv_to)
			? L4_TimePeriod(L4_PointClock_NP(base, rcv_to).raw - base.raw)
			: L4_ZeroTime;
	}

	uint64_t v;
	if(snd_to.raw == L4_ZeroTime.raw
		|| rcv_to.raw == L4_ZeroTime.raw)
	{
		/* instant timeout */
		return false;
	} else if(snd_to.raw != L4_Never.raw
		&& rcv_to.raw != L4_Never.raw)
	{
		v = MIN(uint64_t, time_in_us(snd_to), time_in_us(rcv_to));
	} else if(snd_to.raw != L4_Never.raw) {
		assert(rcv_to.raw == L4_Never.raw);
		v = time_in_us(snd_to);
	} else if(rcv_to.raw != L4_Never.raw) {
		assert(snd_to.raw == L4_Never.raw);
		v = time_in_us(rcv_to);
	} else {
		assert(snd_to.raw == L4_Never.raw);
		assert(rcv_to.raw == L4_Never.raw);
		v = 0;
	}

	*xferto_us_p = v;
	return true;
}


/* when source->ipc != NULL, this function resumes from the next typed item
 * indicated in the ipc_state structure, and may replace source->ipc with
 * another as page faults are generated.
 */
static L4_Word_t do_typed_transfer(
	struct thread *source,
	const void *s_base,
	struct thread *dest,
	void *d_base,
	L4_MsgTag_t tag)
{
	L4_Word_t str_offset = 0;		/* # of bytes copied over this IPC */
	int pos = tag.X.u + 1, last = tag.X.u + tag.X.t, dbr_pos = 0;

	if(source->ipc != NULL) {
		/* IPC state reload. */
		assert(CHECK_FLAG(source->flags, TF_SENDER));
		assert(ipc_partner(source) == dest);

		struct ipc_state *st = source->ipc;
		str_offset = st->str_offset;
		pos = st->pos;
		last = st->last;
		dbr_pos = st->dbr_pos;
	} else {
		/* parameter checks in init case only */
		assert(tag.X.t > 0);
	}

	while(pos <= last) {
		L4_Word_t w0 = L4_VREG(s_base, L4_TCR_MR(pos));
		switch(w0 & 0xe) {
			case 0x8:
			case 0xa: {
				/* MapItem (0x8), GrantItem (0xa) */
				if(unlikely(pos + 1 > last)) goto too_short;
				L4_MapItem_t m = {
					.raw = { w0, L4_VREG(s_base, L4_TCR_MR(pos + 1)) },
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
				L4_VREG(d_base, L4_TCR_MR(pos)) = m.raw[0];
				L4_VREG(d_base, L4_TCR_MR(pos + 1)) = m.raw[1];
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

			default: {
				/* string transfers. */
				assert((w0 & 8) == 0);

				/* parse, validate, and copy the sender's string item.
				 * (FIXME: write a test for this, i.e. that map items are
				 * handled correctly after a stringitem.)
				 */
				int si_start = pos;
				L4_StringItem_t *si = (L4_StringItem_t *)&L4_VREG(s_base,
					L4_TCR_MR(pos)), *prev;
				do {
					prev = si;
					if(pos + 1 > last) goto too_short;
					int subs = L4_Substrings(si);
					if(pos + subs > last) {
						/* FIXME: have some sort of a scanner, first, that
						 * parses and validates string items before e.g. the
						 * map database is accessed, or previous string
						 * transfers are set up.
						 */
						panic("invalid stringitem = invalid IPC (FIXME)");
					}

					/* FIXME: clear sender's string address in interspace
					 * transfers for security's sake.
					 */
					for(int i=0; i < subs + 1; i++) {
						int reg = L4_TCR_MR(pos + i);
						L4_VREG(d_base, reg) = L4_VREG(s_base, reg);
					}

					pos += subs + 1;
					si = (L4_StringItem_t *)&L4_VREG(s_base, L4_TCR_MR(pos));
				} while(L4_CompoundString(prev));
				L4_StringItem_t *send_si = (L4_StringItem_t *)&L4_VREG(s_base,
					L4_TCR_MR(si_start));

				/* some clever buttenoid saw fit to lay the buffer registers
				 * out backwards in memory, so they have to be copied one by
				 * one to look like a proper StringItem, instead of just
				 * pointing them out from the destination thread's UTCB.
				 */
				if(dbr_pos == 0) {
					if((L4_VREG(d_base, L4_TCR_BR(0)) & 1) == 0) {
						/* no string items. */
						goto msg_overflow;
					}
					dbr_pos = 1;
				}
				assert(dbr_pos < 64);
				union {
					L4_Word_t raw[63];
					L4_StringItem_t si;
				} brs;
				brs.raw[0] = L4_VREG(d_base, L4_TCR_BR(dbr_pos));
				brs.raw[1] = L4_VREG(d_base, L4_TCR_BR(dbr_pos + 1));
				if(unlikely(!L4_IsStringItem(&brs.si))) goto msg_overflow;
				int tot_br_words = MIN(int, 64 - dbr_pos,
					L4_Substrings(&brs.si) + 2);
				for(int i=2; i < tot_br_words; i++) {
					brs.raw[i] = L4_VREG(d_base, L4_TCR_BR(dbr_pos + i));
				}
				dbr_pos += tot_br_words;
				L4_StringItem_t *recv_si = &brs.si;

				int send_len = stritemlen(send_si);
				if(send_len > stritemlen(recv_si)) goto msg_overflow;
				/* FIXME: don't do this. don't call stritem_faults() twice.
				 * it's silly.
				 */
				int nf_src = stritem_faults(NULL, 0, source->space, send_si, L4_Readable, -1),
					nf_dst = stritem_faults(NULL, 0, dest->space, recv_si,
						L4_Readable | L4_Writable, -1);
				if(likely(nf_src == 0 && nf_dst == 0)) {
					struct ipc_state st;
					stritem_first(send_si, &st.src_iter);
					stritem_first(recv_si, &st.dst_iter);
					st.src_off = 0;
					st.dst_off = 0;
					st.from = source;
					st.to = dest;
					struct copy_err err;
					int n = do_string_transfer(&err, &st);
					if(unlikely(n != 0)) {
						/* EFAULT doesn't occur per stritem_faults() result.
						 * no other errors are defined.
						 */
						printf("error %d in strxfer; fault %#lx:%#lx (access %#lx)\n",
							n, L4_Address(err.fault), L4_Size(err.fault),
							L4_Rights(err.fault));
						panic("strxfer fault");
					}
					str_offset += send_len;
				} else {
					uint64_t xferto_us = 0;
					/* FIXME: if there was a preceding string transfer with
					 * xfer faults in this IPC, carry its remaining xfer
					 * timeout forward instead of starting from zero
					 */
					/* check ZeroTime before allocator call */
					L4_Time_t snd_to = {
						.raw = (L4_VREG(s_base, L4_TCR_XFERTIMEOUTS) >> 16)
							& 0xffff,
					}, rcv_to = {
						.raw = L4_VREG(d_base, L4_TCR_XFERTIMEOUTS) & 0xffff,
					};
#if 0
					printf("ipc: snd_to=");
					if(snd_to.raw == L4_Never.raw) printf("never");
					else printf("%lu µs", (unsigned long)time_in_us(snd_to));
					printf("; rcv_to=");
					if(rcv_to.raw == L4_Never.raw) printf("never");
					else printf("%lu µs", (unsigned long)time_in_us(rcv_to));
					printf("\n");
#endif
					if(!eval_xfer_timeout(&xferto_us, snd_to, rcv_to)) {
#if 0
						printf("ipc: immediate xfer timeout (nsrc %d, ndst %d)\n",
							nf_src, nf_dst);
#endif
						if(nf_src == 0) goto xfer_timeout_src;
						else goto xfer_timeout_dst;
					}
					bool ok = set_prexfer_fault_state(tag, xferto_us,
						source, send_si, nf_src,
						dest, recv_si, tot_br_words, nf_dst);
					if(!ok) {
						/* FIXME: this should be handled with the syscall
						 * restart on malloc fail mechanism that unmap (and
						 * granting IPC) would use, and which isn't written.
						 */
						panic("out of memory in a critical spot!");
					}
					assert(source->ipc != NULL);
					assert(source->ipc == dest->ipc);
					source->ipc->str_offset = str_offset;
					source->ipc->pos = pos;
					source->ipc->last = last;
					source->ipc->dbr_pos = dbr_pos;
					assert(xferto_us == 0 || source->ipc->xferto_at > 0);

					/* this, with source->ipc != NULL, means that the
					 * operation didn't complete and that another thread
					 * should be scheduled.
					 */
					return 0;
				}

				break;
			}
		}
	}

	/* successful exit; the typed transfer has completed. */
	if(source->ipc != NULL) {
		assert(source->ipc == dest->ipc);
		free(source->ipc);
		source->ipc = NULL;
		dest->ipc = NULL;
		source->flags &= ~TF_SENDER;
	}

	return 0;

	/* FIXME: these failure cases don't properly un-set the C bit in the last
	 * typed word that's copied.
	 *
	 * FIXME: also, the caller must flip the xfer timeout around when writing
	 * the other thread's ErrorCode TCR.
	 */
too_short:
	/* FIXME: return a proper error */
	panic("not enough typed message words");
	return 0;

xfer_timeout_src:
	return (str_offset << 4) | 0xa;

xfer_timeout_dst:
	return (str_offset << 4) | 0xc;

msg_overflow:
	return (str_offset << 4) | 0x8;
}


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

	/* FIXME: a pointer to the sender's UTCB range has been retained in the
	 * ipc_state. it should instead have a pointer to a copy, like it does of
	 * the receiver's buffer items. (FIXME: is this still true?)
	 */
	struct copy_err err;
	int n = do_string_transfer(&err, st);
	if(n != 0) {
		assert(n == EFAULT);
		set_xfer_fault_state(st, err.fault);
		*preempt_p = false;
		return false;
	}

	/* bump the typed IPC state just as do_typed_transfer() would after a
	 * single stringitem iteration
	 */
	L4_StringItem_t *src_si = (L4_StringItem_t *)&st->src_iter.words[0];
	assert(L4_IsStringItem(src_si));

	assert(!L4_CompoundString(src_si));		/* FIXME */
	st->pos += L4_Substrings(src_si) + 2;	/* ^ */

	st->str_offset += stritemlen(src_si);

	L4_Word_t ec = do_typed_transfer(st->from, thread_get_utcb(st->from),
		st->to, thread_get_utcb(st->to), (L4_MsgTag_t){ });
	if(unlikely(ec != 0)) {
		/* FIXME: set error as though it had happened in ipc_send_half()'s
		 * do_ipc_transfer() callsite
		 */
		panic("can't propagate resumed typed-transfer errors");
	} else if(t->ipc != NULL) {
		/* FIXME */
		panic("can't handle more faults from resumed typed xfer");
	}

	/* typed transfer completed. */
	assert(dest->ipc == NULL);
	dest->status = TS_READY;
	dest->wakeup_time = 0;
	sq_update_thread(dest);
	dest->ipc_from.raw = source->id;
	set_ipc_return_thread(dest, thread_get_utcb(dest));

	assert(source->ipc == NULL);
	if(L4_IsNilThread(source->ipc_from)) {
		source->status = TS_READY;
		source->wakeup_time = 0;
		set_ipc_return_thread(source, thread_get_utcb(source));
		TRACE("%s: source returns to userspace\n", __func__);
	} else {
		source->status = TS_R_RECV;
		source->wakeup_time = wakeup_at(L4_Never);	/* FIXME: xfer timeout */
		TRACE("%s: source receives from %lu:%lu\n", __func__,
			L4_ThreadNo(source->ipc_from), L4_Version(source->ipc_from));
	}
	sq_update_thread(source);

	/* assumed to have always completed. wheeee (FIXME: lies!) */
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


int ipc_tstate(struct thread *t) {
	assert(t->ipc != NULL);
	return t->ipc->to == t ? L4_SCHEDRESULT_RECEIVING : L4_SCHEDRESULT_SENDING;
}


/* returns ErrorCode value, or 0 for success. */
static inline L4_Word_t do_ipc_transfer(
	struct thread *source,
	const void *s_utcb,
	struct thread *dest,
	void *d_utcb)
{
	L4_MsgTag_t tag = { .raw = L4_VREG(s_utcb, L4_TCR_MR(0)) };
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
 * FIXME: untested!
 * TODO: this should signal preemption when it occurs: one of the aborted
 * senders may have priority.
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

		struct thread *peer UNUSED = w->thread;
		assert(peer->ipc_to.raw == t->id);
		assert(peer->status == TS_SEND_WAIT || peer->status == TS_XFER
			|| peer->status == TS_STOPPED);
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


/* called from thread_ipc_fail() and from the deleting ThreadControl. takes
 * care of the sendwait_hash entry. leaves errorcode setting to caller's
 * caller.
 *
 * TODO: this should be renamed to indicate its function. there's already
 * abort_waiting_ipc(), which should be renamed as well.
 */
void abort_thread_ipc(struct thread *t)
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

	if(CHECK_FLAG(self->flags, TF_INTR)
		&& is_interrupt(self->ipc_to)
		&& L4_VREG(self_utcb, L4_TCR_MR(0)) == 0)
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

	/* get matching variables, incl. propagation */
	L4_ThreadId_t self_id = { .raw = self->id },
		self_lid = tid_return(dest, self);
	bool propagated = false;
	struct thread *vs = NULL, *sender = self;
	L4_MsgTag_t *tag = (void *)&L4_VREG(self_utcb, L4_TCR_MR(0));
	if(CHECK_FLAG(tag->X.flags, 0x1)) {
		/* propagation (sender fakery). */
		vs = get_tcr_thread(self, self_utcb, L4_TCR_VA_SENDER);
		/* FIXME: also check interrupt propagation, redirector chain */
		if(vs != NULL && (self->space == vs->space
			|| self->space == dest->space))
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
			tag->X.flags &= ~0x1ul;		/* no propagation for you. */
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

		/* pre-transfer faults cause a recipient of the pager's IPC to have a
		 * saved IPC operation. this "had no ipc" condition suffices to
		 * distinguish such a transfer from one where the string transfer
		 * fault is generated.
		 */
		bool had_no_ipc = self->ipc == NULL;
		void *dest_utcb = thread_get_utcb(dest);
		L4_Word_t error = do_ipc_transfer(self, self_utcb, dest, dest_utcb);
		if(unlikely(error != 0)) {
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
		} else if(self->ipc != NULL && had_no_ipc) {
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
				L4_VREG(dest_utcb, L4_TCR_VA_SENDER) = tid_return(dest,
					self).raw;
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
	L4_ThreadId_t peer_tid;
	if(from->space == to->space) peer_tid = get_local_id(to);
	else peer_tid.raw = to->id;
	from->ipc_to = peer_tid;
	from->ipc_from = peer_tid;
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


static struct thread *find_global_sender(
	L4_ThreadId_t *from_tid_p,
	bool *valid_p,
	struct thread *self)
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
	if(w == NULL) return NULL;
	else {
		struct thread *from = w->thread;
		*from_tid_p = w->send_tid;
		htable_delval(&sendwait_hash, &it);
		kmem_cache_free(ipc_wait_slab, w);
		return from;
	}
}


/* always sets *from_tid_p to a local TID. on propagation, ActualSender will
 * be set from retval->id.
 */
static struct thread *find_local_sender(
	L4_ThreadId_t *from_tid_p,
	bool *valid_p,
	struct thread *self)
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
			if(w->dest_tid.raw == self->id
				&& w->thread->space == self->space)
			{
				break;
			}
		}
	}

	if(w == NULL) return NULL;
	else {
		assert(L4_IsLocalId(w->send_tid));

		struct thread *from = w->thread;
		*from_tid_p = w->send_tid;
		htable_delval(&sendwait_hash, &it);
		kmem_cache_free(ipc_wait_slab, w);
		return from;
	}
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

	/* find sender. */
	struct thread *from;
	L4_ThreadId_t from_tid = L4_nilthread;
	if(L4_IsLocalId(self->ipc_from)) {
		assert(self->ipc_from.raw != L4_anythread.raw);
		bool valid;
		from = find_local_sender(&from_tid, &valid, self);
		if(!valid) goto err_no_partner;
		assert(L4_IsLocalId(from_tid));
	} else {
		assert(self->ipc_from.raw != L4_anylocalthread.raw);
		bool valid;
		from = find_global_sender(&from_tid, &valid, self);
		if(!valid && !is_interrupt(self->ipc_from)) {
			/* some things IPC can't relay. for everything else, there's
			 * passive receive.
			 */
			assert(self->ipc_from.raw != L4_anythread.raw);
			goto err_no_partner;
		}
	}

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
		TRACE("%s: active receive from %lu:%lu actual %lu:%lu (to %lu:%lu)\n",
			__func__,
			TID_THREADNUM(from_tid.raw), TID_VERSION(from_tid.raw),
			TID_THREADNUM(from->id), TID_VERSION(from->id),
			TID_THREADNUM(self->id), TID_VERSION(self->id));
		assert(from->status == TS_SEND_WAIT || from->status == TS_XFER);

		void *from_utcb = thread_get_utcb(from);
		L4_Word_t error = do_ipc_transfer(from, from_utcb, self, self_utcb);
		if(unlikely(error != 0)) {
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
		} else if(self->ipc != NULL) {
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
		self->ipc_from = from_tid;
		L4_MsgTag_t tag = { .raw = L4_VREG(self_utcb, L4_TCR_MR(0)) };
		if(L4_IpcPropagated(tag)) {
			L4_VREG(self_utcb, L4_TCR_VA_SENDER) = tid_return(self, from).raw;
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
	// printf("%s: ipc_from=%#lx wasn't valid\n", __func__, self->ipc_from.raw);
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


void sys_ipc(struct x86_exregs *regs)
{
	L4_ThreadId_t to = { .raw = regs->eax }, from = { .raw = regs->edx };
	L4_Word_t timeouts = regs->ecx, mr0 = regs->esi;
	// L4_Word_t utcb_addr = regs->edi;

	struct thread *current = get_current_thread();
	TRACE("%s: called in %lu:%lu; to %#lx, from %#lx, timeouts %#lx\n",
		__func__, TID_THREADNUM(current->id), TID_VERSION(current->id),
		regs->eax, regs->edx, regs->ecx);

	/* TODO: could translate "utcb_addr" into a user-space pointer,
	 * verify that it points to the current thread's UTCB, and if not,
	 * do this slower thing.
	 */
	void *utcb = thread_get_utcb(current);

	/* parameter validation. */
	if(unlikely(to.raw == L4_anythread.raw
		|| to.raw == L4_anylocalthread.raw))
	{
		set_ipc_error(utcb, 4);		/* non-existing partner, send phase */
		set_ipc_return_regs(regs, current, utcb);
		return;
	}
	if(unlikely(!L4_IsNilThread(from)
		&& L4_ThreadNo(from) > last_int_threadno()
		&& L4_ThreadNo(from) < first_user_threadno()))
	{
		set_ipc_error(utcb, 5);		/* non-existing partner, receive phase */
		set_ipc_return_regs(regs, current, utcb);
		return;
	}

	bool preempt = false;
	current->ipc_to = to;
	current->ipc_from = from;
	current->send_timeout.raw = timeouts >> 16;
	current->recv_timeout.raw = timeouts & 0xffff;
	L4_VREG(utcb, L4_TCR_MR(0)) = mr0;
	ipc(current, utcb, &preempt);
	if(preempt && IS_READY(current->status)) {
		/* would return, but was pre-empted */
		assert(!IS_KERNEL_THREAD(current));		/* >implying */
		TRACE("%s: scheduling (pre-empted)\n", __func__);
		thread_save_ctx(current, regs);
		set_ipc_return_regs(&current->ctx, current, utcb);
		return_to_scheduler();
		assert(false);
	} else if(IS_IPC(current->status)) {
		/* IPC ongoing. */
		TRACE("%s: scheduling (ongoing IPC)\n", __func__);
		thread_save_ctx(current, regs);
		/* TODO: schedule the waitee */
		return_to_scheduler();
		assert(false);
	} else {
		/* return from IPC at once. */
		TRACE("%s: returning to caller\n", __func__);
		assert(current->status == TS_RUNNING);
		assert(!IS_KERNEL_THREAD(current));
		set_ipc_return_regs(regs, current, utcb);
	}
}
