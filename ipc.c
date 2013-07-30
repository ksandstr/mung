
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <errno.h>
#include <alloca.h>
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

#define ADJUST_PTR(p, oldbase, newbase) \
	(((void *)(p) - (uintptr_t)(oldbase)) + (uintptr_t)(newbase))


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


struct stritem_iter
{
	/* outputs */
	L4_Word_t ptr, len;

	/* state */
	L4_Word_t *words;
	uint8_t hdr, sub;
	uint8_t max;
};


struct fault_peer {
	L4_Fpage_t *faults;
	uint16_t num, pos;	/* at most 4M per strxfer = 1024 pages /peer */
};


struct str_meta {
	uint8_t first_reg;
	uint8_t n_words;
};


/* in-progress string transfer. at most one per two threads.
 *
 * alloca()ted on the stack when no transfer faults have occurred.
 */
struct ipc_state
{
	uint64_t xferto_at;		/* µs, 0 when not applicable */
	L4_Word_t tot_offset;	/* total # of bytes transferred */

	struct thread *from, *to;

	uint8_t num_strings;	/* # of string transfers */
	uint8_t num_rsis;		/* # of receive buffers */
	uint8_t max_brs;		/* length of longest receiver buffer in words */
	uint8_t str_pos;		/* pos'n of current transfer (<= num_strings) */

	/* first the sender's items, then the receiver's.
	 *
	 * inl[0..1], inl[2..3] when num_strings <= 2; otherwise ptr[0], ptr[1].
	 * (*ptr[] is allocated at end of struct, and has num_strings*2 members.)
	 */
	union {
		struct str_meta inl[4];
		struct str_meta *ptr[2];	/* [0] = from, [1] = to */
	} meta;

	int str_off;			/* byte position in transfer (< 10^22) */
	/* it[] when str_off >= 0, otherwise fault[] */
	union {
		struct stritem_iter it[2];	/* from, to */
		struct fault_peer fault[2];	/* same */
	} xfer;
	int s_off, d_off;		/* per-segment offsets */

	/* implied member. allocated after the structure. accessed with get_rsi()
	 * or get_pre_faults(). length is max_brs words.
	 *
	 * rsi when str_off >= 0, otherwise pre_faults.
	 */
#ifdef NEVER_DEFINED__x
	union {
		L4_StringItem_t rsi;	/* "receiver string item" */
		/* first xfer.fault[0].num are for the source thread, next
		 * xfer.fault[1].num are for dest.
		 *
		 * sizelog2 = PAGE_BITS, access = ro/rw
		 */
		L4_Fpage_t pre_faults[];
	} buf;
#endif
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


/* functions dealing with string transfers.
 *
 * TODO: typed transfers could be moved into ipc_typed.c or some such, leaving
 * ipc.c to deal with the system call and state machine.
 */
static size_t ipc_state_size(int num_strings, int max_brs)
{
	assert(num_strings > 0 && num_strings < 32);
	assert(max_brs >= 2 && max_brs < 64);

	return sizeof(struct ipc_state)
		+ max_brs * sizeof(L4_Word_t)
		+ 2 * num_strings * sizeof(struct str_meta);
}


/* duplicate ipc_state on malloc heap, rejigger pointers. */
static struct ipc_state *dup_state(struct ipc_state *old_st, size_t st_size)
{
	struct ipc_state *st = malloc(st_size);
	if(unlikely(st == NULL)) return NULL;

	memcpy(st, old_st, st_size);
	for(int i=0; i<2; i++) {
		if(st->num_strings > 2) {
			st->meta.ptr[i] = ADJUST_PTR(st->meta.ptr[i], old_st, st);
		}
		if(st->str_off >= 0) {
			st->xfer.it[i].words = ADJUST_PTR(
				st->xfer.it[i].words, old_st, st);
		} else {
			st->xfer.fault[i].faults = ADJUST_PTR(
				st->xfer.fault[i].faults, old_st, st);
		}
	}

	return st;
}


static inline L4_StringItem_t *get_rsi(struct ipc_state *st) {
	assert(st->str_off >= 0);
	return (void *)st + sizeof(struct ipc_state);
}


static inline L4_Fpage_t *get_pre_faults(struct ipc_state *st) {
	assert(st->str_off < 0);
	return (void *)st + sizeof(struct ipc_state);
}


static void stritem_first(
	struct stritem_iter *it,
	L4_StringItem_t *si,
	unsigned int max)		/* max = n_words - 1 */
{
	assert(L4_IsStringItem(si));
	assert(max < 64);

	it->words = (L4_Word_t *)si;
	it->hdr = 0;
	it->sub = 1;
	it->max = max;
	it->ptr = (uintptr_t)si->X.str.substring_ptr[0];
	it->len = si->X.string_length;
}


static bool stritem_next(struct stritem_iter *it)
{
	if(unlikely(it->hdr + it->sub > it->max)) {
		/* out of range */
		return false;
	}

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
		L4_StringItem_t *next = (void *)&si->X.str.substring_ptr[it->sub];
		it->hdr = next->raw - it->words;
		it->sub = 0;
		it->len = next->X.string_length;
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


/* FIXME: move this into something under lib/ ! */
static size_t stritemlen(L4_StringItem_t *si)
{
	size_t len = 0;
	L4_StringItem_t *prev;
	do {
		prev = si;
		len += si->X.string_length * L4_Substrings(si);
		L4_Word_t *wp = (L4_Word_t *)si;
		si = (L4_StringItem_t *)&wp[L4_Substrings(si) + 1];
	} while(L4_CompoundString(prev));
	return len;
}


/* scan the sender's typed items. produce at most t / 2 map/grant offsets (1
 * byte each, being the MR of the first word), and t / 2 struct str_meta.
 *
 * stops on weird string items, or unknown item types.
 */
static void scan_typed_items(
	uint8_t *mapgrant_buf,
	size_t *mapgrant_len_p,
	struct str_meta *str_buf,
	size_t *str_buf_len_p,
	const void *utcb,
	L4_MsgTag_t tag)
{
	size_t n_maps = 0, n_strs = 0;
	/* note: "last" is inclusive, i.e. the MR# of the last register. */
	int pos = tag.X.u + 1, last = pos + tag.X.t - 1;

	while(pos + 1 <= last) {
		L4_Word_t w0 = L4_VREG(utcb, L4_TCR_MR(pos));
		switch(w0 & 0xe) {
			case 0x8:	/* MapItem */
			case 0xa:	/* GrantItem */
				mapgrant_buf[n_maps++] = pos;
				pos += 2;
				continue;

			default:
				if((w0 & 0x8) == 0) {
					/* bit 3 is clear; looks like a string item */
					break;
				}

			case 0xc:	/* ctrl xfer items (not supported) */
			case 0xe:	/* not in use */
				goto end;
		}

		/* StringItem */
		int n_words = 0;
		L4_StringItem_t *si = (void *)&L4_VREG(utcb, L4_TCR_MR(pos)), *prev;
		assert(L4_IsStringItem(si));
		do {
			prev = si;
			int hdr_len = 1 + L4_Substrings(si);
			if(unlikely(pos + n_words + hdr_len > last + 1)) goto end;
			n_words += hdr_len;
			si = (void *)&L4_VREG(utcb, L4_TCR_MR(pos + n_words));
		} while(L4_CompoundString(prev));
		assert(pos + n_words <= last + 1);
		str_buf[n_strs++] = (struct str_meta){
			.first_reg = pos, .n_words = n_words,
		};
		pos += n_words;
	}

end:
	*mapgrant_len_p = n_maps;
	*str_buf_len_p = n_strs;
}


/* scan the receiver's buffer registers. produce at most t / 2 struct
 * str_meta. return length of the longest buffer item in *max_p.
 *
 * stops on weird string items, unknown item types, a cleared C flag, or once
 * max_scan items have been looked at.
 */
static size_t scan_buffer_regs(
	struct str_meta *buf,
	int *max_p,
	void *utcb,
	size_t max_scan)
{
	size_t count = 0, max = 0;
	int pos = 1;
	L4_Word_t w0 = L4_VREG(utcb, L4_TCR_BR(0));	/* BR0 or previous head */
	while(CHECK_FLAG(w0, 1) && pos < 63 && count < max_scan) {
		w0 = L4_VREG(utcb, L4_TCR_BR(pos));
		if((w0 & 0x8) != 0) goto end;	/* not a string item */

		L4_StringItem_t si = { .raw[0] = w0 };
		size_t n_words;
		if(!L4_CompoundString(&si)) {
			n_words = MIN(int, 64 - pos, L4_Substrings(&si) + 1);
		} else {
			L4_StringItem_t prev;
			n_words = 0;
			do {
				prev = si;
				/* consider a compound string buffer terminated when _any_
				 * head has C=0.
				 */
				w0 &= si.raw[0];
				n_words += 1 + L4_Substrings(&si);
				if(pos + n_words > 63) goto end;
				si.raw[0] = L4_VREG(utcb, L4_TCR_BR(pos + n_words));
			} while(L4_CompoundString(&prev));
			assert(pos + n_words < 64);
		}
		buf[count++] = (struct str_meta){
			.first_reg = pos, .n_words = n_words,
		};
		max = MAX(size_t, n_words, max);
		pos += n_words;
	}

end:
	*max_p = max;
	return count;
}


/* FIXME: add parameter to limit the checking length. right now this'll fault
 * in address space that's never used, regardless of actual data size.
 */
static int stritem_faults(
	L4_Fpage_t *faults,
	size_t faults_len,
	struct space *sp,
	const struct stritem_iter *ref_iter,
	int access,
	int max_fail)
{
	int n_faults = 0;
	struct stritem_iter it = *ref_iter;
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


/* NOTE: contrary to the name, this can also be called for in-transfer
 * faults.
 */
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

	const bool is_sender = CHECK_FLAG(t->flags, TF_SENDER);
	TRACE("%s: t=%lu:%lu, t->ipc=%p\n", __func__,
		TID_THREADNUM(t->id), TID_VERSION(t->id), t->ipc);

	if(t->ipc->xferto_at > 0 && ksystemclock() >= t->ipc->xferto_at) {
		/* transfer timed out under the pager transaction, and wasn't killed
		 * by the scheduler before that.
		 *
		 * FIXME: hit this in a test, first
		 */
		printf("%s: post-fault xfer timeout in %lu:%lu\n", __func__,
			TID_THREADNUM(t->id), TID_VERSION(t->id));
		panic("should xfertimeout in post-fault");
	}

	if(t->ipc->str_off >= 0) {
		/* in-transfer fault.
		 * TODO: remove the goto, for obvious reasons
		 */
		goto proceed;
	}

	assert(t->ipc->str_off < 0);
	struct fault_peer *p_self = &t->ipc->xfer.fault[1 - (is_sender ? 1 : 0)],
		*p_other = &t->ipc->xfer.fault[is_sender ? 1 : 0];
	if(!is_sender) {
		assert(t == t->ipc->to);
		SWAP(L4_ThreadId_t, t->ipc_from, t->ipc_to);
	} else {
		assert(t == t->ipc->from);
	}

	if(++p_self->pos < p_self->num) {
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
proceed:
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


/* sends a pagefault on behalf of the source or destination, and puts the
 * other thread in the appropriate wait state.
 */
static void set_xfer_fault_state(struct ipc_state *st, L4_Fpage_t fault)
{
	assert(st->str_off >= 0);	/* no pre_faults in progress */
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
static int copy_interspace_stritem(L4_Fpage_t *fault_p, struct ipc_state *st)
{
	assert(st->str_off >= 0);

	int rc;
	uintptr_t copy_dst = reserve_heap_page();
	uint32_t copy_page = 0;
	int s_off = st->s_off, d_off = st->d_off;
	struct stritem_iter *src_iter = &st->xfer.it[0],
		*dst_iter = &st->xfer.it[1];
	struct space *dest_space = st->to->space, *src_space = st->from->space;
	do {
#if 0
		printf("start of loop; str_off=%d, s_off=%d, d_off=%d\n",
			st->str_off, s_off, d_off);
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
			*fault_p = L4_FpageLog2(dest_page, PAGE_BITS);
			L4_Set_Rights(fault_p, L4_Readable | L4_Writable);
			TRACE("ipc: write fault at %#lx:%#lx\n", L4_Address(*fault_p),
				L4_Size(*fault_p));
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
			st->str_off += n;
			*fault_p = L4_FpageLog2((src_iter->ptr + s_off) & ~PAGE_MASK,
				PAGE_BITS);
			L4_Set_Rights(fault_p, L4_Readable);
			goto fault;
		} else {
			s_off += n;
			d_off += n;
			st->str_off += n;

			assert(d_off <= dst_iter->len);
			if(d_off == dst_iter->len) {
				bool ok UNNEEDED = stritem_next(dst_iter);
				assert(ok);
				d_off = 0;
			}
		}
	} while(s_off < src_iter->len || (s_off = 0, stritem_next(src_iter)));

	rc = 0;

end:
	put_supervisor_page(copy_dst, 0);
	x86_flush_tlbs();
	free_heap_page(copy_dst);
	return rc;

fault:
	st->s_off = s_off;
	st->d_off = d_off;
	rc = -EFAULT;
	goto end;
}


static int copy_stritem(L4_Fpage_t *fault_p, struct ipc_state *st)
{
	/* TODO: add special case for intra-space transfers (with the appropriate
	 * segment etc. hax)
	 */
	return copy_interspace_stritem(fault_p, st);
}


/* copy the sender's string item to the receiver's message registers, and
 * initialize the string item iterator.
 *
 * note: it says "next", but this doesn't actually bump @st->str_pos .
 * next_dst_strbuf() does that instead.
 *
 * TODO: receive addresses should be modified so that they refer to the
 * receiver's buffers
 * TODO[v1]: make a copy of the sender's string item, because right now
 * st->send_si will end up referring to the sender's UTCB which may disappear
 * once address space recycling comes about.
 */
static bool next_src_string(
	struct ipc_state *st,
	void *s_base,
	void *d_base,
	size_t *len_p)
{
	assert(st->str_pos <= st->num_strings);
	if(st->str_pos == st->num_strings) return false;

	const struct str_meta *meta = st->num_strings <= 2
		? &st->meta.inl[0] : st->meta.ptr[0];
	meta += st->str_pos;

	/* copy to receiver. */
	memcpy(&L4_VREG(d_base, L4_TCR_MR(meta->first_reg)),
		&L4_VREG(s_base, L4_TCR_MR(meta->first_reg)),
		meta->n_words * sizeof(L4_Word_t));
	/* FIXME: set the C bit in the first header properly! */

	/* set up us the bomb. */
	st->str_off = 0;
	L4_StringItem_t *si = (L4_StringItem_t *)&L4_VREG(s_base,
		L4_TCR_MR(meta->first_reg));
	if(unlikely(!L4_IsStringItem(si))) return false;
	stritem_first(&st->xfer.it[0], si, meta->n_words - 1);
	st->s_off = 0;
#ifndef NDEBUG
	st->xfer.it[1].words = (void *)0xfaceb00b;	/* mm hmm. */
#endif

	/* FIXME: this can fall off the end if a compound string's last header's C
	 * bit is set in between. this may cause an invalid read access in the
	 * kernel.
	 */
	*len_p = stritemlen(si);
	return true;
}


static bool next_dst_strbuf(struct ipc_state *st, void *d_base, size_t *len_p)
{
	assert(st->str_pos <= st->num_rsis);
	if(st->str_pos == st->num_rsis) return false;

	const struct str_meta *meta = st->num_strings <= 2
		? &st->meta.inl[2] : st->meta.ptr[1];
	assert(st->str_pos < st->num_rsis);
	meta += st->str_pos;

	/* some clever buttenoid saw fit to lay the buffer registers out backwards
	 * in memory, so they have to be copied one by one to look like a proper
	 * StringItem, instead of just pointing them out from the destination
	 * thread's UTCB.
	 */
	L4_StringItem_t *rsi = get_rsi(st);
	for(int i=0; i < meta->n_words; i++) {
		rsi->raw[i] = L4_VREG(d_base, L4_TCR_BR(meta->first_reg + i));
	}
	if(unlikely(!L4_IsStringItem(rsi))) return false;
	/* FIXME: guard against concurrent modification! (due to pagefaults, it
	 * may happen even without SMP.)
	 */
	*len_p = stritemlen(rsi);

	stritem_first(&st->xfer.it[1], get_rsi(st), meta->n_words - 1);
	st->d_off = 0;
	st->str_pos++;
	return true;
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


/* catch pre-transfer faults. if there are none, return 0.
 * otherwise, send fault messages to the appropriate pagers, put the
 * participants to sleep, and return -EFAULT. on immediate xfer timeout,
 * returns positive ErrorCode.
 *
 * FIXME: pass src_len to stritem_faults() once it gets that parameter.
 */
static int check_prexfer_faults(
	struct ipc_state *st,
	size_t src_len,
	void *s_base,
	void *d_base)
{
	struct thread *source = st->from, *dest = st->to;

	/* note that we'll potentially read and write the RSI/faults buffer, so
	 * write to a buffer first instead.
	 */
	assert(st->str_off >= 0);
	const int faults_len = st->max_brs;
	L4_Fpage_t s_faults[faults_len], d_faults[faults_len];
	int nf_src = stritem_faults(s_faults, faults_len, source->space,
			&st->xfer.it[0], L4_Readable, faults_len),
		nf_dst = stritem_faults(d_faults, faults_len, dest->space,
			&st->xfer.it[1], L4_Readable | L4_Writable, faults_len);
	if(nf_src == 0 && nf_dst == 0) return 0;

	uint64_t now = ksystemclock();
	if(st->xferto_at > 0) {
		/* check a previously-evaluated xfer timeout */
		if(st->xferto_at <= now) goto xfer_timeout;
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
		if(!eval_xfer_timeout(&xferto_us, snd_to, rcv_to)) goto xfer_timeout;
		if(xferto_us > 0) {
			/* definite timeout. */
			st->xferto_at = now + xferto_us;
		} else {
			/* the perpetual non-timeout. */
			st->xferto_at = ~0ull;
		}
		assert(st->xferto_at > 0);
	}

	/* prepare the fault handling state. */
	st->str_off = -1;
	if(nf_src + nf_dst > st->max_brs) {
		/* when both > half, both get exactly half. otherwise the longer is
		 * made shorter.
		 */
		assert(st->max_brs >= 2);
		int half = st->max_brs / 2, s = nf_src, d = nf_dst;
		nf_src = MIN(int, nf_src, st->max_brs - MIN(int, half, d));
		nf_dst = MIN(int, nf_dst, st->max_brs - MIN(int, half, s));
		assert(nf_src + nf_dst == st->max_brs);
		assert(!(s > half && d > half)
			|| (nf_src == half && nf_dst == half));
	}
	st->xfer.fault[0] = (struct fault_peer){
		.faults = get_pre_faults(st), .num = nf_src,
	};
	st->xfer.fault[1] = (struct fault_peer){
		.faults = get_pre_faults(st) + nf_src, .num = nf_dst,
	};
	memcpy(st->xfer.fault[0].faults, s_faults, nf_src * sizeof(L4_Fpage_t));
	memcpy(st->xfer.fault[1].faults, d_faults, nf_dst * sizeof(L4_Fpage_t));

	/* send faults & adjust sched status */
	if(nf_src > 0) {
		send_xfer_fault(source, s_faults[0], source->ctx.eip, st->xferto_at);
	} else {
		source->status = TS_SEND_WAIT;
		thread_sleep(source, L4_Never);
	}

	if(nf_dst > 0) {
		send_xfer_fault(dest, d_faults[0], dest->ctx.eip, st->xferto_at);
	} else {
		dest->status = TS_RECV_WAIT;
		thread_sleep(dest, L4_Never);
	}

	if(source->ipc == NULL) {
		assert(!CHECK_FLAG(source->flags, TF_SENDER));
		assert(!CHECK_FLAG(dest->flags, TF_SENDER));
		assert(dest->ipc == NULL);

		source->flags |= TF_SENDER;
		source->ipc = st;
		dest->ipc = st;
	}

	assert(source->ipc == dest->ipc);
	assert(source->ipc->str_off < 0);

	return -EFAULT;

xfer_timeout:
	if(nf_src == 0) {
		/* source-side transfer timeout. */
		return (st->tot_offset << 4) | 0xa;
	} else {
		/* the other kind */
		return (st->tot_offset << 4) | 0xc;
	}
}


/* returns 0 for successful completion, -EFAULT on fault, or -E2BIG when there
 * are too many niggers and not enough hoes. a positive ErrorCode may also
 * happen.
 */
static int do_string_transfer(
	struct ipc_state *st,
	void *s_base,
	void *d_base,
	bool resume)
{
	size_t src_len = 0;
	if(resume) {
		assert(st->str_pos > 0);
		assert(st->str_pos <= st->num_strings);
		if(st->str_off < 0) {
			/* after pre-transfer fault processing. that'll have
			 * overwritten the receive string buffer and the stritem
			 * iterators, so recreate those.
			 */
			st->str_pos--;
			size_t dst_len = 0;
			bool s_ok = next_src_string(st, s_base, d_base, &src_len),
				d_ok = next_dst_strbuf(st, d_base, &dst_len);
			/* (implied by pre-xfer fault processing.) */
			BUG_ON(!s_ok || !d_ok || src_len > dst_len,
				"hey, don't shoot me! src_len=%u, dst_len=%u",
				(unsigned)src_len, (unsigned)dst_len);
		} else {
			/* after in-transfer fault. */
			int str_pos = st->str_pos - 1;	/* debump */
			const struct str_meta *meta = st->num_strings <= 2
				? &st->meta.inl[str_pos] : st->meta.ptr[str_pos];
			L4_StringItem_t *si = (L4_StringItem_t *)&L4_VREG(s_base,
				L4_TCR_MR(meta->first_reg));
			/* FIXME: protect against concurrent modification (i.e. pass
			 * meta->n_words)
			 */
			src_len = stritemlen(si);
		}
	} else {
		if(next_src_string(st, s_base, d_base, &src_len)) {
			size_t dst_len = 0;
			if(next_dst_strbuf(st, d_base, &dst_len)) {
				if(src_len > dst_len) return -E2BIG;

				int rc = check_prexfer_faults(st, src_len, s_base, d_base);
				if(rc != 0) return rc;
			} else {
				/* no more woodchucks. */
				return -E2BIG;
			}
		} else {
			/* completed. */
			return 0;
		}
	}

	L4_Fpage_t fault;
	int rc = copy_stritem(&fault, st);
	if(rc < 0) {
		assert(rc == -EFAULT);
		assert(st->str_off >= 0);
		set_xfer_fault_state(st, fault);
		return rc;
	}
	st->tot_offset += src_len;

	/* yes, i am a recursive function. so? */
	return do_string_transfer(st, s_base, d_base, false);
}


/* returns same as apply_mapitem(), and transfers the map/grantitem if
 * retval >= 0.
 */
static int do_mapgrant_transfer(
	int first_mr,
	bool is_last,
	struct thread *source,
	const void *s_base,
	struct thread *dest,
	void *d_base)
{
	L4_MapItem_t m = {
		.raw = {
			L4_VREG(s_base, L4_TCR_MR(first_mr)),
			L4_VREG(s_base, L4_TCR_MR(first_mr + 1)),
		},
	};
	int given = apply_mapitem(source, s_base, dest, d_base, m);
	if(given < 0) return given;

	m.X.snd_fpage.X.b = 0;
	m.X.snd_fpage.X.rwx = given;
	m.X.C = is_last ? 1 : 0;
	L4_VREG(d_base, L4_TCR_MR(first_mr)) = m.raw[0];
	L4_VREG(d_base, L4_TCR_MR(first_mr + 1)) = m.raw[1];

	return given;
}


/* when source->ipc != NULL, string transfers are resumed. otherwise, may fill
 * source->ipc and dest->ipc and put those threads to sleep. the string
 * transfer fault condition is indicated by -EFAULT. other valid return values
 * are 0 for completion, and >0 for ErrorCode.
 *
 * maps and grants complete or fail immediately (after string transfers), so
 * there's no resume case.
 *
 * FIXME: this has failure cases that don't properly un-set the C bit in the
 * last typed word that was stored.
 *
 * FIXME: also, the caller must flip the xfer timeout around when writing the
 * other thread's ErrorCode TCR.
 */
static int do_typed_transfer(
	struct thread *source,
	void *s_base,
	struct thread *dest,
	void *d_base,
	L4_MsgTag_t tag)
{
	assert(tag.X.t > 1);
	int rc = 0;

	/* (note: could avoid double scan on string transfer resume when it's
	 * known [from a bit in ipc_state, say] that there's no map items. that's
	 * common.)
	 */
	uint8_t map_items[31];
	struct str_meta str_buf[31];
	size_t n_map_items, n_strs;
	scan_typed_items(map_items, &n_map_items, str_buf, &n_strs, s_base, tag);
	struct ipc_state *st = source->ipc;
	if(n_strs > 0 && st == NULL) {
		/* start new transfer. */
		assert(dest->ipc == NULL);
		struct str_meta rsi_buf[31];
		int max_rsi = 0;
		size_t n_rsis = scan_buffer_regs(rsi_buf, &max_rsi, d_base, n_strs);
		assert(n_rsis <= n_strs);

		size_t st_size = ipc_state_size(n_strs, max_rsi);
		st = alloca(st_size);
		*st = (struct ipc_state){
			.from = source, .to = dest,
			.num_strings = n_strs, .num_rsis = n_rsis,
			.max_brs = max_rsi,
		};
		if(n_strs <= 2) {
			memcpy(st->meta.inl, str_buf, sizeof(struct str_meta) * n_strs);
			memcpy(&st->meta.inl[2], rsi_buf,
				sizeof(struct str_meta) * MIN(size_t, n_strs, n_rsis));
		} else {
			st->meta.ptr[0] = (void *)st + max_rsi * sizeof(L4_Word_t);
			st->meta.ptr[1] = st->meta.ptr[0] + n_strs;
		}
		rc = do_string_transfer(st, s_base, d_base, false);
		if(rc > 0) return rc;	/* straight-up error code */
		else if(rc == -EFAULT) {
			assert(source->ipc == st && dest->ipc == st);
			assert(CHECK_FLAG(source->flags, TF_SENDER));

			st = dup_state(st, st_size);
			if(st == NULL) panic("FEHLER FEHLER");	/* FIXME */
			source->ipc = st;
			dest->ipc = st;

			return -EFAULT;
		} else if(rc == -E2BIG) {
			/* message overflow */
			return st->str_off << 4 | 0x8;
		}
		BUG_ON(rc != 0, "unexpected rc=%d", rc);
	} else if(st != NULL) {
		/* resume old transfer. */
		assert(CHECK_FLAG(source->flags, TF_SENDER));
		assert(dest->ipc == source->ipc);
		assert(ipc_partner(source) == dest);
		assert(n_strs > 0);		/* not required, but true */

		rc = do_string_transfer(st, s_base, d_base, true);
		if(rc == -EFAULT) {
			/* continue fault loop. */
			TRACE("%s: continuing with faults\n", __func__);
			return -EFAULT;
		} else if(rc == -E2BIG) {
			rc = st->tot_offset << 4 | 0x8;
		}
		TRACE("%s: returning ec=%#lx\n", __func__, (L4_Word_t)rc);

		/* completed (ok or error). */
		assert(CHECK_FLAG(source->flags, TF_SENDER));
		assert(!CHECK_FLAG(dest->flags, TF_SENDER));
		source->flags &= ~TF_SENDER;
		source->ipc = NULL;
		dest->ipc = NULL;
		free(st);
	}

	/* NOTE: from the spec, it could be inferred that when there are both
	 * string items and map/grant items in a single IPC, they should be
	 * applied in order of appearance.
	 *
	 * however, that makes everything noisy and awful. so let's instead say
	 * that when string buffers and the receive window overlap in the
	 * receiver, mapping operations and string transfers happen in an
	 * undefined order.
	 */
	const int last = tag.X.u + tag.X.t;
	assert(last < 64);
	for(int i=0; rc == 0 && i < n_map_items; i++) {
		rc = do_mapgrant_transfer(map_items[i],
			i == n_map_items - 1 && map_items[i] + 2 <= last,
			source, s_base, dest, d_base);
		if(rc < 0) {
			assert(rc == -ENOMEM);
			/* TODO: do a Very Fancy Thing to resuscitate caller */
			printf("%s: mapgrant transfer had OOM!\n", __func__);
			panic("aaaaaaaa aaaaaaaaaa aaaaaaaaaaa aa aa");
		}
		rc = 0;
	}

	return rc;
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


int ipc_tstate(struct thread *t) {
	assert(t->ipc != NULL);
	return t->ipc->to == t ? L4_SCHEDRESULT_RECEIVING : L4_SCHEDRESULT_SENDING;
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
