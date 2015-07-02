
/* the typed half of IPC transfers.
 *
 * this module addresses two concerns: the actual typed transfers (i.e.
 * apply_mapitem(), apply_io_mapitem(), and do_string_transfer() with its
 * subroutines), and break/resume of string transfers when faults occur and
 * get repaired.
 *
 * there's a single entry point, do_typed_transfer(); it'll resume transfers
 * automagically where appropriate. thread states are only altered by fault
 * processing: a caller of do_typed_transfer() should know what states the
 * partners start in and where they may end up.
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <alloca.h>
#include <errno.h>
#include <ccan/likely/likely.h>

#include <l4/types.h>

#include <ukernel/util.h>
#include <ukernel/misc.h>
#include <ukernel/trace.h>
#include <ukernel/bug.h>
#include <ukernel/thread.h>
#include <ukernel/sched.h>
#include <ukernel/space.h>
#include <ukernel/mapdb.h>
#include <ukernel/ipc.h>


/* NOTE: shares same tracing ID with ipc.c */
#define TRACE(fmt, ...) TRACE_MSG(TRID_IPC, fmt, ##__VA_ARGS__)

#define ADJUST_PTR(p, oldbase, newbase) \
	(((void *)(p) - (uintptr_t)(oldbase)) + (uintptr_t)(newbase))


static void send_xfer_fault(
	struct thread *t,
	L4_Fpage_t fault,
	L4_Word_t ip,
	uint64_t xfto_abs);


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


/* string item iterators.
 *
 * TODO: these should be moved into the library for separate unit testing.
 */
static void stritem_first(
	struct stritem_iter *it,
	L4_StringItem_t *si,
	int n_words)		/* at most this many under @si */
{
	assert(L4_IsStringItem(si));
	assert(n_words >= 0 && n_words < 64);

	it->words = (L4_Word_t *)si;
	it->hdr = 0;
	it->sub = 1;
	it->max = n_words - 1;
	it->ptr = (uintptr_t)si->X.str.substring_ptr[0];
	it->len = si->X.string_length;
}


static bool stritem_next(struct stritem_iter *it)
{
	if(it->hdr + it->sub > it->max) {
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
 * FIXME: the resulting Map/Grant item's contents aren't being tested for.
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


/* returns the return value of mapdb_map_pages(), i.e. a mask of rights that
 * were given across the entire segment that was mapped. so if the MapItem's
 * page is RWX, but the sender's mapdb has only RX, the return value is RX.
 * modifies m->SndFpage to indicate where the map took effect in the
 * destination space, including the map window size.
 *
 * on failure, returns negative errno.
 */
static int apply_mapitem(
	struct thread *source,
	const void *s_base,
	struct thread *dest,
	void *d_base,
	L4_MapItem_t *m)
{
	L4_Fpage_t map_page = L4_MapItemSndFpage(*m),
		wnd = { .raw = L4_VREG(d_base, L4_TCR_BR(0)) & ~0xfUL };

	/* parameter validation */
	if(unlikely(source->space == dest->space
		|| L4_IsNilFpage(map_page)
		|| L4_IsNilFpage(wnd)))
	{
		goto noop;
	}

	bool is_grant;
	switch(m->X.__type) {
		case 0x5: is_grant = true; break;
		case 0x4: is_grant = false; break;
		default:
			/* neither mapitem or grantitem. skip it. */
			goto noop;
	}

	if(unlikely(L4_IsIoFpage(map_page))) {
		if(!L4_IsIoFpage(wnd)		/* no I/O RcvWindow */
			|| L4_IoFpageSizeLog2(wnd) != 16	/* wrong size */
			|| L4_IoFpagePort(wnd) != 0)	/* wrong offset */
		{
			/* it'd seem reasonable that a no-op IO fpage mapping should
			 * return a nil IOFpage, but such a format doesn't exist. instead
			 * it'll be indistinguishable from a failed memory mapping from
			 * the receiver's POV.
			 */
			goto noop;
		} else {
			return apply_io_mapitem(source, s_base, dest, d_base, *m, wnd);
		}
	}

	bool is_cas = wnd.raw == L4_CompleteAddressSpace.raw;
	const L4_Word_t snd_base = L4_MapItemSndBase(*m);
	TRACE("mapping 0x%lx:0x%lx, sndbase 0x%lx, rcvwindow %#lx:%#lx (%s)\n",
		L4_Address(map_page), L4_Size(map_page),
		snd_base, is_cas ? 0 : L4_Address(wnd),
		is_cas ? ~0ul : L4_Size(wnd),
		is_cas ? "CompleteAddressSpace" : "<- that");

	if(is_cas || L4_Size(map_page) < L4_Size(wnd)) {
		/* make the receive region smaller. */
		L4_Word_t mask = L4_Size(map_page) - 1,
			wnd_base = is_cas ? 0 : L4_Address(wnd);
		if(!is_cas && unlikely(snd_base >= L4_Size(wnd))) {
			TRACE("sndbase %#lx too big (wnd size %#lx)\n",
				snd_base, L4_Size(wnd));
			goto noop;
		}
		wnd = L4_FpageLog2(wnd_base + (snd_base & ~mask),
			L4_SizeLog2(map_page));
	} else if(L4_Size(map_page) > L4_Size(wnd)) {
		/* make the send page smaller. */
		L4_Word_t mask = L4_Size(wnd) - 1;
		if(unlikely(snd_base >= L4_Size(map_page))) {
			TRACE("sndbase %#lx too big (map size %#lx)\n",
				snd_base, L4_Size(map_page));
			goto noop;
		}
		map_page = L4_FpageLog2(L4_Address(map_page) + (snd_base & ~mask),
			L4_SizeLog2(wnd));
		L4_Set_Rights(&map_page, L4_Rights(L4_MapItemSndFpage(*m)));
	}
	TRACE("map_page=%#lx:%#lx, sndbase=%#lx, map_page'=%#lx:%#lx\n",
		L4_Address(L4_MapItemSndFpage(*m)), L4_Size(L4_MapItemSndFpage(*m)),
		snd_base, L4_Address(map_page), L4_Size(map_page));
	assert(L4_SizeLog2(map_page) == L4_SizeLog2(wnd));

	if(unlikely(fpage_overlap(wnd, dest->space->utcb_area)
		|| fpage_overlap(wnd, dest->space->kip_area)))
	{
		struct space *dsp = dest->space;
		TRACE("  skipped due to overlap w/ utcb=%#lx:%#lx or kip=%#lx:%#lx\n",
			L4_Address(dsp->utcb_area), L4_Size(dsp->utcb_area),
			L4_Address(dsp->kip_area), L4_Size(dsp->kip_area));
		goto noop;
	}

	int n = mapdb_map_pages(source->space,
		dest->space, &map_page, L4_Address(wnd));
	if(unlikely(n < 0)) {
		/* TODO: address this somehow */
		panic("mapdb_map_pages() failed in typed transfer!");
	}

	int given = L4_Rights(map_page);
	if(is_grant && given != 0) {
		assert((L4_Address(map_page) & 0xc00) == 0);
		mapdb_unmap_fpage(source->space, map_page, true, false, false);
	}

	m->X.snd_fpage.X.s = wnd.X.s;
	m->X.snd_fpage.X.b = wnd.X.b;
	return given;

noop:
	m->X.snd_fpage = L4_Nilpage;
	return 0;
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
	assert(n_maps <= L4_TypedWords(tag) / 2);
	assert(n_strs <= L4_TypedWords(tag) / 2);
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
		struct map_entry *e = mapdb_probe(sp, addr);
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
		 * if t->ipc isn't NULL, we'll clean it up for both peers.
		 */
		if(t->ipc != NULL) {
			struct ipc_state *st = t->ipc;
			st->from->ipc = NULL;
			st->to->ipc = NULL;
			assert(t->ipc == NULL);
			free(st);
		}

		return;
	}

	assert(t->ipc != NULL);

	t->ipc_from = t->orig_ipc_from;
	t->ipc_to = t->orig_ipc_to;
	/* FIXME: also restore send, recv timeouts (needed for other IPC stage) */

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
	t->orig_ipc_from = t->ipc_from;
	t->orig_ipc_to = t->ipc_to;
	set_pf_msg(t, utcb, L4_Address(fault), ip, L4_Rights(fault));
	/* (this can cause calls to ipc_send_half() to nest. due to the way IPC
	 * works in L4.X2, that's completely safe.)
	 */
	hook_push_back(&t->post_exn_call, &prexfer_ipc_hook, NULL);
	struct thread *dst = pager;
	ipc_user(t, &dst);

	assert(IS_IPC(t->status));
	assert(xferto_abs > 0);
	t->wakeup_time = xferto_abs;
	sq_update_thread(t);
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

	struct space *dest_space = st->to->space, *src_space = st->from->space;
	/* this exploits the memcpy_from() fast path under active receive. it's a
	 * no-op in active send.
	 */
	struct space *old_space = space_switch(src_space);

	int rc;
	uintptr_t copy_dst = reserve_heap_page();
	uint32_t copy_page = 0;
	int s_off = st->s_off, d_off = st->d_off;
	struct stritem_iter *src_iter = &st->xfer.it[0],
		*dst_iter = &st->xfer.it[1];
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
		struct map_entry *e = mapdb_probe(dest_space, dest_page);
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
		}

		int seg = MIN(int, PAGE_SIZE - ((dst_iter->ptr + d_off) & PAGE_MASK),
				MIN(int, src_iter->len - s_off, dst_iter->len - d_off));

		int d_pos = ((dst_iter->ptr & PAGE_MASK) + d_off) & PAGE_MASK;
		assert(d_pos >= 0 && seg >= 0);
		assert(d_pos + seg <= PAGE_SIZE);

		size_t n = space_memcpy_from(src_space, (void *)(copy_dst + d_pos),
			src_iter->ptr + s_off, seg);
		s_off += n;
		d_off += n;
		st->str_off += n;
		if(n < seg) {
			/* pop a read fault */
			TRACE("ipc: read fault after %u bytes (seg %d)\n",
				(unsigned)n, seg);
			*fault_p = L4_FpageLog2((src_iter->ptr + s_off) & ~PAGE_MASK,
				PAGE_BITS);
			L4_Set_Rights(fault_p, L4_Readable);
			goto fault;
		} else {
			assert(d_off <= dst_iter->len);
			if(d_off == dst_iter->len) {
				bool ok = stritem_next(dst_iter);
				if(!ok) {
					/* src ended at dst segment boundary. */
					break;
				}
				d_off = 0;
			}
		}
	} while(s_off < src_iter->len || (s_off = 0, stritem_next(src_iter)));

	rc = 0;

end:
	put_supervisor_page(copy_dst, 0);
	free_heap_page(copy_dst);
	space_switch(old_space);
	return rc;

fault:
	st->s_off = s_off;
	st->d_off = d_off;
	rc = -EFAULT;
	goto end;
}


static int copy_intraspace_stritem(L4_Fpage_t *fault_p, struct ipc_state *st)
{
	const size_t memcpy_grain = 64;		/* cache line size */

	assert(st->str_off >= 0);
	assert(st->to->space == st->from->space);

	struct stritem_iter *src_iter = &st->xfer.it[0],
		*dst_iter = &st->xfer.it[1];

	struct space *sp = st->from->space;
	space_switch(sp);

	void *volatile s_ptr = NULL, *volatile d_ptr = NULL;
	L4_Word_t fault_addr;
	if((fault_addr = catch_pf()) != 0) {
		/* fault_addr is the linear (userspace) address. */
		bool is_write = BETWEEN(dst_iter->ptr,
			dst_iter->ptr + dst_iter->len - 1, fault_addr);
		assert(is_write || BETWEEN(src_iter->ptr,
			src_iter->ptr + src_iter->len - 1, fault_addr));

		/* duplicate last 256 bytes of progress so far, if any. on failure
		 * retry from start.
		 *
		 * FIXME: the restart case carries a risk of perpetual looping when
		 * the result is that more than 2 pages are involved per segment, and
		 * a pager can provide only exactly two.
		 */
		if(catch_pf() == 0) {
			size_t progress = 0, n_done = fault_addr
				- (uintptr_t)(is_write ? dst_iter->ptr + st->d_off
					: src_iter->ptr + st->s_off);
			if(n_done > memcpy_grain) {
				/* assume completion (but verify w/ assert) */
				progress = n_done - memcpy_grain;
				n_done = memcpy_grain;
				assert(memcmp(d_ptr, s_ptr, progress) == 0);
				st->s_off += progress;
				st->d_off += progress;
				st->str_off += progress;
			}

			if(n_done > 0) {
				/* copy the tail as an exactly-sized fragment */
				assert(is_write || (((uintptr_t)s_ptr + progress + n_done) & PAGE_MASK) == 0);
				assert(!is_write || (((uintptr_t)d_ptr + progress + n_done) & PAGE_MASK) == 0);
				memcpy(d_ptr + progress, s_ptr + progress, n_done);
				st->s_off += n_done;
				st->d_off += n_done;
				st->str_off += n_done;
			}

			assert(memcmp(s_ptr, d_ptr, progress + n_done) == 0);

			uncatch_pf();
		}

#ifndef NDEBUG
		/* enforce consistency wrt mapdb */
		struct map_entry *e = mapdb_probe(sp, fault_addr);
		assert(e == NULL
			|| !CHECK_FLAG(L4_Rights(e->range),
				is_write ? L4_Writable : L4_Readable));
#endif

		*fault_p = L4_FpageLog2(fault_addr & ~PAGE_MASK, PAGE_BITS);
		L4_Set_Rights(fault_p, L4_Readable | (is_write ? L4_Writable : 0));
		return -EFAULT;
	}

	int s_off = st->s_off, d_off = st->d_off;
	do {
		st->s_off = s_off;
		st->d_off = d_off;
#if 0
		printf("start of loop; str_off=%d, s_off=%d, d_off=%d\n",
			st->str_off, s_off, d_off);
		printf("  src len %d, ptr %#lx\n", (int)src_iter->len,
			(L4_Word_t)src_iter->ptr);
		printf("  dst len %d, ptr %#lx\n", (int)dst_iter->len,
			(L4_Word_t)dst_iter->ptr);
#endif

		int seg = MIN(int, src_iter->len - s_off, dst_iter->len - d_off);
		assert(seg >= 0);

		uintptr_t src_addr = src_iter->ptr + s_off + KERNEL_SEG_SIZE,
			dst_addr = dst_iter->ptr + d_off + KERNEL_SEG_SIZE;

		/* catch illegal access into kernel range.
		 * FIXME: make this cover the segment's end also.
		 */
		if(unlikely(src_addr < KERNEL_SEG_SIZE)) {
			*fault_p = L4_FpageLog2((src_addr - KERNEL_SEG_SIZE) & ~PAGE_MASK,
				PAGE_BITS);
			L4_Set_Rights(fault_p, L4_Readable);
			uncatch_pf();
			return -EFAULT;
		}
		if(unlikely(dst_addr < KERNEL_SEG_SIZE)) {
			*fault_p = L4_FpageLog2((dst_addr - KERNEL_SEG_SIZE) & ~PAGE_MASK,
				PAGE_BITS);
			L4_Set_Rights(fault_p, L4_Writable);
			uncatch_pf();
			return -EFAULT;
		}

		s_ptr = (void *)src_addr;
		d_ptr = (void *)dst_addr;
		memcpy((void *)dst_addr, (void *)src_addr, seg);
		s_off += seg;
		d_off += seg;
		st->str_off += seg;

		assert(d_off <= dst_iter->len);
		if(d_off == dst_iter->len) {
			bool ok = stritem_next(dst_iter);
			if(!ok) {
				/* src ended at dst segment boundary. */
				break;
			}
			d_off = 0;
		}
	} while(s_off < src_iter->len || (s_off = 0, stritem_next(src_iter)));

	uncatch_pf();
	st->s_off = s_off;
	st->d_off = d_off;
	return 0;
}


static int copy_stritem(L4_Fpage_t *fault_p, struct ipc_state *st)
{
	if(st->from->space == st->to->space) {
		return copy_intraspace_stritem(fault_p, st);
	} else {
		return copy_interspace_stritem(fault_p, st);
	}
}


/* copy the sender's string item to the receiver's message registers, and
 * initialize the string item iterator. this function must be called only
 * after a valid next_dst_strbuf(). it also bumps st->str_pos when successful.
 *
 * TODO[v1]: make a copy of the sender's string item, because right now
 * st->send_si will end up referring to the sender's UTCB which may disappear
 * (or relocate; same thing really) once address space recycling comes about.
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

	/* copy to receiver, set/clear C bit as appropriate */
	memcpy(&L4_VREG(d_base, L4_TCR_MR(meta->first_reg)),
		&L4_VREG(s_base, L4_TCR_MR(meta->first_reg)),
		meta->n_words * sizeof(L4_Word_t));
	L4_StringItem_t *rcv_si = (L4_StringItem_t *)&L4_VREG(d_base,
		L4_TCR_MR(meta->first_reg));
	rcv_si->X.C = meta->first_reg + meta->n_words < L4_TypedWords(st->tag);
	/* change string pointers to match receive buffer addresses.
	 *
	 * however, if a substring cannot fit into the portion of a receive buffer
	 * where its copy starts at, the pointer is zeroed instead. this would
	 * typically cause a null-page fault, which helps avoid a neglectful
	 * redirector passing the in-betweens of its scatter data structure along
	 * unnoticed.
	 *
	 * TODO[v1]: this loop could be removed by having stritem_next() store a
	 * word offset to the substring pointer somewhere as well. this way, the
	 * transfer would provide the location of the substring pointer in the
	 * destination's MRs also, which would be written as the copy gathers
	 * fragments from the sender.
	 */
	bool last;
	assert(st->xfer.it[1].words != NULL);
	struct stritem_iter rb_iter = st->xfer.it[1];
	int left = rb_iter.len - st->d_off;
	do {
		last = !L4_CompoundString(rcv_si);
		int sublen = rcv_si->X.string_length, n_subs = L4_Substrings(rcv_si);
		for(int i=0; i < n_subs; i++) {
			if(left < sublen) {
				bool got_next = stritem_next(&rb_iter);
				if(left > 0 || rb_iter.len < sublen) {
					/* zero pointer, dock length. this gives the next
					 * substring a chance to report its address correctly,
					 * making for nicer error reports in the receiver.
					 */
					rcv_si->X.str.substring_ptr[i] = (void *)0;
					left -= sublen;		/* dips below zero */
					if(got_next) left += rb_iter.len;
				} else {
					rcv_si->X.str.substring_ptr[i] = (void *)rb_iter.ptr;
					left = rb_iter.len - sublen;
				}
			} else {
				rcv_si->X.str.substring_ptr[i] = (void *)rb_iter.ptr
					+ rb_iter.len - left;
				left -= sublen;
			}
		}
		rcv_si = (L4_StringItem_t *)&rcv_si->raw[n_subs + 1];
	} while(!last);

	/* set up us the bomb. */
	st->str_off = 0;
	L4_StringItem_t *si = (L4_StringItem_t *)&L4_VREG(s_base,
		L4_TCR_MR(meta->first_reg));
	if(unlikely(!L4_IsStringItem(si))) return false;
	stritem_first(&st->xfer.it[0], si, meta->n_words);
	st->s_off = 0;

	/* TODO: this could fall off the end if a compound string's last header's
	 * C bit is set in between (such as by a cooperating local pager). that
	 * may cause an invalid read access in the kernel.
	 */
	*len_p = stritemlen(si);

	st->str_pos++;
	return true;
}


/* note: it says "next", but this doesn't actually bump @st->str_pos .
 * next_src_string() does that instead.
 */
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

	stritem_first(&st->xfer.it[1], get_rsi(st), meta->n_words);
	st->d_off = 0;
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
			st->str_off = 0;
			size_t dst_len = 0;
			bool d_ok = next_dst_strbuf(st, d_base, &dst_len),
				s_ok = next_src_string(st, s_base, d_base, &src_len);
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
		size_t dst_len = 0;
		bool have_next = next_dst_strbuf(st, d_base, &dst_len);
		if(next_src_string(st, s_base, d_base, &src_len)) {
			if(!have_next || src_len > dst_len) {
				/* no more woodchucks. */
				return -E2BIG;
			} else {
				int rc = check_prexfer_faults(st, src_len, s_base, d_base);
				if(rc != 0) return rc;
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
	int given = apply_mapitem(source, s_base, dest, d_base, &m);
	if(given < 0) return given;

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
int do_typed_transfer(
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
		if(max_rsi < 2) {
			/* receiver can't accept even one string item despite having the
			 * BR0 flag set.
			 */
			return 8;	/* msg overflow */
		}

		size_t st_size = ipc_state_size(n_strs, max_rsi);
		st = alloca(st_size);
		*st = (struct ipc_state){
			.from = source, .to = dest, .tag = tag,
			.num_strings = n_strs, .num_rsis = n_rsis,
			.max_brs = max_rsi,
		};
		struct str_meta *meta[2];
		if(n_strs <= 2) {
			meta[0] = &st->meta.inl[0];
			meta[1] = &st->meta.inl[2];
		} else {
			void *base = &st[1];
			meta[0] = st->meta.ptr[0] = base + max_rsi * sizeof(L4_Word_t);
			meta[1] = st->meta.ptr[1] = st->meta.ptr[0] + n_strs;
		}
		memcpy(meta[0], str_buf, sizeof(struct str_meta) * n_strs);
		memcpy(meta[1], rsi_buf,
			sizeof(struct str_meta) * MIN(size_t, n_strs, n_rsis));
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
