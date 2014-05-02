
/* tests of L4.X2 string transfer features.
 *
 * TODO: tests on the following:
 *   - offset field in L4_ErrorCode() in xfer timeout
 *   - and in xfer abort
 */

#define DROPPAGER_IMPL_SOURCE 1

#include <stdlib.h>
#include <string.h>
#include <alloca.h>
#include <unistd.h>
#include <ccan/compiler/compiler.h>
#include <ccan/str/str.h>
#include <ccan/crc32c/crc32c.h>
#include <ccan/darray/darray.h>

#include <l4/types.h>
#include <l4/space.h>
#include <l4/ipc.h>
#include <l4/thread.h>
#include <l4/message.h>

#include <ukernel/util.h>

#include "defs.h"
#include "test.h"


#define ECHO_LABEL	0x6857		/* "hW" */
#define PING_LABEL	0x6849		/* "hI" */


static L4_ThreadId_t test_tid, stats_tid, drop_tid;
static struct pager_stats *stats;

/* copyright by whomever the fuck. used fairly. the double space should not be
 * corrected.
 */
static const char *copypasta = "John Stalvern waited. The lights above him "
	" blinked and sparked out of the air. There were demons in the base.";

static const uint32_t seed_bins[4] = {
	0xdeadbeef, 0xf0adcafe, 0xb00b1e5, 0x71849a3f,
};


static L4_ThreadId_t start_str_receiver(int n_bufs, size_t buf_size);


static bool read_fault(L4_Word_t addr) {
	return CHECK_FLAG(L4_Rights(get_fault(stats, addr)), L4_Readable);
}


static bool write_fault(L4_Word_t addr) {
	return CHECK_FLAG(L4_Rights(get_fault(stats, addr)), L4_Writable);
}


static void diag_faults(struct pager_stats *st)
{
	diag("%d faults, %d read, %d write:",
		st->n_faults, st->n_read, st->n_write);
	for(int i=0; i <= st->log_top; i++) {
		diag("  %#lx:%#lx, %#x", L4_Address(st->log[i]),
			L4_Size(st->log[i]), L4_Rights(st->log[i]));
	}
}


/* TODO: move into a different module */
void flush_byte_range(
	L4_Word_t first_address,
	L4_Word_t size,
	L4_Word_t access)
{
	if(access == 0) access = L4_FullyAccessible;
	L4_Word_t last_address = (first_address + size + PAGE_SIZE - 1) & ~PAGE_MASK;
	first_address &= ~PAGE_MASK;

	L4_Fpage_t buf[64];
	L4_Word_t addr;
	int size_log2, ct = 0;
	for_page_range(first_address, last_address, addr, size_log2) {
		buf[ct] = L4_FpageLog2(addr, size_log2);
		L4_Set_Rights(&buf[ct], access);
#if 0
		diag("flushing %#lx:%#lx", L4_Address(buf[ct]), L4_Size(buf[ct]));
#endif
		if(++ct == 63) {
			L4_FlushFpages(63, buf);
			ct = 0;
		}
	}
	if(ct > 0) L4_FlushFpages(ct, buf);
}


static void stt_echo_impl(
	const L4_MsgTag_t tag,
	char *recvbuf,
	char *rbuf2,
	size_t rbuf_len,
	L4_StringItem_t *got_si)
{
	/* prepend a thing to the first string buffer. */
	if(!L4_IsStringItem(got_si)) {
		diag("is not a string item");
		goto fail;
	}
	int si_len = stritemlen(got_si);
	recvbuf[MIN(int, rbuf_len - 1, si_len)] = '\0';
	int rec_len = strlen(recvbuf), tmplen = rec_len + 64,
		gs_words = __L4_EndOfString(got_si, NULL)->raw - got_si->raw;
	assert(gs_words >= 2);
	char *tmp = malloc(tmplen);
	if(tmp == NULL) {
		strscpy(recvbuf, "malloc failed", rbuf_len);
	} else {
		snprintf(tmp, tmplen, "echo, echo! %s", recvbuf);
		strscpy(recvbuf, tmp, tmplen);
		free(tmp);
	}

	diag("%s: tag.X.t=%d, gs_words=%d, reply len=%d", __func__,
		(int)L4_TypedWords(tag), gs_words, strlen(recvbuf));
	L4_MsgTag_t rtag = { };
	L4_StringItem_t *out_si = alloca(sizeof(L4_Word_t) * 64);
	if(L4_TypedWords(tag) == 2) {
		/* simple echo string gets a simple reply. */
		*out_si = L4_StringItem(strlen(recvbuf) + 1, recvbuf);
		rtag.X.t = 2;
	} else {
		/* a compound echo gets a compound reply. it'll have the same form as
		 * the sender's string, and a spill segment at the end.
		 */
		int pos = 0;
		bool end = false;
		while(!end) {
			const int n_subs = L4_Substrings(got_si),
				sz = got_si->X.string_length;
			if(pos == 0) {
				diag("first header 0:%d", sz);
				*out_si = L4_StringItem(sz, recvbuf);
				pos = sz;
			} else {
				diag("continuation header %d:%d", pos, sz);
				L4_StringItem_t t = L4_StringItem(sz, &recvbuf[pos]);
				L4_AddSubstringTo(out_si, &t);
				pos += sz;
			}
			for(int i=1; i < n_subs; i++) {
				diag(" ... substring %d = %d:%d", i, pos, sz);
				L4_AddSubstringAddressTo(out_si, &recvbuf[pos]);
				pos += sz;
			}
			rtag.X.t += n_subs + 1;
			end = !L4_CompoundString(got_si);
			got_si = (L4_StringItem_t *)&got_si->raw[n_subs + 1];
		}
		if(strlen(recvbuf) + 1 > pos) {
			diag("tail header %d:%d", pos, strlen(recvbuf) + 1 - pos);
			L4_StringItem_t rest = L4_StringItem(
				strlen(recvbuf) + 1 - pos, &recvbuf[pos]);
			L4_AddSubstringTo(out_si, &rest);
			rtag.X.t += 2;
		}
	}

	/* echo a second string item, too. */
	if(L4_TypedWords(tag) > gs_words) {
		/* skip non-string typed items */
		L4_StringItem_t *next = got_si;
		while(!L4_IsStringItem(next) && gs_words + 2 < L4_TypedWords(tag)) {
			gs_words += 2;
			next++;
		}
		if(!L4_IsStringItem(next)) {
			diag("next typed items had no string item");
			goto fail;
		}
		diag("HAEV SECOND STREGN (%d x %d bytes)",
			(int)next->X.string_length, (int)L4_Substrings(next));
		L4_StringItem_t *next_out = __L4_EndOfString(out_si, NULL);
		rbuf2[MIN(int, rbuf_len - 1, stritemlen(next))] = '\0';
		*next_out = L4_StringItem(strlen(rbuf2) + 1, rbuf2);
		rtag.X.t += 2;
	}
	diag("%s: reply has %lu typed words", __func__, L4_TypedWords(rtag));

	L4_LoadMR(0, rtag.raw);
	L4_LoadMRs(1, L4_TypedWords(rtag), out_si->raw);
	return;

fail:
	L4_LoadMR(0, 0);
}


static void string_test_thread(void *param UNUSED)
{
	const int rbuf_len = 64 * 1024;
	char *recvbuf = malloc(rbuf_len), *rbuf2 = malloc(rbuf_len);
	memset(recvbuf, 0, rbuf_len);
	memset(rbuf2, 0, rbuf_len);
	L4_StringItem_t recv_si = L4_StringItem(rbuf_len, recvbuf),
		r2_si = L4_StringItem(rbuf_len, rbuf2),
		*got_si = alloca(sizeof(L4_Word_t) * 64);

	const size_t acc_size = 1 << 14;
	void *acc_mem = malloc(acc_size * 2);
	fail_if(acc_mem == NULL);
	L4_Fpage_t acc_page = L4_Fpage(
		((L4_Word_t)acc_mem + acc_size - 1) & ~(acc_size - 1),
		acc_size);
	L4_Acceptor_t acc = L4_AddAcceptor(L4_StringItemsAcceptor,
		L4_MapGrantItems(acc_page));

#if 0
	diag("%s running as %lu:%lu", __func__,
		L4_ThreadNo(L4_Myself()), L4_Version(L4_Myself()));
	diag("%s: recvbuf is %p:%#x", __func__, recvbuf, (unsigned)rbuf_len);
#endif
	L4_Time_t delay = L4_ZeroTime;
	int delay_repeat = 0;
	bool delay_spin = false, running = true;
	while(running) {
		L4_ThreadId_t from;
		L4_Accept(acc);
		recv_si.X.C = 1; L4_LoadBRs(1, 2, recv_si.raw);
		r2_si.X.C = 0; L4_LoadBRs(3, 2, r2_si.raw);
		L4_MsgTag_t tag = L4_Wait(&from);

		while(running) {
			if(L4_IpcFailed(tag)) {
				diag("%s got ipc failure, ec %#lx", __func__, L4_ErrorCode());
				break;
			}

			switch(tag.X.label) {
				case QUIT_LABEL: running = false; break;
				case ECHO_LABEL:
					if(tag.X.t == 0 || tag.X.u != 0) {
						diag("invalid echo message");
						L4_LoadMR(0, 0);
						break;
					}
					L4_StoreMRs(L4_UntypedWords(tag) + 1,
						L4_TypedWords(tag), got_si->raw);
					stt_echo_impl(tag, recvbuf, rbuf2, rbuf_len, got_si);
					break;

				case DELAY_LABEL: {
					if(tag.X.u != 3 || tag.X.t > 0) {
						diag("invalid delay message");
						L4_LoadMR(0, 0);
						break;
					}
					L4_Word_t timeword, repeat, spin;
					L4_StoreMR(1, &timeword);
					L4_StoreMR(2, &repeat);
					L4_StoreMR(3, &spin);
					delay.raw = timeword;
					delay_repeat = repeat;
					delay_spin = !!spin;
					break;
				}

				case PING_LABEL: {
					/* reply with the same regs. */
					L4_Word_t regs[64];
					L4_StoreMRs(0, tag.X.u + tag.X.t + 1, regs);
					L4_LoadMRs(0, tag.X.u + tag.X.t + 1, regs);
					break;
				}

				default:
					diag("unknown label %#lx (tag %#lx)", (L4_Word_t)tag.X.label,
						tag.raw);
					L4_LoadMR(0, 0);
			}

			if(running) {
				/* simple acceptor over the entire recvbuf. */
				L4_Accept(acc);
				L4_LoadBRs(1, 2, recv_si.raw);
				L4_LoadBRs(3, 2, r2_si.raw);
				if(delay.raw == L4_ZeroTime.raw) {
					assert(delay_repeat == 0);
					tag = L4_ReplyWait(from, &from);
				} else {
					tag = L4_Reply(from);
					if(delay_spin) usleep(time_in_us(delay)); else L4_Sleep(delay);
					if(--delay_repeat == 0) delay = L4_ZeroTime;
					if(L4_IpcSucceeded(tag)) {
						tag = L4_Wait(&from);
					}
				}
			}
		}
	}

	free(recvbuf);
	free(acc_mem);
	L4_Set_Rights(&acc_page, L4_FullyAccessible);
	L4_FlushFpage(acc_page);
}


static void stt_setup(void)
{
	fail_unless(L4_IsNilThread(test_tid));
	test_tid = start_thread(&string_test_thread, NULL);
	fail_unless(!L4_IsNilThread(test_tid));
}


static void stt_teardown(void)
{
	bool quit_ok = send_quit(test_tid);
	fail_unless(quit_ok, "send_quit() failed, ec %#lx", L4_ErrorCode());

	void *value = join_thread(test_tid);
	fail_unless(value == NULL,
		"unexpected return from string test thread: `%s'", (char *)value);
	test_tid = L4_nilthread;
}


/* simple echo. send string, receev bacon. */
static void echo(
	L4_ThreadId_t serv_tid,
	char *replybuf,
	size_t reply_size,
	L4_StringItem_t *got_si,
	const char *echostr,
	size_t echo_len)
{
	L4_StringItem_t rep_si = L4_StringItem(reply_size, replybuf);
	L4_Accept(L4_StringItemsAcceptor);
	L4_LoadBRs(1, 2, rep_si.raw);
	if(echo_len == 0) echo_len = strlen(echostr);
	L4_StringItem_t si = L4_StringItem(echo_len + 1, (void *)echostr);
	L4_LoadMR(0, (L4_MsgTag_t){ .X.label = ECHO_LABEL, .X.t = 2 }.raw);
	L4_LoadMRs(1, 2, si.raw);
	L4_MsgTag_t tag = L4_Call_Timeouts(serv_tid, TEST_IPC_DELAY,
		TEST_IPC_DELAY);
	fail_unless(L4_IpcSucceeded(tag), "ipc failed: ec %#lx", L4_ErrorCode());
	fail_unless(tag.X.t == 2, "reply tag is weird (%#lx)", tag.raw);

	if(got_si != NULL) {
		L4_StoreMRs(1, 2, got_si->raw);
		fail_unless(L4_IsStringItem(got_si));
	}
}


static void echo_ok_2(
	char *replybuf,
	size_t replybuf_len,
	const char *echostr,
	L4_StringItem_t *got_si)
{
	assert(strlen(echostr) > 7);

	if(!L4_IsStringItem(got_si)) {
		/* make a very short one up. */
		diag("%s: inventing a 2-byte string item", __func__);
		*got_si = L4_StringItem(2, got_si->raw);
	}
	replybuf[MIN(int, replybuf_len - 1, stritemlen(got_si))] = '\0';
	int rlen = strlen(replybuf);
	ok(rlen >= strlen(echostr), "reply length >= input length");
	if(!ok(strends(replybuf, echostr), "echo output ends with input")) {
		diag("replybuf=`%s'", replybuf);
		diag("echostr=`%s'", echostr);
	}
}


/* main test suite */


START_TEST(echo_simple)
{
	plan_tests(5);
	const char *echostr = "does a polar bear crap in the woods?";

	char replybuf[1024];
	L4_StringItem_t got_si;
	echo(test_tid, replybuf, sizeof(replybuf), &got_si, echostr, 0);

	fail_unless(L4_IsStringItem(&got_si));
	replybuf[MIN(int, sizeof(replybuf) - 1, got_si.X.string_length)] = '\0';
	int rlen = strlen(replybuf);
	ok(rlen >= strlen(echostr), "reply length >= input length");
	ok(streq(&replybuf[rlen - strlen(echostr)], echostr),
		"echo output ends with input");
	ok(got_si.X.C == 0, "string item was the last one");
	if(!ok1(L4_Substrings(&got_si) == 1)) {
		diag("...=%d", (int)L4_Substrings(&got_si));
	}

	if(!ok(L4_Substrings(&got_si) >= 1
			&& L4_Substring(&got_si, 1) == &replybuf[0],
		"received stringitem points to replybuf"))
	{
		diag("substr=%p, replybuf=%p",
			L4_Substring(&got_si, 1), &replybuf[0]);
	}
}
END_TEST


/* functions to build various kinds of compound string item. returns the
 * number of words used.
 *
 * some are only valid for the "repercussions of evil" copypasta.
 */
struct piece {
	size_t n, sz;
};


static size_t build_stritem(
	L4_StringItem_t *out_si,
	char *buf,
	const struct piece *pieces,
	size_t n_pieces)
{
	for(int i=0, p=0; i < n_pieces; i++) {
		if(i == 0) {
			*out_si = L4_StringItem(pieces[0].sz, buf);
			p = pieces[0].sz;
		} else {
			L4_StringItem_t t_si = L4_StringItem(pieces[i].sz, &buf[p]);
			p += pieces[i].sz;
			L4_AddSubstringTo(out_si, &t_si);
		}
		for(int j=1; j < pieces[i].n; j++) {
			L4_AddSubstringAddressTo(out_si, &buf[p]);
			p += pieces[i].sz;
		}
	}

	out_si->X.C = 0;
	return __L4_EndOfString(out_si, NULL)->raw - out_si->raw;
}


/* cuts a test string into smaller sections and sends that as a compound
 * string item.
 */
START_LOOP_TEST(echo_compound_send, iter, 0, 1)
{
	plan_tests(5);
	const bool multi = CHECK_FLAG(iter, 1);
	char *echo_str = strdup(copypasta);
	const size_t echo_len = strlen(echo_str);
	diag("echo_len=%d, multi=%s", echo_len, btos(multi));

	char *replybuf = malloc(2048);
	L4_StringItem_t rep_si = L4_StringItem(2048, replybuf);
	rep_si.X.C = 0;

	L4_StringItem_t *send_si = alloca(sizeof(L4_Word_t) * 64),
		*got_si = alloca(sizeof(L4_Word_t) * 64);

	memset(replybuf, 0, 2048);
	L4_Accept(L4_StringItemsAcceptor);
	L4_LoadBRs(1, 2, rep_si.raw);

	int n_words;
	if(multi) {
		/* build a multi-header compound string of both single- and
		 * multi-pointer items.
		 */
		static const struct piece pieces[] = {
			{ 1, 40 }, { 2, 7 }, { 3, 11 }, { 3, 6 }, { 1, 6 },
		};
		n_words = build_stritem(send_si, echo_str,
			pieces, NUM_ELEMENTS(pieces));
	} else {
		/* build a one-header compound string with three pointers, each 37
		 * bytes long.
		 */
		static const struct piece pc = { 3, 37 };
		n_words = build_stritem(send_si, echo_str, &pc, 1);
		assert(n_words == L4_Substrings(send_si) + 1);
	}
	assert(stritemlen(send_si) == echo_len + 1);

	L4_Word_t ec = 0;
	L4_LoadMR(0, (L4_MsgTag_t){ .X.label = ECHO_LABEL, .X.t = n_words }.raw);
	L4_LoadMRs(1, n_words, send_si->raw);
	L4_MsgTag_t tag = L4_Call(test_tid);
	if(L4_IpcSucceeded(tag)) L4_StoreMRs(1, tag.X.t, got_si->raw);
	else ec = L4_ErrorCode();
	if(!ok(L4_IpcSucceeded(tag), "ipc ok")) diag("ec=%#lx", ec);

	fail_unless(L4_IsStringItem(got_si));
	replybuf[MIN(int, 2047, stritemlen(got_si))] = '\0';
	size_t reply_len = strlen(replybuf);
	if(!ok1(reply_len >= echo_len)) {
		diag("reply_len=%d, echo_len=%d", reply_len, echo_len);
	}
	if(!ok1(strends(replybuf, echo_str))) {
		diag("replybuf=`%s'", replybuf);
		diag("echo_str=`%s'", echo_str);
	}

	/* verify pointers returned for the compound string. */
	ok1(L4_CompoundString(got_si));
	diag("subs=%d, is_comp=%s", (int)L4_Substrings(got_si),
		btos(L4_CompoundString(got_si)));
	diag("got_si length = %d words, %d bytes",
		__L4_EndOfString(got_si, NULL)->raw - got_si->raw,
		stritemlen(got_si));
	int pos = 0;
	bool last, all_ok = true;
	do {
		last = !L4_CompoundString(got_si);
		for(int i=1; i <= L4_Substrings(got_si); i++) {
			if(&replybuf[pos] != L4_Substring(got_si, i)) {
				all_ok = false;
				diag("pos=%d, i=%d, &replybuf[pos]=%p, substr=%p",
					pos, i, &replybuf[pos], L4_Substring(got_si, i));
			}
			pos += got_si->X.string_length;
		}
		got_si = (L4_StringItem_t *)&got_si->raw[L4_Substrings(got_si) + 1];
	} while(!last);
	ok(all_ok, "received stringitem points to buffer");

	free(replybuf);
	free(echo_str);
}
END_TEST


/* similar to the send-side tests, this loads a single-header compound string
 * item as receive buffers.
 */
START_LOOP_TEST(echo_compound_recv, iter, 0, 1)
{
	plan_tests(3);
	const bool multi = CHECK_FLAG(iter, 1);
	char *echo_str = strdup(copypasta);
	const size_t echo_len = strlen(echo_str);

	L4_StringItem_t *got_si = alloca(sizeof(L4_Word_t) * 64),
		*rep_si = alloca(sizeof(L4_Word_t) * 64);

	char *replybuf = calloc(1, 2048);
	int n_words;
	if(multi) {
		/* 9*3 + 3*11 + 19*78 = 1542 bytes */
		static const struct piece pieces[] = {
			{ 9, 3 }, { 3, 11 }, { 19, 78 },
		};
		n_words = build_stritem(rep_si, replybuf,
			pieces, NUM_ELEMENTS(pieces));
		assert(n_words < 63);
	} else {
		static const struct piece pc = { 32, 63 };
		n_words = build_stritem(rep_si, replybuf, &pc, 1);
		assert(n_words == L4_Substrings(rep_si) + 1);
	}
	diag("subs=%d, n_words=%d, itemlen=%d",
		L4_Substrings(rep_si), n_words, stritemlen(rep_si));
	assert(stritemlen(rep_si) > 200);
	assert(stritemlen(rep_si) <= 2048);

	L4_Accept(L4_StringItemsAcceptor);
	L4_LoadBRs(1, n_words, rep_si->raw);

	/* now a basic echo thing. */
	L4_StringItem_t send_si = L4_StringItem(111, echo_str);
	L4_Word_t ec = 0;
	L4_LoadMR(0, (L4_MsgTag_t){ .X.label = ECHO_LABEL, .X.t = 2, }.raw);
	L4_LoadMRs(1, 2, send_si.raw);

	L4_MsgTag_t tag = L4_Call(test_tid);
	if(L4_IpcSucceeded(tag)) L4_StoreMRs(1, tag.X.t, got_si->raw);
	else ec = L4_ErrorCode();
	if(!ok(L4_IpcSucceeded(tag), "ipc ok")) diag("ec=%#lx", ec);

	fail_unless(L4_IsStringItem(got_si) || !L4_CompoundString(got_si));
	replybuf[MIN(int, 2047, stritemlen(got_si))] = '\0';
	size_t reply_len = strlen(replybuf);
	if(!ok1(reply_len >= echo_len)) {
		diag("reply_len=%d, echo_len=%d", reply_len, echo_len);
	}
	ok(streq(&replybuf[reply_len - echo_len], echo_str),
		"echo output ends with input");

	free(replybuf);
	free(echo_str);
}
END_TEST


static void add_stritem(L4_StringItem_t **si_p, size_t len, char *str) {
	**si_p = L4_StringItem(len, str);
	(*si_p)++;
}


static void add_mapitem(L4_StringItem_t **si_p)
{
	const size_t n_bytes = 8192;
	/* just a slice of the heap, then. */
	void *something = aligned_alloc(PAGE_SIZE, n_bytes);
	memset(something, '\0', n_bytes);

	L4_MapItem_t mi = L4_MapItem(
		L4_Fpage((L4_Word_t)something, n_bytes), 0);
	(*si_p)->raw[0] = mi.raw[0];
	(*si_p)->raw[1] = mi.raw[1];
	(*si_p)++;
}


/* set up two outgoing transfers, one a simple one and the other a compound
 * one.
 * boolean flags determine the following:
 *   - whether the compound one comes first
 *   - whether it's a single-header item, or has multiple headers
 *   - whether there's a map item in between the two
 */
START_LOOP_TEST(echo_multi_comp, iter, 0, 7)
{
	plan_tests(4);
	const bool comp_first = CHECK_FLAG(iter, 1),
		comp_multi = CHECK_FLAG(iter, 2),
		have_map = CHECK_FLAG(iter, 4);
	diag("comp_first=%s, comp_multi=%s, have_map=%s",
		btos(comp_first), btos(comp_multi), btos(have_map));

	char *echo_str = strdup(copypasta),
		*recvbufs[2] = {
			malloc(64 * 1024), malloc(64 * 1024),
		};
	size_t echo_len = strlen(echo_str);

	/* construction of test case */
	L4_StringItem_t *build_si = alloca(sizeof(L4_Word_t) * 64),
		*si_pos = build_si,
		*got_si = alloca(sizeof(L4_Word_t) * 64);
	if(!comp_first) {
		add_stritem(&si_pos, echo_len + 1, echo_str);
		if(have_map) add_mapitem(&si_pos);
	}
	int n_words;
	if(!comp_multi) {
		static const struct piece pc = { 3, 37 };
		n_words = build_stritem(si_pos, echo_str, &pc, 1);
	} else {
		static const struct piece comp_pcs[] = {
			{ 4, 19 }, { 1, 9 }, { 2, 13 },
		};
		n_words = build_stritem(si_pos, echo_str,
			comp_pcs, NUM_ELEMENTS(comp_pcs));
	}
	si_pos = (L4_StringItem_t *)&si_pos->raw[n_words];
	if(comp_first) {
		if(have_map) add_mapitem(&si_pos);
		add_stritem(&si_pos, echo_len + 1, echo_str);
	}
	int n_typed = si_pos->raw - build_si->raw;

	/* the IPC bit */
	L4_LoadBR(0, L4_StringItemsAcceptor.raw);
	L4_StringItem_t si = L4_StringItem(64 * 1024, recvbufs[0]);
	si.X.C = 1;
	L4_LoadBRs(1, 2, si.raw);
	si = L4_StringItem(64 * 1024, recvbufs[1]);
	si.X.C = 0;
	L4_LoadBRs(3, 2, si.raw);
	L4_LoadMR(0, (L4_MsgTag_t){ .X.label = ECHO_LABEL, .X.t = n_typed }.raw);
	L4_LoadMRs(1, n_typed, build_si->raw);
	L4_MsgTag_t tag = L4_Call(test_tid);

	/* measurements */
	L4_Word_t ec = L4_IpcSucceeded(tag) ? 0 : L4_ErrorCode();
	if(L4_IpcSucceeded(tag)) L4_StoreMRs(1, tag.X.t, got_si->raw);
	if(!ok(L4_IpcSucceeded(tag), "ipc ok")) diag("ec=%#lx", ec);
	echo_ok_2(recvbufs[0], 64 * 1024, echo_str, got_si);

	/* the second is rather simple. */
	recvbufs[1][64 * 1024 - 1] = '\0';
	if(!ok1(streq(recvbufs[1], echo_str))) {
		diag("strlen(r...)=%d, strlen(echo_str)=%d",
			strlen(recvbufs[1]), strlen(echo_str));
	}

	/* cleanup */
	free(echo_str);
	for(int i=0; i < NUM_ELEMENTS(recvbufs); i++) free(recvbufs[i]);
}
END_TEST


/* use str_receiver_fn to receive 2**i - 1, i <- 1..5 simple string items.
 * another flag determines if there should be too few receive buffers just to
 * cover that error case under the same conditions.
 */
START_LOOP_TEST(receive_many_strings, iter, 0, 9)
{
	plan_tests(6);
	const bool too_few = CHECK_FLAG(iter, 1);
	const int i_val = (iter >> 1) + 1, n_strs = (1 << i_val) - 1;
	diag("too_few=%s, n_strs=%d", btos(too_few), n_strs);

	L4_ThreadId_t receiver = start_str_receiver(
		too_few ? n_strs - 1 : n_strs, 512);
	int cp_len = strlen(copypasta);
	L4_StringItem_t strs[n_strs];
	for(int i=0; i < n_strs; i++) {
		strs[i] = L4_StringItem(cp_len - i + 1, (void *)&copypasta[i]);
	}
	L4_LoadMR(0, (L4_MsgTag_t){ .X.t = n_strs * 2 }.raw);
	L4_LoadMRs(1, n_strs * 2, strs[0].raw);
	L4_MsgTag_t tag = L4_Send_Timeout(receiver, TEST_IPC_DELAY);
	L4_Word_t ec = L4_ErrorCode();
	diag("send tag=%#lx, ec=%#lx", tag.raw, ec);

	L4_MsgTag_t end_tag = L4_Receive_Timeout(receiver, TEST_IPC_DELAY);
	IPC_FAIL(end_tag);
	L4_Word_t other_ec; L4_StoreMR(1, &other_ec);
	if(!iff_ok1(other_ec == 0, L4_IpcSucceeded(tag))) {
		diag("other_ec=%#lx", other_ec);
	}
	L4_Word_t checksums[64];
	int n_csums = MAX(int, 0, L4_UntypedWords(end_tag) - 1);
	L4_StoreMRs(2, n_csums, checksums);
	if(!imply_ok1(!too_few, n_csums == n_strs)) {
		diag("n_csums=%d, n_strs=%d", n_csums, n_strs);
	}
	bool all_match = true;
	for(int i=0; i < n_csums; i++) {
		L4_Word_t expect = crc32c(0, &copypasta[i], strlen(&copypasta[i]));
		if(expect != checksums[i]) {
			diag("i=%d, expect=%#lx, checksums[i]=%#lx",
				i, expect, checksums[i]);
			all_match = false;
		}
	}
	imply_ok1(L4_IpcSucceeded(tag), all_match);

	iff_ok1(L4_IpcFailed(tag), too_few);
	imply_ok1(too_few, (ec & 0xf) == 8);
	imply_ok1(too_few, (other_ec & 0xf) == 9);

	xjoin_thread(receiver);
}
END_TEST


/* causes the receiver to allocate enormous buffers with the idea that faults
 * should occur for each string transfer, despite the small amount being
 * actually copied.
 */
START_TEST(receive_many_to_fault)
{
	plan_tests(4);
	const size_t n_strs = 30, buf_size = 128 * 1024;

	L4_ThreadId_t rec = start_str_receiver(n_strs, buf_size);
	L4_StringItem_t strs[n_strs];
	for(int i=0; i < n_strs; i++) {
		strs[i] = L4_StringItem(strlen(&copypasta[i]) + 1,
			(void *)&copypasta[i]);
	}
	const int flt_before = stats->n_faults, w_before = stats->n_write;
	L4_LoadMR(0, (L4_MsgTag_t){ .X.t = n_strs * 2 }.raw);
	L4_LoadMRs(1, n_strs * 2, strs[0].raw);
	L4_MsgTag_t tag = L4_Send(rec);
	IPC_FAIL(tag);
	const int flt_after = stats->n_faults, w_after = stats->n_write;

	L4_Accept(L4_UntypedWordsAcceptor);
	L4_MsgTag_t end_tag = L4_Receive_Timeout(rec, TEST_IPC_DELAY);
	IPC_FAIL(end_tag);
	L4_Word_t remote_ec = ~0ul; L4_StoreMR(1, &remote_ec);
	L4_Word_t checksums[n_strs]; L4_StoreMRs(2, n_strs, checksums);

	ok1(remote_ec == 0);
	ok1(L4_UntypedWords(end_tag) == n_strs + 1);
	if(!ok1(flt_after - flt_before >= n_strs)) {
		diag("flt_before=%d, flt_after=%d, n_strs=%d",
			flt_before, flt_after, n_strs);
	}
	if(!ok1(w_after - w_before >= n_strs)) {
		diag("w_before=%d, w_after=%d, n_strs=%d", w_before, w_after, n_strs);
	}
	/* TODO: check the checksums, too */

	xjoin_thread(rec);
}
END_TEST


START_TEST(echo_long)
{
	uint32_t seed = seed_bins[1];

	const size_t test_len = 24 * 1024 + 1;
	char *echostr = malloc(test_len);
	fail_if(echostr == NULL);
	random_string(echostr, test_len, &seed);
	fail_unless(strlen(echostr) == test_len - 1);

	plan_tests(2);

	char *replybuf = malloc(test_len * 2);
	fail_if(replybuf == NULL);
	L4_StringItem_t got_si;
	echo(test_tid, replybuf, test_len * 2, &got_si, echostr, 0);

	fail_unless(L4_IsStringItem(&got_si));
	replybuf[MIN(int, test_len * 2 - 1, got_si.X.string_length)] = '\0';
	int rlen = strlen(replybuf);
	ok(rlen >= strlen(echostr), "reply length >= input length");
	ok(streq(&replybuf[rlen - strlen(echostr)], echostr),
		"echo output ends with input");

	free(echostr);
	free(replybuf);
}
END_TEST


/* iter should be 0..15. it'll be used to select a seed value for the string
 * parameter.
 *
 * TODO: use the stats pager, too
 */
START_TEST(echo_long_xferfault)
{
	uint32_t seed = seed_bins[2];

	const size_t test_len = 24 * 1024 + 1;
	char *echostr = aligned_alloc(PAGE_SIZE, test_len);
	fail_if(echostr == NULL);
	random_string(echostr, test_len, &seed);
	fail_unless(strlen(echostr) == test_len - 1);

	plan_tests(2);

	L4_ThreadId_t old_pager = L4_Pager();
	int n = __drop_pager_set_params(drop_tid, 2);
	fail_if(n != 0, "n=%d", n);
	L4_Set_Pager(drop_tid);

	/* provoke send-side faults also */
	L4_Fpage_t echo_page = L4_Fpage((L4_Word_t)echostr, test_len * 2);
	L4_Set_Rights(&echo_page, L4_FullyAccessible);
	L4_FlushFpage(echo_page);

	char *replybuf = aligned_alloc(PAGE_SIZE, test_len * 2);
	fail_if(replybuf == NULL);
	L4_StringItem_t got_si;
	echo(test_tid, replybuf, test_len * 2, &got_si, echostr, test_len - 1);
	L4_Set_Pager(old_pager);

	fail_unless(L4_IsStringItem(&got_si));
	replybuf[MIN(int, test_len * 2 - 1, got_si.X.string_length)] = '\0';
	int rlen = strlen(replybuf);
	ok(rlen >= strlen(echostr), "reply length >= input length");
	ok(streq(&replybuf[rlen - strlen(echostr)], echostr),
		"echo output ends with input");

	free(echostr);
	free(replybuf);
}
END_TEST


/* like echo_simple, but flushes mappings from some of the memory where
 * strings are sent and/or received.
 */
START_TEST(echo_with_hole)
{
	plan_tests(6);

	const size_t buf_size = 16 * 1024;
	const char *echostr = "what did the pope say to the bear?";

	char *replybuf = aligned_alloc(PAGE_SIZE, buf_size),
		*sendbuf = aligned_alloc(PAGE_SIZE, buf_size);
	memset(replybuf, 0, buf_size);
	memset(sendbuf, 0, buf_size);
	char *sendstr = &sendbuf[19];
	memcpy(sendstr, echostr, strlen(echostr) + 1);

	/* unmap the largish-enough page around both, ensuring a pagefault on both
	 * sides
	 */
	L4_Word_t sndpage = (L4_Word_t)sendstr & ~PAGE_MASK,
		rpypage = (L4_Word_t)replybuf & ~PAGE_MASK;
	L4_Fpage_t flush[2] = {
		L4_FpageLog2(sndpage, PAGE_BITS),
		L4_FpageLog2(rpypage, PAGE_BITS),
	};
	for(int i=0; i < NUM_ELEMENTS(flush); i++) {
		L4_Set_Rights(&flush[i], L4_FullyAccessible);
	}
	L4_FlushFpages(2, flush);
	L4_ThreadId_t old_pager = L4_Pager();
	L4_Set_Pager(stats_tid);

	L4_StringItem_t got_si;
	fail_unless(stats->n_faults == 0,
		"saw %d faults before echo", stats->n_faults);
	echo(test_tid, replybuf, buf_size - (sendstr - sendbuf),
		&got_si, sendstr, strlen(echostr));
	L4_Set_Pager(old_pager);

	/* echo result */
	replybuf[MIN(int, buf_size - 1, got_si.X.string_length)] = '\0';
	int rlen = strlen(replybuf);
	ok(rlen >= strlen(sendstr), "reply length >= input length");
	ok(streq(&replybuf[MAX(int, 0, rlen - strlen(echostr))], echostr),
		"echo output ends with input");

	/* fault entrails */
	/* TODO: ignore rx faults, use "==" instead */
	ok1(stats->n_faults >= 2);
	ok1(stats->n_write >= 1);
	ok1(read_fault(sndpage));
	ok1(write_fault(rpypage));

	free(replybuf);
	free(sendbuf);

	if(exit_status() > 0) {
		diag_faults(stats);
		diag("replybuf %p, sendstr %p", replybuf, sendstr);
		diag("rpypage %#lx, sndpage %#lx", rpypage, sndpage);
	}
}
END_TEST


static void *bump_and_align(void *ptr, size_t bump, size_t align)
{
	assert(((align - 1) & align) == 0);		/* has 0 or 1 bits set */
	uintptr_t p = (uintptr_t)ptr + bump;
	return (void *)((p + align - 1) & ~(align - 1)) - bump;
}


/* like echo_with_hole, but makes two pages' worth of holes and sends/receives
 * at the border.
 */
START_TEST(echo_with_long_hole)
{
	plan_tests(9);

	const size_t buf_size = 16 * 1024;
	const char *echostr = "what did the pope say to the bear?";
	const int echo_len = strlen(echostr);

	diag("test tid %lu:%lu, self %lu:%lu",
		L4_ThreadNo(test_tid), L4_Version(test_tid),
		L4_ThreadNo(L4_Myself()), L4_Version(L4_Myself()));

	char *replybuf = calloc(1, buf_size), *sendbuf = calloc(1, buf_size);
	// diag("replybuf %p, sendbuf %p", replybuf, sendbuf);
	char *replyptr = bump_and_align(replybuf + PAGE_SIZE,
			echo_len / 2 - 1, PAGE_SIZE),
		*sendptr = bump_and_align(sendbuf + PAGE_SIZE,
			echo_len / 2, PAGE_SIZE);
	// diag("replyptr %p, sendptr %p", replyptr, sendptr);
	fail_unless(((uintptr_t)replyptr & PAGE_MASK) != 0);
	fail_unless(((uintptr_t)sendptr & PAGE_MASK) != 0);
	memcpy(sendptr, echostr, echo_len + 1);

	/* unmap two primitive pages' worth, starting from the first page. */
	L4_Word_t sndpage = (L4_Word_t)sendptr & ~PAGE_MASK,
		rpypage = (L4_Word_t)replyptr & ~PAGE_MASK;
	L4_Fpage_t flush[] = {
		L4_FpageLog2(sndpage, PAGE_BITS),
		L4_FpageLog2(sndpage + PAGE_SIZE, PAGE_BITS),
		L4_FpageLog2(rpypage, PAGE_BITS),
		L4_FpageLog2(rpypage + PAGE_SIZE, PAGE_BITS),
	};
	L4_ThreadId_t old_pager = L4_Pager();
	L4_Set_Pager(stats_tid);
	for(int i=0; i < NUM_ELEMENTS(flush); i++) {
		L4_Set_Rights(&flush[i], L4_FullyAccessible);
		diag("flushing %#lx:%#lx", L4_Address(flush[i]), L4_Size(flush[i]));
	}
	stats->n_faults = 0;
	stats->n_write = 0;
	L4_FlushFpages(NUM_ELEMENTS(flush), flush);

	L4_StringItem_t got_si;
	/* FIXME: this is dependent on text-segment faults. fault-counting tests
	 * should force the entire testbench program in before executing.
	 */
	if(!ok(stats->n_faults == 0, "stats recorded no faults before test")) {
		diag("stats->n_faults=%d", stats->n_faults);
	}
	echo(test_tid, replyptr, buf_size - (sendptr - sendbuf),
		&got_si, sendptr, strlen(echostr));
	L4_Set_Pager(old_pager);

	/* echo result */
	replyptr[MIN(int, buf_size - 1, got_si.X.string_length)] = '\0';
	int rlen = strlen(replyptr);
	ok(rlen >= strlen(sendptr), "reply length >= input length");
	ok(streq(&replyptr[MAX(int, 0, rlen - strlen(echostr))], echostr),
		"echo output ends with input");

	/* fault entrails */
	diag("%d faults, %d write", stats->n_faults, stats->n_write);
	ok1(stats->n_faults >= 4);	/* only mildly useful */
	ok1(stats->n_write >= 2);
	diag_faults(stats);
	ok1(read_fault(sndpage));
	ok1(read_fault(sndpage + PAGE_SIZE));
	ok1(write_fault(rpypage));
	ok1(write_fault(rpypage + PAGE_SIZE));

	free(replybuf);
	free(sendbuf);
}
END_TEST


/* makes at most three pagefaults on the send side, and at least three on the
 * receive side, when the respective do_unmap_{send,recv} argument is true.
 */
/* TODO: use this from non-xferto tests also */
static L4_Word_t faulting_echo(
	uint64_t *call_time_p,
	L4_ThreadId_t serv_tid,
	int test_iter,
	bool do_unmap_send,
	bool do_unmap_recv)
{
	uint32_t seed = seed_bins[test_iter & 0x3] ^ seed_bins[(test_iter >> 2) & 0x3]
		^ 0xb0a7face;

	const size_t test_len = 11 * 1024 + 1;
	char *echostr = aligned_alloc(PAGE_SIZE, test_len);
	fail_if(echostr == NULL);
	random_string(echostr, test_len, &seed);
	int echo_len = strlen(echostr);
	fail_unless(strlen(echostr) == test_len - 1);

	char *replybuf = aligned_alloc(PAGE_SIZE, test_len * 2);
	fail_if(replybuf == NULL);

	if(do_unmap_send) {
		// diag("send buffer %p:%#x", echostr, (unsigned)test_len);
		flush_byte_range((uintptr_t)echostr, test_len, 0);
	}
	if(do_unmap_recv) {
		// diag("recv buffer %p:%#x", replybuf, (unsigned)test_len * 2);
		flush_byte_range((uintptr_t)replybuf, test_len * 2, 0);
	} else {
		memset(replybuf, '\0', test_len * 2);
	}

	L4_Clock_t before = L4_SystemClock();
	L4_StringItem_t got_si;
	L4_StringItem_t rep_si = L4_StringItem(test_len * 2, replybuf);
	L4_Accept(L4_StringItemsAcceptor);
	L4_LoadBRs(1, 2, rep_si.raw);
	L4_StringItem_t si = L4_StringItem(echo_len + 1, (void *)echostr);
	L4_LoadMR(0, (L4_MsgTag_t){ .X.label = ECHO_LABEL, .X.t = 2 }.raw);
	L4_LoadMRs(1, 2, si.raw);
	L4_MsgTag_t tag = L4_Call_Timeouts(test_tid, TEST_IPC_DELAY,
		TEST_IPC_DELAY);
	L4_Word_t ec = L4_ErrorCode();
	fail_unless(L4_IpcFailed(tag) || tag.X.t == 2,
		"reply tag is weird (%#lx)", tag.raw);
	L4_Clock_t after = L4_SystemClock();
	if(call_time_p != NULL) *call_time_p = after.raw - before.raw;
	if(L4_IpcSucceeded(tag)) {
		L4_StoreMRs(1, 2, got_si.raw);
		fail_unless(L4_IsStringItem(&got_si));

		replybuf[MIN(int, test_len * 2 - 1, got_si.X.string_length)] = '\0';
		int rlen = strlen(replybuf);
		fail_unless(rlen >= strlen(echostr), "reply length >= input length");
		fail_unless(streq(&replybuf[rlen - strlen(echostr)], echostr),
			"echo output ends with input");
	}

	free(echostr);
	free(replybuf);

	return ec;
}


/* xferto testcase: transfer timeouts (and lack thereof). */

START_TEST(no_xfer_timeout)
{
	plan_tests(2);

	L4_Set_XferTimeouts(L4_Timeouts(L4_Never, L4_Never));
	L4_Word_t ec = faulting_echo(NULL, test_tid, 0, false, false);
	ok(ec == 0, "no-fault n/n case");

	L4_Set_XferTimeouts(L4_Timeouts(L4_Never, L4_Never));
	ec = faulting_echo(NULL, test_tid, 1, true, true);
	ok(ec == 0, "faulting n/n case");
}
END_TEST


START_TEST(immediate_xfer_timeout)
{
	plan_tests(1 + 3 * 3);

	L4_Word_t tos = L4_Timeouts(L4_ZeroTime, L4_ZeroTime);

	/* part 1: z/z shouldn't cause IPC failure when no faults occurred */
	L4_Set_XferTimeouts(tos);
	L4_Word_t ec = faulting_echo(NULL, test_tid, 0, false, false);
	ok(ec == 0, "no-fault z/z case");

	/* part 2: z/z should cause IPC failure when faults do occur */
	for(int i = 1; i < 4; i++) {
		const bool u_send = CHECK_FLAG(i, 1), u_recv = CHECK_FLAG(i, 2);
		// diag("i %d, u_send %s, u_recv %s", i, btos(u_send), btos(u_recv));

		L4_Set_XferTimeouts(tos);
		ec = faulting_echo(NULL, test_tid, i + 1, u_send, u_recv);

		/* expecting 5 or 6 in send phase, indicating xfer timeout in invoker or
		 * partner's address space (which are the same thing)
		 *
		 * FIXME: these conditions are copypasta'd in finite_xfer_timeout.
		 * share them somehow.
		 */
		int code = (ec >> 1) & 0x7;
		bool send_phase = (ec & 1) == 0;
		diag("ec %#lx, code %d", ec, code);
		ok(code == 5 || code == 6, "code is xfer timeout");
		ok1(!u_send || send_phase);
		ok1(!u_recv || u_send || !send_phase);
	}
}
END_TEST


static bool call_ping(L4_ThreadId_t dest_tid)
{
	L4_LoadMR(0, (L4_MsgTag_t){ .X.label = PING_LABEL, .X.u = 1 }.raw);
	L4_LoadMR(1, ECHO_LABEL ^ DELAY_LABEL);
	L4_MsgTag_t tag = L4_Call(dest_tid);
	return L4_IpcSucceeded(tag);
}


static L4_Word_t delayed_faulting_echo(
	uint64_t *echo_time_p,
	L4_ThreadId_t thread,
	int iter,
	bool u_send,
	bool u_recv,
	L4_Time_t delay,
	int delay_repeat)
{
	/* set up the xfer fault service delay. */
	bool ipc_ok = send_delay(stats_tid, delay, delay_repeat, false);
	fail_unless(ipc_ok, "ec %#lx", L4_ErrorCode());

	/* sync with echo partner. */
	ipc_ok = call_ping(thread);
	fail_unless(ipc_ok, "ec %#lx", L4_ErrorCode());

	L4_ThreadId_t old_pager = L4_Pager();
	L4_Set_Pager(stats_tid);
	L4_Word_t ec = faulting_echo(echo_time_p, thread, iter, u_send, u_recv);
	L4_Set_Pager(old_pager);

	/* clear the delay mode. */
	ipc_ok = send_delay(stats_tid, L4_ZeroTime, 0, false);
	fail_unless(ipc_ok, "ec %#lx", L4_ErrorCode());

	return ec;
}


START_TEST(finite_xfer_timeout)
{
	plan_tests(1 + 2 * 3 + 3 * 4 + 3 * 4);
	const int timeo_ms = 35;
	L4_Word_t tos = L4_Timeouts(L4_TimePeriod(timeo_ms * 1000),
		L4_TimePeriod(timeo_ms * 1000));

	/* part 1: timeout should not be triggered when no faults occur */
	L4_Set_XferTimeouts(tos);
	L4_Word_t ec = faulting_echo(NULL, test_tid, 0, false, false);
	ok(ec == 0, "no-fault timeout case");

	/* part 2: timeout should not be triggered when faults occur, but are
	 * serviced in time
	 */
	for(int i = 1; i < 4; i++) {
		const bool u_send = CHECK_FLAG(i, 1), u_recv = CHECK_FLAG(i, 2);
		// diag("i %d, u_send %s, u_recv %s", i, btos(u_send), btos(u_recv));

		/* first, immediately */
		L4_Set_XferTimeouts(tos);
		ec = faulting_echo(NULL, test_tid, i + 1, u_send, u_recv);
		ok(ec == 0, "fast faulting [%c%c]",
			u_recv ? 'r' : '-', u_send ? 'w' : '-');

		/* then with a delay that'll never exceed timeo_ms. */
		L4_Set_XferTimeouts(tos);
		ec = delayed_faulting_echo(NULL, test_tid, i + 4, u_send, u_recv,
			L4_TimePeriod(1000), timeo_ms / 2 - 1);
		ok(ec == 0, "slow faulting [%c%c]",
			u_recv ? 'r' : '-', u_send ? 'w' : '-');
	}

	/* part 3: xfer timeout should occur at timeo_ms when the first pagefault
	 * is delayed by more than timeo_ms.
	 */
	for(int i = 1; i < 4; i++) {
		const bool u_send = CHECK_FLAG(i, 1), u_recv = CHECK_FLAG(i, 2);
		// diag("i %d, u_send %s, u_recv %s", i, btos(u_send), btos(u_recv));

		L4_Set_XferTimeouts(tos);
		uint64_t took_us = 0;
		ec = delayed_faulting_echo(&took_us, test_tid, i + 7, u_send, u_recv,
			L4_TimePeriod(timeo_ms * 2 * 1000), 16);
		int code = (ec >> 1) & 0x7;
		bool send_phase = (ec & 1) == 0;
		ok(code == 5 || code == 6, "code is xfer timeout");
		ok1(!u_send || send_phase);
		ok1(!u_recv || u_send || !send_phase);

		/* at timeo_ms, plz. */
		int64_t diff_us = (int64_t)took_us - timeo_ms * 1000;
		diag("took_us=%lu, diff_us=%ld", (unsigned long)took_us,
			(long)diff_us);
		ok(diff_us <= 5000, "timed out at timeo_ms=%d", timeo_ms);
	}

	/* part 4: xfer timeout should also occur after timeo_ms when the
	 * individual fault's delay is smaller than timeo_ms, i.e. when xfer
	 * timeout occurs after the first fault.
	 *
	 * FIXME: body copypasta'd from part 3. should be in a function, or
	 * possibly the time period varied by test iteration.
	 */
	for(int i = 1; i < 4; i++) {
		const bool u_send = CHECK_FLAG(i, 1), u_recv = CHECK_FLAG(i, 2);
		// diag("i %d, u_send %s, u_recv %s", i, btos(u_send), btos(u_recv));

		L4_Set_XferTimeouts(tos);
		uint64_t took_us = 0;
		ec = delayed_faulting_echo(&took_us, test_tid, i + 7, u_send, u_recv,
			L4_TimePeriod(timeo_ms / 2 * 1000), 16);
		int code = (ec >> 1) & 0x7;
		bool send_phase = (ec & 1) == 0;
		ok(code == 5 || code == 6, "code is xfer timeout");
		ok1(!u_send || send_phase);
		ok1(!u_recv || u_send || !send_phase);

		/* at timeo_ms, plz. */
		int64_t diff_us = (int64_t)took_us - timeo_ms * 1000;
		diag("took_us=%lu, diff_us=%ld", (unsigned long)took_us,
			(long)diff_us);
		ok(diff_us <= 5000, "timed out at timeo_ms=%d", timeo_ms);
	}
}
END_TEST


/* meta tests
 *
 * TODO: test of string_test_thread()'s "ping" function
 */

/* (this looks like it duplicates self/stats_delay_test, and there's some
 * overlap for sure, but it actually tests string_test_thread()'s delay
 * function rather than that of stats_pager_fn(). TODO: merge the two tests
 * so that they can be pointed at both tests from one code.)
 */
START_TEST(delay_test)
{
	plan_tests(5);

	/* synchronize. */
	bool ipc_ok = call_ping(test_tid);
	fail_unless(ipc_ok, "sync ping failed, ec %#lx", L4_ErrorCode());

	/* without delay, reply should be immediate. */
	L4_Clock_t before = L4_SystemClock();
	L4_LoadMR(0, (L4_MsgTag_t){ .X.label = PING_LABEL, .X.u = 1 }.raw);
	L4_LoadMR(1, ECHO_LABEL ^ DELAY_LABEL);
	L4_MsgTag_t tag = L4_Call_Timeouts(test_tid, L4_ZeroTime, L4_Never);
	L4_Clock_t after = L4_SystemClock();
	if(!ok(L4_IpcSucceeded(tag), "immediate call succeeded")) {
		diag("ec=%#lx", L4_ErrorCode());
	}
	uint64_t diff_us = after.raw - before.raw;
	diag("ipc took %lu µs", (unsigned long)diff_us);
	ok(diff_us < 1000 * 10, "immediate call was immediate");

	/* with delay, there should be a send-side timeout between 16 and 19 ms,
	 * inclusive, rounding down.
	 */
	ipc_ok = send_delay(test_tid, L4_TimePeriod(20 * 1000), 1, false);
	fail_unless(ipc_ok, "ec %#lx", L4_ErrorCode());
	before = L4_SystemClock();
	L4_LoadMR(0, (L4_MsgTag_t){ .X.label = PING_LABEL, .X.u = 1 }.raw);
	L4_LoadMR(1, ECHO_LABEL ^ DELAY_LABEL);
	tag = L4_Call_Timeouts(test_tid, L4_TimePeriod(1000 * 17), L4_Never);
	after = L4_SystemClock();
	if(L4_IpcSucceeded(tag) || L4_ErrorCode() != 0x2) {
		diag("unexpected ec %#lx", L4_ErrorCode());
	}
	ok(L4_IpcFailed(tag) && L4_ErrorCode() == 0x2,
		"delayed call had send-side timeout");
	diff_us = after.raw - before.raw;
	diag("ipc took %lu µs", (unsigned long)diff_us);
	int diff_ms = diff_us / 1000;
	ok1(diff_ms > 16 && diff_ms < 20);

	/* after delay, ping should complete properly. */
	L4_LoadMR(0, (L4_MsgTag_t){ .X.label = PING_LABEL, .X.u = 1 }.raw);
	L4_LoadMR(1, ECHO_LABEL ^ DELAY_LABEL);
	tag = L4_Call_Timeouts(test_tid, TEST_IPC_DELAY, TEST_IPC_DELAY);
	if(!ok(L4_IpcSucceeded(tag), "after ipc ok")) {
		diag("ec=%#lx", L4_ErrorCode());
	}
}
END_TEST


START_TEST(faulting_echo_test)
{
	plan_tests(7);

	L4_Set_XferTimeouts(L4_Timeouts(L4_Never, L4_Never));

	/* part 1: faulting_echo() should produce no faults on second go without
	 * the unmap option.
	 */
	L4_Word_t ec = faulting_echo(NULL, test_tid, 0, false, false);
	fail_unless(ec == 0, "ec %#lx", ec);

	send_reset(stats_tid);
	L4_ThreadId_t old_pager = L4_Pager();
	L4_Set_Pager(stats_tid);
	ec = faulting_echo(NULL, test_tid, 1, false, false);
	L4_Set_Pager(old_pager);
	fail_unless(ec == 0, "ec %#lx", ec);
	ok1(stats->n_faults == 0);

	/* part 2: subsequently, faulting_echo() with do_unmap should produce at
	 * least one page fault; they should be read/write according to send/recv
	 * unmap flag.
	 */
	for(int i=1; i < 4; i++) {
		bool u_send = CHECK_FLAG(i, 1), u_recv = CHECK_FLAG(i, 2);
#if 0
		diag("loop i=%d (u_send %s, u_recv %s)",
			i, btos(u_send), btos(u_recv));
#endif
		send_reset(stats_tid);
		L4_Set_Pager(stats_tid);
		ec = faulting_echo(NULL, test_tid, 2 + i, u_send, u_recv);
		L4_Set_Pager(old_pager);
		fail_unless(ec == 0, "ec %#lx", ec);

		int n_read = stats->n_faults - stats->n_write;
		ok1(!u_send || n_read > 0);
		ok1(!u_recv || stats->n_write > 0);
	}
}
END_TEST


START_TEST(delayed_faulting_echo_test)
{
	plan_tests(2);
	const int delay_ms = 3;

	/* part 1: when no delay is given, the call should return within a
	 * millisecond.
	 */
	uint64_t took_us = 0;
	L4_Word_t ec = delayed_faulting_echo(&took_us, test_tid, 0, true, true,
		L4_ZeroTime, 0);
	fail_if(ec != 0, "ec %#lx", ec);

	diag("took_us=%lu", (unsigned long)took_us);
	ok(took_us <= 2000, "without delay");

	/* part 2: when given, the delay should be seen at least three times. */
	took_us = 0;
	ec = delayed_faulting_echo(&took_us, test_tid, 0, true, true,
		L4_TimePeriod(delay_ms * 1000), 16);
	fail_if(ec != 0, "ec %#lx", ec);

	diag("took_us=%lu", (unsigned long)took_us);
	ok1(took_us > 9000);		/* it's over nine thousand */
}
END_TEST


/* start string_test_thread in a forked space. */
static void fork_stt_setup(void)
{
	assert(L4_IsNilThread(test_tid));

	L4_ThreadId_t parent_tid = L4_Myself();
	int child = fork();
	if(child == 0) {
		L4_LoadMR(0, (L4_MsgTag_t){ .X.u = 1 }.raw);
		L4_LoadMR(1, L4_Myself().raw);
		L4_MsgTag_t tag = L4_Send_Timeout(parent_tid, TEST_IPC_DELAY);
		if(L4_IpcFailed(tag)) exit(L4_ErrorCode());
		else {
			string_test_thread(NULL);
			exit(0);
		}
	}
	fail_if(child < 0, "fork() failed: child = %d", child);

	L4_Word_t got_ctid;
	do {
		L4_MsgTag_t tag = L4_Wait_Timeout(TEST_IPC_DELAY, &test_tid);
		IPC_FAIL(tag);
		L4_StoreMR(1, &got_ctid);
	} while(test_tid.raw != got_ctid);
	fail_unless(!L4_IsNilThread(test_tid));
}


static void fork_stt_teardown(void)
{
	bool quit_ok = send_quit(test_tid);
	fail_unless(quit_ok, "send_quit() failed, ec %#lx", L4_ErrorCode());

	int status, pid = wait(&status);
	fail_unless(pid > 0);
	fail_unless(status == 0);

	test_tid = L4_nilthread;
}


/* start the stats-collecting pager thread. */
static void stats_setup(void)
{
	stats = malloc(sizeof(*stats));
	fail_unless(stats != NULL);
	stats_tid = start_stats_pager(stats);
}


static void stats_teardown(void)
{
	L4_Word_t ec = stop_stats_pager(stats_tid);
	fail_if(ec != 0, "stop_stats_pager() failed, ec %#lx", ec);
}


struct str_receiver_param {
	L4_ThreadId_t parent;
	int n_bufs;		/* how many receive buffers should be defined */
	size_t buf_size;
};


/* receives p->n_bufs string items of at most 64 bytes each, then returns
 * checksums of the received data, along with ErrorCode from previous, to
 * parent before exit.
 */
static void str_receiver_fn(void *param_ptr)
{
	struct str_receiver_param *p = param_ptr;
	L4_ThreadId_t parent = p->parent;
	const size_t buf_size = MAX(size_t, p->buf_size, 64);

	bool invd = p->n_bufs < 0;
	if(p->n_bufs < 0) p->n_bufs = 0;
	char *bufs[p->n_bufs];
	L4_StringItem_t strs[p->n_bufs];
	L4_Word_t checksums[p->n_bufs];
	L4_ThreadId_t old_pager = L4_Pager();
	if(p->n_bufs == 0) {
		if(!invd) {
			/* don't indicate any receive buffers. */
			L4_Accept(L4_UntypedWordsAcceptor);
		} else {
			/* indicate acceptance, but no receive buffers. */
			L4_Accept(L4_StringItemsAcceptor);
			L4_LoadBR(1, 1 << 3);	/* invalid stringitem header. */
			L4_LoadBR(2, 0);
		}
	} else {
		for(int i=0; i < p->n_bufs; i++) {
			bufs[i] = buf_size >= PAGE_SIZE
				? aligned_alloc(PAGE_SIZE, buf_size)
				: malloc(buf_size);
			fail_if(bufs[i] == NULL, "failed to allocate %u bytes (i=%d)",
				(unsigned)buf_size, i);
			memset(bufs[i], 0, buf_size);
			strscpy(bufs[i], CHECK_FLAG(i, 1) ? "NOTHINGNESS" : "EMPTINESS", buf_size);
			strs[i] = L4_StringItem(buf_size, (void *)bufs[i]);
			strs[i].raw[0] |= 1;		/* continuation bit */
		}
		strs[p->n_bufs - 1].raw[0] &= ~1ul;		/* last string buffer */
		L4_Accept(L4_StringItemsAcceptor);
		L4_LoadBRs(1, p->n_bufs * 2, strs[0].raw);

		if(buf_size >= PAGE_SIZE) {
			for(int i=0; i < p->n_bufs; i++) {
				flush_byte_range((uintptr_t)bufs[i], buf_size, L4_FullyAccessible);
			}

			L4_Set_Pager(stats_tid);
			bool ok = send_reset(stats_tid);
			fail_unless(ok, "send_reset() failed");
		}
	}

	L4_MsgTag_t tag = L4_Receive_Timeout(parent, TEST_IPC_DELAY);
	L4_Word_t ec = L4_IpcFailed(tag) ? L4_ErrorCode() : 0;
	L4_Set_Pager(old_pager);

	int num_cs = 0;
	if(L4_IpcSucceeded(tag) && L4_TypedWords(tag) > 0) {
		for(int i=0; i < L4_TypedWords(tag); i += 2) {
			char *ptr = bufs[i / 2];
			ptr[buf_size - 1] = '\0';
			checksums[num_cs++] = crc32c(0, ptr, strlen(ptr));
			// diag("received `%s' (i=%d)", ptr, i);
		}
	}

	L4_LoadBR(0, 0);
	L4_LoadMR(0, (L4_MsgTag_t){ .X.u = 1 + num_cs }.raw);
	L4_LoadMR(1, ec);
	L4_LoadMRs(2, num_cs, checksums);
	L4_MsgTag_t sync_tag = L4_Send_Timeout(parent, TEST_IPC_DELAY);
	if(L4_IpcFailed(sync_tag)) {
		diag("%s: reply to parent failed, ec=%#lx", __func__, L4_ErrorCode());
	}

	for(int i=0; i < p->n_bufs; i++) free(bufs[i]);
	free(param_ptr);
}


static L4_ThreadId_t start_str_receiver(int n_bufs, size_t buf_size)
{
	struct str_receiver_param *p = malloc(sizeof(*p));
	*p = (struct str_receiver_param){
		.parent = L4_Myself(),
		.n_bufs = n_bufs,
		.buf_size = buf_size,
	};
	return xstart_thread(&str_receiver_fn, p);
}


/* check that str_receiver_fn() delivers timeouts, success, and string
 * checksums correctly.
 */
START_TEST(str_receiver_test)
{
	plan_tests(5);

	/* the timeout case. */
	L4_ThreadId_t rec = start_str_receiver(1, 0);
	L4_MsgTag_t tag = L4_Receive_Timeout(rec,
		L4_TimePeriod(L4_PeriodUs_NP(TEST_IPC_DELAY) * 2));
	L4_Word_t remote_ec = ~0ul; L4_StoreMR(1, &remote_ec);
	L4_Word_t ec = L4_ErrorCode();
	if(!ok(L4_IpcSucceeded(tag) && remote_ec == 3, "timed out")) {
		diag("parent tag=%#lx, ec=%#lx, remote_ec=%#lx", tag.raw,
			ec, remote_ec);
	}
	xjoin_thread(rec);

	/* the OK case, with no string items. */
	rec = start_str_receiver(1, 0);

	L4_LoadMR(0, 0);
	tag = L4_Send_Timeout(rec, TEST_IPC_DELAY);
	if(L4_IpcFailed(tag)) diag("send failed, ec=%#lx", L4_ErrorCode());

	tag = L4_Receive_Timeout(rec,
		L4_TimePeriod(L4_PeriodUs_NP(TEST_IPC_DELAY) * 2));
	remote_ec = ~0ul; L4_StoreMR(1, &remote_ec);
	ec = L4_ErrorCode();
	if(!ok(L4_IpcSucceeded(tag) && remote_ec == 0, "IPC ok")) {
		diag("parent tag=%#lx, ec=%#lx, remote_ec=%#lx", tag.raw,
			ec, remote_ec);
	}
	xjoin_thread(rec);

	/* a three-string case. */
	rec = start_str_receiver(3, 256);
	L4_StringItem_t strs[3];
	for(int i=0; i < 3; i++) {
		strs[i] = L4_StringItem(strlen(copypasta) - i + 1,
			(void *)&copypasta[i]);
	}
	L4_LoadMR(0, (L4_MsgTag_t){ .X.t = 6 }.raw);
	L4_LoadMRs(1, 6, strs[0].raw);
	tag = L4_Send_Timeout(rec, TEST_IPC_DELAY);
	IPC_FAIL(tag);
	tag = L4_Receive_Timeout(rec, TEST_IPC_DELAY);
	IPC_FAIL(tag);
	remote_ec = ~0ul; L4_StoreMR(1, &remote_ec);
	L4_Word_t checksums[3]; L4_StoreMRs(2, 3, checksums);
	if(!ok1(remote_ec == 0)) diag("remote_ec=%#lx", remote_ec);
	if(!ok(L4_UntypedWords(tag) >= 4, "returned 3 checksums")) {
		diag("  tag.u=%#lx, tag.t=%#lx, tag.label=%#lx",
			L4_UntypedWords(tag), L4_TypedWords(tag), L4_Label(tag));
	}
	bool all_ok = true;
	for(int i=0; i < 3; i++) {
		L4_Word_t expect = crc32c(0, &copypasta[i],
			strlen(&copypasta[i]));
		if(expect != checksums[i]) {
			diag("i=%d, expect=%#lx, checksums[i]=%#lx",
				i, expect, checksums[i]);
			all_ok = false;
		}
	}
	ok(all_ok, "checksums match");
	xjoin_thread(rec);
}
END_TEST


/* helper thread & param type for full_string_buffers. */
struct fsb_helper_param {
	size_t buf_size, n_bufs;
	L4_ThreadId_t parent_tid;
	bool sleep;
};


static void fsb_helper_fn(void *param_ptr)
{
	struct fsb_helper_param *p = param_ptr;

	darray(uint8_t *) mems; darray_init(mems);
	for(int i=0; i < p->n_bufs; i++) {
		uint8_t *m = malloc(p->buf_size);
		fail_if(m == NULL);
		memset(m, 0, p->buf_size);
		darray_push(mems, m);
	}

	diag("child mem_0=%p, n_bufs=%u", mems.item[0], (unsigned)p->n_bufs);
	if(p->sleep) L4_Sleep(A_SHORT_NAP);
	L4_LoadBR(0, L4_StringItemsAcceptor.raw);
	for(int i=0; i < mems.size; i++) {
		L4_StringItem_t r_si = L4_StringItem(p->buf_size, mems.item[i]);
		r_si.X.C = i + 1 < mems.size;
		L4_LoadBRs(1 + i * 2, 2, r_si.raw);
	}

	L4_MsgTag_t tag = L4_Receive_Timeout(p->parent_tid, TEST_IPC_DELAY);
	if(L4_IpcFailed(tag)) {
		diag("child: ipc failed, ec=%#lx", L4_ErrorCode());
	} else {
		L4_Word_t sums[mems.size];
		for(int i=0; i < mems.size; i++) {
			sums[i] = crc32c(0, mems.item[i], p->buf_size);
			free(mems.item[i]);
		}
		L4_LoadMR(0, (L4_MsgTag_t){ .X.u = mems.size }.raw);
		L4_LoadMRs(1, mems.size, sums);
		L4_Reply(p->parent_tid);
	}

	darray_free(mems);
}


/* perform a string transfer of exactly N bytes.
 *
 * TODO: scatter/gather operation, more receive buffers than there are send
 * buffers
 *
 * TODO: this test has a bunch of implementation overlap with the various
 * str_receiver_fn() users. this overlap should be removed carefully.
 */
START_LOOP_TEST(full_string_buffers, iter, 0, 7)
{
	const bool use_fork = CHECK_FLAG(iter, 1),
		active_receive = CHECK_FLAG(iter, 2),
		many_bufs = CHECK_FLAG(iter, 4);
	const size_t buf_size = 47;

	plan_tests(2);
	diag("buf_size=%u, use_fork=%s, active_receive=%s, many_bufs=%s",
		(unsigned)buf_size, btos(use_fork), btos(active_receive),
		btos(many_bufs));

	struct fsb_helper_param *p = malloc(sizeof(*p));
	fail_if(p == NULL);
	*p = (struct fsb_helper_param){
		.parent_tid = L4_Myself(),
		.buf_size = buf_size,
		.n_bufs = many_bufs ? 5 : 1,
		.sleep = active_receive,
	};
	L4_ThreadId_t child_tid;
	int child = -1;
	if(use_fork) {
		child = fork_tid(&child_tid);
		if(child == 0) {
			fsb_helper_fn(p);
			exit(0);
		}
	} else {
		child_tid = xstart_thread(&fsb_helper_fn, p);
	}

	uint32_t seed = 0xbabecafe ^ (getpid() << 17) ^ child_tid.raw;
	darray(char *) mems; darray_init(mems);
	for(int i=0; i < p->n_bufs; i++) {
		char *mem = malloc(buf_size);
		fail_if(mem == NULL);
		random_string(mem, buf_size, &seed);
		darray_push(mems, mem);
	}
	if(!active_receive) L4_Sleep(A_SHORT_NAP);
	L4_Word_t msg[63];
	int mpos = 0;
	for(int i=0; i < p->n_bufs; i++) {
		L4_StringItem_t si = L4_StringItem(buf_size, mems.item[i]);
		memcpy(&msg[mpos], si.raw, sizeof(L4_Word_t) * 2);
		mpos += 2;
	}
	L4_LoadMR(0, (L4_MsgTag_t){ .X.t = mpos }.raw);
	L4_LoadMRs(1, mpos, msg);
	L4_MsgTag_t tag = L4_Call_Timeouts(child_tid,
		TEST_IPC_DELAY, TEST_IPC_DELAY);
	L4_Word_t got_msg[63];
	L4_StoreMRs(1, tag.X.u + tag.X.t, got_msg);
	if(!ok1(L4_IpcSucceeded(tag))) diag("ec=%#lx", L4_ErrorCode());
	bool crc_ok = true;
	for(int i=0; i < p->n_bufs; i++) {
		L4_Word_t want = crc32c(0, mems.item[i], buf_size);
		if(got_msg[i] != want) {
			diag("got_msg[%d]=%#lx, want=%#lx", i, got_msg[i], want);
			crc_ok = false;
		}
	}
	ok(crc_ok, "all CRCs match");

	if(use_fork) {
		assert(L4_LocalIdOf(child_tid).raw == L4_nilthread.raw);
		int st, dead = wait(&st);
		fail_unless(dead == child);
	} else {
		assert(child == -1);
		xjoin_thread(child_tid);
	}
	free(p);

	for(int i=0; i < mems.size; i++) free(mems.item[i]);
	darray_free(mems);
}
END_TEST


/* test for message overflow error when there are more string items given by
 * the sender, than string buffer items in the receiver.
 *
 * the sole variable is whether there's a single receive string, or none.
 */
START_LOOP_TEST(too_many_items, iter, 0, 1)
{
	plan_tests(6);
	const bool zero = CHECK_FLAG(iter, 1);
	diag("zero=%s", btos(zero));

	L4_ThreadId_t rec = start_str_receiver(zero ? 0 : 1, 0);

	const char *thing = "of late, i've mostly been eating old doorknobs";
	L4_StringItem_t str = L4_StringItem(strlen(thing) + 1, (void *)thing),
		another = L4_StringItem(8, (void *)thing + 16);
	assert(str.X.string_length <= 64);
	L4_LoadMR(0, (L4_MsgTag_t){ .X.t = 4 }.raw);
	L4_LoadMRs(1, 2, str.raw);
	L4_LoadMRs(3, 2, another.raw);
	L4_MsgTag_t tag = L4_Send_Timeout(rec, TEST_IPC_DELAY);
	L4_Word_t ec = L4_ErrorCode(), err_offset = ec >> 4;
	if(!ok1(L4_IpcFailed(tag) && (ec & 0xf) == 8)) {
		diag("tag=%#lx, ec=%#lx", tag.raw, ec);
	}
	imply_ok1(zero, err_offset == 0);
	imply_ok1(!zero, err_offset == strlen(thing) + 1);
	diag("sender's err_offset=%#lx", err_offset);

	tag = L4_Receive_Timeout(rec,
		L4_TimePeriod(L4_PeriodUs_NP(TEST_IPC_DELAY) * 2));
	fail_if(L4_IpcFailed(tag), "ec=%#lx", L4_ErrorCode());
	L4_Word_t remote_ec = ~0ul; L4_StoreMR(1, &remote_ec);
	ec = L4_ErrorCode();
	if(!ok1((remote_ec & 0xf) == 9)) diag("remote_ec=%#lx", remote_ec);
	err_offset = remote_ec >> 4;
	imply_ok1(zero, err_offset == 0);
	imply_ok1(!zero, err_offset == strlen(thing) + 1);
	diag("receiver's err_offset=%#lx", err_offset);
	xjoin_thread(rec);
}
END_TEST


/* message overflow when a string item in the sender is longer than the
 * corresponding buffer in the receiver.
 */
START_TEST(item_too_long)
{
	plan_tests(2);

	L4_ThreadId_t rec = start_str_receiver(1, 64);

	const char *thing = "though there are times when i ask"
		" whether it is i who eats the doorknob, or do the"
		" doorknobs actually eat me? a most vexing conundrum.";
	L4_StringItem_t str = L4_StringItem(strlen(thing) + 1, (void *)thing);
	assert(str.X.string_length > 64);
	L4_LoadMR(0, (L4_MsgTag_t){ .X.t = 2 }.raw);
	L4_LoadMRs(1, 2, str.raw);
	L4_MsgTag_t tag = L4_Send_Timeout(rec, TEST_IPC_DELAY);
	L4_Word_t ec = L4_ErrorCode();
	if(!ok1(L4_IpcFailed(tag) && (ec & 0xf) == 8)) {
		diag("tag=%#lx, ec=%#lx", tag.raw, ec);
	}

	tag = L4_Receive_Timeout(rec,
		L4_TimePeriod(L4_PeriodUs_NP(TEST_IPC_DELAY) * 2));
	fail_if(L4_IpcFailed(tag), "ec=%#lx", L4_ErrorCode());
	L4_Word_t remote_ec = ~0ul; L4_StoreMR(1, &remote_ec);
	ec = L4_ErrorCode();
	if(!ok1((remote_ec & 0xf) == 9)) diag("remote_ec=%#lx", remote_ec);
	xjoin_thread(rec);
}
END_TEST


START_TEST(invalid_buffer_item)
{
	plan_tests(3);

	L4_ThreadId_t rec = start_str_receiver(-1, 0);
	L4_StringItem_t str = L4_StringItem(32, (void *)copypasta);
	L4_LoadMR(0, (L4_MsgTag_t){ .X.t = 2 }.raw);
	L4_LoadMRs(1, 2, str.raw);
	L4_MsgTag_t tag = L4_Send_Timeout(rec, TEST_IPC_DELAY);
	L4_Word_t ec = L4_ErrorCode();

	L4_MsgTag_t sync_tag = L4_Receive_Timeout(rec, TEST_IPC_DELAY);
	IPC_FAIL(sync_tag);
	L4_Word_t remote_ec; L4_StoreMR(1, &remote_ec);

	ok(L4_IpcFailed(tag), "send failed");
	ok1((ec & 0xf) == 0x8);
	ok1((remote_ec & 0xf) == 0x9);

	xjoin_thread(rec);
}
END_TEST


static void add_echo_tests(TCase *tc)
{
	tcase_add_test(tc, echo_simple);
	tcase_add_test(tc, echo_long);
	tcase_add_test(tc, echo_long_xferfault);
	tcase_add_test(tc, echo_with_hole);
	tcase_add_test(tc, echo_with_long_hole);
	tcase_add_test(tc, echo_compound_send);
	tcase_add_test(tc, echo_compound_recv);
	tcase_add_test(tc, echo_multi_comp);
}


static void add_receiver_tests(TCase *tc)
{
	tcase_add_test(tc, receive_many_strings);
	tcase_add_test(tc, receive_many_to_fault);
}


IDL_FIXTURE(drop, drop_pager, &pg_drop_vtab, FIX_QUIT_COND);

static Suite *string_suite(void)
{
	Suite *s = suite_create("string");

	{
		TCase *tc = tcase_create("meta");
		tcase_add_checked_fixture(tc, &stt_setup, &stt_teardown);
		tcase_add_checked_fixture(tc, &stats_setup, &stats_teardown);
		tcase_add_test(tc, delay_test);
		tcase_add_test(tc, faulting_echo_test);
		tcase_add_test(tc, delayed_faulting_echo_test);
		tcase_add_test(tc, str_receiver_test);
		suite_add_tcase(s, tc);
	}

	{
		TCase *tc = tcase_create("error");
		tcase_add_test(tc, too_many_items);
		tcase_add_test(tc, item_too_long);
		tcase_add_test(tc, invalid_buffer_item);
		suite_add_tcase(s, tc);
	}

	{
		TCase *tc = tcase_create("basic");
		tcase_add_checked_fixture(tc, &stt_setup, &stt_teardown);
		tcase_add_checked_fixture(tc, &stats_setup, &stats_teardown);
		ADD_IDL_FIXTURE(tc, drop);
		tcase_add_test(tc, full_string_buffers);	/* NB: not in `space' */
		add_echo_tests(tc);
		add_receiver_tests(tc);
		suite_add_tcase(s, tc);
	}

	/* inter-space cases, i.e. mapdb interactions and so forth. */
	{
		TCase *tc = tcase_create("space");
		tcase_add_checked_fixture(tc, &fork_stt_setup, &fork_stt_teardown);
		tcase_add_checked_fixture(tc, &stats_setup, &stats_teardown);
		ADD_IDL_FIXTURE(tc, drop);
		add_echo_tests(tc);
		add_receiver_tests(tc);
		suite_add_tcase(s, tc);
	}

	/* transfer timeout tests */
	{
		TCase *tc = tcase_create("xferto");
		tcase_add_checked_fixture(tc, &stt_setup, &stt_teardown);
		tcase_add_checked_fixture(tc, &stats_setup, &stats_teardown);
		tcase_add_test(tc, no_xfer_timeout);
		tcase_add_test(tc, immediate_xfer_timeout);
		tcase_add_test(tc, finite_xfer_timeout);
		suite_add_tcase(s, tc);
	}

	return s;
}


static const struct suite_spec s = { &string_suite, 93 };
AUTODATA(testsuites, &s);
