
#ifndef __L4__MESSAGE_H__
#define __L4__MESSAGE_H__

#include <l4/types.h>
#include <l4/vregs.h>


typedef union L4_MsgTag_u {
	L4_Word_t raw;
	struct {
		L4_Word_t u:6;
		L4_Word_t t:6;
		L4_Word_t flags:4;
		L4_Word_t label:16;
	} __attribute__((packed)) X;
} L4_MsgTag_t;

#define L4_Niltag ((L4_MsgTag_t){ .raw = 0 })


/* accessors. */
static inline L4_Bool_t L4_IsMsgTagEqual(
	const L4_MsgTag_t l, const L4_MsgTag_t r)
{
	return l.raw == r.raw;
}

static inline L4_Bool_t L4_IsMsgTagNotEqual(
	const L4_MsgTag_t l, const L4_MsgTag_t r)
{
	return l.raw != r.raw;
}


static inline L4_Word_t L4_Label(L4_MsgTag_t tag) {
	return tag.X.label;
}

static inline L4_Word_t L4_UntypedWords(L4_MsgTag_t tag) {
	return tag.X.u;
}

static inline L4_Word_t L4_TypedWords(L4_MsgTag_t tag) {
	return tag.X.t;
}


/* mutators. mildly wack, but following the Pistachio convenience programming
 * interface...
 */
static inline L4_MsgTag_t L4_MsgTagAddLabel(L4_MsgTag_t t, int label) {
	t.X.label = label;
	return t;
}


static inline L4_MsgTag_t L4_MsgTagAddLabelTo(L4_MsgTag_t *t, int label) {
	t->X.label = label;
	return *t;
}


static inline void L4_Set_Label(L4_MsgTag_t *t, L4_Word_t label) {
	t->X.label = label;
}


/* tag-casting MR0 accessors. */
static inline L4_MsgTag_t L4_MsgTag(void) {
	L4_MsgTag_t t;
	L4_StoreMR(0, &t.raw);
	return t;
}

static inline void L4_Set_MsgTag(L4_MsgTag_t t) {
	L4_LoadMR(0, t.raw);
}


/* map items. adapted from L4Ka::Pistachio. */

typedef union {
	L4_Word_t raw[2];
	struct {
		L4_Word_t C:1;
		L4_Word_t __type:3;
		L4_Word_t __zeros:6;
		L4_Word_t snd_base:22;
		L4_Fpage_t snd_fpage;
	} X;
} L4_MapItem_t;


static inline L4_MapItem_t L4_MapItem(L4_Fpage_t f, L4_Word_t SndBase)
{
	return (L4_MapItem_t){
		.X.__type = 0x04,
		.X.snd_base = SndBase >> 10,
		.X.snd_fpage = f,
	};
}

static inline L4_Bool_t L4_IsMapItem(L4_MapItem_t m) {
	return m.X.__type == 0x04;
}

static inline L4_Fpage_t L4_MapItemSndFpage(L4_MapItem_t m) {
	return m.X.snd_fpage;
}

static inline L4_Word_t L4_MapItemSndBase(L4_MapItem_t m) {
	return m.X.snd_base << 10;
}


/* L4_GrantItem_t. adapted from L4Ka::Pistachio. */

typedef union {
	L4_Word_t raw[2];
	struct {
		L4_Word_t C:1;
		L4_Word_t __type:3;
		L4_Word_t __zeros:6;
		L4_Word_t snd_base:22;
		L4_Fpage_t snd_fpage;
	} X;
} L4_GrantItem_t;


static inline L4_Bool_t L4_IsGrantItem(L4_GrantItem_t g) {
	return g.X.__type == 0x05;
}

static inline L4_GrantItem_t L4_GrantItem(L4_Fpage_t f, L4_Word_t snd_base)
{
	return (L4_GrantItem_t){
		.X = {
			.__type = 0x05,
			.snd_base = snd_base >> 10,
			.snd_fpage.raw = f.raw,
		},
	};
}

static inline L4_Fpage_t L4_GrantItemSndFpage(L4_GrantItem_t m) {
	return m.X.snd_fpage;
}

static inline L4_Word_t L4_GrantItemSndBase(L4_GrantItem_t m) {
	return m.X.snd_base << 10;
}


/* string item */

typedef union {
	L4_Word_t raw[2];
	struct {
		L4_Word_t C:1;
		L4_Word_t __type:3;
		L4_Word_t j:5;
		L4_Word_t c:1;
		L4_Word_t string_length:22;
		union {
			void *string_ptr;
			void *substring_ptr[0];
		} str;
	} X;
} L4_StringItem_t;


static inline L4_StringItem_t L4_StringItem(int size, void *address) {
	return (L4_StringItem_t){
		.X.string_length = size, .X.str.string_ptr = address,
	};
}


static inline L4_Bool_t L4_IsStringItem(L4_StringItem_t *s) {
	return (s->X.__type & 0x04) == 0;
}


static inline L4_Bool_t L4_CompoundString(L4_StringItem_t *s) {
	return s->X.c != 0;
}


static inline L4_Word_t L4_Substrings(L4_StringItem_t *s) {
	return s->X.j + 1;
}


static inline void *L4_Substring(L4_StringItem_t *s, L4_Word_t n) {
	return s->X.str.substring_ptr[n - 1];
}


/* not to be confused with __L4_EndOfStrings(), which skips over all the
 * strings while this one just skips a single (potentially compound) string
 * item.
 */
static inline L4_StringItem_t *__L4_EndOfString(
	L4_StringItem_t *s, L4_StringItem_t **p)
{
	L4_StringItem_t *prev;
	do {
		prev = s;
		s = (L4_StringItem_t *)&s->X.str.substring_ptr[s->X.j + 1];
	} while(prev->X.c);
	if(p != (L4_StringItem_t **)0) *p = prev;
	return s;
}


/* see comment above. this is very different from __L4_EndOfString(), note the
 * absent plural.
 */
static inline L4_StringItem_t *__L4_EndOfStrings(
	L4_StringItem_t *s, L4_StringItem_t **p)
{
	L4_StringItem_t *prev;
	do {
		prev = s;
		s = __L4_EndOfString(s, (L4_StringItem_t **)0);
	} while(prev->X.C);
	if(p != (L4_StringItem_t **)0) *p = prev;
	return s;
}


static inline void __L4_Copy_String(L4_StringItem_t *d, L4_StringItem_t *s)
{
	L4_Word_t *dest = (L4_Word_t *)d, *from = (L4_Word_t *)s,
		*end = (L4_Word_t *)__L4_EndOfString(s, (L4_StringItem_t **)0);
	while(from < end) *(dest++) = *(from++);
}


static inline L4_StringItem_t *L4_AddSubstringTo(
	L4_StringItem_t *dest,
	L4_StringItem_t *substr)
{
	L4_StringItem_t *prev;
	dest = __L4_EndOfString(dest, &prev);
	prev->X.c = 1;
	__L4_Copy_String(dest, substr);
	return dest;
}


static inline L4_StringItem_t *L4_AddSubstringAddressTo(
	L4_StringItem_t *dest,
	void *substring_addr)
{
	L4_StringItem_t *last;
	__L4_EndOfString(dest, &last);
	last->X.str.substring_ptr[++last->X.j] = substring_addr;
	return dest;
}


/* cache allocation hints */

typedef struct L4_CacheAllocationHint_s {
	L4_Word_t raw;
} L4_CacheAllocationHint_t;

#define L4_UseDefaultCacheAllocation ((L4_CacheAllocationHint_t){ .raw = 0 })

static inline L4_CacheAllocationHint_t L4_CacheAllocationHint(
	const L4_StringItem_t s)
{
	return (L4_CacheAllocationHint_t){ .raw = s.raw[0] & 0x6 };
}

/* (these, too, are mildly fucktarded. i don't makes the rules, i just breaks
 * them.)
 */
static inline L4_Bool_t L4_IsCacheAllocationHintEqual(
	const L4_CacheAllocationHint_t l, const L4_CacheAllocationHint_t r)
{
	return (l.raw & 0x6) == (r.raw & 0x6);
}

static inline L4_Bool_t L4_IsCacheAllocationHintNotEqual(
	const L4_CacheAllocationHint_t l, const L4_CacheAllocationHint_t r)
{
	return (l.raw & 0x6) != (r.raw & 0x6);
}

static inline L4_StringItem_t L4_AddCacheAllocationHint(
	L4_StringItem_t s, L4_CacheAllocationHint_t h)
{
	s.raw[0] |= (h.raw & 0x6);
	return s;
}

static inline L4_StringItem_t L4_AddCacheAllocationHintTo(
	L4_StringItem_t *s, L4_CacheAllocationHint_t h)
{
	s->raw[0] |= (h.raw & 0x6);
	return *s;
}


/* message objects */

typedef union L4_Msg_u {
	L4_Word_t raw[64];
	L4_Word_t msg[64];
	L4_MsgTag_t tag;
} L4_Msg_t;


static inline void L4_MsgPut(
	L4_Msg_t *msg, L4_Word_t label,
	int u, L4_Word_t *us,
	int t, L4_Word_t *ts)
{
	for(int i=0; i < u; i++) msg->msg[i + 1] = us[i];
	for(int i=0; i < t; i++) msg->msg[u + i + 1] = ts[i];
	msg->tag = (L4_MsgTag_t){
		.X.label = label, .X.u = u, .X.t = t, .X.flags = 0
	};
}

static inline void L4_MsgGet(const L4_Msg_t *msg, L4_Word_t *us, void *items)
{
	int u = L4_UntypedWords(msg->tag), t = L4_TypedWords(msg->tag);
	for(int i=0; i < u; i++) us[i] = msg->msg[i + 1];
	for(int i=0; i < t; i++) ((L4_Word_t *)items)[i] = msg->msg[u + i + 1];
}

static inline L4_MsgTag_t L4_MsgMsgTag(const L4_Msg_t *msg) {
	return msg->tag;
}

static inline void L4_Set_MsgMsgTag(L4_Msg_t *msg, L4_MsgTag_t t) {
	msg->tag = t;
}

static inline L4_Word_t L4_MsgLabel(L4_Msg_t *msg) {
	return L4_Label(msg->tag);
}

static inline void L4_Set_MsgLabel(L4_Msg_t *msg, L4_Word_t label) {
	msg->tag.X.label = label;
}

static inline void L4_MsgLoad(L4_Msg_t *msg) {
	L4_LoadMRs(0, msg->tag.X.u + msg->tag.X.t + 1, &msg->msg[0]);
}

static inline void L4_MsgStore(L4_MsgTag_t tag, L4_Msg_t *msg) {
	L4_StoreMRs(1, tag.X.u + tag.X.t, &msg->msg[1]);
	msg->tag = tag;
}

static inline void L4_MsgClear(L4_Msg_t *msg) {
	msg->msg[0] = 0;
}

static inline void L4_MsgAppendWord(L4_Msg_t *msg, L4_Word_t w)
{
	if(msg->tag.X.t != 0) {
		for(int i = 1 + msg->tag.X.u + msg->tag.X.t;
			i > msg->tag.X.u + 1;
			i--)
		{
			msg->msg[i] = msg->msg[i - 1];
		}
	}
	msg->msg[++msg->tag.X.u] = w;
}

static inline void L4_MsgAppendMapItem(L4_Msg_t *msg, L4_MapItem_t mi)
{
	_L4_ASSERT(msg->tag.X.u + msg->tag.X.t < 63);
	msg->msg[msg->tag.X.u + msg->tag.X.t + 1] = mi.raw[0];
	msg->msg[msg->tag.X.u + msg->tag.X.t + 2] = mi.raw[1];
	msg->tag.X.t += 2;
}

static inline void L4_MsgAppendGrantItem(L4_Msg_t *msg, L4_GrantItem_t gi)
{
	_L4_ASSERT(msg->tag.X.u + msg->tag.X.t < 63);
	msg->msg[msg->tag.X.u + msg->tag.X.t + 1] = gi.raw[0];
	msg->msg[msg->tag.X.u + msg->tag.X.t + 2] = gi.raw[1];
	msg->tag.X.t += 2;
}

static inline void L4_MsgAppendSimpleStringItem(
	L4_Msg_t *msg, L4_StringItem_t si)
{
	_L4_ASSERT(msg->tag.X.u + msg->tag.X.t < 63);
	si.X.c = 0; si.X.j = 0;	/* non-compound, one substring */
	int next = msg->tag.X.u + msg->tag.X.t + 1;
	msg->msg[next] = si.raw[0];
	msg->msg[next + 1] = si.raw[1];
	msg->tag.X.t += 2;
}

static inline void L4_MsgAppendStringItem(L4_Msg_t *msg, L4_StringItem_t *s)
{
	L4_StringItem_t *dst = (L4_StringItem_t *)&msg->msg[
			msg->tag.X.u + msg->tag.X.t + 1],
		*end = __L4_EndOfString(s, (L4_StringItem_t **)0);
	int n_words = end->raw - s->raw;
	_L4_ASSERT(n_words >= 0);
	_L4_ASSERT(msg->tag.X.u + msg->tag.X.t < 64 - n_words);
	for(int i=0; i < n_words; i++) dst->raw[i] = s->raw[i];
	msg->tag.X.t += n_words;
}

static inline void L4_MsgPutWord(L4_Msg_t *msg, L4_Word_t u, L4_Word_t w) {
	msg->msg[u + 1] = w;
}

static inline void L4_MsgPutMapItem(
	L4_Msg_t *msg, L4_Word_t t, L4_MapItem_t m)
{
	msg->msg[msg->tag.X.u + t + 1] = m.raw[0];
	msg->msg[msg->tag.X.u + t + 2] = m.raw[1];
}

static inline void L4_MsgPutGrantItem(
	L4_Msg_t *msg, L4_Word_t t, L4_GrantItem_t g)
{
	msg->msg[msg->tag.X.u + t + 1] = g.raw[0];
	msg->msg[msg->tag.X.u + t + 2] = g.raw[1];
}

/* missing: L4_MsgPutSimpleStringItem(), L4_MsgPutStringItem() */

static inline L4_Word_t L4_MsgWord(L4_Msg_t *msg, L4_Word_t u) {
	return msg->msg[u + 1];
}

static inline void L4_MsgGetWord(L4_Msg_t *msg, L4_Word_t u, L4_Word_t *w) {
	*w = msg->msg[u + 1];
}

static inline L4_Word_t L4_MsgGetMapItem(
	L4_Msg_t *msg, L4_Word_t t, L4_MapItem_t *m)
{
	m->raw[0] = msg->msg[msg->tag.X.u + t + 1];
	m->raw[1] = msg->msg[msg->tag.X.u + t + 2];
	return 2;
}

static inline L4_Word_t L4_MsgGetGrantItem(
	L4_Msg_t *msg, L4_Word_t t, L4_GrantItem_t *g)
{
	g->raw[0] = msg->msg[msg->tag.X.u + t + 1];
	g->raw[1] = msg->msg[msg->tag.X.u + t + 2];
	return 2;
}

/* missing: L4_MsgGetStringItem() */


/* acceptors & message buffers */

typedef union {
	L4_Word_t raw;
	struct {
		L4_Word_t s:1;
		L4_Word_t c:1;			/* CtrlXfer bit. not supported in mung. */
		L4_Word_t __zeros:2;
		L4_Word_t RcvWindow:28;
	} X;
} L4_Acceptor_t;

typedef union {
	L4_Word_t raw[63];	/* BR1 to BR64 */
	L4_StringItem_t string[0];
} L4_MsgBuffer_t;

#define L4_UntypedWordsAcceptor ((L4_Acceptor_t){ .raw = 0 })
#define L4_StringItemsAcceptor ((L4_Acceptor_t){ .raw = 1 })


static inline L4_Acceptor_t L4_MapGrantItems(L4_Fpage_t rwnd) {
	return (L4_Acceptor_t){ .X.RcvWindow = rwnd.raw >> 4 };
}

static inline L4_Acceptor_t L4_AddAcceptor(L4_Acceptor_t l, L4_Acceptor_t r) {
	l.X.s |= r.X.s;
	l.X.c |= r.X.c;
	if(r.X.RcvWindow != 0) l.X.RcvWindow = r.X.RcvWindow;
	return l;
}

#define L4_AddAcceptorTo(a, b) L4_AddAcceptor((a), (b))


static inline L4_Acceptor_t L4_RemoveAcceptor(L4_Acceptor_t l, L4_Acceptor_t r)
{
	l.X.s &= ~r.X.s;
	l.X.c &= ~r.X.c;
	if(r.X.RcvWindow != 0) l.X.RcvWindow = 0;
	return l;
}

#define L4_RemoveAcceptorFrom(a, b) L4_RemoveAcceptor((a), (b))


static inline L4_Bool_t L4_HasStringItems(L4_Acceptor_t a) {
	return a.X.s != 0;
}

static inline L4_Bool_t L4_HasMapGrantItems(L4_Acceptor_t a) {
	return a.X.RcvWindow != 0;
}

static inline L4_Fpage_t L4_RcvWindow(L4_Acceptor_t a) {
	L4_Fpage_t fp;
	fp.raw = a.raw;
	fp.X.rwx = 0;
	return fp;
}

static inline void L4_Accept(L4_Acceptor_t a) {
	L4_LoadBR(0, a.raw);
}

static inline L4_Acceptor_t L4_Accepted(void) {
	L4_Acceptor_t a;
	L4_StoreMR(0, &a.raw);
	return a;
}

static inline void L4_AcceptStrings(const L4_Acceptor_t a, L4_MsgBuffer_t *b)
{
	const L4_StringItem_t *prev, *t, *s = &b->string[0];
	int i = 1;
	L4_LoadBR(0, a.raw);
	do {
		prev = s;
		do {
			t = s; 
			int n = s->X.j + 2;
			s = (const L4_StringItem_t *)&s->X.str.substring_ptr[n - 1];
			L4_LoadBRs(i, n, t->raw);
			i += n;
		} while(t->X.c);
	} while(prev->X.C);
}

static inline void L4_MsgBufferClear(L4_MsgBuffer_t *b) {
	b->raw[0] = b->raw[2] = 0;
}

/* all the "simple" string item functions are optimizations, and so is
 * this.
 */
static inline void L4_MsgBufferAppendSimpleRcvString(
	L4_MsgBuffer_t *b, L4_StringItem_t s)
{
	s.X.j = 0; s.X.c = 0;	/* non-compound, one substring */
	if(b->raw[0] != 0 || b->raw[2] != 0) {
		L4_StringItem_t *prev, *tmp = __L4_EndOfStrings(&b->string[0], &prev);
		prev->X.C = 1;
		*tmp = s;
	} else {
		b->raw[2] = ~0ul;
		b->string[0] = s;
		b->string[0].X.C = 0;
	}
}

static inline void L4_MsgBufferAppendRcvString(
	L4_MsgBuffer_t *b, L4_StringItem_t *s)
{
	L4_StringItem_t *tmp;
	if(b->raw[0] != 0 || b->raw[2] != 0) {
		L4_StringItem_t *prev;
		tmp = __L4_EndOfStrings(&b->string[0], &prev);
		prev->X.C = 1;
	} else {
		tmp = &b->string[0];
	}
	__L4_Copy_String(tmp, s);
	tmp->X.C = 0;
}


#endif
