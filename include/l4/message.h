
#ifndef __L4__MESSAGE_H__
#define __L4__MESSAGE_H__

#include <l4/types.h>


typedef union L4_MsgTag_u {
	L4_Word_t raw;
	struct {
		L4_Word_t u:6;
		L4_Word_t t:6;
		L4_Word_t flags:4;
		L4_Word_t label:16;
	} __attribute__((packed)) X;
} L4_MsgTag_t;



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
	return (L4_MapItem_t){ .X.__type = 0x04, .X.snd_base = SndBase >> 10,
		.X.snd_fpage = f };
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
		.raw[0] = 0,
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


static inline bool L4_IsStringItem(L4_StringItem_t *s) {
	return (s->X.__type & 0x04) == 0;
}


static inline bool L4_CompoundString(L4_StringItem_t *s) {
	return s->X.c != 0;
}


static inline L4_Word_t L4_Substrings(L4_StringItem_t *s) {
	return s->X.j + 1;
}


static inline void *L4_Substring(L4_StringItem_t *s, L4_Word_t n) {
	return s->X.str.substring_ptr[n - 1];
}


static inline L4_StringItem_t *__L4_EndOfString(
	L4_StringItem_t *s,
	L4_StringItem_t **p)
{
	L4_StringItem_t *prev;
	do {
		prev = s;
		s = (L4_StringItem_t *)&s->X.str.substring_ptr[s->X.j + 1];
	} while(prev->X.c);
	if(p != NULL) *p = prev;
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


#endif
