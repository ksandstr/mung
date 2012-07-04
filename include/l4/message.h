
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

#endif
