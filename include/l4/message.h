
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


#endif
