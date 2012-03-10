
#ifndef __L4__IPC_H__
#define __L4__IPC_H__

#include <l4/types.h>


static inline L4_Word_t L4_Timeouts(L4_Time_t snd, L4_Time_t recv) {
	return (L4_Word_t)snd.raw << 16 | recv.raw;
}


#endif
