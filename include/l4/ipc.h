
#ifndef __L4__IPC_H__
#define __L4__IPC_H__

#include <stdbool.h>

#include <l4/types.h>
#include <l4/message.h>


static inline L4_Word_t L4_Timeouts(L4_Time_t snd, L4_Time_t recv) {
	return (L4_Word_t)snd.raw << 16 | recv.raw;
}

static inline bool L4_IpcFailed(L4_MsgTag_t tag) {
	return (tag.X.flags & 0x8) != 0;
}

static inline bool L4_IpcSucceeded(L4_MsgTag_t tag) {
	return !L4_IpcFailed(tag);
}


#endif
