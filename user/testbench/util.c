
#include <stdbool.h>

#include <l4/types.h>
#include <l4/ipc.h>

#include "defs.h"


bool send_quit(L4_ThreadId_t thread)
{
	L4_LoadMR(0, (L4_MsgTag_t) { .X.label = QUIT_LABEL }.raw);
	return L4_IpcSucceeded(L4_Send_Timeout(thread, TEST_IPC_DELAY));
}
