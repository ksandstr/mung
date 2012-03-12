#ifndef SEEN_UKERNEL_IPC_H
#define SEEN_UKERNEL_IPC_H

#include <stdbool.h>

#include <l4/types.h>
#include <l4/message.h>


struct thread;


extern void init_ipc(void);

/* no timeouts, always call-and-wait. puts current thread in sendwait or
 * recvwait.
 */
extern void ipc_simple(struct thread *dest);

extern L4_MsgTag_t kipc(
	L4_ThreadId_t to,
	L4_ThreadId_t *from_p,
	L4_Word_t timeouts);

/* returns true when active receive or send succeeded, false when not.
 * the value affects scheduling which is determined by the caller.
 */
extern bool ipc_recv_half(struct thread *receiver);
extern bool ipc_send_half(struct thread *sender);


#endif
