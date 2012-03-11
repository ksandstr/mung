#ifndef SEEN_UKERNEL_IPC_H
#define SEEN_UKERNEL_IPC_H

#include <stdbool.h>

#include <l4/message.h>


struct thread;


extern void init_ipc(void);

/* no timeouts, always call-and-wait. puts current thread in sendwait or
 * recvwait.
 */
extern void ipc_simple(struct thread *dest);

/* receive IPC in kernel thread. switches out until then. */
extern L4_MsgTag_t kipc_recv(struct thread **from_p);

/* returns true when active receive succeeded, false when not. */
extern bool ipc_recv_half(struct thread *receiver);


#endif
