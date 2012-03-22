#ifndef SEEN_UKERNEL_IPC_H
#define SEEN_UKERNEL_IPC_H

#include <stdbool.h>

#include <l4/types.h>
#include <l4/message.h>

#include <ukernel/x86.h>


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

/* the full L4.X2 IPC system call. registers and everything. */
extern void ipc_syscall(struct x86_exregs *regs);

/* returns true when active receive or send succeeded, false when not.
 * the value affects scheduling which is determined by the caller.
 *
 * when these return false, and thread status is not the corresponding
 * TS_{RECV,SEND}_WAIT, the IPC operation should return. the thread's
 * ErrorCode will have been set.
 */
extern bool ipc_recv_half(struct thread *receiver);
extern bool ipc_send_half(struct thread *sender);


#endif
