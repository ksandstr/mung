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

/* same, but for "from" */
extern void ipc_user(struct thread *from, struct thread *to);


extern L4_MsgTag_t kipc(
	L4_ThreadId_t to,
	L4_ThreadId_t *from_p,
	L4_Word_t timeouts);

/* the full L4.X2 IPC system call. registers and everything. */
extern void sys_ipc(struct x86_exregs *regs);

/* returns true when active receive or send succeeded, false when not.
 * the value affects scheduling which is determined by the caller.
 *
 * when these return false, and thread status is not the corresponding
 * TS_{RECV,SEND}_WAIT, the IPC operation should return. the thread's
 * ErrorCode will have been set.
 *
 * when ipc_recv_half() returns true, and the thread status is TS_READY, and
 * *preempt_p is returned as true, the caller should preempt the current
 * receiver thread if it is currently executing.
 *
 * NOTE: this is flawed, as ipc_send_half() may also cause preemption by
 * activating a higher-priority receiver on the same CPU. see github issue #2.
 */
extern bool ipc_recv_half(struct thread *receiver, bool *preempt_p);
extern bool ipc_send_half(struct thread *sender);

/* used by the scheduler */
extern void set_ipc_error_thread(struct thread *t, L4_Word_t ec);


#endif
