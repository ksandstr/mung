#ifndef SEEN_UKERNEL_IPC_H
#define SEEN_UKERNEL_IPC_H


struct thread;


extern void init_ipc(void);

/* no timeouts, always call-and-wait. puts current thread in sendwait or
 * recvwait.
 */
extern void ipc_simple(struct thread *dest);

/* receive IPC in kernel thread. switches out until then. */
extern void kipc_recv(struct thread **from_p);


#endif
