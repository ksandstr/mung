#ifndef SEEN_UKERNEL_IPC_H
#define SEEN_UKERNEL_IPC_H

#include <stdbool.h>

#include <l4/types.h>
#include <l4/message.h>

#include <ukernel/x86.h>


struct thread;
struct ipc_state;


extern void init_ipc(void);

/* no timeouts, always call-and-wait. puts @from in sendwait, recvwait, or
 * r_ready (when preempted at send phase). uses an absolute xfer timeout when
 * @xferto_abs > 0.
 */
extern void ipc_user(
	struct thread *from,
	struct thread *to,
	uint64_t xferto_abs);

/* effect a string transfer timeout on the ongoing IPC transaction and its
 * peers. drops both out of IPC, sets error code, destroys @st, makes peers
 * READY.
 */
extern void ipc_xfer_timeout(struct ipc_state *st);

/* one thing that thread_ipc_fail() doesn't do. used by the deleting mode of
 * ThreadControl to abort waiting IPCs that depend on a moribund thread's
 * rendezvous.
 *
 * this function also does the other thing, i.e. aborting threads waiting for
 * IPC from @t specifically.
 */
extern void abort_waiting_ipc(struct thread *t, L4_Word_t errorcode);

/* removes the ipc_wait structure associated with the passive send in @t. */
extern void abort_thread_ipc(struct thread *t);


extern L4_MsgTag_t kipc(
	L4_ThreadId_t to,
	L4_ThreadId_t *from_p,
	L4_Word_t timeouts);

/* the full L4.X2 IPC system call. registers and everything. */
extern void sys_ipc(struct x86_exregs *regs);

/* perform one half of the IPC system call. ipc_send_half() is only valid for
 * non-STOPPED, non-halted threads.
 *
 * returns true when active half-ipc succeeded immediately. @t->state will
 * depend on TF_HALT: if it is set, @t->state == STOPPED; if it's clear,
 * @t->state == READY (or R_RECV for the send half iff @t->ipc_from !=
 * L4_nilthread).
 *
 * returns false when either active half-ipc failed (timeouts, nonex't peers,
 * etc), or was changed into the passive form. in the former case, @t->state
 * \in {READY, R_RECV, STOPPED}, same conditions as above; in the latter,
 * IS_IPC_WAIT(@t->status).
 *
 * on error, @t's ErrorCode will be set.
 *
 * when ipc_{recv,send}_half() succeeds and *preempt_p is true, the caller
 * should preempt the current receiver thread if it is currently executing.
 */
extern bool ipc_recv_half(struct thread *t, void *t_utcb, bool *preempt_p);

/* as ipc_recv_half(), but requires @peer->state == TS_XFER */
extern bool ipc_resume(struct thread *peer, bool *preempt_p);

/* partner thread of a thread in TS_XFER. accessor of struct ipc_state. */
extern struct thread *ipc_partner(struct thread *t);

/* tstate as returned by Schedule.
 * precond: @t->ipc != NULL
 */
extern int ipc_tstate(struct thread *t);

/* used by the scheduler */
extern void set_ipc_error_thread(struct thread *t, L4_Word_t ec);

/* used from interrupt delivery */
extern void set_ipc_return_regs(
	struct x86_exregs *regs,
	struct thread *current,
	void *utcb);


/* actually from exception.c */
extern void set_pf_msg(
	struct thread *t,
	void *utcb,
	L4_Word_t fault_addr,
	L4_Word_t ip,
	int fault_access);


#endif
