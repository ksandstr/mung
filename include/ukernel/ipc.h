#ifndef SEEN_UKERNEL_IPC_H
#define SEEN_UKERNEL_IPC_H

#include <stdbool.h>

#include <l4/types.h>
#include <l4/message.h>

#include <ukernel/x86.h>
#include <ukernel/misc.h>


struct thread;
struct ipc_state;

extern void init_ipc(void);

/* IPC by kernelspace to userspace on @from's behalf. used to send exception &
 * pagefault messages to the exception handler or pager in *@to_p . requires
 * closed IPC with timeouts always ∞.
 *
 * return value is the UTCB segment where the caller should compose its
 * outgoing message. this may be the redirector of @from's address space, the
 * actual recipient's UTCB, or the sender's own UTCB. in the lattermost case,
 * MR0..MR@n_regs are saved to be restored before return to userspace. *@to_p
 * may be altered in the case that redirection applies in @from's address
 * space towards the old value of *@to_p . the returned UTCB's MR0 will be set
 * to @tag .
 *
 * calling sequence:
 *   - when @from == current: ipc_user(), hook_push_front(), return_to_ipc().
 *   - otherwise: ipc_user(), hook_push_front(), ipc_user_complete(), ...
 */
extern void *ipc_user(
	L4_MsgTag_t tag,
	struct thread *from, void *from_utcb,
	struct thread **to_p,
	int n_regs);

/* returns true if sendphase completed immediately. */
extern bool ipc_user_complete(
	struct thread *from,
	void *msg_utcb,			/* return value of ipc_user() */
	struct thread **to_p);

/* effect a string transfer timeout on the ongoing IPC transaction and its
 * peers. drops both out of IPC, sets error code, destroys @st, makes peers
 * READY.
 */
extern void ipc_xfer_timeout(struct ipc_state *st);

/* one thing that thread_ipc_fail() doesn't do. used by the deleting mode of
 * ThreadControl to abort waiting IPCs that depend on a freshly-deleted
 * thread's rendezvous.
 *
 * this function also does the other thing, i.e. aborting threads waiting for
 * IPC from @with_tid specifically.
 *
 * previously known as abort_waiting_ipc().
 */
extern void cancel_ipc_to(L4_ThreadId_t dest_tid, L4_Word_t errorcode);

/* removes the ipc_wait structure associated with the passive send in @t.
 *
 * previously known as abort_thread_ipc().
 */
extern void cancel_ipc_from(struct thread *t);


/* accessor of `redir_wait'. removes @t from it. */
extern void remove_redir_wait(struct thread *t);


/* the full L4.X2 IPC system call. registers and everything. */
extern SYSCALL L4_Word_t sys_ipc(
	L4_ThreadId_t to,
	L4_ThreadId_t fromspec,
	L4_Word_t timeouts,
	void *utcb,		/* caller's UTCB in kernel space */
	L4_Word_t mr0);

/* glueless sys_lipc(). called either from _sysenter_top directly, or from a
 * glue routine. the entry contract is rather complicated; see glue_lipc() for
 * details.
 *
 * returns whatever goes in the platform return value register if Lipc's
 * conditions aren't met and the wrap-around Ipc operation returns to caller
 * immediately; or if Lipc's send-half times out or errors immediately.
 */
extern SYSCALL L4_Word_t sys_lipc(
	L4_ThreadId_t to,
	L4_ThreadId_t fromspec,
	L4_Word_t timeouts,
	void *utcb,		/* caller's UTCB in kernel space (unverified) */
	L4_Word_t mr0, L4_Word_t mr1, L4_Word_t mr2);

/* perform active IPC receive, or put @t into passive receive.
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
 * ipc_recv_half() may cause thread_wake() to be called, which can alter the
 * scheduling queue or cause a preemption.
 */
extern bool ipc_recv_half(struct thread *t, void *t_utcb);

/* as ipc_recv_half(), but requires @peer->state == TS_XFER */
extern bool ipc_resume(struct thread *peer);

/* interface for redirection processing outside ipc.c. returns false if @t is
 * known not to have any chance of pre-empting the current thread.
 *
 * this function will never execute @t's receive phase. it'll put @t into a
 * timeouted R_RECV state instead.
 */
extern bool redo_ipc_send_half(struct thread *t);

/* partner thread of a thread in TS_XFER. accessor of struct ipc_state. */
extern struct thread *ipc_partner(struct thread *t);

/* used by the scheduler */
extern void set_ipc_error_thread(struct thread *t, L4_Word_t ec);

/* used from interrupt delivery */
extern void set_ipc_return_regs(
	struct x86_regs *regs,
	struct thread *current,
	void *utcb);

/* wraps ipc_user() to send a total_quantum exhaustion message to @sched from
 * @t. returns true if the send phase succeeded immediately, false otherwise.
 *
 * NOTE: callers should set an IPC hook to set @t to a descheduled-READY
 * status before calling send_tq_ipc().
 */
extern bool send_tq_ipc(
	struct thread *t, struct thread *sched,
	L4_Clock_t body);

/* actually from exception.c . same interface convention as send_exn_ipc(),
 * but doesn't set a post_exn_call hook since in-transfer faults do it
 * differently. also, the caller must arrange to restore BR0 in @utcb in its
 * post hook.
 */
extern void *send_pf_ipc(
	struct thread *t, void *utcb,
	L4_Word_t fault_addr, L4_Word_t fault_ip, int fault_access,
	struct thread **handler_p);


/* from ipc_typed.c */

struct stritem_iter
{
	/* outputs */
	L4_Word_t ptr, len;

	/* state */
	L4_Word_t *words;
	uint8_t hdr, sub;
	int8_t max;			/* -1 .. 63 */
};


struct fault_peer {
	L4_Fpage_t *faults;
	uint16_t num, pos;	/* at most 4M per strxfer = 1024 pages /peer */
};


struct str_meta {
	uint8_t first_reg;
	uint8_t n_words;
};


/* in-progress string transfer. at most one per two threads. variable length;
 * see `buf' at the end. alloca()ted on the stack when no transfer faults have
 * occurred.
 *
 * this structure encodes a typed transfer that was paused due to
 * string-transfer faults (v1: or mid-transfer pre-emption). it's present in a
 * <struct thread> iff TF_XFER is set, also implying !BLOCKED && IPC for both
 * threads, SENDING for `from', and !SENDING for `to'. these flags will be
 * restored at when XFER is cleared at completion of the paused transfer.
 */
struct ipc_state
{
	uint64_t xferto_at;		/* µs, 0 when not applicable */
	L4_Word_t tot_offset;	/* total # of bytes transferred */
	L4_MsgTag_t tag;		/* saved tag of the entire IPC */

	struct thread *from, *to;

	uint8_t num_strings;	/* # of string transfers */
	uint8_t num_rsis;		/* # of receive buffers */
	uint8_t max_brs;		/* length of longest receiver buffer in words */
	uint8_t str_pos;		/* pos'n of current transfer (<= num_strings) */

	/* first the sender's items, then the receiver's.
	 *
	 * inl[0..1], inl[2..3] when num_strings <= 2; otherwise ptr[0], ptr[1].
	 * (*ptr[] is allocated at end of struct, and has num_strings*2 members.)
	 */
	union {
		struct str_meta inl[4];
		struct str_meta *ptr[2];	/* [0] = from, [1] = to */
	} meta;

	int str_off;			/* byte position in transfer (< 10^22) */
	/* it[] when str_off >= 0, otherwise fault[] */
	union {
		struct stritem_iter it[2];	/* from, to */
		struct fault_peer fault[2];	/* same */
	} xfer;
	int s_off, d_off;		/* per-segment offsets */

	/* implied member. allocated after the structure. accessed with get_rsi()
	 * or get_pre_faults(). length is max_brs words.
	 *
	 * rsi when str_off >= 0, otherwise pre_faults.
	 */
#ifdef NEVER_DEFINED__x
	union {
		L4_StringItem_t rsi;	/* "receiver string item" */
		/* first xfer.fault[0].num are for the source thread, next
		 * xfer.fault[1].num are for dest.
		 *
		 * sizelog2 = PAGE_BITS, access = ro/rw
		 */
		L4_Fpage_t pre_faults[];
	} buf;
#endif
};


static inline L4_StringItem_t *get_rsi(struct ipc_state *st) {
	assert(st->str_off >= 0);
	return (void *)st + sizeof(struct ipc_state);
}


static inline L4_Fpage_t *get_pre_faults(struct ipc_state *st) {
	assert(st->str_off < 0);
	return (void *)st + sizeof(struct ipc_state);
}


extern int do_typed_transfer(
	struct thread *source, void *s_base,
	struct thread *dest, void *d_base,
	L4_MsgTag_t tag);

#endif
