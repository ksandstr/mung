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


extern L4_MsgTag_t kipc(
	L4_ThreadId_t to,
	L4_ThreadId_t *from_p,
	L4_Word_t timeouts);

/* the full L4.X2 IPC system call. registers and everything. */
extern SYSCALL L4_Word_t sys_ipc(
	void *utcb,
	L4_ThreadId_t to,
	L4_ThreadId_t from,
	L4_Word_t timeouts);

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


/* in-progress string transfer. at most one per two threads.
 *
 * alloca()ted on the stack when no transfer faults have occurred.
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
	struct thread *source,
	void *s_base,
	struct thread *dest,
	void *d_base,
	L4_MsgTag_t tag);

#endif
