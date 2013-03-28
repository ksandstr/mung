
#ifndef __L4__IPC_H__
#define __L4__IPC_H__

#include <stdbool.h>

#include <l4/types.h>
#include <l4/message.h>
#include <l4/thread.h>
#include <l4/syscall.h>


static inline L4_Word_t L4_Timeouts(L4_Time_t snd, L4_Time_t recv) {
	return (L4_Word_t)snd.raw << 16 | recv.raw;
}

static inline bool L4_IpcFailed(L4_MsgTag_t tag) {
	return (tag.X.flags & 0x8) != 0;
}

static inline bool L4_IpcSucceeded(L4_MsgTag_t tag) {
	return !L4_IpcFailed(tag);
}

static inline bool L4_IpcPropagated(L4_MsgTag_t tag) {
	return (tag.X.flags & 0x1) != 0;
}

static inline bool L4_IpcRedirected(L4_MsgTag_t tag) {
	return (tag.X.flags & 0x2) != 0;
}

static inline bool L4_IpcXcpu(L4_MsgTag_t tag) {
	return (tag.X.flags & 0x4) != 0;
}

static inline void L4_Set_Propagation(L4_MsgTag_t *tag) {
	/* the other flags are overwritten since they're ignored by IPC. */
	tag->X.flags = 1;
}


static inline L4_MsgTag_t L4_Call_Timeouts(
	L4_ThreadId_t peer,
	L4_Time_t snd_timeout,
	L4_Time_t rcv_timeout)
{
	L4_ThreadId_t dummy;
	return L4_Ipc(peer, peer, L4_Timeouts(snd_timeout, rcv_timeout), &dummy);
}

static inline L4_MsgTag_t L4_Call(L4_ThreadId_t peer) {
	return L4_Call_Timeouts(peer, L4_Never, L4_Never);
}


static inline L4_MsgTag_t L4_Send_Timeout(
	L4_ThreadId_t peer,
	L4_Time_t timeout)
{
	L4_ThreadId_t dummy;
	return L4_Ipc(peer, L4_nilthread, L4_Timeouts(timeout, L4_Never), &dummy);
}

static inline L4_MsgTag_t L4_Send(L4_ThreadId_t peer) {
	return L4_Send_Timeout(peer, L4_Never);
}

static inline L4_MsgTag_t L4_Reply(L4_ThreadId_t peer) {
	return L4_Send_Timeout(peer, L4_ZeroTime);
}


static inline L4_MsgTag_t L4_Receive_Timeout(
	L4_ThreadId_t from,
	L4_Time_t timeout)
{
	L4_ThreadId_t dummy;
	return L4_Ipc(L4_nilthread, from, L4_Timeouts(L4_Never, timeout), &dummy);
}

static inline L4_MsgTag_t L4_Receive(L4_ThreadId_t from) {
	return L4_Receive_Timeout(from, L4_Never);
}


static inline L4_MsgTag_t L4_Wait_Timeout(
	L4_Time_t rcv_timeout,
	L4_ThreadId_t *from_p)
{
	return L4_Ipc(L4_nilthread, L4_anythread,
		L4_Timeouts(L4_Never, rcv_timeout), from_p);
}

static inline L4_MsgTag_t L4_Wait(L4_ThreadId_t *from_p) {
	return L4_Wait_Timeout(L4_Never, from_p);
}


static inline L4_MsgTag_t L4_ReplyWait_Timeout(
	L4_ThreadId_t to,
	L4_Time_t rcv_timeout,
	L4_ThreadId_t *from_p)
{
	return L4_Ipc(to, L4_anythread, L4_Timeouts(L4_ZeroTime, rcv_timeout),
		from_p);
}

static inline L4_MsgTag_t L4_ReplyWait(
	L4_ThreadId_t to,
	L4_ThreadId_t *from_p)
{
	return L4_ReplyWait_Timeout(to, L4_Never, from_p);
}


static inline L4_MsgTag_t L4_Lcall(L4_ThreadId_t to) {
	L4_ThreadId_t dummy;
	return L4_Ipc(to, to, L4_Timeouts(L4_Never, L4_Never), &dummy);
}

static inline L4_MsgTag_t L4_LreplyWait(
	L4_ThreadId_t to,
	L4_ThreadId_t *from_p)
{
	return L4_Ipc(to, L4_anylocalthread, L4_Timeouts(L4_ZeroTime, L4_Never),
		from_p);
}


static inline void L4_Sleep(L4_Time_t t) {
	/* i'm 12 and what is this */
	L4_LoadMR(0, L4_Receive_Timeout(L4_MyGlobalId(), t).raw);
}


#endif
