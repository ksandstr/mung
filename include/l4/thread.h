
#ifndef __L4__THREAD_H__
#define __L4__THREAD_H__

#include <stdbool.h>

#include <l4/types.h>
#include <l4/vregs.h>
#include <l4/syscall.h>


/* error returns from ThreadControl and ExchangeRegisters */
#define L4_ERROR_OK 0
#define L4_ERROR_NO_PRIVILEGE 1
#define L4_ERROR_INVALID_THREAD 2
#define L4_ERROR_INVALID_SPACE 3
#define L4_ERROR_INVALID_SCHEDULER 4
#define L4_ERROR_INVALID_PARAM 5
#define L4_ERROR_UTCB_AREA 6
#define L4_ERROR_KIP_AREA 7
#define L4_ERROR_NO_MEM 8


/* thread states */

typedef struct L4_ThreadState {
	L4_Word_t raw;
} L4_ThreadState_t;


static inline bool L4_ThreadWasHalted(L4_ThreadState_t st) {
	return (st.raw & (1 << 0)) != 0;
}

static inline bool L4_ThreadWasReceiving(L4_ThreadState_t st) {
	return (st.raw & (1 << 1)) != 0;
}

static inline bool L4_ThreadWasSending(L4_ThreadState_t st) {
	return (st.raw & (1 << 2)) != 0;
}

static inline bool L4_ThreadWasIpcing(L4_ThreadState_t st) {
	return (st.raw & (1 << 3)) != 0;
}


static inline L4_ThreadId_t L4_GlobalIdOf(L4_ThreadId_t tid)
{
	if(L4_IsGlobalId(tid)) return tid;
	L4_Word_t dummy;
	L4_ThreadId_t t_dummy;
	return L4_ExchangeRegisters(tid, 0, 0, 0, 0, 0, L4_nilthread,
		&dummy, &dummy, &dummy, &dummy, &dummy, &t_dummy);
}


static inline L4_ThreadId_t L4_LocalIdOf(L4_ThreadId_t tid)
{
	if(L4_IsLocalId(tid)) return tid;
	L4_Word_t dummy;
	L4_ThreadId_t t_dummy;
	return L4_ExchangeRegisters(tid, 0, 0, 0, 0, 0, L4_nilthread,
		&dummy, &dummy, &dummy, &dummy, &dummy, &t_dummy);
}


static inline bool L4_SameThreads(L4_ThreadId_t a, L4_ThreadId_t b) {
	return L4_GlobalIdOf(a).raw == L4_GlobalIdOf(b).raw;
}


/* TCR-related functions */

static inline L4_ThreadId_t L4_MyGlobalId(void) {
	void *utcb = __L4_Get_UtcbAddress();
	return (L4_ThreadId_t){ .raw = L4_VREG(utcb, L4_TCR_MYGLOBALID) };
}


static inline L4_ThreadId_t L4_Myself(void) {
	return L4_MyGlobalId();
}


static inline L4_ThreadId_t L4_MyLocalId(void) {
	L4_ThreadId_t tid = {
		.raw = (L4_Word_t)__L4_Get_UtcbAddress(),
	};
	return tid;
}

/* TODO: L4_ProcessorNo(), L4_ErrorCode_String(),
 * L4_XferTimeouts(), L4_Set_XferTimeouts(),
 * (???) L4_WordSizeMask(), L4_Reset_WordSizeMask()
 */


static inline L4_Word_t L4_UserDefinedHandle(void) {
	void *utcb = __L4_Get_UtcbAddress();
	return L4_VREG(utcb, L4_TCR_USERDEFINEDHANDLE);
}


static inline void L4_Set_UserDefinedHandle(L4_Word_t value) {
	void *utcb = __L4_Get_UtcbAddress();
	L4_VREG(utcb, L4_TCR_USERDEFINEDHANDLE) = value;
}


static inline L4_ThreadId_t L4_Pager(void) {
	void *utcb = __L4_Get_UtcbAddress();
	return (L4_ThreadId_t){ .raw = L4_VREG(utcb, L4_TCR_PAGER) };
}

static inline void L4_Set_Pager(L4_ThreadId_t pg) {
	void *utcb = __L4_Get_UtcbAddress();
	L4_VREG(utcb, L4_TCR_PAGER) = pg.raw;
}

static inline L4_Word_t L4_XferTimeouts(void) {
	void *utcb = __L4_Get_UtcbAddress();
	return L4_VREG(utcb, L4_TCR_XFERTIMEOUTS);
}

static inline void L4_Set_XferTimeouts(L4_Word_t newval) {
	void *utcb = __L4_Get_UtcbAddress();
	L4_VREG(utcb, L4_TCR_XFERTIMEOUTS) = newval;
}

static inline void L4_Set_VirtualSender(L4_ThreadId_t id) {
	void *utcb = __L4_Get_UtcbAddress();
	L4_VREG(utcb, L4_TCR_VA_SENDER) = id.raw;
}

static inline L4_ThreadId_t L4_ActualSender(void) {
	void *utcb = __L4_Get_UtcbAddress();
	return (L4_ThreadId_t){ .raw = L4_VREG(utcb, L4_TCR_VA_SENDER) };
}

static inline L4_ThreadId_t L4_IntendedReceiver(void) {
	void *utcb = __L4_Get_UtcbAddress();
	return (L4_ThreadId_t){ .raw = L4_VREG(utcb, L4_TCR_INTENDEDRECEIVER) };
}


static inline L4_ThreadId_t L4_ExceptionHandler(void) {
	void *utcb = __L4_Get_UtcbAddress();
	return (L4_ThreadId_t){ .raw = L4_VREG(utcb, L4_TCR_EXCEPTIONHANDLER) };
}

static inline void L4_Set_ExceptionHandler(L4_ThreadId_t pg) {
	void *utcb = __L4_Get_UtcbAddress();
	L4_VREG(utcb, L4_TCR_EXCEPTIONHANDLER) = pg.raw;
}


static inline L4_Word_t L4_ErrorCode(void) {
	void *utcb = __L4_Get_UtcbAddress();
	return L4_VREG(utcb, L4_TCR_ERRORCODE);
}


static inline void L4_Start(L4_ThreadId_t t)
{
	L4_Word_t dummy;
	L4_ThreadId_t dummy_id;
	L4_ExchangeRegisters (t, (1 << 8) + 6, 0, 0, 0, 0, L4_nilthread,
		&dummy, &dummy, &dummy, &dummy, &dummy, &dummy_id);
}


static inline void L4_Start_SpIp(L4_ThreadId_t t, L4_Word_t sp, L4_Word_t ip)
{
	L4_Word_t dummy;
	L4_ThreadId_t dummy_id;
	L4_ExchangeRegisters (t, (3 << 3) + (1 << 8) + 6, sp, ip, 0, 0,
		L4_nilthread, &dummy, &dummy, &dummy, &dummy, &dummy, &dummy_id);
}


static inline void L4_Start_SpIpFlags(
	L4_ThreadId_t t,
	L4_Word_t sp,
	L4_Word_t ip,
	L4_Word_t flags)
{
	L4_Word_t dummy;
	L4_ThreadId_t dummy_id;

	L4_ExchangeRegisters (t, (7 << 3) + (1 << 8) + 6, sp, ip, flags, 0,
		L4_nilthread, &dummy, &dummy, &dummy, &dummy, &dummy, &dummy_id);
}


static inline L4_ThreadState_t L4_Stop(L4_ThreadId_t tid)
{
	L4_Word_t dummy;
	L4_ThreadId_t dummy_id;
	L4_ThreadState_t state;

	L4_ExchangeRegisters(tid, 1 | 1 << 8, 0, 0, 0, 0,
		L4_nilthread, &state.raw, &dummy, &dummy, &dummy, &dummy,
		&dummy_id);
	return state;
}


static inline L4_ThreadState_t L4_Stop_SpIpFlags(
	L4_ThreadId_t tid,
	L4_Word_t *sp_p,
	L4_Word_t *ip_p,
	L4_Word_t *flags_p)
{
	L4_Word_t dummy;
	L4_ThreadId_t dummy_id;
	L4_ThreadState_t state;

	L4_ExchangeRegisters(tid, 1 | 1 << 8 | 1 << 9, 0, 0, 0, 0,
		L4_nilthread, &state.raw, sp_p, ip_p, flags_p, &dummy,
		&dummy_id);
	return state;
}


static inline L4_Word_t L4_UserDefinedHandleOf(L4_ThreadId_t dest)
{
	L4_Word_t dummy, ret;
	L4_ThreadId_t dummy_id;
	L4_ExchangeRegisters(dest, 0x200, 0, 0, 0, 0, L4_nilthread,
		&dummy, &dummy, &dummy, &dummy, &ret, &dummy_id);
	return ret;
}


static inline void L4_Set_UserDefinedHandleOf(
	L4_ThreadId_t dest,
	L4_Word_t value)
{
	L4_Word_t dummy;
	L4_ThreadId_t dummy_id;
	L4_ExchangeRegisters(dest, (1 << 6), 0, 0, 0, value, L4_nilthread,
		&dummy, &dummy, &dummy, &dummy, &dummy, &dummy_id);
}


static inline L4_ThreadId_t L4_PagerOf(L4_ThreadId_t thread)
{
	L4_Word_t dummy;
	L4_ThreadId_t ret;
	L4_ExchangeRegisters(thread, (1 << 9), 0, 0, 0, 0, L4_nilthread,
		&dummy, &dummy, &dummy, &dummy, &dummy, &ret);
	return ret;
}


static inline void L4_Set_PagerOf(L4_ThreadId_t dest, L4_ThreadId_t pager)
{
	L4_Word_t dummy;
	L4_ThreadId_t dummy_id;
	L4_ExchangeRegisters(dest, (1 << 7), 0, 0, 0, 0, pager,
		&dummy, &dummy, &dummy, &dummy, &dummy, &dummy_id);
}


#endif
