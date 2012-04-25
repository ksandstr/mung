
#ifndef __L4__THREAD_H__
#define __L4__THREAD_H__

#include <assert.h>

#include <l4/types.h>
#include <l4/vregs.h>
#include <l4/syscall.h>


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
	assert(L4_IsLocalId(tid));
	return tid;
}


static inline L4_ThreadId_t L4_Pager(void) {
	void *utcb = __L4_Get_UtcbAddress();
	return (L4_ThreadId_t){ .raw = L4_VREG(utcb, L4_TCR_PAGER) };
}


static inline L4_Word_t L4_ErrorCode(void) {
	void *utcb = __L4_Get_UtcbAddress();
	return L4_VREG(utcb, L4_TCR_ERRORCODE);
}


#endif
