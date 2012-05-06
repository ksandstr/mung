
#ifndef __L4__SCHEDULE_H__
#define __L4__SCHEDULE_H__

#include <l4/types.h>
#include <l4/syscall.h>


static inline void L4_Yield(void) {
	L4_ThreadSwitch(L4_nilthread);
}


static inline L4_Word_t L4_Set_Priority(L4_ThreadId_t tid, L4_Word_t prio)
{
    L4_Word_t dummy;
    prio &= 0xff;
    return L4_Schedule(tid, ~0UL, ~0UL, prio, ~0UL, &dummy);
}


static inline L4_Word_t L4_Set_ProcessorNo(L4_ThreadId_t tid, L4_Word_t cpu_no)
{
    L4_Word_t dummy;
    cpu_no &= 0xffff;
    return L4_Schedule(tid, ~0UL, cpu_no, ~0UL, ~0UL, &dummy);
}


static inline L4_Word_t L4_Set_Timeslice(
	L4_ThreadId_t tid,
	L4_Time_t timeslice,
	L4_Time_t totalquantum)
{
    L4_Word_t timectrl = (timeslice.raw << 16) | totalquantum.raw;
    return L4_Schedule(tid, timectrl, ~0UL, ~0UL, ~0UL, &timectrl);
}


static inline L4_Word_t L4_Timeslice(
	L4_ThreadId_t tid,
	L4_Time_t *timeslice,
	L4_Time_t *totalquantum)
{
    L4_Word_t res, timectrl;
    res = L4_Schedule(tid, ~0UL, ~0UL, ~0UL, ~0UL, &timectrl);
    timeslice->raw = timectrl >> 16;
    totalquantum->raw = timectrl;
    return res;
}


static inline L4_Word_t L4_Set_PreemptionDelay(
	L4_ThreadId_t tid,
	L4_Word_t sensitivePrio,
	L4_Word_t maxDelay)
{
    L4_Word_t dummy;
    L4_Word_t pctrl = ((sensitivePrio & 0xff) << 16) | (maxDelay & 0xffff);
    return L4_Schedule(tid, ~0UL, ~0UL, ~0UL, pctrl, &dummy);
}


/* L4_Schedule() reults (low 8 bits only) */

#define L4_SCHEDRESULT_ERROR		(0)
#define L4_SCHEDRESULT_DEAD		(1)
#define L4_SCHEDRESULT_INACTIVE		(2)
#define L4_SCHEDRESULT_RUNNING		(3)
#define L4_SCHEDRESULT_PENDING_SEND	(4)
#define L4_SCHEDRESULT_SENDING		(5)
#define L4_SCHEDRESULT_WAITING		(6)
#define L4_SCHEDRESULT_RECEIVING	(7)


#endif /* !__L4__SCHEDULE_H__ */
