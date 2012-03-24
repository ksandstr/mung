
/* <l4/vregs.h> is not seen in L4Ka::Pistachio. */

#ifndef __L4__VREGS_H__
#define __L4__VREGS_H__

#include <ccan/compiler/compiler.h>
#include <l4/types.h>


static inline CONST_FUNCTION void *__L4_Get_UtcbAddress(void) {
	void *ptr;
	__asm__ (
		"\tmovl %%gs:0, %0\n"
		: "=r" (ptr));
	return ptr;
}


#define L4_VREG(base, pos) (((L4_Word_t *)(base))[(pos)])


/* names of TCRs and VRs in an UTCB segment, tied to word offsets. */

#define L4_TCR_THREADWORD0 (-4)
#define L4_TCR_THREADWORD1 (-5)
#define L4_TCR_VA_SENDER (-6)		/* VirtualSender/ActualSender */
#define L4_TCR_INTENDEDRECEIVER (-7)
#define L4_TCR_XFERTIMEOUTS (-8)
#define L4_TCR_ERRORCODE (-9)
#define L4_TCR_COP_PREEMPT (-10)	/* COPflags, Preemptflags */
#define L4_TCR_EXCEPTIONHANDLER (-11)
#define L4_TCR_PAGER (-12)
#define L4_TCR_USERDEFINEDHANDLE (-13)
#define L4_TCR_PROCESSORNO (-14)
#define L4_TCR_MYGLOBALID (-15)
#define L4_TCR_MR(n) (n)
#define L4_TCR_BR(n) (-(n) - 16)

#endif
