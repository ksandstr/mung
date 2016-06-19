
/* <l4/vregs.h> is not seen in L4Ka::Pistachio. */

#ifndef __L4__VREGS_H__
#define __L4__VREGS_H__

#include <l4/types.h>


static inline __attribute__((__const__)) void *__L4_Get_UtcbAddress(void) {
	void *ptr;
	__asm__ (
		"\tmovl %%gs:0, %0\n"
		: "=r" (ptr));
	return ptr;
}


/* NOTE: this is UNSAFE from user space as there's no "volatile" constraint on
 * the compiler. it should be moved into <ukernel/thread.h> and replaced with
 * calls to the L4.X2 C API where appropriate.
 */
#define L4_VREG(base, pos) (((L4_Word_t *)(base))[(pos)])

/* userspace needs this to defeat the compiler. */
#define L4_VREG_VOLATILE(base, pos) (((volatile L4_Word_t *)(base))[(pos)])


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


static inline void L4_LoadMR(int i, L4_Word_t w) {
	*(volatile L4_Word_t *)&L4_VREG(__L4_Get_UtcbAddress(), L4_TCR_MR(i)) = w;
}


static inline void L4_LoadMRs(int i, int num, const L4_Word_t *w)
{
	void *utcb = __L4_Get_UtcbAddress();
	for(int c = 0; c < num; c++, i++) {
		*(volatile L4_Word_t *)&L4_VREG(utcb, L4_TCR_MR(i)) = w[c];
	}
}


static inline void L4_StoreMR(int i, L4_Word_t *ptr) {
	void *utcb = __L4_Get_UtcbAddress();
	*ptr = L4_VREG(utcb, L4_TCR_MR(i));
}


static inline void L4_StoreMRs(int i, int num, L4_Word_t *ptr)
{
	void *utcb = __L4_Get_UtcbAddress();
	for(int c = 0; c < num; c++, i++) {
		ptr[c] = L4_VREG(utcb, L4_TCR_MR(i));
	}
}


static inline void L4_LoadBR(int i, L4_Word_t w) {
	*(volatile L4_Word_t *)&L4_VREG(__L4_Get_UtcbAddress(), L4_TCR_BR(i)) = w;
}


static inline void L4_LoadBRs(int i, int num, const L4_Word_t *w)
{
	void *utcb = __L4_Get_UtcbAddress();
	for(int c = 0; c < num; c++, i++) {
		*(volatile L4_Word_t *)&L4_VREG(utcb, L4_TCR_BR(i)) = w[c];
	}
}


static inline void L4_StoreBR(int i, L4_Word_t *ptr) {
	void *utcb = __L4_Get_UtcbAddress();
	*ptr = L4_VREG(utcb, L4_TCR_BR(i));
}


static inline void L4_StoreBRs(int i, int num, L4_Word_t *ptr)
{
	void *utcb = __L4_Get_UtcbAddress();
	for(int c = 0; c < num; c++, i++) {
		ptr[c] = L4_VREG(utcb, L4_TCR_BR(i));
	}
}


/* "bit test and set" */
#define __BTAS(bit, word) ({ \
		L4_Word8_t _v = 0; \
		__asm__ __volatile__ ("bts %1, %2; setc %0" \
			: "=a" (_v) \
			: "i" ((bit)), "m" ((word))); \
		_v; \
	})
/* "bit test and clear" */
#define __BTAC(bit, word) ({ \
		L4_Word8_t _v = 0; \
		__asm__ __volatile__ ("btr %1, %2; setc %0" \
			: "=a" (_v) \
			: "i" ((bit)), "m" ((word))); \
		_v; \
	})


/* CopFlags access. entirely untested. */

static inline void L4_Set_CopFlag(L4_Word_t n)
{
	__asm__ __volatile__ ("orl %0, %1"
		:: "ir" (1 << (8 + n)),
		   "m" (L4_VREG(__L4_Get_UtcbAddress(), L4_TCR_COP_PREEMPT)));
}


static inline void L4_Clr_CopFlag(L4_Word_t n)
{
	__asm__ __volatile__ ("andl %0, %1"
		:: "ir" (~(1 << (8 + n))),
		   "m" (L4_VREG(__L4_Get_UtcbAddress(), L4_TCR_COP_PREEMPT)));
}


static inline L4_Bool_t L4_EnablePreemptionFaultException(void) {
	return (L4_Bool_t)__BTAS(5, L4_VREG(__L4_Get_UtcbAddress(), L4_TCR_COP_PREEMPT));
}


static inline L4_Bool_t L4_DisablePreemptionFaultException(void) {
	return (L4_Bool_t)__BTAC(5, L4_VREG(__L4_Get_UtcbAddress(), L4_TCR_COP_PREEMPT));
}


static inline L4_Bool_t L4_DisablePreemption(void) {
	return (L4_Bool_t)__BTAS(6, L4_VREG(__L4_Get_UtcbAddress(), L4_TCR_COP_PREEMPT));
}


static inline L4_Bool_t L4_EnablePreemption(void) {
	return (L4_Bool_t)__BTAC(6, L4_VREG(__L4_Get_UtcbAddress(), L4_TCR_COP_PREEMPT));
}


static inline L4_Bool_t L4_PreemptionPending(void) {
	return (L4_Bool_t)__BTAC(7, L4_VREG(__L4_Get_UtcbAddress(), L4_TCR_COP_PREEMPT));
}


#endif
