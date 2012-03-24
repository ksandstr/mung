
#ifndef __L4__SYSCALL_H__
#define __L4__SYSCALL_H__

#include <ccan/compiler/compiler.h>

#include <l4/types.h>
#include <l4/ipc.h>
#include <l4/vregs.h>


/* as in L4Ka::Pistachio: position-independent ia32 support. */
#ifdef __pic__
#define __L4_SAVE_REGS "\tpushl %%ebx; pushl %%ebp\n"
#define __L4_RESTORE_REGS "\tpopl %%ebp; popl %%ebx\n"
#define __L4_CLOBBER_REGS "cc"
#else
#define __L4_SAVE_REGS "\tpushl %%ebp\n"
#define __L4_RESTORE_REGS "\tpopl %%ebp\n"
#define __L4_CLOBBER_REGS "ebx", "cc"
#endif

#ifdef __cplusplus
#define _C_ "C"
#else
#define _C_
#endif


static inline void *L4_KernelInterface(
	L4_Word_t *apiver,
	L4_Word_t *apiflags,
	L4_Word_t *kernelid)
{
	L4_Word_t baseaddr;
	asm volatile (
		"lock; nop"
		: "=a" (baseaddr), "=c" (*apiver), "=d" (*apiflags), "=S" (*kernelid));
	return (void *)baseaddr;
}


static inline L4_Clock_t L4_SystemClock(void)
{
	extern _C_ void __L4_SystemClock(void);

	L4_Clock_t result;
	asm volatile (
		"\tcall *%%ecx\n"
		: "=A" (result.raw)
		: "c" (__L4_SystemClock)
		: "esi", "edi");
	return result;
}


#if 0
L4_ThreadId_t L4_ExchangeRegisters(
	void *kip,
	L4_ThreadId_t dest,
	L4_Word_t *control_p,
	L4_Word_t *sp_p, L4_Word_t *ip_p, L4_Word_t *flags_p,
	L4_Word_t *udh_p,
	L4_ThreadId_t *pager_p)
{
	/* TODO: lift this from Pistachio. shit sucks. */
	L4_ThreadId_t result;
	asm volatile (
		"\tpushl %[scvec]\n"
		"\tmovl %[pager], %%ebp\n"
		"\tcall *(%%esp)\n"
		"\tmovl %%ebp, %[pager]\n"
		: [pager] "=m" (*pager_p),
		  "=a" (result.raw), "=c" (*control_p), "=d" (*sp_p),
		  "=S" (*ip_p), "=D" (*flags_p), "=b" (*udh_p)
		: [scvec] "r" (kip + *(L4_Word_t *)(kip + 0xec))
		: "ebp", "memory");

	return result;
}
#endif


static inline CONST_FUNCTION void *__L4_Get_UtcbAddress(void) {
	void *ptr;
	asm volatile (
		"\tmovl %%gs:0, %0\n"
		: "=r" (ptr));
	return ptr;
}


static inline L4_MsgTag_t L4_Ipc(
	L4_ThreadId_t to,
	L4_ThreadId_t fromspec,
	L4_Word_t timeouts,
	L4_ThreadId_t *from_p)
{
	void *utcb = __L4_Get_UtcbAddress();
	L4_MsgTag_t tag;
#ifdef __pic__
#error "PIC support not quite here yet."
#else
	asm volatile (
		__L4_SAVE_REGS
		"\tcall __L4_Ipc\n"
		"\tmovl %%ebp, %[mr2_out]\n"
		__L4_RESTORE_REGS
		: "=a" (*from_p), "=S" (tag.raw), "=b" (L4_VREG(utcb, L4_TCR_MR(1))),
		  [mr2_out] "=m" (L4_VREG(utcb, L4_TCR_MR(2)))
		: "a" (to.raw), "c" (timeouts), "d" (fromspec.raw),
		  "S" (L4_VREG(utcb, L4_TCR_MR(0))), "D" (utcb));
#endif
	return tag;
}

#endif
