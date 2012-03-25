
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
	__asm__ __volatile__ (
		"lock; nop"
		: "=a" (baseaddr), "=c" (*apiver), "=d" (*apiflags), "=S" (*kernelid));
	return (void *)baseaddr;
}


static inline L4_Clock_t L4_SystemClock(void)
{
	extern _C_ void __L4_SystemClock(void);

	L4_Clock_t result;
	__asm__ __volatile__ (
		"\tcall *%%ecx\n"
		: "=A" (result.raw)
		: "c" (__L4_SystemClock)
		: "esi", "edi");
	return result;
}


static inline L4_ThreadId_t L4_ExchangeRegisters(
	L4_ThreadId_t dest,
	L4_Word_t control,
	L4_Word_t sp, L4_Word_t ip,
	L4_Word_t flags, L4_Word_t udh,
	L4_ThreadId_t pager,
	L4_Word_t *control_out,
	L4_Word_t *sp_out, L4_Word_t *ip_out,
	L4_Word_t *flags_out, L4_Word_t *udh_out,
	L4_ThreadId_t *pager_out)
{
	extern _C_ void __L4_ExchangeRegisters(void);

	/* mechanism via L4Ka::Pistachio, rewritten here. */
	L4_ThreadId_t result;
	struct {
		L4_Word_t ebp, edi, ebx, esi;
	} __attribute__((packed)) in = {
		.ebp = pager.raw,
		.ebx = udh,
		.esi = ip,
		.edi = flags,
	};
	__asm__ __volatile__ (
		__L4_SAVE_REGS
		"\tpushl %%esi\n"
		"\tpushl %%edi\n"
		"\tmovl (%%esi), %%ebp\n"
		"\tmovl 4(%%esi), %%edi\n"
		"\tmovl 8(%%esi), %%ebx\n"
		"\tmovl 12(%%esi), %%esi\n"
		"\tcall *(%%esp)\n"
		"\txchgl %%esi, 4(%%esp)\n"
		"\tmovl %%edi, 4(%%esi)\n"
		"\tmovl %%ebx, 8(%%esi)\n"
		"\tmovl %%ebp, (%%esi)\n"
		"\tpopl %%edi\n"
		"\tpopl %%esi\n"
		__L4_RESTORE_REGS
		: "=a" (result), "=c" (*control_out), "=d" (*sp_out), "=S" (*ip_out)
		: "a" (dest.raw), "c" (control), "d" (sp), "S" (&in),
		  "D" (__L4_ExchangeRegisters)
		: "memory", __L4_CLOBBER_REGS);

	pager_out->raw = in.ebp;
	*flags_out = in.edi;
	*udh_out = in.ebx;

	return result;
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
	__asm__ __volatile__ (
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


static inline L4_MsgTag_t L4_Lipc(
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
	__asm__ __volatile__ (
		__L4_SAVE_REGS
		"\tcall __L4_Lipc\n"
		"\tmovl %%ebp, %[mr2_out]\n"
		__L4_RESTORE_REGS
		: "=a" (*from_p), "=S" (tag.raw), "=b" (L4_VREG(utcb, L4_TCR_MR(1))),
		  [mr2_out] "=m" (L4_VREG(utcb, L4_TCR_MR(2)))
		: "a" (to.raw), "c" (timeouts), "d" (fromspec.raw),
		  "S" (L4_VREG(utcb, L4_TCR_MR(0))), "D" (utcb));
#endif
	return tag;
}


static inline void L4_Unmap(L4_Word_t control)
{
	extern _C_ void __L4_Unmap(void);
	void *utcb = __L4_Get_UtcbAddress();
	__asm__ __volatile__ (
		__L4_SAVE_REGS
		"\tcall *%%ecx\n"
		__L4_RESTORE_REGS
		: "=S" (L4_VREG(utcb, L4_TCR_MR(0)))
		: "S" (L4_VREG(utcb, L4_TCR_MR(0))), "D" (utcb),
		  "a" (control), "c" (__L4_Unmap)
		: "edx", __L4_CLOBBER_REGS);
}


static inline L4_Word_t L4_ThreadControl(
	L4_ThreadId_t dest,
	L4_ThreadId_t space,
	L4_ThreadId_t scheduler,
	L4_ThreadId_t pager,
	void *utcb_location)
{
	extern _C_ void __L4_ThreadControl(void);
	L4_Word_t result, dummy;
	__asm__ __volatile__(
		__L4_SAVE_REGS
		"\tmovl %%edi, %%ebx\n"
		"\tmovl %[utcbloc], %%edi\n"
		"\tcall *%%ebx\n"
		__L4_RESTORE_REGS
		: "=a" (result),
		  "=c" (dummy), "=d" (dummy), "=S" (dummy), "=D" (dummy)
		: "a" (dest.raw), "c" (pager.raw), "d" (scheduler.raw),
		  "S" (space.raw), "D" (__L4_ThreadControl),
		  [utcbloc] "m" (utcb_location)
		: __L4_CLOBBER_REGS);
	return result;
}


static inline void L4_ThreadSwitch(L4_ThreadId_t dest)
{
	extern _C_ void __L4_ThreadSwitch(void);
	__asm__ __volatile__(
		"\tcall *%%ecx\n"
		:: "a" (dest.raw), "c" (__L4_ThreadSwitch)
		: "memory", __L4_CLOBBER_REGS);
}


static inline L4_Word_t L4_Schedule(
	L4_ThreadId_t dest,
	L4_Word_t timectl,
	L4_Word_t procctl,
	L4_Word_t prioctl,
	L4_Word_t preemptctl,
	L4_Word_t *timectl_out)
{
	extern _C_ void __L4_Schedule(void);
	L4_Word_t result, dummy;
	__asm__ __volatile__ (
		__L4_SAVE_REGS
		"\tmovl %%edi, %%ebx\n"
		"\tmovl %[preemptctl], %%edi\n"
		"\tcall *%%ebx\n"
		__L4_RESTORE_REGS
		: "=a" (result), "=c" (dummy), "=d" (*timectl_out),
		  "=S" (dummy), "=D" (dummy)
		: "a" (dest.raw), "c" (prioctl), "d" (timectl),
		  "S" (procctl), [preemptctl] "m" (preemptctl),
		  "D" (__L4_Schedule)
		: __L4_CLOBBER_REGS);
	return result;
}


#endif
