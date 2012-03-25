
# syscall trampolines that set themselves up automagically.


	.section .data.syscalls, "wax", @progbits

	.global __L4_SystemClock
	.global __L4_MemoryControl
	.global __L4_ExchangeRegisters
	.global __L4_Ipc
	.global __L4_Lipc
	.global __L4_Unmap
	.global __L4_ThreadSwitch
	.global __L4_Schedule
	.global __L4_SpaceControl
	.global __L4_ThreadControl
	.global __L4_ProcessorControl
	.align 16,0
__L4_SystemClock:	call __L4__syscall_setup
__L4_MemoryControl:	call __L4__syscall_setup
__L4_ExchangeRegisters:	call __L4__syscall_setup
__L4_Ipc:		call __L4__syscall_setup
__L4_Lipc:		call __L4__syscall_setup
__L4_Unmap:		call __L4__syscall_setup
__L4_ThreadSwitch:	call __L4__syscall_setup
__L4_Schedule:		call __L4__syscall_setup
__L4_SpaceControl:	call __L4__syscall_setup
__L4_ThreadControl:	call __L4__syscall_setup
__L4_ProcessorControl:	call __L4__syscall_setup
	.previous



	.section .text

	.align 16,0
__L4__syscall_setup:
	subl $5, (%esp)			/* return to the same stub */
	pushal
/* are we here in state 0? if so, go to state 1 and proceed. */
	xorl %eax, %eax
	movl $1, %ecx
	lock cmpxchgl %ecx, __L4__syscall_spinlock
	je 3f
/* if not, wait until state 2 (i.e. other CPU has finished). */
4:	movl __L4__syscall_spinlock, %eax
	cmpl $2, %eax
	je 5f
	pause
	jmp 4b

3:	lock
	nop
	xorl %ecx, %ecx
	jmp 2f
1:	movzwl __L4__syscall_table+4(%ecx), %ebx	# ebx = offset index in kip
	movl %eax, %edx
	subl $5, %edx
	movb $0xe9, (%edi)		# JMP rel32
	addl (%eax, %ebx, 1), %edx	# edx = syscall address - 5
	subl %edi, %edx			# ... i.e. abs - stub - 5
	movl %edx, 1(%edi)
	addl $6, %ecx
2:	movl __L4__syscall_table(%ecx), %edi	# edi = stub address
	testl %edi, %edi
	jne 1b
/* sync memory writes and indicate completion */
	movl $1, %eax
	movl $2, %ecx
	lock cmpxchgl %ecx, __L4__syscall_spinlock
5:	popal
	ret


#define ENTRY(name, index) \
	.long __L4_##name; \
	.word index;

__L4__syscall_table:
	ENTRY(SystemClock, 0xf0)
	ENTRY(MemoryControl, 0xdc)
	ENTRY(ExchangeRegisters, 0xec)
	ENTRY(Ipc, 0xe0)
	ENTRY(Lipc, 0xe4)
	ENTRY(Unmap, 0xe8)
	ENTRY(ThreadSwitch, 0xf4)
	ENTRY(Schedule, 0xf8)
	ENTRY(SpaceControl, 0xd0)
	ENTRY(ThreadControl, 0xd4)
	ENTRY(ProcessorControl, 0xd8)
	.long 0

	.align 64,0
__L4__syscall_spinlock:	.long 0
