
#ifndef SEEN_SYSCALL_H
#define SEEN_SYSCALL_H

/* system call names. */
#define SC_IPC 1
#define SC_LIPC 2
#define SC_THREADSWITCH 3
#define SC_UNMAP 4
#define SC_SCHEDULE 5
#define SC_SPACECONTROL 6
#define SC_THREADCONTROL 7
#define SC_PROCESSORCONTROL 8

/* not passed through the basic_sc software interrupt bottom half */
#define SC_MEMCTL 9
#define SC_EXREGS 10

#endif
