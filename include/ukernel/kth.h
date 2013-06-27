
#ifndef SEEN_UKERNEL_KTH_H
#define SEEN_UKERNEL_KTH_H

#include <l4/types.h>
#include <ukernel/thread.h>


struct thread;

extern struct thread *kth_init(L4_ThreadId_t boot_tid);
extern struct thread *kth_start(void (*function)(void *), void *parameter);


#endif
