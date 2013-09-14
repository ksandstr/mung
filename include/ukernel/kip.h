
#ifndef SEEN_KIP_H
#define SEEN_KIP_H

#include <l4/types.h>
#include <l4/kip.h>


extern void *kip_mem;

/* parameter is a pointer to the KCP, which gets reworked to be a proper
 * KIP. the range between kern_start and kern_end will be added to MemoryInfo
 * as a reserved range before the first dedicated range.
 */
extern void make_kip(
	void *mem,
	L4_Word_t kern_start,
	L4_Word_t kern_end,
	int max_irq,
	L4_Time_t sched_prec);


/* friendly neighbourhood accessors */

static inline int first_user_threadno(void) {
	L4_KernelInterfacePage_t *kip = kip_mem;
	return kip->ThreadInfo.X.UserBase;
}


static inline int last_int_threadno(void) {
	L4_KernelInterfacePage_t *kip = kip_mem;
	return kip->ThreadInfo.X.SystemBase - 1;
}

#endif
