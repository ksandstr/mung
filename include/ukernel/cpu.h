
#ifndef SEEN_CPU_H
#define SEEN_CPU_H

#include <ccan/compiler/compiler.h>

#include <ukernel/x86.h>
#include <ukernel/misc.h>


typedef struct x86_features cpu_features;

/* x86: only valid after scan_cpuid(), declared in <ukernel/x86.h> */
extern const cpu_features *get_features(void);
extern bool cpu_has_sysenter(void);
extern bool use_sysenter;


extern SYSCALL L4_Word_t sys_processorcontrol(
	L4_Word_t proc_no,
	L4_Word_t internal_freq,
	L4_Word_t external_freq,
	L4_Word_t voltage);


#endif
