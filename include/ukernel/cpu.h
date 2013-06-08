
#ifndef SEEN_CPU_H
#define SEEN_CPU_H

#include <ccan/compiler/compiler.h>

#include <ukernel/x86.h>


typedef struct x86_features cpu_features;


/* x86: only valid after scan_cpuid(), declared in <ukernel/x86.h> */
extern const cpu_features *get_features(void);

extern void sys_processorcontrol(struct x86_exregs *regs);


#endif
