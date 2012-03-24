
#include <stdio.h>

#include <ukernel/x86.h>
#include <ukernel/cpu.h>


void sys_processorcontrol(struct x86_exregs *regs)
{
	printf("%s: called\n", __func__);
}
