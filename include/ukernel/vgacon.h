
/* x86 PC VGA console access for mbiloader, the microkernel, and testbench. */

#ifndef _UKERNEL_VGACON_H
#define _UKERNEL_VGACON_H

#include <stdlib.h>

#define VGA_BASE 0xa0000
#define VGA_SIZE (256 * 1024)
#define VGA_TEXT_BASE 0xb8000


extern void vgacon_write(void *baseaddr, const char *bytes, size_t count);

#endif
