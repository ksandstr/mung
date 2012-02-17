/* quasi-runtime functions for the microkernel. */

#ifndef SEEN_UKERNEL_MISC_H
#define SEEN_UKERNEL_MISC_H

#include <ccan/compiler/compiler.h>


#define CHECK_FLAG(mask, bit) (((mask) & (bit)) != 0)


extern void NORETURN panic(const char *message);

#endif
