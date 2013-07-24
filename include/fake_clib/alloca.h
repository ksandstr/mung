
#ifndef _ALLOCA_H
#define _ALLOCA_H

#include <stdlib.h>

extern void *alloca(size_t __size);

#ifdef __GNUC__
#define alloca(size) __builtin_alloca((size))
#else
#error "don't know how to do alloca on this compiler!"
#endif

#endif
