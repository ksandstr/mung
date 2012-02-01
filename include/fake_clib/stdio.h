
/* facsimiles of stdio routines for kernel-space. */

#ifndef SEEN_FAKE_CLIB_STDIO_H
#define SEEN_FAKE_CLIB_STDIO_H

#include <stddef.h>


struct fake_clib_file {
	unsigned char foo;
};

typedef struct fake_clib_file FILE;


extern void printf(const char *fmt, ...)
	__attribute__((format(printf, 1, 2)));

extern void fprintf(FILE *stream, const char *fmt, ...)
	__attribute__((format(printf, 2, 3)));


extern FILE *stdout, *stderr;


#endif
