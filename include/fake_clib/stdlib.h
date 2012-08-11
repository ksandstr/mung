
#ifndef SEEN_FAKE_CLIB_STDLIB_H
#define SEEN_FAKE_CLIB_STDLIB_H

#include <stddef.h>
#include <ccan/compiler/compiler.h>


extern void *malloc(size_t size) __attribute__((malloc));
extern void free(void *ptr);
extern void *calloc(size_t nmemb, size_t size) __attribute__((malloc));
extern void *realloc(void *ptr, size_t size);
extern void *valloc(size_t size);

extern int posix_memalign(void **memptr, size_t alignment, size_t size);

extern NORETURN void abort(void);

extern int abs(int j);
extern long int labs(long int j);
extern long long int llabs(long long int j);


#endif
