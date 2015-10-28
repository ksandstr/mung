
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

static inline void *aligned_alloc(size_t alignment, size_t size) {
	void *ptr;
	int n = posix_memalign(&ptr, alignment, size);
	return n == 0 ? ptr : NULL;
}


extern NORETURN void abort(void);

extern int abs(int j);
extern long int labs(long int j);
extern long long int llabs(long long int j);

#if defined(__GNUC__) && !defined(__IN_ABS_IMPL)
#define abs(x) __builtin_abs((x))
#define labs(x) __builtin_labs((x))
#define llabs(x) __builtin_llabs((x))
#endif

extern int atexit(void (*function)(void));


extern void qsort(
	void *data, size_t count, size_t size,
	int (*compare_fn)(const void *, const void *));


#endif
