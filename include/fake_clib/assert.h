
#ifndef _ASSERT_H
#define _ASSERT_H

#include <ccan/compiler/compiler.h>


extern NORETURN void __assert_failure(
	const char *condition,
	const char *file,
	unsigned int line,
	const char *function);


#ifndef NDEBUG
#define assert(condition) do { \
		if(!(condition)) { \
			__assert_failure(#condition, __FILE__, __LINE__, __func__); \
		} \
	} while(0)
#else
#define assert(condition) do { } while(0)
#endif


#endif
