
#ifndef _ASSERT_H
#define _ASSERT_H

#include <stdnoreturn.h>


extern noreturn void __assert_failure(
	const char *condition,
	const char *file, int line, const char *func);


#ifndef NDEBUG
#define assert(condition) do { \
		if(!(condition)) { \
			__assert_failure(#condition, __FILE__, __LINE__, __func__); \
		} \
	} while(0)
#else
/* this shuts the compiler up. */
#define assert(condition) do { (void)sizeof((condition)); } while(0)
#endif


#endif
