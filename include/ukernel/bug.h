
/* macros for declaring that a condition indicates an irrecoverable problem,
 * and that the system should be halted immediately
 *
 * rather severe, eh? so these are only appropriate for checking return values
 * in kernel init code.
 */

#ifndef SEEN_BUG_H
#define SEEN_BUG_H

#include <stdlib.h>

#include <ccan/likely/likely.h>
#include <ccan/compiler/compiler.h>


/* note that unlike assert(), this doesn't shove the stringified condition
 * anywhere. (with GCC that doesn't do much good anyhow given how the string
 * can have a pile of expanded macro forms in it.)
 *
 * requires a printf-style message after the condition.
 */
#define BUG_ON(cond, ...) \
	do { \
		int _cond = !!(cond); \
		if(unlikely(!_cond)) { \
			bug(__FILE__, __LINE__, __func__, ##__VA_ARGS__); \
		} \
	} while(0)


extern NORETURN PRINTF_FMT(4, 5) void bug(
	const char *file,
	int line,
	const char *func,
	const char *fmt,
	...);


#endif
