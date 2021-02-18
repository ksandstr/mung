
/* macros for indicating situations so severe that the system should be halted
 * immediately. microkernel promises have been violated, and so forth.
 *
 * that's rather severe, right? BUG_ON() is like an assert() that doesn't
 * vanish under NDEBUG. it's especially appropriate for checking return values
 * in kernel init code, and other spots where that condition 1) may come up as
 * result of coding error, and 2) is known to cause fuckage that's either
 * irrecoverable or not worth it.
 *
 * fun detail: under NDEBUG, WARN_ON() becomes BUG_ON().
 */

#ifndef SEEN_BUG_H
#define SEEN_BUG_H

#include <stdlib.h>
#include <stdnoreturn.h>
#include <ccan/compiler/compiler.h>
#include <ccan/likely/likely.h>


/* note that unlike assert(), this doesn't shove the stringified condition
 * anywhere. (with GCC that doesn't do much good anyhow given how the string
 * can have a pile of expanded macro forms in it.)
 *
 * requires a printf-style message after the condition.
 */
#define BUG_ON(cond, ...) \
	do { \
		int _cond = !!(cond); \
		if(unlikely(_cond)) { \
			bug(__FILE__, __LINE__, __func__, ##__VA_ARGS__); \
		} \
	} while(0)


extern noreturn PRINTF_FMT(4, 5) void bug(
	const char *file, int line, const char *func,
	const char *fmt, ...);


#ifdef NDEBUG
#define WARN_ON(cond) BUG_ON(cond, "WARN_ON(%s)", #cond)
#else
#define WARN_ON(cond) \
	do { \
		int _cond = !!(cond); \
		if(unlikely(_cond)) { \
			printf("WARNING: %s:%d: %s\n", __FILE__, __LINE__, #cond); \
		} \
	} while(0)
#endif


#endif
