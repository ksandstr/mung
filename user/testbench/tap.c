
/* TODO: make this interface conform better with the Test Anything
 * Protocol as implemented in e.g. libtap.
 */

#include <stdio.h>
#include <stdarg.h>
#include <ccan/likely/likely.h>

#include "defs.h"
#include "test.h"


void _fail_unless(
	int result,
	const char *file,
	int line,
	const char *expr,
	...)
{
	if(unlikely(!result)) {
		va_list ap;
		char buf[512];
		va_start(ap, expr);
		const char *msg = va_arg(ap, char *);
		if(msg == NULL) msg = expr;
		vsnprintf(buf, sizeof(buf), msg, ap);
		va_end(ap);
		printf("FAIL: %s\n", buf);
		flush_log(true);

		/* meh */
		asm volatile ("int $1");
	}
}
