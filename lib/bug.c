
#include <stdio.h>
#include <stdarg.h>

#include <ukernel/misc.h>
#include <ukernel/bug.h>


NORETURN void bug(
	const char *file,
	int line,
	const char *func,
	const char *msgfmt,
	...)
{
	va_list args;
	va_start(args, msgfmt);

	static char msg[256];
	int pos = snprintf(msg, sizeof(msg), "BUG: on %s:%d (in `%s'): ",
		file, line, func);
	vsnprintf(&msg[pos], sizeof(msg) - pos, msgfmt, args);

	panic(msg);

	printf("%s: spinning forever due to returned panic()\n", __func__);
	for(;;) { /* don't return. */ }

	va_end(args);		/* and your horse too! */
}
