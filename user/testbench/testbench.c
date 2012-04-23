
/* TODO: extend this roottask to support enough of the Check unit testing
 * framework to be useful in running proper unit tests. output via serial port
 * and so forth.
 */

#include <l4/types.h>
#include <l4/ipc.h>
#include <l4/syscall.h>


void abort(void)
{
	L4_ThreadId_t dummy;
	for(;;) {
		L4_Ipc(L4_nilthread, L4_nilthread, L4_Timeouts(L4_Never, L4_Never),
			&dummy);
	}
}


void con_putstr(const char *str)
{
	/* TODO: add serial I/O */
}


void *malloc(size_t size)
{
	return NULL;
}


void __assert_failure(
	const char *condition,
	const char *file,
	unsigned int line,
	const char *function)
{
}


int main(void)
{
	return 0;
}
