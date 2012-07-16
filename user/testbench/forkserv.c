
/* service that takes over the root task's memory, letting it fork
 * subprocesses (and subprocesses of those). this is useful for test cases
 * involving map operations in IPC, or the Unmap system call.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

#include <l4/types.h>
#include <l4/ipc.h>
#include <l4/syscall.h>


/* FIXME: copypasta'd from user/sigma0.c . merge these. */
void con_putstr(const char *str)
{
	size_t len = strlen(str);
	L4_LoadMR(0, (L4_MsgTag_t){
		.X.label = 0x5370, /* "pS" */
		.X.u = (len + 3) / 4,
	}.raw);
	for(int i=0; i * 4 < len; i++) {
		L4_LoadMR(i + 1, *(L4_Word_t *)&str[i * 4]);
	}
	L4_Call(L4_Pager());
}


void __assert_failure(
	const char *condition,
	const char *file,
	unsigned int line,
	const char *function)
{
	printf("assert(%s) failed in `%s' (%s:%u)\n", condition, function,
		file, line);
	for(;;) { asm volatile ("int $1"); }
}


void abort(void)
{
	printf("abort() called!\n");
	for(;;) { asm volatile ("int $1"); }
}


void malloc_panic(void) {
	printf("%s: called!\n", __func__);
	abort();
}


int sched_yield(void)
{
	L4_ThreadSwitch(L4_nilthread);
	return 0;
}


/* FIXME: this will totally fail as soon as something under main() calls
 * sbrk(). so don't, until this one has been fixed.
 *
 * (this program can provide a noncontiguous heap by asking for chunks of
 * memory from around 0x600000 on up directly from sigma0. this adds inertia
 * to the corresponding setting in dlmalloc.c for a unit test's sake. baaaad.)
 */
void *sbrk(intptr_t increment)
{
	return NULL;
}


int main(void)
{
	printf("hello, world\n");
	return 5;
}
