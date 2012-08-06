
#include "defs.h"


int fork(void)
{
	return -1;
}


int wait(int *status)
{
	return -1;
}


void exit(int status)
{
	for(;;) {
		asm volatile ("int $1");
	}
}
