#ifndef _FAKE_CLIB_SYS_WAIT_H
#define _FAKE_CLIB_SYS_WAIT_H

#include <sys/types.h>


#ifndef __KERNEL__

extern pid_t wait(int *status);
/* TODO: add waitpid(), perhaps waitid() also */


#define WIFEXITED(st) (((st) & 1) == 0)
#define WEXITSTATUS(st) ((int)(st) >> 1)
#define WIFSIGNALED(st) (((st) & 1) == 1)
#define WTERMSIG(st) (((st) >> 1) & 0x1f)

/* non-portable: segv address w/o low 6 bits */
#define WSEGVADDR(st) ((st) & ~0x3ful)

#endif

#endif
