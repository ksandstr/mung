#ifndef _FAKE_CLIB_SYS_WAIT_H
#define _FAKE_CLIB_SYS_WAIT_H

#include <sys/types.h>


#ifndef __KERNEL__

extern pid_t wait(int *wstatus);
extern pid_t waitpid(pid_t pid, int *wstatus, int options);
/* (the testbench/forkserv personality has no concept of process groups or
 * process file descriptors, so there won't be a waitid(2).)
 */


/* waitpid() options */
#define WNOHANG 1


#define WIFEXITED(st) (((st) & 1) == 0)		/* peace, bitch */
#define WEXITSTATUS(st) ((int)(st) >> 1)
#define WIFSIGNALED(st) (((st) & 1) == 1)
#define WTERMSIG(st) (((st) >> 1) & 0x1f)

/* non-portable: segv address w/o low 6 bits */
#define WSEGVADDR(st) ((st) & ~0x3ful)

#endif

#endif
