#ifndef _FAKE_CLIB_SYS_WAIT_H
#define _FAKE_CLIB_SYS_WAIT_H

#include <sys/types.h>


#ifndef __KERNEL__

extern pid_t wait(int *status);
/* TODO: add waitpid(), perhaps waitid() also */


/* TODO: add WIFEXITED(), WEXITSTATUS(), WIFSIGNALED(), WTERMSIG(). the others
 * aren't supported by forkserv.
 */
#endif

#endif
