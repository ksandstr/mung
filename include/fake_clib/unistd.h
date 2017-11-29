
#ifndef SEEN_FAKE_CLIB_UNISTD_H
#define SEEN_FAKE_CLIB_UNISTD_H

#include <stdint.h>


extern int getpagesize(void);

/* FIXME: add pid_t somewhere */
extern int getpid(void);

extern long sysconf(int name);

extern int brk(void *addr);
extern void *sbrk(intptr_t increment);


/* sysconf() names. most of the POSIX ones are missing; this is here only to
 * serve CCAN talloc.
 */
enum {
	_SC_PAGESIZE,
#define _SC_PAGESIZE _SC_PAGESIZE
#define _SC_PAGE_SIZE _SC_PAGESIZE
};

#endif
