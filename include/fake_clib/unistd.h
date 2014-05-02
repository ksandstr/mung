
#ifndef SEEN_FAKE_CLIB_UNISTD_H
#define SEEN_FAKE_CLIB_UNISTD_H

#include <stdint.h>
/* breaking the law, breaking the law */
#include <sys/types.h>


extern int getpagesize(void);

extern pid_t getpid(void);

extern long sysconf(int name);

extern int brk(void *addr);
extern void *sbrk(intptr_t increment);


/* sysconf() names. most of the POSIX ones are missing. */
enum {
	_SC_PAGESIZE,
#define _SC_PAGESIZE _SC_PAGESIZE
#define _SC_PAGE_SIZE _SC_PAGESIZE
	_SC_NPROCESSORS_ONLN,
};


#ifndef __KERNEL__

extern pid_t fork(void);

#endif

#endif
