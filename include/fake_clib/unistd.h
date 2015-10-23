
#ifndef SEEN_FAKE_CLIB_UNISTD_H
#define SEEN_FAKE_CLIB_UNISTD_H

extern int getpagesize(void);

/* FIXME: add pid_t somewhere */
extern int getpid(void);

extern long sysconf(int name);


/* sysconf() names. most of the POSIX ones are missing; this is here only to
 * serve CCAN talloc.
 */
enum {
	_SC_PAGESIZE,
#define _SC_PAGESIZE _SC_PAGESIZE
#define _SC_PAGE_SIZE _SC_PAGESIZE
};

#endif
