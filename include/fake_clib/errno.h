
#ifndef _ERRNO_H
#define _ERRNO_H

/* pull error codes from µiX or somewhere as required... */

#define ENOENT	2	/* no such file or directory */
#define EIO		5	/* i/o error */
#define E2BIG	7	/* argument list too long */
#define EBADF	9	/* bad file number */
#define ECHILD	10	/* no child processes */
#define ENOMEM	12	/* out of memory */
#define EFAULT	14	/* bad address */
#define EBUSY	16	/* device or resource busy */
#define EEXIST	17	/* file exists */
#define ENOTDIR	20	/* not a directory */
#define EISDIR	21	/* is a directory */
#define EINVAL	22	/* invalid value */
#define ENOSYS	28	/* function not implemented */


extern int *__errno_location(void);
#define errno (*__errno_location())

#endif
