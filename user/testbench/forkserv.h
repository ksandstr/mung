
#ifndef SEEN_TESTBENCH_FORKSERV_H
#define SEEN_TESTBENCH_FORKSERV_H


/* IPC label definitions. stubs aren't necessary with a single call site. */

#define FORKSERV_SEND_PAGE 0x7350	/* "sP" */
#define FORKSERV_SEND_PAGE_2 0x7351	/* "sQ", not in dispatcher */
#define FORKSERV_ADD_TID 0x6154		/* "aT" */
#define FORKSERV_FORK 0x664f		/* "fO" */
#define FORKSERV_SBRK 0x7342		/* "sB" (more of a brk(2), though) */
#define FORKSERV_EXIT 0x7a5a		/* "zZ" */


#endif
