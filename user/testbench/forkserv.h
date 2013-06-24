
#ifndef SEEN_TESTBENCH_FORKSERV_H
#define SEEN_TESTBENCH_FORKSERV_H

#include <assert.h>

#include <l4/types.h>
#include <l4/thread.h>
#include <l4/ipc.h>

#include "defs.h"
#include "forkserv-defs.h"


/* IPC label definitions. */

#define FORKSERV_SEND_PAGE_2 0x7351	/* "sQ", not in dispatcher */

#if 0
#define FORKSERV_AS_CFG 0x6143		/* "aC" */
#define FORKSERV_SEND_PAGE 0x7350	/* "sP" */
#define FORKSERV_ADD_TID 0x6154		/* "aT" */
#define FORKSERV_FORK 0x664f		/* "fO" */
#define FORKSERV_SBRK 0x7342		/* "sB" (more of a brk(2), though) */
#define FORKSERV_EXIT QUIT_LABEL	/* ("zZ" was recycled for DELAY_LABEL) */
#define FORKSERV_EXIT_THREAD 0x7a54	/* "zT" */
#define FORKSERV_WAIT 0x7741		/* "wA" */
#define FORKSERV_NEW_THREAD 0x2e21	/* ".!" */
#define FORKSERV_UNMAP 0x754d		/* "uM" */
#define FORKSERV_GETPID 0x6750		/* "gP" */
#endif


/* forkserv-to-pager API for doing privileged system calls.
 *
 * input: untyped in-parameters in spec order, one per word, starting from MR1.
 * replies: MR1 = return value, MR2 = errorcode, MR3 and on = out-parameters
 * in spec order, one per word. no typed words.
 */

#define FPAGER_THREADCTL 0x7443		/* "tC" */
#define FPAGER_SPACECTL 0x7354		/* "sT" */


#endif
