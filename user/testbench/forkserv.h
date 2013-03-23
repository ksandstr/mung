
#ifndef SEEN_TESTBENCH_FORKSERV_H
#define SEEN_TESTBENCH_FORKSERV_H

#include <assert.h>

#include <l4/types.h>
#include <l4/thread.h>
#include <l4/ipc.h>

#include "defs.h"


/* IPC label definitions. stubs aren't necessary with a single call site. */

#define FORKSERV_AS_CFG 0x6143		/* "aC" */
#define FORKSERV_SEND_PAGE 0x7350	/* "sP" */
#define FORKSERV_SEND_PAGE_2 0x7351	/* "sQ", not in dispatcher */
#define FORKSERV_ADD_TID 0x6154		/* "aT" */
#define FORKSERV_FORK 0x664f		/* "fO" */
#define FORKSERV_SBRK 0x7342		/* "sB" (more of a brk(2), though) */
#define FORKSERV_EXIT QUIT_LABEL	/* ("zZ" was recycled for DELAY_LABEL) */
#define FORKSERV_EXIT_THREAD 0x7a54	/* "zT" */
#define FORKSERV_WAIT 0x7741		/* "wA" */
#define FORKSERV_NEW_THREAD 0x2e21	/* ".!" */
#define FORKSERV_UNMAP 0x754d		/* "uM" */


/* this one is used from unit tests, though. forkserv calls unmap (not flush)
 * on flexpages in the caller's virtual address space. fpages are returned as
 * accessed by forkserv, with 63 returned indicating that there might've been
 * more. calling it with 4k pages ensures 1:1 pass/return; otherwise @fpages
 * should have at least 63 fpages' worth of room.
 */
static inline L4_MsgTag_t forkserv_unmap(
	L4_ThreadId_t forkserv_tid,
	L4_Word_t *n_p,		/* in-out parameter */
	L4_Fpage_t *fpages)
{
	assert(n_p != NULL && *n_p > 0 && *n_p <= 63);
	L4_LoadMR(0, (L4_MsgTag_t){ .X.u = *n_p, .X.label = FORKSERV_UNMAP }.raw);
	L4_LoadMRs(1, *n_p, (L4_Word_t *)fpages);
	L4_MsgTag_t tag = L4_Call(forkserv_tid);
	if(L4_IpcSucceeded(tag)) {
		*n_p = tag.X.u;
		L4_StoreMRs(1, *n_p, (L4_Word_t *)fpages);
	}
	return tag;
}


static inline L4_MsgTag_t forkserv_new_thread(
	L4_ThreadId_t *tid_p,
	L4_Word_t space_id,
	L4_Word_t ip,
	L4_Word_t sp,
	int req_threadno)
{
	L4_LoadMR(0, (L4_MsgTag_t){ .X.label = FORKSERV_NEW_THREAD,
		.X.u = 4 }.raw);
	L4_LoadMR(1, space_id);
	L4_LoadMR(2, ip);
	L4_LoadMR(3, sp);
	L4_LoadMR(4, req_threadno);
	L4_MsgTag_t tag = L4_Call(L4_Pager());
	if(L4_IpcSucceeded(tag)) {
		L4_StoreMR(1, &tid_p->raw);
	}
	return tag;
}


/* forkserv-to-pager API for doing privileged system calls.
 *
 * input: untyped in-parameters in spec order, one per word, starting from MR1.
 * replies: MR1 = return value, MR2 = errorcode, MR3 and on = out-parameters
 * in spec order, one per word. no typed words.
 */

#define FPAGER_THREADCTL 0x7443		/* "tC" */
#define FPAGER_SPACECTL 0x7354		/* "sT" */


#endif
