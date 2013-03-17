/* pager that flushes pages that were mapped a set number of faults ago. used
 * to confirm that string transfers and/or transfer timeouts work properly in
 * the face of in-transfer pagefaults, which this produces when combined with
 * pre-transfer faults.
 */

#include <stdint.h>
#include <stdbool.h>
#include <ukernel/util.h>

#include <l4/types.h>
#include <l4/ipc.h>
#include <l4/space.h>

#include "defs.h"
#include "test.h"


static void drop_pager_fn(void *param_ptr)
{
	struct drop_param *param = param_ptr;
	for(int i=0; i < LOG_SIZE; i++) param->log[i] = L4_Nilpage;
	param->log_top = LOG_SIZE - 1;		/* start at 0 */

	bool run = true;
	while(run) {
		L4_ThreadId_t from;
		L4_MsgTag_t tag = L4_Wait(&from);

		for(;;) {
			if(L4_IpcFailed(tag)) {
				diag("%s: reply/wait failed, ec %#lx", __func__,
					L4_ErrorCode());
				break;
			}

			if(tag.X.label == QUIT_LABEL) {
				run = false;
				break;
			} else if(tag.X.label >> 4 == 0xffe
				&& tag.X.u == 2 && tag.X.t == 0)
			{
				L4_Word_t faddr, fip;
				L4_StoreMR(1, &faddr);
				L4_StoreMR(2, &fip);
				int rwx = tag.X.label & 0x000f;
#if 0
				diag("%s: pf in %lu:%lu at %#lx, ip %#lx", __func__,
					L4_ThreadNo(from), L4_Version(from), faddr, fip);
#endif
				param->log_top = (param->log_top + 1) % LOG_SIZE;
				param->log[param->log_top] = L4_FpageLog2(faddr, 12);
				L4_Set_Rights(&param->log[param->log_top], rwx);

				int dpos = param->log_top - param->keep;
				if(dpos < 0) dpos += LOG_SIZE;
				assert(dpos >= 0 && dpos < LOG_SIZE);
				L4_Fpage_t drop = param->log[dpos];
				if(!L4_IsNilFpage(drop)) {
					diag("flushing %#lx:%#lx (dpos %d)",
						L4_Address(drop), L4_Size(drop), dpos);
					L4_Set_Rights(&drop, L4_FullyAccessible);
					L4_FlushFpage(drop);
				}

				/* pass it on. */
				L4_LoadMR(0, (L4_MsgTag_t){ .X.label = 0xffe0 | rwx,
					.X.u = 2 }.raw);
				L4_LoadMR(1, faddr);
				L4_LoadMR(2, fip);
				L4_LoadBR(0, L4_CompleteAddressSpace.raw);
				tag = L4_Call(L4_Pager());
				if(L4_IpcFailed(tag)) {
					diag("drop-to-pager IPC failed, ec %lu",
						L4_ErrorCode());
					break;
				} else if(tag.X.t != 2 || tag.X.u != 0) {
					diag("drop-to-pager IPC returned weird tag %#lx",
						tag.raw);
					break;
				} else {
					/* reply. */
					L4_LoadMR(0, 0);
					tag = L4_ReplyWait(from, &from);
				}
			} else {
				diag("drop pager got weird IPC from %#lx (label %#lx)",
					from.raw, tag.X.label);
				break;
			}
		}
	}
}


L4_ThreadId_t start_drop_pager(struct drop_param *param)
{
	param->log_top = 0;
	L4_ThreadId_t pg_tid = start_thread(&drop_pager_fn, param);
	fail_if(L4_IsNilThread(pg_tid), "can't start drop pager");
	for(int i=0; i < 10; i++) L4_ThreadSwitch(pg_tid);
	fail_if(param->log_top == 0, "drop pager acting weird");

	return pg_tid;
}


L4_Word_t stop_drop_pager(L4_ThreadId_t tid) {
	/* and roll! */
	return stop_stats_pager(tid);
}
