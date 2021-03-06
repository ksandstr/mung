
/* pager that collects statistics. used to confirm that pagefaults occurred as
 * specified.
 */

#include <stdbool.h>
#include <ukernel/util.h>

#include <l4/types.h>
#include <l4/ipc.h>

#include "defs.h"
#include "test.h"


static void reset_stats(struct pager_stats *stats) {
	*stats = (struct pager_stats){
		.log_top = LOG_SIZE - 1,	/* start at 0 */
	};
}


/* a statistics-gathering pager thread that passes faults up to sigma0, so
 * that the reply (ignored as it is) will result in a situation where the
 * faulting thread recovers properly.
 *
 * the mapped pages aren't tracked.
 */
static void stats_pager_fn(void *param_ptr)
{
	struct pager_stats *stats = param_ptr;
	reset_stats(stats);

	/* NOTE: the repeating delay thing was copypasta'd from
	 * string_test_thread(). it could be moved into a "delay_ctx" structure
	 * with a couple of functions to do reply[/delay]/wait in a re-used
	 * manner. we'll see.
	 */
	L4_Time_t delay = L4_ZeroTime;
	int delay_repeat = 0;
	bool delay_spin = false, run = true;
	while(run) {
		L4_ThreadId_t from;
		L4_LoadBR(0, 0);
		L4_MsgTag_t tag = L4_Wait(&from);

		for(;;) {
			if(L4_IpcFailed(tag)) {
				diag("reply/wait failed, ec %#lx", L4_ErrorCode());
				break;
			}

			if(tag.X.label == QUIT_LABEL) {
				run = false;
				break;
			} else if(tag.X.label == RESET_LABEL) {
				reset_stats(stats);
				L4_LoadMR(0, 0);
			} else if(tag.X.label == DELAY_LABEL
				&& tag.X.u == 3 && tag.X.t == 0)
			{
				L4_Word_t t, r, s;
				L4_StoreMR(1, &t);
				L4_StoreMR(2, &r);
				L4_StoreMR(3, &s);
				delay.raw = t;
				delay_repeat = r;
				delay_spin = !!s;
				L4_LoadMR(0, 0);
			} else if(tag.X.label >> 4 == 0xffe
				&& tag.X.u == 2 && tag.X.t == 0)
			{
				L4_Word_t faddr, fip;
				L4_StoreMR(1, &faddr);
				L4_StoreMR(2, &fip);
				int rwx = tag.X.label & 0x000f;
				stats->n_faults++;
				if(CHECK_FLAG(rwx, L4_Readable)) stats->n_read++;
				if(CHECK_FLAG(rwx, L4_Writable)) stats->n_write++;
				if(CHECK_FLAG(rwx, L4_eXecutable)) stats->n_exec++;
				stats->log_top = (stats->log_top + 1) % LOG_SIZE;
				stats->log[stats->log_top] = L4_FpageLog2(faddr, 12);
				L4_Set_Rights(&stats->log[stats->log_top], rwx);
#if 0
				diag("%s: pf in %lu:%lu at %#lx, ip %#lx", __func__,
					L4_ThreadNo(from), L4_Version(from), faddr, fip);
#endif

				/* pass it on. */
				L4_LoadMR(0, (L4_MsgTag_t){ .X.label = 0xffe0 | rwx,
					.X.u = 2 }.raw);
				L4_LoadMR(1, faddr);
				L4_LoadMR(2, fip);
				L4_LoadBR(0, L4_CompleteAddressSpace.raw);
				tag = L4_Call(L4_Pager());
				if(L4_IpcFailed(tag)) {
					diag("stats-to-pager IPC failed, ec %lu",
						L4_ErrorCode());
					stats->n_fail++;
					break;
				} else if(tag.X.t != 2 || tag.X.u != 0) {
					diag("stats-to-pager IPC returned weird tag %#lx",
						tag.raw);
					stats->n_fail++;
					break;
				} else {
					/* reply. */
					L4_LoadMR(0, 0);
				}
			} else {
				diag("pager got weird IPC from %#lx (label %#lx)",
					from.raw, tag.X.label);
				break;
			}

			if(run) {
				L4_LoadBR(0, 0);
				if(delay.raw == L4_ZeroTime.raw) {
					assert(delay_repeat == 0);
					tag = L4_ReplyWait(from, &from);
				} else {
					tag = L4_Reply(from);
					if(delay_spin) usleep(time_in_us(delay)); else L4_Sleep(delay);
					if(--delay_repeat == 0) delay = L4_ZeroTime;
					if(L4_IpcSucceeded(tag)) {
						tag = L4_Wait(&from);
					}
				}
			}
		}
	}
}


L4_ThreadId_t start_stats_pager(struct pager_stats *stats_mem)
{
	stats_mem->n_faults = 12345;
	L4_ThreadId_t pg_tid = xstart_thread(&stats_pager_fn, stats_mem);
	/* synchronize with the reset label. */
	bool ok = send_reset(pg_tid);
	fail_unless(ok, "stats pager synch failed, ec=%#lx", L4_ErrorCode());
	fail_unless(stats_mem->n_faults == 0, "stats pager acting weird");

	return pg_tid;
}


L4_Word_t stop_stats_pager(L4_ThreadId_t tid)
{
	send_quit(tid);
	xjoin_thread(tid);
	return 0;
}


L4_Fpage_t get_fault(struct pager_stats *stats, L4_Word_t addr)
{
	addr &= ~PAGE_MASK;
	for(int i=0; i <= stats->log_top; i++) {
		if(ADDR_IN_FPAGE(stats->log[i], addr)) return stats->log[i];
	}
	return L4_Nilpage;
}
