
/* unit tests concerning the SpaceControl system call, and pager operation. */

/* TODO: the timeout values for poke(), peek(), and send_quit() should be
 * considered and adjusted to something reasonable and module global.
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

#include <l4/types.h>
#include <l4/ipc.h>
#include <l4/syscall.h>

#include <ukernel/util.h>

#include "defs.h"
#include "test.h"


#define QUIT_LABEL 0xdead
#define POKE_LABEL 0x05e7	/* "set" */
#define PEEK_LABEL 0x06e7	/* "get" */


struct pager_stats {
	int n_faults, n_read, n_write, n_exec, n_fail;
};


/* a statistics-gathering pager thread that passes faults up to sigma0, so
 * that the reply (ignored as it is) will result in a situation where the
 * faulting thread recovers properly.
 *
 * the mapped pages aren't tracked. (TODO: this would be a good candidate for
 * a test case where each test is started from a clean slate.)
 */
static void stats_pager_fn(void *param_ptr)
{
	struct pager_stats *stats = param_ptr;
	*stats = (struct pager_stats){ };

	bool run = true;
	while(run) {
		L4_ThreadId_t from;
		L4_MsgTag_t tag = L4_Wait(&from);

		for(;;) {
			if(L4_IpcFailed(tag)) {
				diag("reply/wait failed, ec %#lx", L4_ErrorCode());
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
				stats->n_faults++;
				if(CHECK_FLAG(rwx, L4_Readable)) stats->n_read++;
				if(CHECK_FLAG(rwx, L4_Writable)) stats->n_write++;
				if(CHECK_FLAG(rwx, L4_eXecutable)) stats->n_exec++;

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
					tag = L4_ReplyWait(from, &from);
				}
			} else {
				diag("pager got weird IPC from %#lx (label %#lx)",
					from.raw, tag.X.label);
				break;
			}
		}
	}
}


/* poke/peek thread. obeys POKE, PEEK, and QUIT. */
static void poke_peek_fn(void *param_ptr)
{
	for(;;) {
		L4_ThreadId_t from;
		L4_MsgTag_t tag = L4_Wait(&from);

		for(;;) {
			if(L4_IpcFailed(tag)) break;

			if(tag.X.label == QUIT_LABEL) return;
			else if(tag.X.label == PEEK_LABEL) {
				L4_Word_t addr;
				L4_StoreMR(1, &addr);
				L4_LoadMR(0, (L4_MsgTag_t){ .X.u = 1 }.raw);
				L4_LoadMR(1, *(uint8_t *)addr);
			} else if(tag.X.label == POKE_LABEL) {
				L4_Word_t addr, value;
				L4_StoreMR(1, &addr);
				L4_StoreMR(2, &value);
				*(uint8_t *)addr = value;
				L4_LoadMR(0, 0);
			} else {
				diag("stats pager thread got weird IPC from %#lx (label %#lx)",
					from.raw, tag.X.label);
				break;
			}

			/* reply. */
			tag = L4_ReplyWait(from, &from);
		}
	}
}


static bool poke(L4_ThreadId_t thread, L4_Word_t address, uint8_t value)
{
	L4_LoadBR(0, 0);
	L4_LoadMR(0, (L4_MsgTag_t){ .X.label = POKE_LABEL, .X.u = 2 }.raw);
	L4_LoadMR(1, address);
	L4_LoadMR(2, value);
	L4_MsgTag_t tag = L4_Call_Timeouts(thread, L4_Never,
		L4_TimePeriod(50 * 1000));
	return L4_IpcSucceeded(tag);
}


static bool peek(uint8_t *value_p, L4_ThreadId_t thread, L4_Word_t address)
{
	L4_LoadBR(0, 0);
	L4_LoadMR(0, (L4_MsgTag_t){ .X.label = PEEK_LABEL, .X.u = 1 }.raw);
	L4_LoadMR(1, address);
	L4_MsgTag_t tag = L4_Call_Timeouts(thread, L4_Never,
		L4_TimePeriod(50 * 1000));
	if(L4_IpcFailed(tag)) return false;
	else {
		L4_Word_t w;
		L4_StoreMR(1, &w);
		*value_p = w;
		return true;
	}
}


static bool send_quit(L4_ThreadId_t thread)
{
	L4_LoadMR(0, (L4_MsgTag_t) { .X.label = QUIT_LABEL }.raw);
	return L4_IpcSucceeded(L4_Send_Timeout(thread, L4_TimePeriod(50 * 1000)));
}


/* fixtures for the pager test case. */

static struct pager_stats *pg_stats = NULL;
static L4_ThreadId_t pg_pager, pg_poker;


static void fixture_fail(const char *fmt, ...)
{
	va_list al;
	va_start(al, fmt);
	char buf[1024];
	vsnprintf(buf, sizeof(buf), fmt, al);
	va_end(al);
	fprintf(stderr, "fixture failed: `%s'\n", buf);

	/* FIXME: add a way for fixtures to fail in a cleaner way, i.e.
	 * setjmp/longjmp
	 */
	abort();
}


static void pager_setup(void)
{
	pg_stats = malloc(sizeof(*pg_stats));
	pg_stats->n_faults = 12345;
	pg_pager = start_thread(&stats_pager_fn, pg_stats);
	if(L4_IsNilThread(pg_pager)) {
		fixture_fail("can't setup stats pager");
	}
	L4_ThreadSwitch(pg_pager);
	if(pg_stats->n_faults != 0) {
		fixture_fail("stats pager acting weird");
	}

	pg_poker = start_thread(&poke_peek_fn, NULL);
	if(L4_IsNilThread(pg_poker)) {
		fixture_fail("can't start poke/peek thread");
	}

	assert(L4_IsGlobalId(pg_pager));
	L4_Set_PagerOf(pg_poker, pg_pager);
/* FIXME: restore when the sys_exregs() can handle readouts */
#if 0
	if(L4_PagerOf(pg_poker).raw != pg_pager.raw) {
		fixture_fail("poker's pager TCR wasn't set");
	}
#endif
}


static void pager_teardown(void)
{
	if(send_quit(pg_poker)) join_thread(pg_poker);
	else {
		/* FIXME: do forced teardown of a failed poker thread */
		fixture_fail("can't stop hung poker thread");
	}

	if(send_quit(pg_pager)) join_thread(pg_pager);
	else fixture_fail("can't stop pager thread");

	free(pg_stats);
	pg_stats = NULL;
}


/* no-hole case */
START_TEST(poke_peek_nofault_test)
{
	plan_tests(3);

	void *ptr = malloc(12 * 1024);
	*(uint8_t *)ptr = 0;
	bool ok = poke(pg_poker, (L4_Word_t)ptr, 0x42);
	skip_start(!ok, 3, "poke failed");
		ok(*(uint8_t *)ptr == 0x42, "poke() did set a byte");
		uint8_t val = 0;
		ok = peek(&val, pg_poker, (L4_Word_t)ptr);
		skip_start(!ok, 2, "peek failed");
			ok(val == 0x42, "peek() returned the right byte");
			*(uint8_t *)ptr = 0xab;
			ok = peek(&val, pg_poker, (L4_Word_t)ptr);
			skip_start(!ok, 1, "second peek failed");
				ok(val == 0xab, "second peek() returned the right byte");
			skip_end;
		skip_end;
	skip_end;

	free(ptr);
}
END_TEST


/* hole case */
START_TEST(poke_peek_fault_test)
{
	plan_tests(4);

	uint8_t *valid = malloc(12 * 1024);
	skip_start((L4_Word_t)valid < 0xf00000, 4, "alloc must be above 15M");
		memset(valid, 0, 12 * 1024);

		uint8_t *invalid = valid - 0xa00000;	/* 10 MiB backward. */
		bool ok = poke(pg_poker, (L4_Word_t)invalid, 0xaf);
		skip_start(!ok, 3, "poke failed: ec %#lx", L4_ErrorCode());
			ok(pg_stats->n_faults == 1 && pg_stats->n_write == 1,
				"poke write-faults");
			uint8_t val;
			ok = peek(&val, pg_poker, (L4_Word_t)invalid);
			skip_start(!ok, 2, "peek failed: ec %#lx", L4_ErrorCode());
				ok(pg_stats->n_faults == 1, "peek doesn't read-fault");
				ok(val == 0xaf, "peek returns poked value");
			skip_end;
		skip_end;

		invalid += 16 * 1024;
		uint8_t val;
		const int old_n = pg_stats->n_faults, old_r = pg_stats->n_read;
		ok = peek(&val, pg_poker, (L4_Word_t)invalid);
		skip_start(!ok, 1, "peek failed: ec %#lx", L4_ErrorCode());
			ok(pg_stats->n_faults == old_n + 1
				&& pg_stats->n_read == old_r + 1, "peek caused read fault");
		skip_end;
	skip_end;

	free(valid);
}
END_TEST


/* tcase "ctl" */

START_TEST(spacectl_basic)
{
	plan_tests(2);

	/* first, an inactive thread. */
	L4_ThreadId_t tid = L4_GlobalId(1024, 199), self = L4_Myself();
	L4_Word_t res = L4_ThreadControl(tid, self, self, L4_nilthread,
		(void *)-1);
	skip_start(res != 1, 2, "creating ThreadControl failed, ec %#lx",
			L4_ErrorCode());
		ok(res == 1, "space & first thread created");
		L4_Word_t ctl_out;
		res = L4_SpaceControl(tid, 0, L4_FpageLog2(0x100000, 12),
			L4_FpageLog2(0x101000, 14), L4_anythread, &ctl_out);
		skip_start(res != 1, 1, "SpaceControl failed, ec %#lx",
				L4_ErrorCode());
			ok(res == 1, "space configured");

			/* TODO: add thread to space, test valid UTCB locations, and
			 * invalid ones outside the area
			 */
		skip_end;
	skip_end;
}
END_TEST


START_TEST(spacectl_iface)
{
	L4_Fpage_t kip_area = L4_FpageLog2(0x100000, 12),
		utcb_area = L4_FpageLog2(0x200000, 12);

	/* FIXME: get this from KIP */
	const L4_Word_t utcb_size = 512;

	plan_tests(2 + L4_Size(utcb_area) / utcb_size);

	L4_ThreadId_t tid = L4_GlobalId(23042, 199), self = L4_Myself();
	L4_Word_t res = L4_ThreadControl(tid, tid, self, L4_nilthread,
		(void *)-1);
	skip_start(res != 1, 1, "creating ThreadControl failed, ec %#lx",
			L4_ErrorCode());
		L4_Word_t ctl_out;

		/* TODO: test overlapping of kip_area with utcb_area */
		/* TODO: test too small kip_area & utcb_area */

		res = L4_SpaceControl(tid, 0, kip_area, utcb_area, L4_anythread,
			&ctl_out);
		skip_start(res != 1, 1, "SpaceControl failed, ec %#lx",
				L4_ErrorCode());

			/* configure valid threads within the UTCB area. */
			for(L4_Word_t addr = L4_Address(utcb_area);
				addr < L4_Address(utcb_area) + L4_Size(utcb_area);
				addr += utcb_size)
			{
				res = L4_ThreadControl(tid, tid, L4_nilthread, L4_nilthread,
					(void *)addr);
				ok(res == 1, "can set UTCB at %#lx", addr);
			}

			/* and outside the UTCB area. */
			L4_Word_t out_posns[] = {
				L4_Address(utcb_area) - utcb_size,
				L4_Address(utcb_area) + L4_Size(utcb_area),
			};
			for(int i=0; i < NUM_ELEMENTS(out_posns); i++) {
				L4_Word_t addr = out_posns[i];
				res = L4_ThreadControl(tid, tid, L4_nilthread, L4_nilthread,
					(void *)addr);
				ok(res == 0 && L4_ErrorCode() == 6,
					"addr %#lx is outside UTCB range", addr);
			}

			/* TODO: test UTCB position change while thread is activated. */
		skip_end;
	skip_end;
}
END_TEST


Suite *space_suite(void)
{
	Suite *s = suite_create("space");

	TCase *pager_case = tcase_create("pager");
	tcase_add_checked_fixture(pager_case, &pager_setup, &pager_teardown);
	tcase_add_test(pager_case, poke_peek_nofault_test);
	tcase_add_test(pager_case, poke_peek_fault_test);
	suite_add_tcase(s, pager_case);

	TCase *ctl_case = tcase_create("ctl");
	tcase_add_test(ctl_case, spacectl_basic);
	tcase_add_test(ctl_case, spacectl_iface);
	suite_add_tcase(s, ctl_case);

	return s;
}
