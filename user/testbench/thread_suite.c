
/* tests concerning the ThreadControl and ExchangeRegister system calls, and
 * TCR access.
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <unistd.h>

#include <ccan/compiler/compiler.h>
#include <ccan/talloc/talloc.h>

#include <l4/types.h>
#include <l4/kip.h>
#include <l4/ipc.h>
#include <l4/space.h>
#include <l4/schedule.h>
#include <l4/syscall.h>

#include <ukernel/util.h>

#include "forkserv-defs.h"
#include "defs.h"
#include "test.h"


bool as_exists(L4_ThreadId_t space_id)
{
	/* it's not properly specified in L4.X2, but a L4_Nilpage is the no-op for
	 * SpaceControl's fields.
	 */
	L4_Word_t old_ctl, res = L4_SpaceControl(space_id, 0, L4_Nilpage,
		L4_Nilpage, L4_nilthread, &old_ctl);
	L4_Word_t ec = L4_ErrorCode();
	fail_if(res == 0 && ec != L4_ERROR_INVALID_SPACE,
		"res=%lu, ec=%#lx", res, ec);
	return res == 1;
}


bool thr_exists(L4_ThreadId_t tid) {
	/* funnily enough, as all threads exist within a space, ... */
	return as_exists(L4_GlobalIdOf(tid));
}


static L4_Word_t exregs_ctl(L4_ThreadId_t tid) {
	L4_Word_t ctl, foo;
	L4_ThreadId_t dummy, ret = L4_ExchangeRegisters(tid, 0x200,
		0, 0, 0, 0, L4_nilthread, &ctl, &foo, &foo, &foo, &foo, &dummy);
	fail_if(L4_IsNilThread(ret), "ec=%#lx", L4_ErrorCode());
	return ctl;
}


bool thr_is_halted(L4_ThreadId_t tid) {
	return CHECK_FLAG(exregs_ctl(tid), 1);		/* the H bit */
}


bool thr_in_recv(L4_ThreadId_t tid) {
	return CHECK_FLAG(exregs_ctl(tid), 2);		/* the R bit */
}


bool thr_in_send(L4_ThreadId_t tid) {
	return CHECK_FLAG(exregs_ctl(tid), 4);		/* the S bit */
}


/* TODO: add a forkserv (or some such) call that lets a forking test case call
 * Schedule on threads it created. then re-enable forking in the exregs case.
 */
L4_Word_t get_schedstate(L4_ThreadId_t tid) {
	L4_Word_t dummy, res = L4_Schedule(tid, ~0ul, ~0, ~0, ~0, &dummy);
	fail_if(res == L4_SCHEDRESULT_ERROR,
		"Schedule failed: ec=%#lx", L4_ErrorCode());
	return res;
}


struct ir_param {
	L4_ThreadId_t partner;
	L4_Word_t value;
};


static void ipc_returner(void *param)
{
	struct ir_param *p = param;
	L4_LoadMR(0, (L4_MsgTag_t){ .X.u = 1, .X.label = 0xdaf7 }.raw);
	L4_LoadMR(1, p->value ^ 0xbabe);	/* also known as a calf */
	L4_MsgTag_t tag = L4_Call(p->partner);
	if(L4_IpcFailed(tag)) {
		diag("%s: call failed, ec=%#lx", __func__, L4_ErrorCode());
	}
	L4_Word_t do_exit;
	L4_StoreMR(1, &do_exit);
	if(do_exit != 0) {
		/* distinguish from a plain exit. (the outer clause is there because
		 * this is also called from test_as_exists, which uses forking and
		 * thus shouldn't call exit_thread().)
		 */
		exit_thread(param);
	}
}


/* confirm the properties of thr_exists(). mainly that when we know that a
 * thread exists (i.e. it does a particular IPC operation back to us),
 * thr_exists() returns true; and when that thread has been joined,
 * thr_exists() returns false.
 */
START_TEST(test_thr_exists)
{
	plan_tests(4);

	struct ir_param *p = malloc(sizeof(*p));
	p->partner = L4_Myself();
	p->value = 0x82904ba4;
	L4_ThreadId_t test_tid = start_thread(&ipc_returner, p),
		wrong_ver = L4_GlobalId(L4_ThreadNo(test_tid),
			L4_Version(test_tid) ^ 0x1f);
	fail_unless(L4_IsGlobalId(test_tid));

	L4_MsgTag_t tag = L4_Receive_Timeout(test_tid, TEST_IPC_DELAY);
	fail_if(L4_IpcFailed(tag), "ec=%#lx", L4_ErrorCode());
	L4_Word_t value;
	L4_StoreMR(1, &value);
	fail_unless(value == (p->value ^ 0xbabe));
	ok(thr_exists(test_tid), "exists during call");
	ok(!thr_exists(wrong_ver), "wrong version doesn't exist");

	/* verify the thread's presence with ExchangeRegisters, too. */
	L4_ThreadId_t test_ltid = L4_LocalIdOf(test_tid);
	fail_if(L4_IsNilThread(test_ltid), "ec=%#lx", L4_ErrorCode());

	L4_LoadMR(0, (L4_MsgTag_t){ .X.u = 1 }.raw);
	L4_LoadMR(1, 1);		/* plz to be calling exit_thread() */
	tag = L4_Reply(test_tid);
	fail_if(L4_IpcFailed(tag), "ec=%#lx", L4_ErrorCode());

	L4_Word_t ec = 0;
	void *result = join_thread_long(test_tid, L4_TimePeriod(10 * 1000), &ec);
	fail_if(result != p, "can't join helper thread: ec=%#lx", ec);
	ok(!thr_exists(test_tid), "removed after join");
	ok(!thr_exists(wrong_ver), "wrong version still doesn't exist");

	/* same for its absence. */
	test_ltid = L4_LocalIdOf(test_tid);
	fail_unless(L4_IsNilThread(test_ltid));

	free(p);
}
END_TEST


/* similar to test_thr_exists, but instead of start_thread() and end_thread()
 * there'll be forking.
 */
START_TEST(test_as_exists)
{
	plan_tests(4);

	struct ir_param *p = malloc(sizeof(*p));
	p->partner = L4_Myself();
	p->value = 0x82904ba4;
	L4_ThreadId_t test_tid;
	int child = fork_tid(&test_tid);
	if(child == 0) {
		diag("child-side getpid()=%d", getpid());
		ipc_returner(p);
		free(p);
		exit(0);
	}

	L4_ThreadId_t wrong_ver = L4_GlobalId(L4_ThreadNo(test_tid),
		L4_Version(test_tid) ^ 0x1f);

	L4_MsgTag_t tag = L4_Receive_Timeout(test_tid, TEST_IPC_DELAY);
	fail_if(L4_IpcFailed(tag), "ec=%#lx", L4_ErrorCode());
	L4_Word_t value;
	L4_StoreMR(1, &value);
	fail_unless(value == (p->value ^ 0xbabe));
	ok(as_exists(test_tid), "exists during call");
	ok(!as_exists(wrong_ver), "wrong version doesn't exist");

	L4_LoadMR(0, (L4_MsgTag_t){ .X.u = 1 }.raw);
	L4_LoadMR(1, 0);	/* no thread exits please, we're british */
	tag = L4_Reply(test_tid);
	fail_if(L4_IpcFailed(tag), "ec=%#lx", L4_ErrorCode());

	int st, dead = wait(&st);
	fail_if(dead < 0);
	fail_if(dead != child, "expected dead=%d, got %d instead",
		child, dead);
	ok(!as_exists(test_tid), "removed after wait");
	ok(!as_exists(wrong_ver), "wrong version still doesn't exist");

	free(p);
}
END_TEST


/* TODO: move this into util.c or somewhere */
static void del_thread(L4_ThreadId_t tid)
{
	/* "i liked the part where he said L4_nilthread" */
	L4_Word_t res = L4_ThreadControl(tid, L4_nilthread, L4_nilthread,
		L4_nilthread, (void *)-1);
	fail_unless(res == 1, "ec=%#lx", L4_ErrorCode());
}


/* NOTE: this can be called with utcb=true at most once. after that, it'll
 * fail with the UTCB reuse code.
 */
static void mk_thread(L4_ThreadId_t tid, bool utcb)
{
	L4_KernelInterfacePage_t *kip = L4_GetKernelInterface();
	L4_Word_t utcb_ptr = (L4_Word_t)-1l;
	if(utcb) {
		/* c wut I did thar? */
		L4_ThreadId_t other = start_thread(&exit_thread, NULL);
		L4_ThreadId_t ltid = L4_LocalIdOf(other);
		join_thread(other);
		utcb_ptr = ltid.raw & ~(L4_UtcbSize(kip) - 1);
	}
	L4_Word_t res = L4_ThreadControl(tid, L4_Myself(), L4_Myself(),
		L4_nilthread, (void *)utcb_ptr);
	fail_unless(res == 1, "ec=%#lx", L4_ErrorCode());
	fail_unless(!utcb
		|| (L4_LocalIdOf(tid).raw & ~(L4_UtcbSize(kip) - 1)) == utcb_ptr);
}


START_TEST(threadctl_basic)
{
	L4_Fpage_t kip_area = L4_FpageLog2(0x100000, 12),
		utcb_area = L4_FpageLog2(0x200000, 12);

	L4_KernelInterfacePage_t *kip = L4_GetKernelInterface();
	const L4_Word_t utcb_size = L4_UtcbSize(kip);

	int n_tests;
	plan_tests(n_tests = 4 + L4_Size(utcb_area) / utcb_size);
	diag("utcb_size=%lu", utcb_size);

	L4_ThreadId_t tid = L4_GlobalId(2369, 199), self = L4_Myself();
	L4_Word_t res = L4_ThreadControl(tid, tid, self, L4_nilthread,
		(void *)-1);
	fail_unless(res == 1, "creating ThreadControl failed, ec %#lx",
		L4_ErrorCode());

	L4_Word_t ctl_out;

	res = L4_SpaceControl(tid, 0, kip_area, utcb_area, L4_anythread,
		&ctl_out);
	fail_unless(res == 1, "SpaceControl failed, ec %#lx", L4_ErrorCode());

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

	res = L4_ThreadControl(tid, L4_nilthread, L4_nilthread,
		L4_nilthread, (void *)-1);
	ok(res == 1, "thread/space delete ok");

	/* test that further threads cannot be created since the space is gone. */
	L4_ThreadId_t non_tid = L4_GlobalId(2300, 123);
	res = L4_ThreadControl(non_tid, tid, L4_MyGlobalId(),
		L4_nilthread, (void *)-1);
	if(!ok(res == 0 && L4_ErrorCode() == 3,
		"post-delete thread creation fails properly"))
	{
		diag("res=%lu, ec=%#lx", res, L4_ErrorCode());

		/* clean up if it was created anyway. */
		res = L4_ThreadControl(non_tid, L4_nilthread, L4_nilthread,
			L4_nilthread, (void *)-1);
		fail_if(res != 1, "cleanup ThreadControl failed, ec=%#lx",
			L4_ErrorCode());
	}
}
END_TEST


static L4_Word_t privilege_case(L4_Word_t *ec_p)
{
	/* ox cat, ruler of wildebeest */
	L4_ThreadId_t tid = L4_GlobalId(0xca7, 111);
	L4_Word_t res = L4_ThreadControl(tid, L4_Myself(), L4_Myself(),
		L4_nilthread, (void *)-1);
	*ec_p = L4_ErrorCode();
	if(res == 1) {
		/* clean up on success. */
		L4_Word_t r2 = L4_ThreadControl(tid, L4_nilthread, L4_nilthread,
			L4_nilthread, (void *)-1);
		fail_if(r2 != 1, "on delete, ec=%#lx", L4_ErrorCode());
	}

	return res;
}


START_TEST(privilege)
{
	plan_tests(2);

	/* point 1: should succeed from a privileged space. */
	L4_Word_t ec, res = privilege_case(&ec);
	if(!ok1(res == 1)) diag("ec=%#lx", ec);

	/* point 2: should fail from a non-privileged (forked) space. */
	int child = fork();
	if(child == 0) {
		res = privilege_case(&ec);
		if(!ok1(res == 0 && ec == 1)) diag("res=%lu, ec=%#lx", res, ec);
		exit(0);
	} else {
		int status, dead = wait(&status);
		fail_unless(dead == child, "reaped %d (expected %d)",
			dead, child);
	}
}
END_TEST


/* returns 0 on success, ErrorCode on failure */
static int try_create_thread(L4_ThreadId_t tid)
{
	L4_ThreadId_t space = L4_Myself();
	L4_Word_t res = L4_ThreadControl(tid, space, L4_Myself(),
		L4_nilthread, (void *)-1);
	if(res == 0) return L4_ErrorCode();
	else {
		del_thread(tid);
		return 0;
	}
}


/* there's two kinds of version field: those that have the last bits cleared
 * (i.e. local TIDs), and those that don't. the former is not valid for
 * creation, and the latter is. this tests tries both for cases where the
 * example is supposed to pass; those are user threads that've got valid
 * version numbers.
 */
START_TEST(thread_id_validity)
{
	plan_tests(7);	/* 5 cases, 2 repeated */

	L4_KernelInterfacePage_t *kip = L4_GetKernelInterface();
	int last_int = kip->ThreadInfo.X.SystemBase - 1,
		last_kern = kip->ThreadInfo.X.UserBase - 1;
	fail_if(last_int == 0);
	fail_if(last_kern <= last_int,
		"last_kern=%d, last_int=%d", last_kern, last_int);

	const struct {
		L4_ThreadId_t tid;
		bool pass;
	} cases[5] = {
		{ L4_GlobalId(last_int, 1), false },
		{ L4_GlobalId(last_kern, 1), false },
		{ L4_GlobalId(last_kern + 9999, 1), true },
		{ L4_GlobalId(0x3ffff, 2), true },
		{ L4_GlobalId(0x12345, 256), false },	/* local TID, still */
	};
	for(int version=1; version >= 0; --version) {
		for(int i=0; i < NUM_ELEMENTS(cases); i++) {
			if(!version && !cases[i].pass) {
				/* no point in testing when there's no pass/fail line */
				continue;
			}
			L4_ThreadId_t test_tid = L4_GlobalId(L4_ThreadNo(cases[i].tid),
				version ? L4_Version(cases[i].tid) : 0);
			int n = try_create_thread(test_tid);
			bool expect = cases[i].pass && L4_IsGlobalId(test_tid);
			if(!ok((n == 0) == expect, "%s creation of %lu:%lu",
				expect ? "succeed" : "fail",
				L4_ThreadNo(test_tid), L4_Version(test_tid)))
			{
				diag("n=%d, expect=%s", n, btos(expect));
			}
		}
	}
}
END_TEST


/* returns 0 on success, ErrorCode on failure */
static int try_scheduler_id(L4_ThreadId_t sched_tid)
{
	L4_ThreadId_t t_tid = L4_GlobalId(0xbeef, 3);
	mk_thread(t_tid, false);

	/* now, the experiment. */
	L4_Word_t res = L4_ThreadControl(t_tid, L4_Myself(), sched_tid,
		L4_nilthread, (void *)-1);
	int rv = res == 0 ? L4_ErrorCode() : 0;

	del_thread(t_tid);
	return rv;
}


/* four things:
 *   - that interrupts and kernel threads cannot be set as scheduler;
 *   - that valid, existing thread IDs will be accepted for scheduler;
 *   - that those will be accepted even if specified as local thread IDs;
 *   - that thread IDs that don't exist won't be accepted for scheduler.
 *
 * it follows from what ThreadControl will create that invalid thread IDs will
 * not be accepted.
 */
START_TEST(scheduler_id_validity)
{
	plan_tests(7);
	const L4_ThreadId_t valid_tid = L4_GlobalId(0xdead, 17),
		nonex_tid = L4_GlobalId(0xb007, 19);	/* bovine footwear */
	int (*try)(L4_ThreadId_t thing) = &try_scheduler_id;

	/* part 1. */
	const L4_KernelInterfacePage_t *kip = L4_GetKernelInterface();
	int last_kern = kip->ThreadInfo.X.UserBase - 1,
		last_int = kip->ThreadInfo.X.SystemBase - 1;
	ok1(try(L4_GlobalId(last_int, 1)) == 4);
	ok1(try(L4_GlobalId(last_kern, 13)) == 4);

	/* part 2. */
	/* shouldn't be valid before creation. */
	ok(try(valid_tid) != 0, "valid rejected before create");
	mk_thread(valid_tid, true);
	ok(try(valid_tid) == 0, "valid works after create");
	L4_ThreadId_t ltid = L4_LocalIdOf(valid_tid);
	fail_unless(L4_IsLocalId(ltid));
	ok(try(ltid) == 0, "valid works as local tid");		/* part 3 */
	del_thread(valid_tid);

	/* part 4. */
	mk_thread(nonex_tid, true);
	ltid = L4_LocalIdOf(nonex_tid);
	del_thread(nonex_tid);
	ok1(try(nonex_tid) == 4);
	fail_unless(L4_IsNilThread(L4_LocalIdOf(nonex_tid)));
	fail_unless(L4_IsNilThread(L4_GlobalIdOf(ltid)));
	if(!ok(try(ltid) == 4, "reject nonexistant local TID")) {
		diag("ltid=%#lx", ltid.raw);
	}
}
END_TEST


static bool try_mk_in_space(
	L4_ThreadId_t tid,
	L4_ThreadId_t spacespec,
	L4_Word_t *ec_p)
{
	L4_Word_t res = L4_ThreadControl(tid, spacespec, L4_Myself(),
		L4_nilthread, (void *)-1);
	if(res == 1) {
		/* delete it right away. */
		del_thread(tid);
		*ec_p = 0;
	} else {
		*ec_p = L4_ErrorCode();
	}

	return res == 1;
}


/* check negative properties of SpaceSpecifier with all five settings:
 *
 *   - first, that only SpaceSpecifier = non-nil Global TID creates threads;
 *     - (also, conveniently, that SpaceSpecifier = nil succeeds on a
 *       non-extant thread)
 *   - second, that only SpaceSpecifier = nil deletes threads;
 *   - third, that only SpaceSpecifier = dest creates address spaces;
 *   - fourth, that a sub-user or local SpaceSpecifier isn't valid;
 *   - (TODO) fifth, that SpaceSpecifier != nil migrates threads
 *     - (sixth, implied), that it works even to an address space that's
 *       created by the moving ThreadControl.
 *
 * (testing for presence in an address space can be done with another thread
 * that does an ExchangeRegisters on the global ID. if the result is
 * L4_nilthread, then it's in a foreign space. so for inter-space migrations
 * this could test the pre-state and both ends of the transition, but for the
 * "created space" case only that the thread has left the building.
 *
 * clearly that's not good enough, so parts five and six are left for another
 * day.)
 */
START_LOOP_TEST(spacespec_validity, iter, 0, 14)
{
	L4_KernelInterfacePage_t *kip = L4_GetKernelInterface();
	int last_int = kip->ThreadInfo.X.SystemBase - 1,
		last_kern = kip->ThreadInfo.X.UserBase - 1;
	if(last_int == 0 || last_kern <= last_int) {
		/* TODO: skip some individual cases instead? */
		plan_skip_all("no interrupt or system threads");
		return;
	}

	/* different kinds of SpaceSpecifier. */
	const L4_ThreadId_t oth = L4_GlobalId(0xb00b, 17),	/* aka. udder */
		oth_nonex = L4_GlobalId(L4_ThreadNo(oth) + 1,
			(L4_Version(oth) + 1) | 7),
		local = L4_LocalIdOf(L4_Myself()),
		test_tid = L4_GlobalId(0xc0de, 13);		/* moo cant */
	assert(L4_IsLocalId(local));
	assert(!thr_exists(test_tid));
	mk_thread(oth, false);
	const struct {
		L4_ThreadId_t space;
		const char *name;
		bool valid;		/* false if it's never supposed to pass */
	} cases[15] = {
		/* part 4: the invalid SpaceSpecifiers. */
		{ local, "local TID", false },
		{ L4_GlobalId(last_int, 1), "int range, v=1", false },
		{ L4_GlobalId(last_int, 0), "int range, v=0", false },
		{ L4_GlobalId(last_int, 41), "int range, v=41", false },
		{ L4_GlobalId(last_int, 0x1f00),
			"int range, local id, nonzero version", false },
		{ L4_GlobalId(last_kern, 1), "kern range", false },
		{ L4_GlobalId(L4_ThreadNo(test_tid), L4_Version(test_tid) ^ 0x1f),
			"dest with wrong version", false },
		{ L4_GlobalId(L4_ThreadNo(oth), L4_Version(oth) ^ 0x1f),
			"extant with wrong version", false },

		/* special ones */
		{ L4_anythread, "anythread", false },
		{ L4_anylocalthread, "anylocalthread", false },
		{ (L4_ThreadId_t){ .raw = ~0u }, "xmas tree", false },

		/* the rest */
		{ L4_nilthread, "nil", true },
		{ oth, "extant TID", true },
		{ oth_nonex, "non-extant TID", true },
		{ test_tid, "SpaceSpecifier = dest", true },
	};
	assert(iter >= 0 && iter < NUM_ELEMENTS(cases));

	plan_tests(6);

	const L4_ThreadId_t spec = cases[iter].space;
	const bool valid = cases[iter].valid;
	const char *name = cases[iter].name;
	assert(strstr(name, "local") == NULL || L4_IsLocalId(spec));
	diag("for %s [spec.raw=%#08lx, dest.raw=%#lx]:", name,
		spec.raw, test_tid.raw);

	const bool is_nil = L4_IsNilThread(spec),
		is_local = L4_IsLocalId(spec),
		is_int = L4_ThreadNo(spec) <= last_int,
		is_system = L4_ThreadNo(spec) <= last_kern && !is_int,
		is_user = !is_int && !is_system,
		is_nonex = spec.raw == oth_nonex.raw;
	assert(!valid || !is_local || is_nil);
	L4_Word_t ec;

	/* part 1: only SpaceSpecifier != nil creates threads. */
	{
		assert(!thr_exists(test_tid));
		bool create_ok = try_mk_in_space(test_tid, spec, &ec),
			invd_space = !create_ok && ec == L4_ERROR_INVALID_SPACE;
		assert(!thr_exists(test_tid));
		imply_ok1(!valid, invd_space);		/* part 4: the test point */
		imply_ok1(create_ok, (is_user && !is_local && !is_nonex) || is_nil);

		/* bonus! */
		if(!imply_ok(is_nil, ec == 0, "deletion idempotence")) {
			diag("is_nil=%s, ec=%#lx", btos(is_nil), ec);
		}
	}

	/* part 2: only SpaceSpecifier == nil deletes threads. */
	{
		mk_thread(test_tid, false);
		assert(thr_exists(test_tid));
		L4_ThreadControl(test_tid, spec, L4_nilthread,
			L4_nilthread, (void *)-1);
		iff_ok1(!thr_exists(test_tid), is_nil);

		if(thr_exists(test_tid)) del_thread(test_tid);
	}

	/* part 3: only SpaceSpecifier == dest creates spaces. */
	{
		assert(!as_exists(test_tid));
		assert(!as_exists(oth_nonex));
		const bool ex_before = as_exists(spec);
		L4_Word_t res = L4_ThreadControl(test_tid, spec, L4_Myself(),
			L4_Myself(), (void *)-1);
		if(res == 0) diag("res=%lu, ec=%#lx", res, L4_ErrorCode());
		const bool create_ok = res != 0,
			did_create = !ex_before && as_exists(test_tid);
		iff_ok1(did_create, create_ok && spec.raw == test_tid.raw);

		/* in particular, the non-existent but otherwise valid & non-"dest"
		 * spacespec doesn't create.
		 */
		imply_ok1(is_nonex, !did_create);

		if(as_exists(test_tid)) del_thread(test_tid);
		assert(!as_exists(test_tid));
	}

	/* TODO: parts 5 and 6 */

	del_thread(oth);
}
END_TEST


/* test whether UTCB relocation retains TCR values (it should) */
START_TEST(relocate_utcb)
{
	plan_tests(4);
	const L4_Word_t udh_val = 0xbeefc0de;
	const L4_ThreadId_t pager_tid = L4_GlobalId(0xbaaa, 17);

	/* create a thread and set its pager and udh. */
	L4_ThreadId_t o_tid = L4_GlobalId(0xbeef, 5);
	mk_thread(o_tid, true);
	L4_Set_UserDefinedHandleOf(o_tid, udh_val);
	L4_Set_PagerOf(o_tid, pager_tid);

	/* part 1: examine that the UDH etc. are right before the UTCB switch. */
	ok1(L4_UserDefinedHandleOf(o_tid) == udh_val);
	ok1(L4_PagerOf(o_tid).raw == pager_tid.raw);

	/* part 2: relocate the UTCB, re-check these values */
	L4_KernelInterfacePage_t *kip = L4_GetKernelInterface();
	L4_Word_t utcb = L4_LocalIdOf(o_tid).raw & ~(L4_UtcbSize(kip) - 1),
		new_utcb = utcb + L4_UtcbSize(kip);
	diag("moving utcb=%#lx -> utcb'=%#lx", utcb, new_utcb);
	L4_Word_t ret = L4_ThreadControl(o_tid, o_tid, L4_nilthread, L4_nilthread,
		(void *)new_utcb);
	fail_if(ret == 0, "ec=%#lx", L4_ErrorCode());
	ok1(L4_UserDefinedHandleOf(o_tid) == udh_val);
	ok1(L4_PagerOf(o_tid).raw == pager_tid.raw);

	del_thread(o_tid);
}
END_TEST


static bool try_del(L4_ThreadId_t tid) {
	L4_Word_t res = L4_ThreadControl(tid, L4_nilthread, L4_nilthread,
		L4_nilthread, (void *)-1);
	return res != 0;
}


/* test that a deleting ThreadControl identifies "dest" by its ThreadNo field
 * alone. version bits should be ignored.
 */
START_TEST(deletion_by_threadno)
{
	plan_tests(4);

	L4_ThreadId_t tid = L4_GlobalId(0xb007, 17);
	assert(!thr_exists(tid));

	/* base case: should delete with the exact same thread ID. */
	mk_thread(tid, false);
	bool del_ok = try_del(tid);
	ok(del_ok, "base case");
	imply_ok1(del_ok, !thr_exists(tid));
	if(thr_exists(tid)) del_thread(tid);

	/* part 1: should delete also with a different version number. */
	mk_thread(tid, false);
	L4_ThreadId_t new_ver = L4_GlobalId(L4_ThreadNo(tid),
		L4_Version(tid) ^ 0x1f);
	assert(L4_Version(tid) != L4_Version(new_ver));
	del_ok = try_del(new_ver);
	ok(del_ok, "delete altered version");
	imply_ok1(del_ok, !thr_exists(tid) && !thr_exists(new_ver));
	if(thr_exists(tid)) del_thread(tid);
}
END_TEST


static void suicide_thread_fn(void *param)
{
	L4_Sleep(A_SHORT_NAP);
	if(param != NULL) {
		L4_Word_t res = L4_ThreadControl(L4_Myself(), L4_nilthread,
			L4_nilthread, L4_nilthread, (void *)-1);
		diag("returned from suicide ThreadControl; res=%lu", res);
	} else {
		diag("not committing suicide");
	}
}


/* test that self-deletion causes the thread to be destroyed. */
START_LOOP_TEST(suicide, iter, 0, 1)
{
	plan_tests(4);
	const bool die = CHECK_FLAG(iter, 1);
	diag("die=%s", btos(die));

	L4_ThreadId_t other = xstart_thread(&suicide_thread_fn,
		die ? "die!" : NULL);
	L4_MsgTag_t tag = L4_Receive_Timeout(other, TEST_IPC_DELAY);
	L4_Word_t ec = L4_ErrorCode();
	ok1(L4_IpcFailed(tag));
	imply_ok1(die, ec == 5);	/* peer doesn't exist */
	imply_ok1(!die, ec == 3);	/* receive timed out */
	iff_ok1(!thr_exists(other), die);
	if(exit_status() != 0) {
		diag("ec=%#lx", ec);
	}

	if(!die) xjoin_thread(other);
	else {
		/* it's easier to just recreate the TID instead of changing
		 * kill_thread() to handle threads that were deleted out of band.
		 */
		L4_Word_t res = L4_ThreadControl(other, L4_Myself(), L4_Myself(),
			L4_Pager(), (void *)-1);
		fail_if(res != 1, "ec=%#lx", L4_ErrorCode());
		kill_thread(other);
	}
}
END_TEST


/* modify self to set scheduler or pager. */
START_LOOP_TEST(modify_self, iter, 0, 1)
{
	const bool set_scheduler = CHECK_FLAG(iter, 1);
	diag("set_scheduler=%s", btos(set_scheduler));
	plan_tests(3);
	void *tctx = talloc_new(NULL);

	struct pager_stats *stats = talloc(tctx, struct pager_stats);
	const L4_ThreadId_t safe_pager = start_stats_pager(stats),
		old_pager = L4_Pager();

	L4_ThreadId_t self = L4_Myself();
	diag("self=%lu:%lu, safe_pager=%lu:%lu, old_pager=%lu:%lu",
		L4_ThreadNo(self), L4_Version(self),
		L4_ThreadNo(safe_pager), L4_Version(safe_pager),
		L4_ThreadNo(old_pager), L4_Version(old_pager));

	/* change it forward. */
	L4_Word_t ret = L4_ThreadControl(self, self,
		set_scheduler ? safe_pager : L4_nilthread,
		!set_scheduler ? safe_pager : L4_nilthread,
		(void *)-1);
	/* implies that current thread was resumed */
	L4_Word_t ec = L4_ErrorCode();
	if(!ok1(ret == 1)) {
		diag("ThreadControl failed, ret=%lu, ec=%#lx", ret, ec);
	}

	imply_ok1(!set_scheduler, L4_SameThreads(L4_Pager(), safe_pager));

	const int old_faults = stats->n_faults;

	/* cause a pagefault.
	 *
	 * TODO: also cause a scheduling event, and make the stats pager record
	 * that also.
	 */
	const size_t fault_size = PAGE_SIZE * 16;
	void *memory = talloc_array(tctx, uint8_t, fault_size);
	memset(memory, 0, fault_size);
	L4_Fpage_t fp = L4_FpageLog2(
		((L4_Word_t)memory + PAGE_SIZE - 1) & ~PAGE_MASK, PAGE_BITS);
	L4_Set_Rights(&fp, L4_Writable);
	L4_FlushFpage(fp);
	memset(memory, 0xba, fault_size);

	/* measure the fault. */
	if(!imply_ok1(!set_scheduler, stats->n_faults > old_faults)) {
		diag("old_faults=%d, stats->n_faults=%d",
			old_faults, stats->n_faults);
	}

	/* change the pager back. */
	ret = L4_ThreadControl(self, self, L4_nilthread, old_pager, (void *)-1);
	fail_if(ret != 1, "ret=%u, ec=%#lx", ret, L4_ErrorCode());
	fail_unless(L4_SameThreads(L4_Pager(), old_pager));

	stop_stats_pager(safe_pager);
	talloc_free(tctx);
}
END_TEST


/* thread that may be queried wrt the global ID it sees for itself. it'll also
 * report roughly where its stack frame is.
 *
 * TODO: should also return Pager, ExceptionHandler, UserDefinedHandle, etc.
 * to see that they weren't affected.
 */
static void stomp_thread_fn(void *param_ptr UNUSED)
{
	diag("%s: running", __func__);

	L4_ThreadId_t sender;
	L4_LoadBR(0, 0);
	L4_MsgTag_t tag = L4_WaitLocal_Timeout(L4_Never, &sender);
	if(L4_IpcFailed(tag)) {
		printf("%s: waitlocal failed, ec=%#lx\n", __func__, L4_ErrorCode());
		return;
	}
	L4_Word_t fall_asleep; L4_StoreMR(1, &fall_asleep);

	L4_LoadMR(0, (L4_MsgTag_t){ .X.u = 2 }.raw);
	L4_LoadMR(1, L4_MyGlobalId().raw);
	L4_LoadMR(2, (L4_Word_t)&sender);
	tag = L4_Reply(sender);
	if(L4_IpcFailed(tag)) {
		printf("%s: reply failed, ec=%#lx\n", __func__, L4_ErrorCode());
	}

	if(fall_asleep) {
		/* this might fail immediately when the thread's global ID isn't what
		 * the UTCB indicates. which may happen because of things.
		 */
		L4_Sleep(L4_TimePeriod(60 * 1000 * 1000));

		/* so for an encore, pop off into never-never land in an unportable
		 * manner.
		 */
		L4_Set_ExceptionHandler(L4_nilthread);
		asm volatile ("int $123");
	}
}


/* test whether a version-overwriting ThreadControl's effect is seen in the
 * UTCB also.
 */
START_TEST(version_stomp)
{
	plan_tests(3);

	L4_ThreadId_t orig_tid = xstart_thread(&stomp_thread_fn, NULL);
	L4_LoadBR(0, 0);
	L4_LoadMR(0, (L4_MsgTag_t){ .X.u = 1 }.raw);
	L4_LoadMR(1, 0);
	L4_MsgTag_t tag = L4_Call(orig_tid);
	fail_if(L4_IpcFailed(tag), "ec=%#lx", L4_ErrorCode());
	L4_ThreadId_t report_tid; L4_StoreMR(1, &report_tid.raw);
	L4_Word_t report_stk; L4_StoreMR(2, &report_stk);
	ok(L4_SameThreads(report_tid, orig_tid), "base case TID report");

	/* step on its version bits, then restart it. */
	L4_ThreadId_t new_tid = L4_GlobalId(L4_ThreadNo(orig_tid),
		L4_Version(orig_tid) ^ 0x120);
	assert(new_tid.raw != orig_tid.raw);
	L4_Word_t res = L4_ThreadControl(new_tid, orig_tid,
		L4_nilthread, L4_nilthread, (void *)-1);
	fail_if(res == 0, "threadctl ec=%#lx", L4_ErrorCode());
	assert(thr_exists(new_tid));
	assert(!thr_exists(orig_tid));
	L4_Start_SpIp(new_tid, report_stk & ~0xf, (L4_Word_t)&stomp_thread_fn);
	/* measure again. */
	L4_LoadBR(0, 0);
	L4_LoadMR(0, (L4_MsgTag_t){ .X.u = 1 }.raw);
	L4_LoadMR(1, 1);
	tag = L4_Call_Timeouts(new_tid, TEST_IPC_DELAY, TEST_IPC_DELAY);
	L4_StoreMR(1, &report_tid.raw);
	L4_StoreMR(2, &report_stk);
	ok1(L4_IpcSucceeded(tag));
	skip_start(L4_IpcFailed(tag), 1, "ipc failed, ec=%#lx", L4_ErrorCode()) {
		if(!ok(L4_SameThreads(report_tid, new_tid),
			"modified case TID report"))
		{
			diag("report_tid=%lu:%lu, new_tid=%lu:%lu",
				L4_ThreadNo(report_tid), L4_Version(report_tid),
				L4_ThreadNo(new_tid), L4_Version(new_tid));
		}
	} skip_end;

	/* restore version bits, clean up */
	res = L4_ThreadControl(orig_tid, new_tid, L4_nilthread,
		L4_nilthread, (void *)-1);
	if(res == 0) {
		diag("2nd threadctl ec=%#lx", L4_ErrorCode());
	}
	assert(!thr_exists(new_tid));
	assert(thr_exists(orig_tid));

	kill_thread(orig_tid);
}
END_TEST


/* try to create a thread with an invalid UTCB location, then try to delete
 * that thread ID, then try to create it again.
 */
START_TEST(create_with_invalid_utcb)
{
	L4_KernelInterfacePage_t *kip = L4_GetKernelInterface();
	plan_tests(8);

	/* get a known unused TID. */
	L4_ThreadId_t dest_tid = xstart_thread(&exit_thread, NULL);
	L4_Word_t old_utcb = L4_LocalIdOf(dest_tid).raw & ~(L4_UtcbSize(kip) - 1);
	dest_tid = L4_GlobalIdOf(dest_tid);
	xjoin_thread(dest_tid);
	diag("dest_tid=%lu:%lu", L4_ThreadNo(dest_tid), L4_Version(dest_tid));

	ok1(!thr_exists(dest_tid));
	L4_Word_t res = L4_ThreadControl(dest_tid, L4_Myself(),
		L4_Myself(), L4_Pager(), (void *)0x666);
	ok(res != 1, "create w/ utcb=0x666 fails");
	ok(!thr_exists(dest_tid), "... and didn't create thread");
	res = L4_ThreadControl(dest_tid, L4_nilthread, L4_nilthread,
		L4_nilthread, (void *)-1);
	ok(res == 1, "... so delete looks idempotent");

	ok1(!thr_exists(dest_tid));
	res = L4_ThreadControl(dest_tid, L4_Myself(),
		L4_Myself(), L4_Pager(), (void *)old_utcb);
	if(!ok(res == 1, "create w/ valid utcb")) {
		diag("ec=%lu", L4_ErrorCode());
	}
	ok(thr_exists(dest_tid), "... created thread");
	res = L4_ThreadControl(dest_tid, L4_nilthread, L4_nilthread,
		L4_nilthread, (void *)-1);
	ok(res == 1, "... deleted thread");
}
END_TEST


/* create a thread with an UTCB location, the modify that same thread while
 * specifying the same UTCB location. this should have the same effect as
 * (void *)-1 for the latter.
 */
START_LOOP_TEST(modify_with_same_utcb, iter, 0, 1)
{
	const bool wildcard = !!(iter & 1);
	diag("wildcard=%s", btos(wildcard));
	plan_tests(1);

	L4_KernelInterfacePage_t *kip = L4_GetKernelInterface();
	L4_ThreadId_t dest_tid = xstart_thread(&exit_thread, NULL);
	void *utcb = wildcard ? (void *)-1
		: (void *)(L4_LocalIdOf(dest_tid).raw & ~(L4_UtcbSize(kip) - 1));
	diag("dest_tid[local]=%#lx, utcb=%p", L4_LocalIdOf(dest_tid).raw, utcb);
	/* switch scheduler to current thread. */
	L4_Word_t res = L4_ThreadControl(dest_tid, dest_tid, L4_Myself(),
		L4_nilthread, utcb), ec = L4_ErrorCode();
	if(!ok(res != 0, "scheduler-setting ThreadControl")) {
		diag("ErrorCode=%lu", ec);
	}
	xjoin_thread(dest_tid);
}
END_TEST


/* tcase "exregs" */

/* test that ExchangeRegisters which sets the halt bit causes the read-out
 * halt bit to change accordingly. very basic.
 */
START_TEST(halt_bit_smoke)
{
	plan_tests(3);

	L4_ThreadId_t target = xstart_thread(&receive_and_exit, NULL);
	L4_Sleep(A_SHORT_NAP);
	ok(!thr_is_halted(target), "thread starts out not halted");

	L4_Stop(target);
	ok(thr_is_halted(target), "thread H bit set after stop");
	L4_Start(target);
	ok(!thr_is_halted(target), "thread H bit clear after re-start");

	diag("cleaning up...");
	send_quit(target);
	xjoin_thread(target);
}
END_TEST


static void send_and_exit(void *param_ptr)
{
	L4_ThreadId_t dest = { .raw = (L4_Word_t)param_ptr };
	L4_LoadMR(0, 0);
	L4_MsgTag_t tag = L4_Send_Timeout(dest, TEST_IPC_DELAY);
	if(L4_IpcFailed(tag)) {
		diag("%s: send failed, ec=%#lx", __func__, L4_ErrorCode());
	}
	exit_thread("done");
}


/* test read-out of the S bit.
 *
 * TODO: also test in-transfer states where the helper is in its send phase,
 * passive and active.
 */
START_TEST(read_s_bit)
{
	plan_tests(4);
	L4_ThreadId_t target = xstart_thread(&send_and_exit,
		(void *)L4_Myself().raw);
	L4_Sleep(A_SHORT_NAP);

	L4_Word_t ss = get_schedstate(target), ec = L4_ErrorCode();
	if(!ok(ss == L4_SCHEDRESULT_PENDING_SEND,
		"thread schedstate is PENDING_SEND"))
	{
		diag("ss=%#lx, ec=%#lx", ss, ec);
	}

	ok1(!thr_in_recv(target));
	ok(thr_in_send(target), "thread S bit is set (before receive)");
	L4_MsgTag_t tag = L4_Receive_Timeout(target, TEST_IPC_DELAY);
	IPC_FAIL(tag);
	ok(!thr_in_send(target), "thread S bit is clear (after receive)");

	diag("cleaning up...");
	xjoin_thread(target);
}
END_TEST


/* test read-out of the R bit.
 * TODO: see todo of read_s_bit
 */
START_TEST(read_r_bit)
{
	plan_tests(4);
	L4_ThreadId_t target = xstart_thread(&receive_and_exit, NULL);
	L4_Sleep(A_SHORT_NAP);

	L4_Word_t ss = get_schedstate(target), ec = L4_ErrorCode();
	if(!ok(ss == L4_SCHEDRESULT_WAITING,
		"thread schedstate is WAITING"))
	{
		diag("ss=%#lx, ec=%#lx", ss, ec);
	}

	ok1(!thr_in_send(target));
	ok(thr_in_recv(target), "thread R bit is set (before send)");
	L4_LoadMR(0, 0);
	L4_MsgTag_t tag = L4_Send_Timeout(target, TEST_IPC_DELAY);
	IPC_FAIL(tag);
	ok(!thr_in_send(target), "thread R bit is clear (after send)");

	diag("cleaning up...");
	send_quit(target);
	xjoin_thread(target);
}
END_TEST


static void send_or_receive_sleeper(void *param_ptr)
{
	const L4_Time_t timeout = L4_TimePeriod(10 * 1000);
	bool sleep_in_recv = (bool)param_ptr;
	L4_ThreadId_t dummy;
	L4_MsgTag_t tag;
	if(sleep_in_recv) {
		tag = L4_Ipc(L4_nilthread, L4_Myself(),
			L4_Timeouts(L4_ZeroTime, timeout), &dummy);
	} else {
		tag = L4_Ipc(L4_Myself(), L4_nilthread,
			L4_Timeouts(timeout, L4_ZeroTime), &dummy);
	}
	exit_thread((void *)(L4_IpcFailed(tag) ? L4_ErrorCode() : 0));
}


/* test that when a send-phase sleep is aborted by ExchangeRegisters, the
 * abort-signifying error code is returned, and the same for a receive-phase
 * sleep.
 */
START_LOOP_TEST(abort_send_or_receive, iter, 0, 7)
{
	const bool sleep_in_recv = CHECK_FLAG(iter, 1),
		abort_send = CHECK_FLAG(iter, 2), abort_recv = CHECK_FLAG(iter, 4);
	diag("sleep_in_recv=%s, abort_send=%s, abort_recv=%s",
		btos(sleep_in_recv), btos(abort_send), btos(abort_recv));
	plan_tests(12);

	L4_ThreadId_t sleeper = xstart_thread(&send_or_receive_sleeper,
		(void *)sleep_in_recv);
	L4_Clock_t start_time = L4_SystemClock();
	L4_Sleep(L4_TimePeriod(2 * 1000));
	iff_ok1(sleep_in_recv, thr_in_recv(sleeper));
	iff_ok1(!sleep_in_recv, thr_in_send(sleeper));

	L4_Word_t ctl_out, dummy;
	L4_ThreadId_t dummy_tid, ltid = L4_ExchangeRegisters(sleeper,
		0x200 | (abort_send ? 0x004 : 0) | (abort_recv ? 0x002 : 0),
		0, 0, 0, 0, L4_nilthread,
		&ctl_out, &dummy, &dummy, &dummy, &dummy, &dummy_tid);
	ok(L4_SameThreads(ltid, sleeper) && L4_IsLocalId(ltid),
		"exregs return value is valid");
	/* additionally test that the ctl_out bits reflect the pre-interrupt
	 * state.
	 */
	imply_ok1(!sleep_in_recv,
		CHECK_FLAG(ctl_out, 0x004) && !CHECK_FLAG(ctl_out, 0x002));
	imply_ok1(sleep_in_recv,
		CHECK_FLAG(ctl_out, 0x002) && !CHECK_FLAG(ctl_out, 0x004));

	void *retptr = xjoin_thread(sleeper);
	L4_Clock_t end_time = L4_SystemClock();
	int diff = end_time.raw - start_time.raw;
	L4_Word_t ec = (L4_Word_t)retptr;
	const bool timeout = (ec & ~1) == 2, aborted = (ec & ~1) == 6;
	diag("diff=%d, ec=%lu, timeout=%s, aborted=%s", diff, ec,
		btos(timeout), btos(aborted));
	ok1(ec != 0);
	imply_ok1(aborted, diff < 10000);
	imply_ok1(timeout, diff >= 10000);
	iff_ok1(sleep_in_recv && abort_recv, ec == 7);
	iff_ok1(!sleep_in_recv && abort_send, ec == 6);
	imply_ok(!abort_recv && !abort_send, timeout, "timeout when no abort");
	imply_ok((sleep_in_recv && !abort_recv) || (!sleep_in_recv && !abort_send),
		timeout, "timeout when abort doesn't match");
}
END_TEST


/* actually tests whether a thread is dead, or inactive. these states are
 * entered when e.g. a thread raises an exception but the exception handler
 * thread is either not set or cannot be found.
 */
static bool is_halted(L4_ThreadId_t tid) {
	L4_Word_t s = get_schedstate(tid);
	return s == L4_SCHEDRESULT_DEAD || s == L4_SCHEDRESULT_INACTIVE;
}


/* TODO: check ExchangeRegisters error with fail_if(), report ErrorCode if
 * that happens, use this instead of L4_Stop(), L4_Start() in the exregs tcase
 */
bool set_h_bit(L4_ThreadId_t tid, bool value)
{
	L4_Word_t dummy;
	L4_ThreadId_t dummy_tid, ret = L4_ExchangeRegisters(tid,
		(value ? 1 : 0) | 0x100, 0, 0, 0, 0, L4_nilthread,
		&dummy, &dummy, &dummy, &dummy, &dummy, &dummy_tid);
	return L4_IsNilThread(ret);
}


static void receive_and_die_fn(void *param UNUSED)
{
	L4_ThreadId_t tid;
	L4_MsgTag_t tag = L4_Wait(&tid);
	fail_if(L4_IpcFailed(tag), "%s: ec=%#lx", __func__, L4_ErrorCode());
	L4_ThreadId_t exh_tid = L4_nilthread;
	if(L4_UntypedWords(tag) > 0) L4_StoreMR(1, &exh_tid.raw);

	L4_LoadMR(0, 0);
	L4_Reply(tid);

	diag("popping exception to %lu:%lu",
		L4_ThreadNo(exh_tid), L4_Version(exh_tid));
	L4_Set_ExceptionHandler(exh_tid);
	asm volatile ("int $1");

	diag("exh helper thread exiting");
}


/* self-test on is_halted(). */
START_TEST(halt_pred_test)
{
	plan_tests(2);

	L4_ThreadId_t oth = xstart_thread(&receive_and_die_fn, NULL);
	ok1(!is_halted(oth));

	L4_LoadMR(0, (L4_MsgTag_t){ .X.u = 1 }.raw);
	L4_LoadMR(1, L4_nilthread.raw);
	L4_MsgTag_t tag = L4_Call(oth);
	IPC_FAIL(tag);
	L4_Sleep(L4_TimePeriod(100));
	ok1(is_halted(oth));

	kill_thread(oth);
}
END_TEST


static void fault_to_given_pager_fn(void *param UNUSED)
{
	const size_t mem_size = 8192;
	uint8_t *memory = aligned_alloc(PAGE_SIZE, mem_size);
	memset(memory, 0, mem_size);
	diag("test memory is at %p", memory);

	L4_ThreadId_t sender;
	L4_MsgTag_t tag = L4_Wait(&sender);
	fail_if(L4_IpcFailed(tag), "ec=%#lx", L4_ErrorCode());
	fail_if(L4_UntypedWords(tag) < 1);
	L4_ThreadId_t pg;
	L4_StoreMR(1, &pg.raw);
	L4_LoadMR(0, 0);
	L4_Reply(sender);
	diag("faulting to pager %lu:%lu", L4_ThreadNo(pg), L4_Version(pg));

	L4_Fpage_t page = L4_Fpage((L4_Word_t)memory, mem_size);
	L4_Set_Rights(&page, L4_FullyAccessible);
#if 0
	diag("flushing %#lx:%#lx (fpage=%#lx)",
		L4_Address(page), L4_Size(page), page.raw);
#endif
	L4_FlushFpage(page);
	L4_ThreadId_t old_pager = L4_Pager();
	L4_Set_Pager(pg);
	memset(memory, 1, mem_size);
	L4_Set_Pager(old_pager);

	diag("fault test thread exiting");
	free(memory);
}


/* tcase "tqe" */

/* test that deleting and modifying forms of ThreadControl work on threads
 * that've not been rescheduled with a new total_quantum. this test's
 * self-verification overlaps somewhat with related tests in sched_suite.
 *
 * variables:
 *   - short_tq: whether the total_quantum is too short for the spin to
 *     complete in one segment
 *   - op_rename: whether to modify the thread's version bits
 *   - do_halt: halt thread before calling threadctl
 *   - halt_before_ipc: halt thread in passive TQE send
 *   - halt_and_abort: not just halt, but also abort IPC
 *   - skip_ipc: don't receive IPC from thread at all
 *
 * TODO:
 *   - [v1] test motion between spaces as well, though that's well fancy
 */
START_LOOP_TEST(tqe_mod_del_threadctl, iter, 0, 63)
{
	const bool short_tq = CHECK_FLAG(iter, 1),
		op_rename = CHECK_FLAG(iter, 2), do_halt = CHECK_FLAG(iter, 4),
		halt_before_ipc = CHECK_FLAG(iter, 8),
		halt_and_abort = CHECK_FLAG(iter, 16),
		skip_ipc = CHECK_FLAG(iter, 32);
	const int spin_ms = 20, my_pri = find_own_priority();
	const L4_Time_t ts_len = L4_TimePeriod(spin_ms * 1000 / 2),
		totq_len = L4_TimePeriod((short_tq ? spin_ms * 2 / 3 : spin_ms * 2) * 1000),
		ipc_len = L4_TimePeriod(spin_ms * 3 * 1000);
	diag("short_tq=%s, op_rename=%s, do_halt=%s, halt_before_ipc=%s",
		btos(short_tq), btos(op_rename), btos(do_halt),
		btos(halt_before_ipc));
	diag("  halt_and_abort=%s, skip_ipc=%s",
		btos(halt_and_abort), btos(skip_ipc));
	diag("spin_ms=%d, my_pri=%d, ts_len=%luµs, totq_len=%luµs",
		spin_ms, my_pri, (unsigned long)time_in_us(ts_len),
		(unsigned long)time_in_us(totq_len));
	if(halt_and_abort && !halt_before_ipc) {
		plan_skip_all("halt_and_abort has no effect");
		goto end;
	}
	if(halt_before_ipc && (!do_halt || !short_tq)) {
		plan_skip_all("halt_before_ipc has no effect");
		goto end;
	}
	plan_tests(8);

	L4_ThreadId_t spinner = start_spinner(my_pri - 2, spin_ms,
		ts_len, totq_len, false, false, false);
	spinner = L4_GlobalIdOf(spinner);

	if(do_halt && halt_before_ipc) {
		/* let it spin into its TQE state first. */
		L4_Sleep(L4_TimePeriod(spin_ms * 1000 + 2000));
		if(halt_and_abort) L4_AbortIpc_and_stop(spinner);
		else L4_Stop(spinner);
	}

	/* wait for tqe msg, if any. */
	bool got_msg = false, got_done = false;
	while(!skip_ipc) {
		L4_ThreadId_t sender;
		L4_MsgTag_t tag = L4_Wait_Timeout(ipc_len, &sender);
		if(L4_IpcFailed(tag)) {
			if(L4_ErrorCode() == 3) break;
			diag("tqe msg wait failed, ec=%#lx", L4_ErrorCode());
		} else if(L4_Label(tag) == 0xffd0) {
			got_msg = true;
		} else if(tag.raw == 0 && L4_SameThreads(sender, spinner)) {
			got_done = true;
			break;
		}
	}

	if(do_halt && !halt_before_ipc) {
		if(halt_and_abort) L4_AbortIpc_and_stop(spinner);
		else L4_Stop(spinner);
	}
	diag("got_msg=%s, got_done=%s", btos(got_msg), btos(got_done));

	/* experiment validation */
	skip_start(skip_ipc, 3, "skipped the IPC section") {
		imply_ok1(!short_tq, !got_msg);
		imply_ok1(got_msg, short_tq);
		iff_ok1(got_done, !short_tq);
	} skip_end;

	skip_start(!op_rename || !got_msg, 2,
		"not set to modify, or no TQE state")
	{
		/* change its version bits just to cause a panic */
		L4_ThreadId_t new_tid = L4_GlobalId(L4_ThreadNo(spinner),
			(L4_Version(spinner) ^ 0x1200) | 0x120);
		fail_unless(new_tid.raw != spinner.raw);
		L4_Word_t res = L4_ThreadControl(new_tid, spinner, L4_nilthread,
			L4_nilthread, (void *)-1);
		fail_if(res == 0, "threadctl ec=%#lx", L4_ErrorCode());
		ok1(thr_exists(new_tid));
		ok1(!thr_exists(spinner));

		/* change them back. */
		res = L4_ThreadControl(spinner, new_tid, L4_nilthread,
			L4_nilthread, (void *)-1);
		fail_if(res == 0, "threadctl ec=%#lx", L4_ErrorCode());
		fail_unless(thr_exists(spinner) && !thr_exists(new_tid));
	} skip_end;
	imply_ok1(got_msg, thr_exists(spinner));

	/* analysis */
	L4_Word_t ec = 0;
	void *rptr = join_thread_long(spinner, ipc_len, &ec);
	bool join_ok = rptr != NULL || ec == 0;
	imply_ok1(!skip_ipc && join_ok, got_done);
	if(!join_ok) {
		diag("killing spinner thread");
		kill_thread(spinner);
	}

	ok1(!thr_exists(spinner));

end:
	;
}
END_TEST


/* test about aborting a spinner's TQE IPC before it completes. the expected
 * behaviour is that the passive send-phase is thrown away, the thread enters
 * TQ=0 limbo, and comes out of it once reset.
 *
 * variables:
 *   - interrupt the passive send, or not (for validation)
 *   - give a new total quantum, or not (adds a no-op case)
 *   - resume interrupted thread before quantum reset, or after
 */
START_LOOP_TEST(tqe_abort_ipc, iter, 0, 7)
{
	const int spin_ms = 20, my_pri = find_own_priority();
	const L4_Time_t ts_len = L4_TimePeriod(spin_ms * 1000 / 2),
		totq_len = L4_TimePeriod(spin_ms * 1000 * 2 / 3),
		ipc_len = L4_TimePeriod(spin_ms * 3 * 1000);
	const bool brk_send = CHECK_FLAG(iter, 1), add_tq = CHECK_FLAG(iter, 2),
		resume_late = CHECK_FLAG(iter, 4);
	diag("brk_send=%s, add_tq=%s, resume_late=%s",
		btos(brk_send), btos(add_tq), btos(resume_late));
	diag("spin_ms=%d, my_pri=%d", spin_ms, my_pri);
	if(resume_late && (!add_tq || !brk_send)) {
		plan_skip_all("redundant iteration");
		goto end;
	}
	plan_tests(4);

	L4_ThreadId_t spinner = start_spinner(my_pri - 2, spin_ms,
		ts_len, totq_len, false, false, false);

	/* wait 'til it pops. */
	L4_Sleep(L4_TimePeriod(time_in_us(totq_len) + 5000));
	L4_Word_t timectl = 0,
		st = L4_Schedule(spinner, ~0ul, ~0ul, ~0ul, ~0ul, &timectl);
	L4_Time_t rem_tq = { .raw = timectl & 0xffff };

	/* confirm test state. */
	if(!ok1(time_in_us(rem_tq) == 0)) {
		diag("st=%#lx, timectl=%#lx [rem_ts=%uµs, rem_tq=%uµs]", st, timectl,
			(unsigned)time_in_us((L4_Time_t){ .raw = timectl >> 16 }),
			rem_tq.raw);
	}

	if(brk_send) {
		/* break & re-enter */
		L4_AbortIpc_and_stop(spinner);
		if(!resume_late) L4_Start(spinner);
		if(add_tq) {
			diag("setting tq=∞ after abort, %s resume",
				resume_late ? "before" : "after");
			L4_Set_Timeslice(spinner, ts_len, L4_Never);
		}
		if(resume_late) L4_Start(spinner);
	}

	/* wait for tqe msg, if any. */
	bool got_msg = false, got_done = false;
	for(;;) {
		L4_ThreadId_t sender;
		L4_MsgTag_t tag = L4_Wait_Timeout(ipc_len, &sender);
		if(L4_IpcFailed(tag)) {
			if(L4_ErrorCode() == 3) break;
			diag("tqe msg wait failed, ec=%#lx", L4_ErrorCode());
			break;
		} else if(L4_Label(tag) == 0xffd0) {
			got_msg = true;
			if(add_tq) {
				diag("setting tq=∞ on RPC");
				L4_Set_Timeslice(spinner, ts_len, L4_Never);
			}
		} else if(tag.raw == 0 && L4_SameThreads(sender, spinner)) {
			got_done = true;
			break;
		}
	}

	iff_ok1(got_msg, !brk_send);
	iff_ok1(got_done, add_tq);

	L4_Word_t ec = 0;
	void *retptr = join_thread_long(spinner, TEST_IPC_DELAY, &ec);
	bool join_ok = retptr != NULL || ec == 0;
	iff_ok1(add_tq, join_ok);
	if(!join_ok) {
		diag("retptr=%p, ec=%#lx", retptr, ec);
		kill_thread(spinner);
	}

end:
	;	/* label at end of compound statement, my eye */
}
END_TEST


START_TEST(halt_on_missing_pager)
{
	plan_tests(3);
	L4_ThreadId_t self = L4_Myself();
	diag("self=%lu:%lu", L4_ThreadNo(self), L4_Version(self));

	/* base case: fault to this thread. */
	L4_ThreadId_t oth = xstart_thread(&fault_to_given_pager_fn, NULL);
	L4_LoadMR(0, (L4_MsgTag_t){ .X.u = 1 }.raw);
	L4_LoadMR(1, self.raw);
	L4_MsgTag_t tag = L4_Call(oth);
	fail_if(L4_IpcFailed(tag));
	tag = L4_Receive_Timeout(oth, TEST_IPC_DELAY);
	fail_if(L4_IpcFailed(tag), "didn't receive fault, ec=%#lx", L4_ErrorCode());
	L4_Word_t faddr, fip;
	L4_StoreMR(1, &faddr);
	L4_StoreMR(2, &fip);
	ok(tag.X.label >> 4 == 0xffe, "got test fault");
	diag("got test fault: ip=%#lx, addr=%#lx", fip, faddr);
	/* forward it to our own pager. */
	L4_LoadMR(0, tag.raw);
	L4_LoadMR(1, faddr);
	L4_LoadMR(2, fip);
	tag = L4_Call(L4_Pager());
	fail_if(L4_IpcFailed(tag));
	L4_MapItem_t mi;
	L4_StoreMRs(1, 2, mi.raw);
	L4_Set_PagerOf(oth, L4_Pager());
	L4_LoadMR(0, (L4_MsgTag_t){ .X.t = 2 }.raw);
	L4_LoadMRs(1, 2, mi.raw);
	tag = L4_Reply(oth);
	fail_if(L4_IpcFailed(tag), "ec=%#lx", L4_ErrorCode());
	xjoin_thread(oth);

	/* test case: generate a fault while pager is L4_nilthread. this should
	 * halt the helper thread.
	 */
	oth = xstart_thread(&fault_to_given_pager_fn, NULL);
	L4_ThreadSwitch(oth);
	ok1(!is_halted(oth));
	L4_LoadMR(0, (L4_MsgTag_t){ .X.u = 1 }.raw);
	L4_LoadMR(1, L4_nilthread.raw);
	tag = L4_Call(oth);
	fail_if(L4_IpcFailed(tag));
	L4_ThreadSwitch(oth);
	L4_Sleep(L4_TimePeriod(1000));
	ok1(is_halted(oth));
	kill_thread(oth);
}
END_TEST


START_TEST(halt_on_missing_exh)
{
	plan_tests(3);

	/* base case: pop an exception to this thread. */
	L4_ThreadId_t oth = xstart_thread(&receive_and_die_fn, NULL);
	L4_LoadMR(0, (L4_MsgTag_t){ .X.u = 1 }.raw);
	L4_LoadMR(1, L4_Myself().raw);
	L4_MsgTag_t tag = L4_Call(oth);
	fail_if(L4_IpcFailed(tag));
	tag = L4_Receive(oth);
	L4_Word_t words[64];
	L4_StoreMRs(1, L4_UntypedWords(tag) + L4_TypedWords(tag), words);
	ok(tag.X.label >> 4 == 0xffb, "got test exception");
	diag("test exn has u=%lu, t=%lu",
		L4_UntypedWords(tag), L4_TypedWords(tag));
	/* reply as-is, bumping past INT $n (2 bytes) */
	words[0] += 2;
	L4_LoadMR(0, (L4_MsgTag_t){ .X.u = L4_UntypedWords(tag) }.raw);
	L4_LoadMRs(1, L4_UntypedWords(tag), words);
	tag = L4_Reply(oth);
	fail_if(L4_IpcFailed(tag), "ec=%#lx", L4_ErrorCode());
	xjoin_thread(oth);

	/* test case: generate an exception while handler is L4_nilthread. this
	 * should halt the helper thread.
	 */
	oth = xstart_thread(&receive_and_die_fn, NULL);
	L4_ThreadSwitch(oth);
	ok1(!is_halted(oth));
	L4_LoadMR(0, (L4_MsgTag_t){ .X.u = 1 }.raw);
	L4_LoadMR(1, L4_nilthread.raw);
	tag = L4_Call(oth);
	fail_if(L4_IpcFailed(tag));
	L4_ThreadSwitch(oth);
	ok1(is_halted(oth));
	kill_thread(oth);
}
END_TEST


/* TODO: start collecting these utility thread functions somewhere. likely
 * there's a bunch of duplication between test suites.
 */
void receive_and_exit(void *param UNUSED)
{
	L4_ThreadId_t sender;
	L4_MsgTag_t tag = L4_Wait(&sender);
	if(L4_IpcFailed(tag)) {
		diag("%s: IPC failed, ec=%lu", __func__, L4_ErrorCode());
		exit_thread("IPC failed!");
	} else {
		sender = L4_GlobalIdOf(sender);
		diag("%s: got IPC from %lu:%lu (u=%lu, t=%lu)", __func__,
			L4_ThreadNo(sender), L4_Version(sender),
			L4_UntypedWords(tag), L4_TypedWords(tag));
		exit_thread("ok!");
	}
}


/* check that faulting threads halt when their pager is deleted after the pf
 * message has been delivered.
 *
 * variables:
 *   - [delayed_measure] whether the measuring thread sleeps before testing
 *     the subject's status. this is required to cover both passive receive and
 *     pending passive receive. (this serves to exclude the case where a thread
 *     ID is both deleted and created while the subject is in pending receive.)
 */
START_LOOP_TEST(halt_on_lost_pager, iter, 0, 1)
{
	const bool delayed_measure = CHECK_FLAG(iter, 1);
	diag("delayed_measure=%s", btos(delayed_measure));
	plan_tests(2);

	L4_ThreadId_t oth = xstart_thread(&fault_to_given_pager_fn, NULL),
		fake_pager = xstart_thread(&receive_and_exit, NULL);
	fail_if(L4_Set_Priority(oth, 2) == 0,
		"setpri(oth) failed, ec=%#lx", L4_ErrorCode());
	fail_if(L4_Set_Priority(fake_pager, 3) == 0,
		"setpri(fake_pager) failed, ec=%#lx", L4_ErrorCode());
	L4_LoadMR(0, (L4_MsgTag_t){ .X.u = 1 }.raw);
	L4_LoadMR(1, fake_pager.raw);
	L4_MsgTag_t tag = L4_Call(oth);
	IPC_FAIL(tag);
	L4_Word_t ec;
	void *res = join_thread_long(fake_pager, TEST_IPC_DELAY, &ec);
	fail_if(res == NULL || !streq(res, "ok!"),
		"join of fake pager failed, ec=%#lx", ec);
	ok1(!thr_exists(fake_pager));	/* validity precondition */
	if(delayed_measure) L4_Sleep(A_SHORT_NAP);
	if(!ok1(is_halted(oth))) {
		diag("schedstate=%lu", get_schedstate(oth));
	}

	kill_thread(oth);
}
END_TEST


/* same as halt_on_lost_pager, but for the ExceptionHandler. */
START_LOOP_TEST(halt_on_lost_exh, iter, 0, 1)
{
	const bool delayed_measure = CHECK_FLAG(iter, 1);
	diag("delayed_measure=%s", btos(delayed_measure));
	plan_tests(2);

	L4_ThreadId_t oth = xstart_thread(&receive_and_die_fn, NULL),
		fake_exh = xstart_thread(&receive_and_exit, NULL);
	fail_if(L4_Set_Priority(oth, 2) == 0,
		"setpri(oth) failed, ec=%#lx", L4_ErrorCode());
	fail_if(L4_Set_Priority(fake_exh, 3) == 0,
		"setpri(fake_exh) failed, ec=%#lx", L4_ErrorCode());
	L4_LoadMR(0, (L4_MsgTag_t){ .X.u = 1 }.raw);
	L4_LoadMR(1, fake_exh.raw);
	L4_MsgTag_t tag = L4_Call(oth);
	IPC_FAIL(tag);
	L4_Word_t ec;
	void *res = join_thread_long(fake_exh, TEST_IPC_DELAY, &ec);
	fail_if(res == NULL || !streq(res, "ok!"),
		"join of fake exceptionhandler failed, ec=%#lx", ec);
	ok1(!thr_exists(fake_exh));	/* validity precondition */
	if(delayed_measure) L4_Sleep(A_SHORT_NAP);
	if(!ok1(is_halted(oth))) {
		diag("schedstate=%lu", get_schedstate(oth));
	}

	kill_thread(oth);
}
END_TEST


enum ipc_mode {
	SEND_ONLY = 0,
	RECV_ONLY = 1,
	CALL = 2,
};


static void ipc_sender_fn(L4_ThreadId_t parent_tid)
{
	L4_ThreadId_t target;
	L4_MsgTag_t tag = L4_Receive_Timeout(parent_tid, TEST_IPC_DELAY);
	if(L4_IpcFailed(tag)) {
		diag("%s: initial wait failed, ec=%#lx", __func__, L4_ErrorCode());
		return;
	}
	assert(L4_UntypedWords(tag) >= 2);
	L4_StoreMR(1, &target.raw);
	L4_Word_t raw_mode; L4_StoreMR(2, &raw_mode);
	enum ipc_mode mode = raw_mode;
	L4_Word_t ec;
	switch(mode) {
		case SEND_ONLY:
			L4_LoadMR(0, 0);
			tag = L4_Send(target);
			break;
		case RECV_ONLY:
			tag = L4_Receive(target);
			break;
		case CALL:
			L4_LoadMR(0, 0);
			tag = L4_Call(target);
			break;
		default:
			diag("%s: invalid mode %d", __func__, (int)mode);
			return;
	}
	if(L4_IpcFailed(tag)) ec = L4_ErrorCode(); else ec = 0;
	L4_LoadMR(0, (L4_MsgTag_t){ .X.u = 2 }.raw);
	L4_LoadMR(1, tag.raw);
	L4_LoadMR(2, ec);
	tag = L4_Send_Timeout(parent_tid, TEST_IPC_DELAY);
	if(L4_IpcFailed(tag)) {
		diag("%s: reply failed, ec=%#lx", __func__, L4_ErrorCode());
	}
}


struct ipc_peer_param {
	bool succeed;
	enum ipc_mode mode;
	L4_ThreadId_t oth;
};


static void ipc_peer_fn(void *param_ptr)
{
	struct ipc_peer_param *param = param_ptr;
	bool succeed = param->succeed;
	enum ipc_mode mode = param->mode;
	L4_ThreadId_t oth = param->oth;
	free(param);

	L4_ThreadId_t sender;
	L4_MsgTag_t tag;
	switch(mode) {
		case CALL:
			tag = L4_Wait_Timeout(TEST_IPC_DELAY, &sender);
			IPC_FAIL(tag);
			if(succeed) {
				L4_LoadMR(0, 0);
				L4_Reply(sender);
			}
			break;
		case SEND_ONLY:
			if(succeed) {
				tag = L4_Receive_Timeout(oth, TEST_IPC_DELAY);
				IPC_FAIL(tag);
			}
			break;
		case RECV_ONLY:
			if(succeed) {
				L4_LoadMR(0, 0);
				tag = L4_Send_Timeout(oth, TEST_IPC_DELAY);
				IPC_FAIL(tag);
			}
			break;
	}
	if(!succeed) {
		/* ensure measurement before exit */
		L4_Sleep(A_SHORT_NAP);
	}
}


/* there are two kinds of peer loss: outright deletion and modification of the
 * version field. both should pop an error in the helper thread, and their
 * absence should return successfully.
 *
 * this test goes over three paths to the error condition: first, from the
 * send-wait status; second, from the recv-wait status, and third, from a
 * recv-wait (or r_recv) status that was transitioned into from a send-wait.
 *
 * so there are nine iterations: {none, deletion, modification} * {send, recv,
 * call}.
 */
START_LOOP_TEST(err_on_lost_peer, iter, 0, 8)
{
	const int loss_mode = iter % 3;
	const enum ipc_mode ipc_mode = iter / 3;

	const char *im_str = NULL;
	switch(ipc_mode) {
		case SEND_ONLY: im_str = "send_only"; break;
		case RECV_ONLY: im_str = "recv_only"; break;
		case CALL: im_str = "call"; break;
		default: fail_if(true, "oooooh!");
	}
	diag("loss_mode=%d, ipc_mode=%s", loss_mode, im_str);

	plan_tests(3);

	L4_ThreadId_t parent = L4_Myself(), oth;
	int oth_pid = fork_tid(&oth);
	if(oth_pid == 0) {
		ipc_sender_fn(parent);
		diag("client exiting");
		exit(0);
	}

	struct ipc_peer_param *param = malloc(sizeof(*param));
	param->succeed = loss_mode == 0;
	param->mode = ipc_mode;
	param->oth = oth;
	L4_ThreadId_t peer_tid = xstart_thread(&ipc_peer_fn, param);
	diag("oth_pid=%d", oth_pid);
	const L4_ThreadId_t old_peer_tid = peer_tid;
	diag("oth=%lu:%lu, peer_tid=%lu:%lu",
		L4_ThreadNo(oth), L4_Version(oth),
		L4_ThreadNo(peer_tid), L4_Version(peer_tid));

	L4_LoadMR(0, (L4_MsgTag_t){ .X.u = 2 }.raw);
	L4_LoadMR(1, peer_tid.raw);
	L4_LoadMR(2, ipc_mode);
	L4_MsgTag_t tag = L4_Send(oth);
	IPC_FAIL(tag);
	if(ipc_mode == CALL) {
		/* ensure that sender's call gets to the receive phase */
		L4_Sleep(A_SHORT_NAP);
	}
	switch(loss_mode) {
		case 2: {
			/* swap out its version bits. this kills the cat. */
			L4_ThreadId_t new_tid = L4_GlobalId(L4_ThreadNo(peer_tid),
				(L4_Version(peer_tid) + 123) | 13);
			assert(new_tid.raw != peer_tid.raw);
			L4_Word_t res = L4_ThreadControl(new_tid, peer_tid,
				L4_nilthread, L4_nilthread, (void *)-1);
			fail_unless(res == 1,
				"threadcontrol failed, res=%lu, ec=%#lx",
				res, L4_ErrorCode());
			fail_if(thr_exists(peer_tid));
			peer_tid = new_tid;
			break;
		}

		case 1: {
			/* deletion. */
			kill_thread(peer_tid);
			fail_if(thr_exists(peer_tid));
			peer_tid = L4_nilthread;
			break;
		}

		case 0:
			/* nothing happens. */
			break;

		default:
			fail_if(true, "invalid loss_mode=%d", loss_mode);
	}

	tag = L4_Receive(oth);
	IPC_FAIL(tag);
	assert(L4_UntypedWords(tag) >= 2);
	L4_MsgTag_t oth_tag;
	L4_Word_t oth_ec;
	L4_StoreMR(1, &oth_tag.raw);
	L4_StoreMR(2, &oth_ec);
	diag("oth_tag.succeeded=%s, oth_ec=%#lx",
		btos(L4_IpcSucceeded(oth_tag)), oth_ec);

	iff_ok1(L4_IpcSucceeded(oth_tag), loss_mode == 0);
	iff_ok1((oth_ec & 1) == 1, ipc_mode != SEND_ONLY && loss_mode > 0);
	imply_ok1(loss_mode > 0, (oth_ec >> 1) == 2);

	if(loss_mode == 2) {
		/* reinstate the peer & let it terminate. */
		diag("reinstating v%lu (new was v%lu)",
			L4_Version(old_peer_tid), L4_Version(peer_tid));
		L4_Word_t res = L4_ThreadControl(old_peer_tid, peer_tid,
			L4_nilthread, L4_nilthread, (void *)-1);
		fail_if(res != 1, "reinstating tc failed, res=%lu, ec=%#lx",
			res, L4_ErrorCode());
		peer_tid = old_peer_tid;
		L4_Start(peer_tid);
	}

	int dead;
	do {
		int st;
		dead = wait(&st);
		if(dead != oth_pid) {
			diag("dead=%d, oth_pid=%d (?)", dead, oth_pid);
		}
	} while(dead != oth_pid);

	if(!L4_IsNilThread(peer_tid)) xjoin_thread(peer_tid);
}
END_TEST


/* create a non-activated thread and overwrite its version bits. */
START_TEST(tid_stomp)
{
	plan_tests(1);
	const int tno = 0xfade;

	const L4_ThreadId_t first_tid = L4_GlobalId(tno, 1234);
	assert((L4_Version(first_tid) & 0x3f) != 0);
	mk_thread(first_tid, false);

	L4_ThreadId_t after_tid = L4_GlobalId(tno, L4_Version(first_tid) ^ 0x3f);
	L4_Word_t res = L4_ThreadControl(after_tid, L4_Myself(),
		L4_nilthread, L4_nilthread, (void *)-1);
	if(!ok1(res == 1)) diag("ec=%#lx", L4_ErrorCode());

	del_thread(after_tid);
}
END_TEST


static void pop_interrupt_to(L4_ThreadId_t dest)
{
	L4_ThreadId_t old_exh = L4_ExceptionHandler();
	L4_Set_ExceptionHandler(dest);
	asm volatile ("int $99" ::: "memory");
	L4_Set_ExceptionHandler(old_exh);
}


static void flip_thread_version(
	L4_ThreadId_t sender, L4_MsgTag_t frame_tag, L4_Word_t *frame)
{
	sender = L4_GlobalIdOf(sender);
	L4_ThreadId_t new_tid = L4_GlobalId(L4_ThreadNo(sender),
		L4_Version(sender) ^ 0x3f00);
	assert((L4_Version(new_tid) & 0x3f) != 0);
	L4_Word_t res = L4_ThreadControl(new_tid, L4_Myself(), L4_nilthread,
		L4_nilthread, (void *)-1);
	if(res != 1) {
		diag("renaming threadctl failed, ec=%lu", L4_ErrorCode());
		return;
	}
	diag("starting new_tid=%lu:%lu",
		L4_ThreadNo(new_tid), L4_Version(new_tid));
	void *stk = aligned_alloc(PAGE_SIZE, PAGE_SIZE);
	L4_Word_t *sp = (L4_Word_t *)(stk + PAGE_SIZE - 16 + 4);
	*(--sp) = L4_Myself().raw;
	*(--sp) = 0xabadc0de;
	L4_Start_SpIp(new_tid, (L4_Word_t)sp, (L4_Word_t)&pop_interrupt_to);
	L4_MsgTag_t tag = L4_Receive_Timeout(new_tid, L4_TimePeriod(2000));
	if(L4_IpcFailed(tag)) {
		diag("%s: can't get exception message, ec=%lu",
			__func__, L4_ErrorCode());
		return;
	}
	/* load the old frame and pass it down. */
	L4_LoadMR(0, (L4_MsgTag_t){ .X.u = frame_tag.X.u }.raw);
	L4_LoadMRs(1, frame_tag.X.u, frame);
	tag = L4_Reply(new_tid);
	if(L4_IpcFailed(tag)) {
		diag("exn reply failed, ec=%lu", L4_ErrorCode());
		return;
	}

	free(stk);
}


static void rename_helper_fn(void *param_ptr)
{
	bool running = true;
	while(running) {
		L4_ThreadId_t sender;
		L4_MsgTag_t tag = L4_Wait(&sender);
		for(;;) {
			if(L4_IpcFailed(tag)) {
				diag("%s: ipc failed, ec=%#lx", __func__, L4_ErrorCode());
				break;
			}

			bool reply;
			if((L4_Label(tag) & 0xfff0) == 0xffb0) {
				L4_Word_t frame[64];
				L4_StoreMRs(1, tag.X.u, frame);
				frame[0] += 2;	/* skip over int $nn */
				flip_thread_version(sender, tag, frame);
				reply = false;
			} else if(L4_Label(tag) == QUIT_LABEL) {
				running = false;
				break;
			} else {
				diag("%s: sender=%lu:%lu, tag=%#lx", __func__,
					L4_ThreadNo(sender), L4_Version(sender), tag.raw);
				reply = false;
			}
			if(!reply) break;

			tag = L4_ReplyWait(sender, &sender);
		}
	}
}


/* tid_stomp ][: the stompening */
START_TEST(version_switcheroo)
{
	plan_tests(3);

	L4_ThreadId_t first = L4_MyGlobalId();
	diag("first=%lu:%lu", L4_ThreadNo(first), L4_Version(first));

	L4_ThreadId_t helper = xstart_thread(&rename_helper_fn, NULL);
	diag("helper=%lu:%lu", L4_ThreadNo(first), L4_Version(first));
	L4_Sleep(L4_TimePeriod(2000));
	pop_interrupt_to(helper);

	L4_ThreadId_t second = L4_MyGlobalId();
	diag("second=%lu:%lu", L4_ThreadNo(second), L4_Version(second));
	ok1(!L4_SameThreads(first, second));

	pop_interrupt_to(helper);
	L4_ThreadId_t third = L4_MyGlobalId();
	diag("third=%lu:%lu", L4_ThreadNo(third), L4_Version(third));
	ok1(!L4_SameThreads(second, third));
	ok1(L4_SameThreads(third, first));

	send_quit(helper);
	xjoin_thread(helper);
}
END_TEST


START_TEST(reuse_utcb_pages)
{
	plan_tests(4);

	L4_KernelInterfacePage_t *kip = L4_GetKernelInterface();
	const int utcb_size = L4_UtcbSize(kip),
		n_threads = (4 * PAGE_SIZE) / utcb_size;
	diag("utcb_size=%d, n_threads=%d", utcb_size, n_threads);

	/* start a bunch of threads, then join them. this should cause recycling
	 * of at least two UTCB pages.
	 */
	L4_ThreadId_t tids[n_threads];
	int not_started = 0;
	for(int i=0; i < n_threads; i++) {
		tids[i] = start_thread(&exit_thread, &tids[i]);
		if(L4_IsNilThread(tids[i])) {
			diag("i=%d: couldn't start thread", i);
			not_started++;
			continue;
		}
	}

	int not_joined = 0;
	for(int i=0; i < n_threads; i++) {
		if(L4_IsNilThread(tids[i])) continue;
		L4_Word_t ec = 0;
		void *ptr = join_thread_long(tids[i], L4_TimePeriod(3 * 1000), &ec);
		if(ptr != &tids[i]) {
			diag("join_thread_long: ptr=%p, ec=%#lx", ptr, ec);
			not_joined++;
		}
	}

	ok(not_joined == 0, "all setup threads joined");
	ok(not_started < n_threads / 3, "enough setup threads created");

	/* now do it again. presumably the previously-released UTCB pages will be
	 * reused.
	 */
	not_started = 0;
	for(int i=0; i < n_threads; i++) {
		tids[i] = start_thread(&exit_thread, &tids[i]);
		if(L4_IsNilThread(tids[i])) {
			diag("i=%d: couldn't start thread", i);
			not_started++;
			continue;
		}
	}

	not_joined = 0;
	for(int i=0; i < n_threads; i++) {
		if(L4_IsNilThread(tids[i])) continue;
		L4_Word_t ec = 0;
		void *ptr = join_thread_long(tids[i], L4_TimePeriod(3 * 1000), &ec);
		if(ptr != &tids[i]) {
			diag("join_thread_long: ptr=%p, ec=%#lx", ptr, ec);
			not_joined++;
		}
	}

	ok(not_joined == 0, "all re-use threads joined");
	ok(not_started < n_threads / 3, "enough re-use threads created");
}
END_TEST


static Suite *thread_suite(void)
{
	Suite *s = suite_create("thread");

	{
		TCase *tc = tcase_create("self");
		tcase_set_fork(tc, false);	/* for consistency */
		tcase_add_test(tc, test_thr_exists);
		tcase_add_test(tc, test_as_exists);
		tcase_add_test(tc, halt_pred_test);
		suite_add_tcase(s, tc);
	}

	/* tests on the ThreadControl system call. */
	{
		TCase *tc = tcase_create("ctl");
		tcase_set_fork(tc, false);	/* needs wheel bit */
		tcase_add_test(tc, threadctl_basic);
		tcase_add_test(tc, privilege);
		tcase_add_test(tc, thread_id_validity);
		tcase_add_test(tc, scheduler_id_validity);
		tcase_add_test(tc, spacespec_validity);
		tcase_add_test(tc, relocate_utcb);
		tcase_add_test(tc, deletion_by_threadno);
		tcase_add_test(tc, suicide);
		tcase_add_test(tc, modify_self);
		/* TODO: add modify_other */
		tcase_add_test(tc, version_stomp);
		tcase_add_test(tc, create_with_invalid_utcb);
		tcase_add_test(tc, modify_with_same_utcb);
		suite_add_tcase(s, tc);
	}

	/* tests on the ExchangeRegisters syscall. */
	{
		TCase *tc = tcase_create("exregs");
		tcase_set_fork(tc, false);
		tcase_add_test(tc, halt_bit_smoke);
		tcase_add_test(tc, read_s_bit);
		tcase_add_test(tc, read_r_bit);
		tcase_add_test(tc, abort_send_or_receive);
		suite_add_tcase(s, tc);
	}

	/* API cases when the thread's total_quantum has reached zero (i.e. been
	 * exhausted), but it hasn't yet been assigned a new one.
	 */
	{
		TCase *tc = tcase_create("tqe");
		tcase_set_fork(tc, false);
		tcase_add_test(tc, tqe_mod_del_threadctl);
		/* TODO: test ExchangeRegisters to halt/resume a thread */
		tcase_add_test(tc, tqe_abort_ipc);
		suite_add_tcase(s, tc);
	}

	/* tests on threads changing state. */
	{
		TCase *tc = tcase_create("state");
		tcase_set_fork(tc, false);	/* needs Schedule, ThreadControl */
		tcase_add_test(tc, halt_on_missing_pager);
		tcase_add_test(tc, halt_on_missing_exh);
		tcase_add_test(tc, halt_on_lost_pager);
		tcase_add_test(tc, halt_on_lost_exh);
		tcase_add_test(tc, err_on_lost_peer);
		suite_add_tcase(s, tc);
	}

	/* cases that would panic the microkernel. generally through asserts. */
	{
		TCase *tc = tcase_create("panic");
		tcase_set_fork(tc, false);
		tcase_add_test(tc, tid_stomp);
		tcase_add_test(tc, version_switcheroo);
		suite_add_tcase(s, tc);
	}

	/* tests suggested by implementation detail. */
	{
		TCase *tc = tcase_create("impl");
		tcase_add_test(tc, reuse_utcb_pages);
		suite_add_tcase(s, tc);
	}

	return s;
}


static const struct suite_spec s = { &thread_suite, 99 };
AUTODATA(testsuites, &s);
