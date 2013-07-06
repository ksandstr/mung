
/* unit tests concerning the ThreadControl and ExchangeRegister system calls,
 * and TCR access.
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdarg.h>
#include <string.h>
#include <ccan/compiler/compiler.h>

#include <l4/types.h>
#include <l4/kip.h>
#include <l4/ipc.h>
#include <l4/syscall.h>

#include <ukernel/util.h>

#include "defs.h"
#include "test.h"


/* TODO: move these into test.h */
/* >implying implications */
#define imply_ok1(left, right) \
	ok(!(left) || (right), "%s --> %s", #left, #right)

#define imply_ok(left, right, test, ...) \
	ok(!(left) || (right), test, ##__VA_ARGS__)

/* NOTE: alias for left == right, printed as "iff". useful, but not used right
 * now.
 */
#define iff_ok1(left, right) \
	ok((left) == (right), "%s iff %s", #left, #right)


/* TODO: make a self-test for this, and thr_exists()! */
static bool as_exists(L4_ThreadId_t space_id)
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


static bool thr_exists(L4_ThreadId_t tid) {
	/* funnily enough, as all threads exist within a space, ... */
	return as_exists(L4_GlobalIdOf(tid));
}


/* TODO: move this into util.c or somewhere */
static void del_thread(L4_ThreadId_t tid)
{
	/* "i liked the part where he said L4_nilthread" */
	L4_Word_t res = L4_ThreadControl(tid, L4_nilthread, L4_nilthread,
		L4_nilthread, (void *)-1);
	fail_unless(res == 1, "ec=%#lx", L4_ErrorCode());
}


static void nothing_fn(void *param UNUSED) {
	/* jack shit. */
}


/* NOTE: this can be called with utcb=true at most once. after that, it'll
 * fail with the UTCB reuse code.
 */
static void mk_thread(L4_ThreadId_t tid, bool utcb)
{
	L4_KernelInterfacePage_t *kip = L4_GetKernelInterface();
	L4_Word_t utcb_ptr = (L4_Word_t)-1l;
	if(utcb) {
		L4_ThreadId_t other = start_thread(&nothing_fn, NULL);
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


/* create a non-activated thread and overwrite its version bits.
 *
 * (the real API test would start an actual thread, overwrite version, restart
 * it on another stack, overwrite that again with the previous bits, then
 * clean up with join_thread(). TODO: implement this.)
 */
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


Suite *thread_suite(void)
{
	Suite *s = suite_create("thread");

	{
		TCase *tc = tcase_create("api");
		tcase_set_fork(tc, false);	/* must be run in privileged space */
		tcase_add_test(tc, threadctl_basic);
		tcase_add_test(tc, privilege);
		tcase_add_test(tc, thread_id_validity);
		tcase_add_test(tc, scheduler_id_validity);
		tcase_add_test(tc, spacespec_validity);
		tcase_add_test(tc, relocate_utcb);
		suite_add_tcase(s, tc);
	}

	{
		TCase *tc = tcase_create("panic");
		tcase_set_fork(tc, false);
		tcase_add_test(tc, tid_stomp);
		suite_add_tcase(s, tc);
	}

	return s;
}
