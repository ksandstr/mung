
/* unit tests concerning the SpaceControl and Unmap system calls, and pager
 * operation.
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <assert.h>
#include <ctype.h>
#include <ccan/compiler/compiler.h>
#include <ccan/crc/crc.h>

#include <l4/types.h>
#include <l4/ipc.h>
#include <l4/kip.h>
#include <l4/space.h>
#include <l4/syscall.h>

#include <ukernel/util.h>

#include "defs.h"
#include "test.h"
#include "forkserv.h"


#define POKE_LABEL 0x05e7	/* "set" */
#define PEEK_LABEL 0x06e7	/* "get" */


/* poke/peek thread. obeys POKE, PEEK, and QUIT. */
static void poke_peek_fn(void *param_ptr)
{
#if 0
	diag("%s: started as %lu:%lu. pager is %#lx", __func__,
		L4_ThreadNo(L4_MyGlobalId()), L4_Version(L4_MyGlobalId()),
		L4_Pager());
#endif
	for(;;) {
		L4_ThreadId_t from;
		L4_MsgTag_t tag = L4_Wait(&from);

		for(;;) {
			if(L4_IpcFailed(tag)) break;

			if(tag.X.label == QUIT_LABEL) {
				// diag("%s: quitting", __func__);
				return;
			} else if(tag.X.label == PEEK_LABEL) {
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
		TEST_IPC_DELAY);
	return L4_IpcSucceeded(tag);
}


static bool peek(uint8_t *value_p, L4_ThreadId_t thread, L4_Word_t address)
{
	L4_LoadBR(0, 0);
	L4_LoadMR(0, (L4_MsgTag_t){ .X.label = PEEK_LABEL, .X.u = 1 }.raw);
	L4_LoadMR(1, address);
	L4_MsgTag_t tag = L4_Call_Timeouts(thread, L4_Never,
		TEST_IPC_DELAY);
	if(L4_IpcFailed(tag)) return false;
	else {
		L4_Word_t w;
		L4_StoreMR(1, &w);
		*value_p = w;
		return true;
	}
}


/* fixtures for the pager test case. */

static struct pager_stats *pg_stats;
static L4_ThreadId_t pg_pager, pg_poker;


static void pager_setup(void)
{
	pg_stats = malloc(sizeof(*pg_stats));
	fail_unless(pg_stats != NULL);
	pg_stats->n_faults = 12345;
	pg_stats->log_top = LOG_SIZE - 1;	/* to start at 0 */
	pg_pager = start_stats_pager(pg_stats);

	pg_poker = start_thread(&poke_peek_fn, NULL);
	fail_if(L4_IsNilThread(pg_poker), "can't start poke/peek thread");

	fail_unless(L4_IsGlobalId(pg_pager));
	L4_Set_PagerOf(pg_poker, pg_pager);
	assert(L4_PagerOf(pg_poker).raw == pg_pager.raw);

	/* settle the poker and pager in. this aims to cause all the faults that
	 * the poker is going to cause right off the bat, so it doesn't confuse
	 * measurement.
	 */
	uint8_t *thing = malloc(PAGE_SIZE);
	memset(thing, 0, PAGE_SIZE);
	thing[1] = 0xb5;		/* all alone in the night */
	uint8_t val = 0;
	if(!peek(&val, pg_poker, (uintptr_t)&thing[1])) {
		printf("%s: peek() failed, ec=%#lx\n", __func__,
			L4_ErrorCode());
	} else if(val != thing[1]) {
		printf("%s: peek() returned wrong data %#x\n", __func__, val);
	}
	if(!send_reset(pg_pager)) {
		printf("%s: send_reset() failed, ec=%#lx\n", __func__,
			L4_ErrorCode());
		/* TODO: should subsequently also fuck the pooch. */
	}
	free(thing);
}


static void pager_teardown(void)
{
	send_quit(pg_poker);
	xjoin_thread(pg_poker);

	L4_Word_t err = stop_stats_pager(pg_pager);
	fail_if(err != 0, "can't stop pager thread; ipc ec %#lx", err);

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
	fail_unless(ok, "poke failed");
	ok(*(uint8_t *)ptr == 0x42, "poke() did set a byte");

	uint8_t val = 0;
	ok = peek(&val, pg_poker, (L4_Word_t)ptr);
	fail_unless(ok, "peek failed");
	ok(val == 0x42, "peek() returned the right byte");
	*(uint8_t *)ptr = 0xab;
	ok = peek(&val, pg_poker, (L4_Word_t)ptr);
	fail_unless(ok, "second peek failed");
	ok(val == 0xab, "second peek() returned the right byte");

	free(ptr);
}
END_TEST


/* hole case */
START_TEST(poke_peek_fault_test)
{
	plan_tests(4);

	uint8_t *nonfaulted = sbrk(16 * 1024);

	L4_Word_t test_addr = (L4_Word_t)&nonfaulted[0];
	bool ok = poke(pg_poker, test_addr, 0xaf);
	fail_unless(ok, "poke failed: ec %#lx", L4_ErrorCode());
	ok(pg_stats->n_faults == 1 && pg_stats->n_write == 1,
		"poke write-faults");
	uint8_t val = 0;
	ok = peek(&val, pg_poker, test_addr);
	fail_unless(ok, "peek failed: ec %#lx", L4_ErrorCode());
	ok(pg_stats->n_faults == 1, "peek doesn't read-fault");
	ok(val == 0xaf, "peek returns poked value");

	test_addr = (L4_Word_t)&nonfaulted[4 * 1024];
	const int old_n = pg_stats->n_faults, old_r = pg_stats->n_read;
	val = 0;
	ok = peek(&val, pg_poker, test_addr);
	fail_unless(ok, "peek failed: ec %#lx", L4_ErrorCode());
	ok(pg_stats->n_faults == old_n + 1
		&& pg_stats->n_read == old_r + 1, "peek caused read fault");

	/* "nonfaulted" isn't released. who cares? this stuff runs in a fork
	 * anyway.
	 */
}
END_TEST


static void illegal_accessor(void *param_ptr)
{
	volatile uint8_t *p = param_ptr;
	*p = 0x23;
	diag("still alive (p=%#p)", p);
	exit_thread(NULL);
}


/* cause an illegal access, returning the segfault address to caller. for an
 * address outside the user-mappable range, this confirms that the kernel will
 * signal faults for such accesses rather than panicing.
 */
static uintptr_t cause_segv_at(uintptr_t address)
{
	L4_ThreadId_t tid = xstart_thread(&illegal_accessor, (void *)address);
	L4_Word_t ec = 0;
	void *ret = join_thread_long(tid, TEST_IPC_DELAY, &ec);
	if(ec != 0) diag("%s: ec=%#lx", __func__, ec);
	return (uintptr_t)ret;
}


START_TEST(illegal_access_test)
{
	plan_tests(4);

	/* base case 0: test access that isn't illegal. */
	void *mem = valloc(16 * 1024);
	memset(mem, 0, 16 * 1024);
	uintptr_t ret = cause_segv_at((uintptr_t)mem + 711);
	if(!ok(ret == 0, "legal access is legal")) {
		diag("ret=%#lx", ret);
	}
	free(mem);

	/* base case 1: access that's legal, but unmapped by forkserv. */
	ret = cause_segv_at(PAGE_SIZE + 711);
	if(!ok((ret & ~PAGE_MASK) == PAGE_SIZE, "unmapped access was caught")) {
		diag("ret=%#lx", ret);
	}

	/* explore the KIP's MemoryDescs to find a position outside the permitted
	 * virtual address range. if none exists, skip the test.
	 */
	L4_KernelInterfacePage_t *kip = L4_GetKernelInterface();
	for(int i=0; i <= 0xffff; i++) {
		L4_MemoryDesc_t *md = L4_MemoryDesc(kip, i);
		if(md == NULL) {
			skip(1, "no small virtual MemoryDesc found");
			break;
		} else if(L4_IsMemoryDescVirtual(md)
			&& L4_MemoryDescType(md) != L4_ReservedMemoryType
			&& (L4_MemoryDescLow(md) > 0
				|| L4_MemoryDescHigh(md) < ~(L4_Word_t)0))
		{
			diag("doing vfault in %#lx..%#lx (type %d)",
				L4_MemoryDescLow(md), L4_MemoryDescHigh(md),
				(int)L4_MemoryDescType(md));
			uintptr_t addr;
			if(L4_MemoryDescLow(md) > 0) addr = L4_MemoryDescLow(md) - 1;
			else addr = L4_MemoryDescHigh(md) + 1;
			ret = cause_segv_at(addr);
			if(!ok(ret == addr, "illegal access at %#lx", addr)) {
				diag("ret=%#lx", ret);
			}
			break;
		}
	}

	/* same, but for a reserved range. */
	for(int i=0; i <= 0xffff; i++) {
		L4_MemoryDesc_t *md = L4_MemoryDesc(kip, i);
		if(md == NULL) {
			skip(1, "no reserved kernel range found");
			break;
		} else if(L4_IsMemoryDescVirtual(md)
			&& L4_MemoryDescType(md) == L4_ReservedMemoryType)
		{
			diag("doing rfault in %#lx..%#lx (type %d)",
				L4_MemoryDescLow(md), L4_MemoryDescHigh(md),
				(int)L4_MemoryDescType(md));
			uintptr_t addr;
			if(L4_MemoryDescLow(md) > 0) addr = L4_MemoryDescLow(md);
			else addr = L4_MemoryDescHigh(md);
			ret = cause_segv_at(addr);
			if(!ok(ret == addr, "illegal access at %#lx", addr)) {
				diag("ret=%#lx", ret);
			}
			break;
		}
	}
}
END_TEST


/* tcase "ctl" */

START_TEST(spacectl_basic)
{
	plan_tests(1);

	/* first, an inactive thread. */
	L4_ThreadId_t tid = L4_GlobalId(1024, 199), self = L4_Myself();
	L4_Word_t res = L4_ThreadControl(tid, tid, self, L4_nilthread,
		(void *)-1);
	fail_unless(res == 1, "creating ThreadControl failed, ec %#lx",
		L4_ErrorCode());

	L4_Word_t ctl_out;
	res = L4_SpaceControl(tid, 0, L4_FpageLog2(0x100000, 12),
		L4_FpageLog2(0x104000, 14), L4_anythread, &ctl_out);
	if(res != 1) diag("SpaceControl failed, ec %#lx", L4_ErrorCode());
	ok(res == 1, "space configured");

	/* TODO: destroy thread, space */
}
END_TEST


START_TEST(spacectl_iface)
{
	L4_KernelInterfacePage_t *kip = L4_GetKernelInterface();
	int kip_s = kip->KipAreaInfo.X.s;
	L4_Fpage_t kip_area = L4_FpageLog2(0x111000, kip_s),
		utcb_area = L4_FpageLog2(0x200000, 12);

	/* find top of the virtual address space. */
	L4_Word_t vaddr_end = ~(L4_Word_t)0;
	for(int i=0; i < kip->MemoryInfo.n; i++) {
		L4_MemoryDesc_t *md = L4_MemoryDesc(kip, i);
		if(L4_IsMemoryDescVirtual(md)) {
			if(L4_MemoryDescType(md) == L4_ConventionalMemoryType) {
				vaddr_end = MIN(L4_Word_t, vaddr_end,
					L4_MemoryDescHigh(md));
			} else {
				vaddr_end = MIN(L4_Word_t, vaddr_end,
					L4_MemoryDescLow(md));
			}
		}
	}

	plan_tests(7);

	L4_ThreadId_t tid = L4_GlobalId(23042, 199), self = L4_Myself();
	L4_Word_t res = L4_ThreadControl(tid, tid, self, L4_nilthread,
		(void *)-1);
	fail_unless(res == 1, "creating ThreadControl failed, ec %#lx",
		L4_ErrorCode());

	L4_Word_t ctl_out;
	res = L4_SpaceControl(tid, 0, kip_area, utcb_area, L4_anythread,
		&ctl_out);
	ok(res == 1, "valid SpaceControl is valid");

	/* overlaps between kip_area and utcb_area */
	L4_Fpage_t bad_utcb[3] = {
		L4_FpageLog2(L4_Address(kip_area), 14),		/* low */
		L4_FpageLog2(L4_Address(kip_area) & ~((1 << 14) - 1), 14),	/* high */
		L4_FpageLog2(L4_Address(kip_area) & ~((1 << 16) - 1), 14),	/* in */
	};
	for(int i=0; i < NUM_ELEMENTS(bad_utcb); i++) {
		L4_Fpage_t bad_utcb_area = bad_utcb[i];
		fail_unless(RANGE_OVERLAP(FPAGE_LOW(bad_utcb_area), FPAGE_HIGH(bad_utcb_area),
			FPAGE_LOW(kip_area), FPAGE_HIGH(kip_area)));
		res = L4_SpaceControl(tid, 0, kip_area, bad_utcb_area,
			L4_anythread, &ctl_out);
		L4_Word_t ec = L4_ErrorCode();
		ok(res == 0 && ec == 7, "error 7 on KIP/UTCB area overlap");
	}

	/* KIP area that's too small */
	L4_Fpage_t bad_kip_area = L4_FpageLog2(L4_Address(kip_area),
		kip->KipAreaInfo.X.s - 1);
	res = L4_SpaceControl(tid, 0, bad_kip_area, utcb_area,
		L4_anythread, &ctl_out);
	L4_Word_t ec = L4_ErrorCode();
	ok(res == 0 && ec == 7, "kip area %#lx:%#lx is the wrong size",
		L4_Address(bad_kip_area), L4_Size(bad_kip_area));

	/* TODO: test too large an utcb_area */

	/* UTCB and KIP areas outside the address space */
	bad_kip_area = L4_FpageLog2(vaddr_end + 0x10000, kip_s);
	res = L4_SpaceControl(tid, 0, bad_kip_area, utcb_area,
		L4_anythread, &ctl_out);
	ec = L4_ErrorCode();
	ok(res == 0 && ec == 7, "kip area %#lx:%#lx out of range",
		L4_Address(bad_kip_area), L4_Size(bad_kip_area));

	L4_Fpage_t bad_utcb_area = L4_FpageLog2(vaddr_end + 0x10000, 18);
	res = L4_SpaceControl(tid, 0, kip_area, bad_utcb_area,
		L4_anythread, &ctl_out);
	ec = L4_ErrorCode();
	ok(res == 0 && ec == 6, "utcb area %#lx:%#lx out of range",
		L4_Address(bad_utcb_area), L4_Size(bad_utcb_area));

	/* TODO: test UTCB/KIP area change while thread is activated. */

	/* destroy thread, and with it the address space too */
	res = L4_ThreadControl(tid, L4_nilthread, L4_nilthread,
		L4_nilthread, (void *)-1);
	fail_if(res != 1, "cleanup threadctl failed, ec=%#lx", L4_ErrorCode());
}
END_TEST


/* tcase "unmap" */

/* TODO: move the access flag test points out into a different test */
START_TEST(simple_flush)
{
	const uint8_t poke_val = 0x22;

	plan_tests(8);

	const size_t mem_size = 3 * 4096;
	void *ptr = valloc(mem_size);
	memset(ptr, 0, mem_size);

	bool ok = poke(pg_poker, (L4_Word_t)ptr, poke_val);
	fail_unless(ok, "poke of address %p failed", ptr);

	L4_Fpage_t ptr_page = L4_FpageLog2((L4_Word_t)ptr, 12);
	L4_Set_Rights(&ptr_page, L4_FullyAccessible);
	L4_FlushFpages(1, &ptr_page);
	ok(CHECK_FLAG(L4_Rights(ptr_page), L4_Readable),
		"poke caused read access");
	ok(CHECK_FLAG(L4_Rights(ptr_page), L4_Writable),
		"poke caused write access");

	int old_r = pg_stats->n_read;
	uint8_t val;
	ok = peek(&val, pg_poker, (L4_Word_t)ptr);
	fail_unless(ok, "peek of address %p failed", ptr);

	ok(pg_stats->n_read == old_r + 1, "read fault after unmap");
	ok(val == poke_val, "peeked value is correct");
	ok(val == *(uint8_t *)ptr, "peeked value in memory");

	L4_Fpage_t st = L4_GetStatus(ptr_page);
	ok(CHECK_FLAG(L4_Rights(st), L4_Readable),
		"peek caused read access");
	ok(!CHECK_FLAG(L4_Rights(st), L4_Writable),
		"peek didn't cause write access");

	int old_f = pg_stats->n_faults;
	ok = poke(pg_poker, (L4_Word_t)ptr, 0xff ^ poke_val);
	fail_unless(ok, "second poke of address %p failed", ptr);
	ok(pg_stats->n_faults == old_f + 1, "fault after flush");

	free(ptr);
}
END_TEST


/* "partial" meaning "to read only". does both immediate (own call to
 * L4_FlushFpages()) and recursive (via forkserv).
 *
 * TODO: split this up to test privilege revocation, and access bit reading,
 * separately.
 */
START_LOOP_TEST(partial_flush, iter, 0, 1)
{
	const bool recursive = CHECK_FLAG(iter, 1);
	diag("recursive=%s", btos(recursive));

	plan_tests(4);

	const size_t mem_size = 3 * 4096;
	void *ptr = valloc(mem_size);
	memset(ptr, 0, mem_size);

	L4_Fpage_t ptr_page = L4_FpageLog2((L4_Word_t)ptr, 12);
	L4_Set_Rights(&ptr_page, L4_Writable);
	if(recursive) {
		diag("calling forkserv unmap");
		unsigned num = 1;
		int n = forkserv_unmap(L4_Pager(), &ptr_page, 1, &ptr_page, &num);
		fail_unless(n == 0, "n=%d", n);
	} else {
		diag("calling L4_FlushFpages()");
		L4_FlushFpages(1, &ptr_page);
	}
	diag("unmap returned");

	uint8_t val;
	int old_f = pg_stats->n_faults;
	bool ok = peek(&val, pg_poker, (L4_Word_t)ptr);
	fail_unless(ok);
	ok(pg_stats->n_faults == old_f, "peek caused no fault");

	int old_w = pg_stats->n_write;
	ok = poke(pg_poker, (L4_Word_t)ptr, 0x22);
	fail_unless(ok);
	ok(pg_stats->n_write == old_w + 1, "poke caused one fault");
	L4_Fpage_t last = pg_stats->log[pg_stats->log_top];
	ok(L4_Address(last) == ((L4_Word_t)ptr & ~PAGE_MASK),
		"last fault on %#lx", (L4_Word_t)ptr & ~PAGE_MASK);
	ok(CHECK_FLAG_ALL(L4_Rights(last), L4_Readable | L4_Writable),
		"last fault was r/w");

	free(ptr);
}
END_TEST


/* "large" meaning "multiple pages" */
START_TEST(large_flush)
{
	const size_t test_size = 32 * 1024;

	plan_tests(2);

	void *mem_base = malloc(test_size * 2);
	fail_unless(mem_base != NULL);
	char *mem = (void *)(((uintptr_t)mem_base + test_size - 1) & ~(test_size - 1));
	uint32_t boring_seed = 0x81be0a94;
	random_string(mem, test_size, &boring_seed);
	diag("mem is %#x..%#x (base %p)",
		(uintptr_t)mem, (uintptr_t)mem + test_size - 1, mem_base);

	/* should cause no faults before the flush. */
	fail_unless(pg_stats->n_faults == 0);
	for(uintptr_t pos = 0; pos < test_size; pos += PAGE_SIZE) {
		uint8_t val;
		bool ok = peek(&val, pg_poker, (uintptr_t)&mem[pos]);
		fail_unless(ok);
		fail_unless(mem[pos] == val, "expected %#x, got %#x",
			(unsigned)mem[pos], (unsigned)val);

		char c = mem[pos];
		ok = poke(pg_poker, (uintptr_t)&mem[pos], toupper(c));
		fail_unless(ok);
		fail_unless(mem[pos] == toupper(c));
		mem[pos] = c;
	}
	ok(pg_stats->n_faults == 0, "base case (no faults)");

	L4_Fpage_t flush_page = L4_Fpage((uintptr_t)mem, test_size * 4);
	L4_Set_Rights(&flush_page, L4_FullyAccessible);
	diag("flushing %#lx..%#lx",
		FPAGE_LOW(flush_page), FPAGE_HIGH(flush_page));
	L4_FlushFpage(flush_page);

	/* should cause both a read and a write fault separately afterward. */
	fail_unless(pg_stats->n_faults == 0);
	int expected = 0;
	for(uintptr_t pos = 0; pos < test_size; pos += PAGE_SIZE) {
		uint8_t val;
		bool ok = peek(&val, pg_poker, (uintptr_t)&mem[pos]);
		fail_unless(ok);
		fail_unless(mem[pos] == val, "expected %#x, got %#x",
			(unsigned)mem[pos], (unsigned)val);
		expected++;

		char c = mem[pos];
		ok = poke(pg_poker, (uintptr_t)&mem[pos], toupper(c));
		fail_unless(ok);
		fail_unless(mem[pos] == toupper(c));
		mem[pos] = c;
		expected++;
	}
	diag("%d faults, r %d, w %d", pg_stats->n_faults,
		pg_stats->n_read, pg_stats->n_write);
	ok1(pg_stats->n_faults >= expected);

	free(mem_base);
}
END_TEST


START_TEST(no_unmap_kip_utcb)
{
	const L4_Word_t test_addrs[] = {
		(L4_Word_t)L4_GetKernelInterface(),
		L4_MyLocalId().raw,
	};
	plan_tests(1 + 2 * NUM_ELEMENTS(test_addrs));

	L4_Fpage_t fp[NUM_ELEMENTS(test_addrs)];
	for(int i=0; i < NUM_ELEMENTS(test_addrs); i++) {
		fp[i] = L4_FpageLog2(test_addrs[i] & ~PAGE_MASK, 12);
		L4_Set_Rights(&fp[i], L4_FullyAccessible);
	}
	diag("calling L4_UnmapFpages()");
	L4_UnmapFpages(2, fp);

	ok(true, "didn't die");
	for(int i=0; i < NUM_ELEMENTS(test_addrs); i++) {
		diag("i=%d, fp[i]=%#lx", i, fp[i].raw);
		if(!ok1(L4_Address(fp[i]) == (test_addrs[i] & ~PAGE_MASK))) {
			diag("got %#lx, expected %#lx", L4_Address(fp[i]),
				test_addrs[i] & ~PAGE_MASK);
		}
		ok1(L4_Rights(fp[i]) == 0);
	}
}
END_TEST


START_TEST(no_flush_kip_utcb)
{
	void *kip_addr = L4_GetKernelInterface();

	const L4_Word_t test_addrs[] = {
		(L4_Word_t)kip_addr,
		L4_MyLocalId().raw,
	};
	const size_t kip_crc_len = 0xff;
	plan_tests(4 + 2 * NUM_ELEMENTS(test_addrs));

	uint32_t kip_crc = crc32c(0, kip_addr, kip_crc_len);

	L4_Fpage_t fp[NUM_ELEMENTS(test_addrs)];
	for(int i=0; i < NUM_ELEMENTS(test_addrs); i++) {
		fp[i] = L4_FpageLog2(test_addrs[i] & ~PAGE_MASK, PAGE_BITS);
		L4_Set_Rights(&fp[i], L4_FullyAccessible);
	}
	diag("calling L4_FlushFpages()");
	L4_FlushFpages(2, fp);

	ok(true, "didn't die");
	uint32_t kip_crc_after = crc32c(0, kip_addr, kip_crc_len);
	ok1(kip_crc_after == kip_crc);

	for(int i=0; i < NUM_ELEMENTS(test_addrs); i++) {
		diag("i=%d, fp[i]=%#lx", i, fp[i].raw);
		if(!ok1(L4_Address(fp[i]) == (test_addrs[i] & ~PAGE_MASK))) {
			diag("got %#lx, expected %#lx", L4_Address(fp[i]),
				test_addrs[i] & ~PAGE_MASK);
		}
		ok1(L4_Rights(fp[i]) == 0);
	}

	L4_ThreadId_t tid = xstart_thread(&access_memory_fn, kip_addr + 0x40);
	L4_Word_t ec = 0;
	void *ret = join_thread_long(tid, TEST_IPC_DELAY, &ec);
	if(!ok(ret != NULL || ec == 0, "illegal KIP access ok")) {
		diag("ret=%p, ec=%#lx", ret, ec);
	}
	if(!ok1(ret == kip_addr + 0x40)) {
		diag("ret=%p", ret);
	}
}
END_TEST


/* local access queries. i.e. whether the local space's momentary access bits
 * get delivered and reset per spec.
 */
START_TEST(local_access)
{
	plan_tests(2);

	char *testmem = valloc(PAGE_SIZE);
	memset(testmem, 0, PAGE_SIZE);
	diag("testmem=%p", testmem);
	L4_Fpage_t pg = L4_FpageLog2((L4_Word_t)testmem, PAGE_BITS);
	L4_GetStatus(pg);	/* clear off the access bits. */

	strlcpy(testmem, "whatever", PAGE_SIZE);
	int after_write = L4_Rights(L4_GetStatus(pg));
	if(!ok1(CHECK_FLAG_ALL(after_write, L4_ReadWriteOnly))) {
		diag("after_write=%#x", (unsigned)after_write);
	}

	diag("testmem contains `%s'", testmem);
	int after_read = L4_Rights(L4_GetStatus(pg));
	if(!ok1((after_read & L4_ReadWriteOnly) == L4_Readable)) {
		diag("after_read=%#x", (unsigned)after_read);
	}
}
END_TEST


/* like L4_Rights(L4_GetStatus(@pg)), but indirect. */
static int get_status_from_fs(L4_Fpage_t pg)
{
	L4_Set_Rights(&pg, 0);
	unsigned num = 1;
	int n = forkserv_unmap(L4_Pager(), &pg, 1, &pg, &num);
	if(n < 0) {
		diag("%s: forkserv_unmap failed, n=%d", __func__, n);
		return 0;
	} else if(num == 0) {
		diag("%s: forkserv_unmap didn't return a page?", __func__);
		return 0;
	} else {
		return L4_Rights(pg);
	}
}


/* parent-to-child access queries. i.e. whether a child space's momentary
 * access bits get delivered and reset per spec, to the parent.
 */
START_TEST(parent_access)
{
	plan_tests(2);

	char *testmem = valloc(PAGE_SIZE);
	memset(testmem, 0, PAGE_SIZE);
	diag("testmem=%p", testmem);
	L4_Fpage_t pg = L4_FpageLog2((L4_Word_t)testmem, PAGE_BITS);
	get_status_from_fs(pg);

	strlcpy(testmem, "umm and whatnot", PAGE_SIZE);
	int after_write = get_status_from_fs(pg);
	if(!ok1(CHECK_FLAG_ALL(after_write, L4_ReadWriteOnly))) {
		diag("after_write=%#x", (unsigned)after_write);
	}

	diag("testmem contains `%s'", testmem);
	int after_read = get_status_from_fs(pg);
	if(!ok1((after_read & L4_ReadWriteOnly) == L4_Readable)) {
		diag("after_read=%#x", (unsigned)after_read);
	}
}
END_TEST


/* tests whether a parent's access query will interfere with that of the
 * child, or vice versa.
 */
START_LOOP_TEST(access_interference, iter, 0, 1)
{
	const bool parent_first = CHECK_FLAG(iter, 1);
	diag("parent_first=%s", btos(parent_first));

	plan_tests(4);

	char *testmem = valloc(PAGE_SIZE);
	memset(testmem, 0, PAGE_SIZE);
	L4_Fpage_t pg = L4_FpageLog2((L4_Word_t)testmem, PAGE_BITS);
	diag("pg=%#lx:%#lx", L4_Address(pg), L4_Size(pg));
	get_status_from_fs(pg);
	L4_GetStatus(pg);

	strlcpy(testmem, "something or other", PAGE_SIZE);
	unsigned parent, child;
	if(parent_first) parent = get_status_from_fs(pg);
	child = L4_Rights(L4_GetStatus(pg));
	if(!parent_first) parent = get_status_from_fs(pg);
	diag("[rw] parent=%#x, child=%#x", parent, child);
	ok1(CHECK_FLAG_ALL(parent, L4_ReadWriteOnly));
	ok1(CHECK_FLAG_ALL(child, L4_ReadWriteOnly));

	diag("testmem contains `%s'", testmem);
	if(parent_first) parent = get_status_from_fs(pg);
	child = L4_Rights(L4_GetStatus(pg));
	if(!parent_first) parent = get_status_from_fs(pg);
	diag("[ro] child=%#x, parent=%#x", child, parent);
	ok1((parent & L4_ReadWriteOnly) == L4_Readable);
	ok1((child & L4_ReadWriteOnly) == L4_Readable);
}
END_TEST


static void helpful_assistant(size_t map_size, L4_ThreadId_t p_tid);

/* creates a chain of mappings between two address spaces up to maximum
 * length. this is intended to cause recursion-induced stack overflow via
 * Unmap, and to test methods to detect it and (eventually) overcome such
 * cases.
 *
 * sensitive people may wish to avert their eyes from this point down.
 */
START_LOOP_TEST(deep_recursive_unmap, iter, 0, 1)
{
	/* previously, 11598 was a magic number where the kernel threw an
	 * uncorrectable hissyfit in the chain-building stage. below that, not.
	 * since the recursive unmap_entry_in_group() is structurally unable to
	 * deal with great recursion past this point, this value cannot be
	 * usefully tested without it shitting up test reports for the next coupla
	 * years.
	 *
	 * so the test values are 10 for semi-guaranteed deep_call()
	 * non-activation and 450 to fail in its absence, overflow any static
	 * recursion limit, and fill up the kernel heap.
	 */
	const size_t chainlens[] = { 10, 450 };
	const size_t map_size = PAGE_SIZE,	/* TODO: vary this */
		chain_length = chainlens[iter & 0x1];
	const bool burn_space = true;		/* TODO: this, too */
	diag("map_size=%u, chain_length=%u, burn_space=%s",
		map_size, chain_length, btos(burn_space));
	plan_tests(9);

	L4_ThreadId_t igor_tid, p_tid = L4_Myself();
	int igor = fork_tid(&igor_tid);
	if(igor == 0) {
		helpful_assistant(map_size, p_tid);
		exit(0);
	}

	if(burn_space) {
		/* burn a bit of heap space so that the same addresses don't show up
		 * for both participants, making debugging a bit easier.
		 */
		sbrk(MIN(size_t, 0x123456, map_size * chain_length / 8 * 11));
	}

	/* make us a chain. the pattern is simple: create some memory, map it to
	 * the peasant, have it carry that back into our accept range (allocated
	 * previously). rinse, repeat.
	 */
	void *pageptr = sbrk(map_size);
	memset(pageptr, 0xf3, map_size);
	uint8_t *headptr = pageptr;
	L4_Fpage_t snd_page = L4_Fpage((L4_Word_t)pageptr, map_size);
	L4_Set_Rights(&snd_page, L4_ReadWriteOnly);
	bool ipc_ok = true;
	for(size_t i=0; i < chain_length; i++) {
		pageptr = sbrk(map_size);
		L4_Fpage_t rcv_page = L4_Fpage((L4_Word_t)pageptr, map_size);
		L4_MapItem_t mi = L4_MapItem(snd_page, 0);
		L4_Accept(L4_MapGrantItems(rcv_page));
		L4_LoadMR(0, (L4_MsgTag_t){ .X.t = 2 }.raw);
		L4_LoadMRs(1, 2, mi.raw);
		L4_MsgTag_t tag = L4_Call_Timeouts(igor_tid,
			TEST_IPC_DELAY, TEST_IPC_DELAY);
		if(L4_IpcFailed(tag)) {
			diag("parent's ipc failed, ec=%#lx", L4_ErrorCode());
			ipc_ok = false;
			break;
		}
		snd_page = rcv_page;
		L4_Set_Rights(&snd_page, L4_ReadWriteOnly);
	}
	diag("final snd_page=%#lx:%#lx", L4_Address(snd_page), L4_Size(snd_page));
	ok(ipc_ok, "chain created");

	/* confirm that the chain is properly real */
	volatile uint8_t *tailptr = (uint8_t *)L4_Address(snd_page),
		preflush_val = *tailptr;
	diag("*tailptr = %#x", (unsigned)*tailptr);
	ok1(tailptr[map_size - 1] == headptr[map_size - 1]);
	ok1(tailptr[17] == headptr[17]);

	ok(headptr[123] != 0x8e, "modification base case");
	tailptr[123] = 0x8e;
	ok(headptr[123] == 0x8e, "modification post-case");

	/* the destructive test. this should produce the same read-result as
	 * earlier, but read as zero after being written to.
	 */
	L4_Fpage_t flush_page = L4_Fpage((L4_Word_t)headptr, map_size);
	L4_Set_Rights(&flush_page, L4_Writable);
	L4_UnmapFpage(flush_page);
	ok(tailptr[123] == 0x8e, "post-flush read 1");
	ok(tailptr[map_size - 1] == headptr[map_size - 1], "post-flush read 2");
	ok(*tailptr == preflush_val, "post-flush read 3");
	/* fall down, go boom */
	tailptr[234] = 0xff;
	ok1(tailptr[235] == 0x00);

	/* even the best help can't clean up after itself these days. what's the
	 * world coming to
	 */
	send_quit(igor_tid);
	int st, dead = wait(&st);
	fail_if(dead != igor);
}
END_TEST


/* our helpful assistant, yes. he is very simple now. */
static void helpful_assistant(size_t map_size, L4_ThreadId_t p_tid)
{
	int n_timeouts = 0;
	for(;;) {
		void *posptr = sbrk(map_size);
		L4_Fpage_t map_page = L4_Fpage((L4_Word_t)posptr, map_size);
		L4_Accept(L4_MapGrantItems(map_page));
		L4_MsgTag_t tag = L4_Receive_Timeout(p_tid, TEST_IPC_DELAY);

		if(L4_IpcFailed(tag)) {
			L4_Word_t ec = L4_ErrorCode();
			if(ec == 3 && ++n_timeouts >= 3) {
				diag("%s: enough timeouts in receive phase, exiting",
					__func__);
				return;
			} else {
				diag("%s: ipc failed, ec=%#lx", __func__, ec);
				continue;
			}
		}

		if(L4_Label(tag) == QUIT_LABEL) return;
		L4_Set_Rights(&map_page, L4_FullyAccessible);
		L4_MapItem_t mi = L4_MapItem(map_page, 0);
		posptr = sbrk(map_size);
		map_page = L4_Fpage((L4_Word_t)posptr, map_size);
		L4_Accept(L4_MapGrantItems(map_page));
		L4_LoadMR(0, (L4_MsgTag_t){ .X.t = 2 }.raw);
		L4_LoadMRs(1, 2, mi.raw);
		tag = L4_Reply(p_tid);
		if(L4_IpcFailed(tag)) {
			diag("%s: reply failed, ec=%#lx", __func__, L4_ErrorCode());
		}
	}
}


Suite *space_suite(void)
{
	Suite *s = suite_create("space");

	{
		TCase *tc = tcase_create("api");
		tcase_set_fork(tc, false);
		tcase_add_test(tc, spacectl_basic);
		tcase_add_test(tc, spacectl_iface);
		suite_add_tcase(s, tc);
	}

	{
		TCase *tc = tcase_create("pager");
		tcase_add_checked_fixture(tc, &pager_setup, &pager_teardown);
		tcase_add_test(tc, poke_peek_nofault_test);
		tcase_add_test(tc, poke_peek_fault_test);
		tcase_add_test(tc, illegal_access_test);
		suite_add_tcase(s, tc);
	}

	{
		TCase *tc = tcase_create("unmap");
		tcase_add_checked_fixture(tc, &pager_setup, &pager_teardown);
		tcase_add_test(tc, simple_flush);
		tcase_add_test(tc, partial_flush);
		tcase_add_test(tc, large_flush);
		tcase_add_test(tc, no_unmap_kip_utcb);
		tcase_add_test(tc, no_flush_kip_utcb);
		tcase_add_test(tc, local_access);
		tcase_add_test(tc, parent_access);
		tcase_add_test(tc, access_interference);
		tcase_add_test(tc, deep_recursive_unmap);
		suite_add_tcase(s, tc);
	}

	return s;
}
