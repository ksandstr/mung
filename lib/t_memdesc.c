/* tests for lib/memdesc.c */

#include <stdlib.h>
#include <stdbool.h>
#include <string.h>

#include <l4/types.h>
#include <l4/kip.h>

#include <ukernel/util.h>
#include <ukernel/memdesc.h>
#include <ukernel/ktest.h>

#if KTEST

/* TODO: further tests on mdb_query():
 *   - that virtual/non-virtual sections don't influence the result when not
 *     matched by the @virtual parameter;
 *   - that dedicated sections are skipped if !@dedicated;
 *   - that reserved sections can be had with @t == L4_ReservedMemoryType, and
 *     that this applies to reserved _and_ dedicated sections iff both are
 *     set.
 *   - complicated patterns of MemoryDescs
 *     - different permutations of MemoryDesc entries, since that shouldn't
 *       matter (but the algorithm behaviour changes)
 *     - noncontiguity, i.e. that there may be holes rather than negative
 *       ranges
 *   - ... once the modification API comes in, tests that mdb_query()'s result
 *     on the modified buffer matches the mods performed.
 */


/* shuffle a memdescbuf according to a permutation number. */
static void shuffle_mdb(struct memdescbuf *mdb, unsigned perm)
{
	unsigned ixes[mdb->len];
	gen_perm(ixes, mdb->len, perm);
	L4_MemoryDesc_t t[mdb->len];
	memcpy(t, mdb->ptr, sizeof(L4_MemoryDesc_t) * mdb->len);
	for(int i=0; i < mdb->len; i++) mdb->ptr[i] = t[ixes[i]];
}


/* simple dataset w/ just a virtual address space. it's like mung, but also
 * chops off the first 4k because fuck you.
 */
static void dset_virtual_only(struct memdescbuf *mdb)
{
	static const L4_MemoryDesc_t test_descs[] = {
		/* virtual address space. it's like mung, but also chops off the first
		 * 4k because fuck you.
		 */
		{ .x.v = 1, .x.type = L4_ConventionalMemoryType,
			.x.low = 0, .x.high = ~(L4_Word_t)0 >> 10 },
		{ .x.v = 1, .x.type = L4_ReservedMemoryType,
			.x.low = (L4_Word_t)0xc0000000 >> 10,
			.x.high = ~(L4_Word_t)0 >> 10 },
		{ .x.v = 1, .x.type = L4_DedicatedMemoryType,
			.x.low = 0, .x.high = 4095 >> 10 },
	};

	L4_MemoryDesc_t *buf = malloc(sizeof(*buf) * NUM_ELEMENTS(test_descs));
	memcpy(buf, test_descs, NUM_ELEMENTS(test_descs) * sizeof(*buf));
	*mdb = (struct memdescbuf){ .ptr = buf, .len = NUM_ELEMENTS(test_descs) };
}


/* queries on an empty buffer. */
START_TEST(basic_empty_query)
{
	static const L4_Word_t types[] = {
		L4_ConventionalMemoryType,
		L4_ReservedMemoryType,
		L4_DedicatedMemoryType,
		L4_SharedMemoryType,
	};

	plan_tests(NUM_ELEMENTS(types) * 2);

	struct memdescbuf mdb = { };
	assert(mdb.size == 0);
	assert(mdb.len == 0);

	for(int i=0; i < NUM_ELEMENTS(types); i++) {
		L4_Fpage_t out = mdb_query(&mdb, 0, ~0ul,
			true, false, types[i]);
		ok(L4_IsNilFpage(out), "no virtual memory (type=%#x)", types[i]);

		out = mdb_query(&mdb, 0, ~0ul, false, false, types[i]);
		ok(L4_IsNilFpage(out), "no physical memory (type=%#x)", types[i]);
	}
}
END_TEST


/* tests various simple queries. */
START_LOOP_TEST(basic_simple_query, iter, 0, 5)
{
	struct memdescbuf mdb; dset_virtual_only(&mdb);
	shuffle_mdb(&mdb, iter);
	diag("dataset=virtual_only[len=%u], iter=%d", mdb.len, iter);

	plan_tests(5);

	/* since there's a hole in the zero page, queries ending in it should
	 * return Nilpage.
	 */
	ok(L4_IsNilFpage(mdb_query(&mdb, 0, 0xfff,
			true, false, L4_ConventionalMemoryType)),
		"full nilpage is absent");
	ok(L4_IsNilFpage(mdb_query(&mdb, 0, 0x7ff,
			true, false, L4_ConventionalMemoryType)),
		"partial nilpage is absent");
	ok(L4_IsNilFpage(mdb_query(&mdb, 0, 0x3ff,
			true, false, L4_ConventionalMemoryType)),
		"tiniest nilpage is absent");

	ok(L4_IsNilFpage(mdb_query(&mdb, 0x400, 0x7ff,
			true, false, L4_ConventionalMemoryType)),
		"inner tiny within nilpage is absent");

	/* a simpler case for the upper 1G */
	ok(L4_IsNilFpage(mdb_query(&mdb, 0xc0010000, 0xffff,
			true, false, L4_ConventionalMemoryType)),
		"a 64k within kernel region is absent");

	free(mdb.ptr);
}
END_TEST


/* tests simple full enumeration of all addresses. */
START_LOOP_TEST(basic_full_enum, iter, 0, 5)
{
	struct memdescbuf mdb; dset_virtual_only(&mdb);
	shuffle_mdb(&mdb, iter);
	diag("dataset=virtual_only[len=%u], iter=%d", mdb.len, iter);

	plan_tests(4);

	/* scan the full virtual space and measure pages seen. */
	bool had_zeropage = false, had_high_addr = false,
		had_sub1meg = false, had_super2gig = false;
	L4_Word_t q_start = 0, q_end = ~0ul;
	diag("query=[%#lx, %#lx]", q_start, q_end);
	for(;;) {
		L4_Fpage_t part = mdb_query(&mdb, q_start, q_end,
			true, false, L4_ConventionalMemoryType);
		if(L4_IsNilFpage(part)) {
			diag("done.");
			break;
		}

		diag("part=%#lx:%#lx", L4_Address(part), L4_Size(part));
		if(fpage_overlap(part, L4_FpageLog2(0, 12))) {
			diag("  overlaps zero page!");
			had_zeropage = true;
		}
		if(fpage_overlap(part, L4_Fpage(0xc0000000, 0x40000000))) {
			diag("  contains high addresses!");
			had_high_addr = true;
		}
		if(fpage_overlap(part, L4_Fpage(0, 0x100000))) {
			had_sub1meg = true;
		}
		if(fpage_overlap(part, L4_FpageLog2(0x80000000, 31))) {
			had_super2gig = true;
		}
		q_start = L4_Address(part) + L4_Size(part);
	}

	ok(!had_zeropage, "scan excluded zero page");
	ok(!had_high_addr, "scan excluded uppermost gigabyte");
	ok(had_sub1meg, "scan found space below 1M");
	ok(had_super2gig, "scan found space above 2G");

	free(mdb.ptr);
}
END_TEST


void ktest_memdesc(void)
{
	RUN(basic_empty_query);
	RUN(basic_simple_query);
	RUN(basic_full_enum);
}

#endif
