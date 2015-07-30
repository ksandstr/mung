/* tests for lib/memdesc.c */

#include <stdlib.h>
#include <stdbool.h>
#include <string.h>

#include <l4/types.h>
#include <l4/kip.h>

#include <ukernel/mm.h>
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


/* whether there aren't any nil MemoryDescs in @mdb. */
static bool is_mdb_dense(struct memdescbuf *mdb) {
	for(int i=0; i < mdb->len; i++) {
		if(mdb->ptr[i].x.low == mdb->ptr[i].x.high) return false;
	}
	return true;
}


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


/* tests on mdb_set() [modification]. */

/* check that mdb_set() returns false for a full (i.e. all-zero) buffer. */
START_TEST(basic_zero_buf)
{
	plan_tests(1);
	todo_start("mdb_set() unimplemented");

	struct memdescbuf buf = { };
	assert(buf.size == 0 && buf.len == 0);
	bool ok = mdb_set(&buf, 0, PAGE_SIZE * 8 - 1,
		false, L4_ConventionalMemoryType, 0);
	ok(!ok, "set() in zero buffer should fail");
}
END_TEST


/* queries a range. returns true iff there were no gaps in that range, and its
 * end was seen.
 */
static bool q_range_present(
	struct memdescbuf *buf,
	L4_Word_t q_start, L4_Word_t q_end,
	bool virtual, bool dedicated, L4_Word_t typ)
{
	bool have_gap = false, end_seen = false;
	for(;;) {
		L4_Fpage_t part = mdb_query(buf, q_start, q_end,
			virtual, dedicated, typ);
		if(L4_IsNilFpage(part)) break;

		if(FPAGE_LOW(part) > q_start) have_gap = true;
		if(FPAGE_HIGH(part) == q_end) end_seen = true;
		q_start = L4_Address(part) + L4_Size(part);
	}
	return !have_gap && end_seen;
}


/* simplest modification. starts with a empty database and adds two descs,
 * testing coverage in between.
 *
 * variables:
 *   - whether non-conventional is reserved or dedicated
 *   - order of setting operations [conventional first, or not]
 *   - whether overlap is to the side, or entirely contained
 *   - which side / which one is bigger, depending on above
 */
START_LOOP_TEST(basic_modify, iter, 0, 15)
{
	const bool oth_dedicated = CHECK_FLAG(iter, 1),
		oth_last = CHECK_FLAG(iter, 2),
		over_contained = CHECK_FLAG(iter, 4),
		over_right = CHECK_FLAG(iter, 8);
	diag("oth_dedicated=%s, oth_last=%s, over_contained=%s, over_right=%s",
		btos(oth_dedicated), btos(oth_last), btos(over_contained),
		btos(over_right));
	plan_tests(6);
	todo_start("mdb_set() unimplemented");

	struct memdescbuf buf = { .size = 20 };
	buf.ptr = malloc(sizeof(L4_MemoryDesc_t) * buf.size);
	assert(buf.ptr != NULL);

	/* operational parameters */
	const L4_Word_t oth_type = oth_dedicated ? L4_DedicatedMemoryType
		: L4_ReservedMemoryType;
	L4_Word_t fst_start = 0x1000, fst_end = PAGE_SIZE * 400 - 1,
		oth_start = over_contained ? fst_start + PAGE_SIZE * 12
			: fst_end - PAGE_SIZE * 113 + 1,
		oth_end = over_contained ? fst_end - PAGE_SIZE * 97
			: oth_start + PAGE_SIZE * 400 - 1;
	if(over_right) {
		SWAP(L4_Word_t, fst_start, oth_start);
		SWAP(L4_Word_t, fst_end, oth_end);
	}

	/* base case: there's nothing in fst or oth. */
	ok(!q_range_present(&buf, fst_start, fst_end,
			false, false, L4_ConventionalMemoryType),
		"fst=[%#lx, %#lx] not present", fst_start, fst_end);
	ok(!q_range_present(&buf, oth_start, oth_end,
			false, oth_dedicated, oth_type),
		"oth=[%#lx, %#lx] not present", oth_start, oth_end);

	/* modify. */
	bool ok;
	if(!oth_last) {
		ok = mdb_set(&buf, oth_start, oth_end, false, oth_type, 0);
		assert(ok);
	}
	ok = mdb_set(&buf, fst_start, fst_end, false,
		L4_ConventionalMemoryType, 0);
	assert(ok);
	if(oth_last) {
		ok = mdb_set(&buf, oth_start, oth_end, false, oth_type, 0);
		assert(ok);
	}
	imply_ok1(!over_contained || (over_right != oth_last), buf.len >= 2);
	ok1(is_mdb_dense(&buf));

	/* measure the result. */

	/* the full range between fst_start and fst_end should show up as
	 * conventional memory iff !oth_last.
	 */
	bool fst_present = q_range_present(&buf, fst_start, fst_end,
		false, false, L4_ConventionalMemoryType);
	iff_ok1(fst_present, !oth_last);

	/* searching for dedicated memory should turn up oth_start..oth_end iff
	 * oth_last && oth_dedicated.
	 */
	bool oth_ded_found = q_range_present(&buf, oth_start, oth_end,
		false, true, L4_DedicatedMemoryType);
	iff_ok1(oth_ded_found, oth_last && oth_dedicated);

	free(buf.ptr);
}
END_TEST


/* test that mdb_set() merges items into similar neighbours. this covers the
 * case where there's one on either side of a middle range.
 *
 * variables:
 *   - whether boundaries are exact, or if there's overlap.
 *
 * todo:
 *   - that it doesn't merge into dissimilar ones.
 *   - merging into a gap between two items
 */
START_LOOP_TEST(merge_left_right_between, iter, 0, 1)
{
	const bool overlap = CHECK_FLAG(iter, 1);
	diag("overlap=%s", btos(overlap));
	plan_tests(5);
	todo_start("mdb_set() unimplemented");

	L4_MemoryDesc_t descs[8];
	struct memdescbuf buf = { .ptr = descs, .size = NUM_ELEMENTS(descs) };

	const L4_Word_t mid_start = 0x4000, mid_end = 0xffff;
	bool ok = mdb_set(&buf, mid_start, mid_end, false,
		L4_ConventionalMemoryType, 0);
	if(!ok1(ok && buf.len == 1)) {
		diag("ok=%s, buf.len=%u", btos(ok), (unsigned)buf.len);
	}

	/* merge on the left of the given range. */
	ok = mdb_set(&buf, (!overlap ? mid_end : mid_end - 0x2000) + 1,
		0x1d000, false, L4_ConventionalMemoryType, 0);
	if(!ok(ok && buf.len == 1, "one item after left merge")) {
		diag("ok=%s, buf.len=%u", btos(ok), (unsigned)buf.len);
	}

	/* and the right. */
	ok = mdb_set(&buf, 0x1000,
		(!overlap ? mid_start : mid_start + 0x2000) - 1,
		false, L4_ConventionalMemoryType, 0);
	if(!ok(ok && buf.len == 1, "one item after right merge")) {
		diag("ok=%s, buf.len=%u", btos(ok), (unsigned)buf.len);
	}

	/* split the range up down the middle w/ a reserved range. */
	const L4_Word_t cut_start = 0x3000, cut_end = 0x1afff;
	ok = mdb_set(&buf, cut_start, cut_end, false, L4_ReservedMemoryType, 0);
	if(!ok(ok && buf.len == 3, "three items after split")) {
		diag("ok=%s, buf.len=%u", btos(ok), (unsigned)buf.len);
	}

	/* and join them back up again. */
	ok = mdb_set(&buf,
		overlap ? cut_start - 0x1000 : cut_start,
		overlap ? cut_end + 0x1000 : cut_end,
		false, L4_ConventionalMemoryType, 0);
	if(!ok(ok && buf.len == 1, "one item after fixup")) {
		diag("ok=%s, buf.len=%u", btos(ok), (unsigned)buf.len);
	}
}
END_TEST


/* test that mdb_set() merges two items of the same type, that overlap on both
 * sides.
 *
 * variables:
 *   - which one goes in first.
 */
START_LOOP_TEST(merge_inside, iter, 0, 1)
{
	const bool swapped = CHECK_FLAG(iter, 1);
	diag("swapped=%s", btos(swapped));
	plan_tests(2);
	todo_start("mdb_set() unimplemented");

	L4_Word_t fst_start = 0x10000, fst_end = 0x1ffff,
		snd_start = 0x13000, snd_end = 0x1dfff;
	if(swapped) {
		SWAP(L4_Word_t, fst_start, snd_start);
		SWAP(L4_Word_t, fst_end, snd_end);
	}

	L4_MemoryDesc_t descs[16];
	struct memdescbuf buf = { .ptr = descs, .size = NUM_ELEMENTS(descs) };

	bool ok = mdb_set(&buf, fst_start, fst_end, false,
		L4_ConventionalMemoryType, 0);
	assert(ok);
	ok = mdb_set(&buf, snd_start, snd_end, false,
		L4_ConventionalMemoryType, 0);
	if(!ok(ok && buf.len == 1, "merged into single item")) {
		diag("ok=%s, buf.len=%u", btos(ok), (unsigned)buf.len);
	}
	ok(ok && q_range_present(&buf, MIN(L4_Word_t, fst_start, snd_start),
			MAX(L4_Word_t, fst_end, snd_end), false, false,
			L4_ConventionalMemoryType),
		"full range is present");
}
END_TEST


void ktest_memdesc(void)
{
	RUN(basic_empty_query);
	RUN(basic_simple_query);
	RUN(basic_full_enum);

	RUN(basic_zero_buf);
	RUN(basic_modify);
	RUN(merge_left_right_between);
	RUN(merge_inside);
}

#endif
