/* tests for lib/string.c */

#include <stdlib.h>
#include <string.h>
#include <ccan/array_size/array_size.h>

#include <ukernel/ktest.h>


#if KTEST


/* a case of known breakage, iterated over various alignments of the first
 * input.
 */
START_LOOP_TEST(strcmp_breakage_TyTiPtva, iter, 0, 7)
{
	const char *teststr = "TyTiPtva";
	char copybuf[32], othbuf[32], *copy = &copybuf[iter];
	strscpy(copy, teststr, sizeof(copybuf) - iter);

	diag("align=%d, copy=%p, teststr=%p", iter, copy, teststr);
	plan_tests(1 + 8);

	ok1(strcmp(copy, teststr) == 0);
	for(int oth_align = 0; oth_align < 8; oth_align++) {
		char *oth = &othbuf[oth_align];
		strscpy(oth, teststr, sizeof(othbuf) - oth_align);
		if(!ok(strcmp(copy, oth) == 0, "copy == oth [align=%d]", oth_align)) {
			diag("oth=%p", oth);
		}
	}
}
END_TEST


/* tests on memchr(3). */

/* positive match at various positions.
 *
 * iter variables:
 *   - starting offset within a page-aligned buffer
 *   - high bit of test character
 *   - swapping test and fill character ('\0')
 */
START_LOOP_TEST(memchr_positive, iter, 0, 15)
{
	static const int positions[] = { 0, 1, 2, 256, 777, 1021, 1022, 1023 };
	const int start_offset = iter & 3,
		actual_char = (~iter & 4) ? 'w' : 0xf7,
		test_char = (~iter & 8) ? '\0' : actual_char,
		fill_char = (iter & 8) ? '\0' : actual_char;
	diag("start_offset=%d, test_char=%#02x, fill_char=%#02x",
		start_offset, test_char, fill_char);
	plan_tests(ARRAY_SIZE(positions));

	unsigned char *raw = aligned_alloc(1 << 12, 2 * 1024),
		*mem = raw + start_offset;

	for(int i=0; i < ARRAY_SIZE(positions); i++) {
		const int pos = positions[i];
		subtest_start("pos=%d", pos);
		plan_tests(4);
		memset(raw, fill_char, 2 * 1024);
		ok(memchr(mem, test_char, 1024) == NULL, "before set");
		mem[pos] = test_char;
		void *ret = memchr(mem, test_char, 1024);
		ok1(ret != NULL);
		ok1(ret == mem + pos);
		ok1(*(unsigned char *)ret == test_char);
		subtest_end();
	}

	free(raw);
}
END_TEST


/* negative match with matching chars before the start and past the end.
 * variables the same as in memchr_positive.
 */
START_LOOP_TEST(memchr_negative, iter, 0, 15)
{
	static const int positions[] = { -3, -2, -1, 512, 513, 514 };
	const int start_offset = iter & 3,
		actual_char = (~iter & 4) ? 'w' : 0xf7,
		test_char = (~iter & 8) ? '\0' : actual_char,
		fill_char = (iter & 8) ? '\0' : actual_char;
	diag("start_offset=%d, test_char=%#02x, fill_char=%#02x",
		start_offset, test_char, fill_char);
	plan_tests(ARRAY_SIZE(positions));

	unsigned char *raw = aligned_alloc(1 << 12, 8 * 1024),
		*mem = raw + 4096 + start_offset;
	diag("raw=%p, mem=%p", raw, mem);

	for(int i=0; i < ARRAY_SIZE(positions); i++) {
		const int pos = positions[i];
		subtest_start("pos=%d", pos);
		plan_tests(2);
		memset(raw, fill_char, 8 * 1024);
		ok(memchr(mem, test_char, 512) == NULL, "when not set");
		mem[pos] = test_char;
		void *ret = memchr(mem, test_char, 512);
		if(!ok(ret == NULL, "when set")) {
			diag("ret=%p off=%d (%#02x)", ret,
				(int)(ret - (void *)mem), *(unsigned char *)ret);
		}
		subtest_end();
	}

	free(raw);
}
END_TEST


void ktest_string(void)
{
	RUN(strcmp_breakage_TyTiPtva);
	RUN(memchr_positive);
	RUN(memchr_negative);
}

#endif
