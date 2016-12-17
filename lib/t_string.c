/* tests for lib/string.c */

#include <string.h>

#include <ukernel/ktest.h>


#if KTEST


/* a case of known breakage, iterated over various alignments of the first
 * input.
 */
START_LOOP_TEST(strcmp_breakage_TyTiPtva, iter, 0, 7)
{
	const char *teststr = "TyTiPtva";
	char copybuf[32], othbuf[32], *copy = &copybuf[iter];
	strlcpy(copy, teststr, sizeof(copybuf) - iter);

	diag("align=%d, copy=%p, teststr=%p", iter, copy, teststr);
	plan_tests(1 + 8);

	ok1(strcmp(copy, teststr) == 0);
	for(int oth_align = 0; oth_align < 8; oth_align++) {
		char *oth = &othbuf[oth_align];
		strlcpy(oth, teststr, sizeof(othbuf) - oth_align);
		if(!ok(strcmp(copy, oth) == 0, "copy == oth [align=%d]", oth_align)) {
			diag("oth=%p", oth);
		}
	}
}
END_TEST


void ktest_string(void)
{
	RUN(strcmp_breakage_TyTiPtva);
}

#endif
