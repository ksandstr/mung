/* tests for lib/stritem.c */

#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <alloca.h>
#include <ccan/array_size/array_size.h>
#include <ccan/minmax/minmax.h>

#include <l4/types.h>

#include <ukernel/util.h>
#include <ukernel/ktest.h>


#if KTEST

START_TEST(stritemlen_simple)
{
	plan_tests(5);

	/* zero. */
	L4_StringItem_t si0 = L4_StringItem(0, NULL);
	ok1(stritemlen(&si0) == 0);

	/* L4.X2's substring maximum length. */
	L4_StringItem_t si4m = L4_StringItem(4 * 1024 * 1024 - 1, "foo bar");
	if(!ok1(stritemlen(&si4m) == 4 * 1024 * 1024 - 1)) {
		diag("stritemlen returned %d", stritemlen(&si4m));
	}

	/* non-compound substrings. */
	L4_Word_t sbuf[64];
	L4_StringItem_t *si = (L4_StringItem_t *)&sbuf[0];
	/* length 0. */
	*si = L4_StringItem(0, "foo");
	L4_AddSubstringAddressTo(si, "bar");
	L4_AddSubstringAddressTo(si, "zot");
	ok(stritemlen(si) == 0, "stritemlen(3x len=0 non-compound) == 0");
	/* length 3. */
	*si = L4_StringItem(3, "foo");
	L4_AddSubstringAddressTo(si, "bar");
	L4_AddSubstringAddressTo(si, "zot");
	ok(stritemlen(si) == 9, "stritemlen(3x len=3 non-compound) == 9");
	/* length 4M-1. */
	*si = L4_StringItem(4 * 1024 * 1024 - 1, "foo");
	L4_AddSubstringAddressTo(si, "bar");
	L4_AddSubstringAddressTo(si, "zot");
	ok(stritemlen(si) == (4 * 1024 * 1024 - 1) * 3,
		"stritemlen(3x len=max non-compound) == max * 3");
}
END_TEST


/* variables:
 *   - whether there's a third limb in the compound string
 *   - whether there are 0 or 3 additional substrings in the second component
 */
START_LOOP_TEST(stritemlen_compound, iter, 0, 3)
{
	const bool third_limb = CHECK_FLAG(iter, 1),
		extra_substrings = CHECK_FLAG(iter, 2);
	diag("third_limb=%s, extra_substrings=%s",
		btos(third_limb), btos(extra_substrings));
	plan_tests(1);

	int exp = 0;
	L4_Word_t sbuf[64];
	L4_StringItem_t *si = (L4_StringItem_t *)&sbuf[0];
	/* a multi-limbed compound string. */
	*si = L4_StringItem(17, "foo"); exp += 17;
	L4_StringItem_t part = L4_StringItem(3, "bar");
	L4_StringItem_t *three = L4_AddSubstringTo(si, &part); exp += 3;
	if(extra_substrings) {
		L4_AddSubstringAddressTo(si, "zot"); exp += 3;
		L4_AddSubstringAddressTo(three, "qwe"); exp += 3;
		L4_AddSubstringAddressTo(si, "rty"); exp += 3;
	}
	if(third_limb) {
		part = L4_StringItem(22, NULL);
		L4_StringItem_t *twotwo = L4_AddSubstringTo(si, &part); exp += 22;
		L4_AddSubstringAddressTo(twotwo, "uio"); exp += 22;
	}
	if(!ok1(stritemlen(si) == exp)) {
		diag("expecting %d, got %d", exp, stritemlen(si));
	}
}
END_TEST


/* the iterator should emit as many segments as there are substrings in each
 * section of the potentially-compound string item, and the length of those
 * segments should total the stritemlen() value.
 */
START_LOOP_TEST(stritem_iter_basic, iter, 0, 3)
{
	const bool is_compound = CHECK_FLAG(iter, 1);
	const int num_substrings = CHECK_FLAG(iter, 2) ? 7 : 1;
	diag("is_compound=%s, num_substrings=%d",
		btos(is_compound), num_substrings);
	plan_tests(1);

	L4_Word_t sbuf[63];
	L4_StringItem_t *si = (L4_StringItem_t *)&sbuf[0];
	*si = L4_StringItem(7, "foo");
	for(int i=1; i < num_substrings; i++) {
		char *buf = alloca(10);
		memset(buf, 0, 10);
		snprintf(buf, 10, "i=%d", i);
		L4_AddSubstringAddressTo(si, buf);
	}
	if(is_compound) {
		L4_StringItem_t head = L4_StringItem(17, "bar"),
			*out = L4_AddSubstringTo(si, &head);
		for(int i=1; i < num_substrings * 3; i++) {
			char *buf = alloca(17);
			memset(buf, 0, 17);
			snprintf(buf, 17, "i=%d", i);
			L4_AddSubstringAddressTo(out, buf);
		}
	}

	int total = 0;
	struct stritem_iter it;
	stritem_first(&it, si, ARRAY_SIZE(sbuf));
	do {
		diag("total=%d, it.len=%d, it.ptr=`%s'",
			total, it.len, (char *)it.ptr);
		total += it.len;
	} while(stritem_next(&it));

	if(!ok1(stritemlen(si) == total)) {
		diag("stritemlen(si)=%d, total=%d", stritemlen(si), total);
	}
}
END_TEST


void ktest_stritem(void)
{
	/* validation for stritemlen(). */
	RUN(stritemlen_simple);
	RUN(stritemlen_compound);

	/* iteration tests. */
	RUN(stritem_iter_basic);
}

#endif
