
/* in-kernel tests for lib/vsnprintf.c */

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <limits.h>
#include <ctype.h>

#include <ukernel/util.h>
#include <ukernel/ktest.h>


#if KTEST

static bool check_digits(const char *buf, uint64_t acc)
{
	bool digits_ok = true;
	int pos = strlen(buf);
	while(acc > 0 && --pos >= 0 && buf[pos] != '-') {
		if(!isdigit(buf[pos]) || buf[pos] - '0' != acc % 10) {
			diag("buf[%d]=`%c' (expected %d)",
				pos, buf[pos], (int)(acc % 10));
			digits_ok = false;
		}
		acc /= 10;
	}
	return digits_ok;
}


/* encode various signed int values, see if they decode back properly. */
START_TEST(signed_int)
{
	static const signed int testvals[] = {
		0, 1, -1, 100, -100, 255, -255, 256, -256,
		65535, -65535, 65536, -65536,
		INT_MAX, INT_MIN,
	};

	plan_tests(NUM_ELEMENTS(testvals) * 3);

	for(int t = 0; t < NUM_ELEMENTS(testvals); t++) {
		const signed int val = testvals[t];
		char buf[100];
		int n = snprintf(buf, sizeof(buf), "%d", val);
		diag("val=%d, n=%d, buf=`%s'", val, n, buf);
		ok1(n > 0);
		ok1(val >= 0 || buf[0] == '-');
		ok1(check_digits(buf, llabs(val)));
	}
}
END_TEST


/* same, for signed long ints. */
START_TEST(signed_long_int)
{
	static const signed long int testvals[] = {
		0, 1, -1, 100, -100, 255, -255, 256, -256,
		65535, -65535, 65536, -65536,
		INT_MAX, INT_MIN, LONG_MAX, LONG_MIN,
	};

	plan_tests(NUM_ELEMENTS(testvals) * 3);

	for(int t = 0; t < NUM_ELEMENTS(testvals); t++) {
		const signed long int val = testvals[t];
		char buf[100];
		int n = snprintf(buf, sizeof(buf), "%ld", val);
		diag("val=%ld, n=%d, buf=`%s'", val, n, buf);
		ok1(n > 0);
		ok1(val >= 0 || buf[0] == '-');
		ok1(check_digits(buf, llabs(val)));
	}
}
END_TEST


/* same, for signed long long ints. */
START_TEST(signed_long_long_int)
{
	static const signed long long int testvals[] = {
		0, 1, -1, 100, -100, 255, -255, 256, -256,
		65535, -65535, 65536, -65536,
		INT_MAX, INT_MIN, LONG_MAX, LONG_MIN, LLONG_MAX, LLONG_MIN,
	};

	plan_tests(NUM_ELEMENTS(testvals) * 3);

	for(int t = 0; t < NUM_ELEMENTS(testvals); t++) {
		const signed long long int val = testvals[t];
		char buf[100];
		int n = snprintf(buf, sizeof(buf), "%lld", val);
		diag("val=%lld, n=%d, buf=`%s'", val, n, buf);
		ok1(n > 0);
		ok1(val >= 0 || buf[0] == '-');
		ok1(check_digits(buf, llabs(val)));
	}
}
END_TEST


/* same, for unsigned long long ints. */
START_TEST(unsigned_long_long_int)
{
	static const unsigned long long int testvals[] = {
		0, 1, 100, 255, 256, 65535, 65536,
		INT_MAX, UINT_MAX, LONG_MAX, ULONG_MAX, ULLONG_MAX,
	};

	plan_tests(NUM_ELEMENTS(testvals) * 3);

	for(int t = 0; t < NUM_ELEMENTS(testvals); t++) {
		const unsigned long long int val = testvals[t];
		char buf[100];
		int n = snprintf(buf, sizeof(buf), "%llu", val);
		diag("val=%llu, n=%d, buf=`%s'", val, n, buf);
		ok1(n > 0);
		ok1(val >= 0 || buf[0] == '-');
		ok1(check_digits(buf, val));
	}
}
END_TEST


/* TODO: add tests for something besides the various sizes of integer. */
void ktest_vsnprintf(void)
{
	RUN(signed_int);
	RUN(signed_long_int);
	RUN(signed_long_long_int);
	RUN(unsigned_long_long_int);
}

#endif
