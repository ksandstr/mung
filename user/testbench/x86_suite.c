
#include <math.h>
#include <stdint.h>
#include <string.h>

#include <l4/types.h>
#include <l4/ipc.h>
#include <l4/schedule.h>
#include <l4/syscall.h>

#include <ccan/compiler/compiler.h>

#include "test.h"
#include "defs.h"


#ifdef __MMX__
#include <mmintrin.h>
#endif


/* rounding mode field in the FPU control word (i.e. {11:10}) */
#define X86_FPUCW_RND_EVEN 0
#define X86_FPUCW_RND_DOWN 1
#define X86_FPUCW_RND_UP 2
#define X86_FPUCW_RND_TRUNC 3


static inline uint16_t x86_fpu_getcw(void) {
	uint16_t ax;
	asm volatile ("fnstcw %0" : "=m" (ax));
	return ax;
}


static inline void x86_fpu_setcw(uint16_t new_cw) {
	asm volatile ("fldcw %0" :: "m" (new_cw));
}


/* a helper thread that sleeps for 2 ms, then alters the FPU rounding mode. */
static void sleep_and_twiddle(void *param UNUSED)
{
	L4_Sleep(L4_TimePeriod(2 * 1000));

	uint16_t cw = x86_fpu_getcw();
	// diag("cw=%#04x", cw);
	int rmode = (cw >> 9) & 0x3;
	rmode = (rmode + 1) & 0x3;
	cw &= ~0x0c00;
	cw |= rmode << 9;
	x86_fpu_setcw(cw);
	// diag("cw'=%#04x", cw);
}


START_LOOP_TEST(fpu_cw_restore, iter, 0, 3)
{
	plan_tests(1);

	L4_ThreadId_t helper_tid = start_thread(&sleep_and_twiddle, NULL);
	fail_if(L4_IsNilThread(helper_tid));

	uint16_t cw = x86_fpu_getcw();
	cw &= ~0x0c00;
	cw |= iter << 9;
	x86_fpu_setcw(cw);
	fail_unless(x86_fpu_getcw() == cw);

	join_thread(helper_tid);
	uint16_t post_cw = x86_fpu_getcw();
	if(!ok1(cw == post_cw)) {
		diag("pre_cw=%#04x post_cw=%#04x", cw, post_cw);
	}
}
END_TEST


START_TEST(mmx_smoketest)
{
#ifndef __MMX__
	plan_skip_all("no MMX support at compile-time");
#else
	plan_tests(2);

	union {
		uint8_t o[8];
		__m64 m;
	} a, b, out;
	for(int i=0; i < 8; i++) {
		a.o[i] = 0x4a - i;
		b.o[i] = i + 0xd6;
	}
	out.m = _mm_add_pi8(a.m, b.m);
	_mm_empty();
	pass("didn't die");

	bool still_ok = true;
	for(int i=0; i < 8; i++) {
		uint8_t expect = a.o[i] + b.o[i];
		if(out.o[i] != expect) {
			still_ok = false;
			diag("i=%d: out=%#02x, expected %#02x", i, out.o[i], expect);
		}
	}
	ok(still_ok, "results consistent");
#endif
}
END_TEST


struct Suite *x86_suite(void)
{
	Suite *s = suite_create("x86");

	/* smoke testing */
	{
		TCase *tc = tcase_create("smoke");
		tcase_add_test(tc, mmx_smoketest);
		suite_add_tcase(s, tc);
	}

	/* FPU context tests */
	{
		TCase *tc = tcase_create("fpuctx");
		tcase_add_test(tc, fpu_cw_restore);
		suite_add_tcase(s, tc);
	}

	return s;
}
