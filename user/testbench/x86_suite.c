
#define X86EXHANDLER_IMPL_SOURCE 1

#include <math.h>
#include <stdint.h>
#include <string.h>
#include <ccan/compiler/compiler.h>

#include <l4/types.h>
#include <l4/ipc.h>
#include <l4/schedule.h>
#include <l4/syscall.h>

#include <ukernel/util.h>

#include "test.h"
#include "defs.h"
#include "x86-suite-defs.h"


#ifdef __MMX__
#include <mmintrin.h>
#endif

#ifdef __SSE__
#include <xmmintrin.h>
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


#ifdef __SSE__
/* a helper thread that sleeps for 2 ms, then alters the SSE rounding mode. */
static void sleep_and_twiddle_sse(void *param UNUSED)
{
	L4_Sleep(L4_TimePeriod(2 * 1000));

	int rmode = _MM_GET_ROUNDING_MODE();
	diag("rmode=%#x", rmode);
	int new_rmode = (rmode + _MM_ROUND_DOWN) & _MM_ROUND_MASK;
	_MM_SET_ROUNDING_MODE(new_rmode);
	diag("rmode'=%#x", new_rmode);
}
#endif


START_LOOP_TEST(sse_cw_restore, iter, 0, 3)
{
#ifndef __SSE__
	plan_skip_all("no SSE support at compile-time");
#else
	plan_tests(1);

	L4_ThreadId_t helper_tid = start_thread(&sleep_and_twiddle_sse, NULL);
	fail_if(L4_IsNilThread(helper_tid));

	static const unsigned modes[] = {
		_MM_ROUND_NEAREST,
		_MM_ROUND_DOWN,
		_MM_ROUND_UP,
		_MM_ROUND_TOWARD_ZERO,
	};

	_MM_SET_ROUNDING_MODE(modes[iter]);
	unsigned pre_mode = _MM_GET_ROUNDING_MODE();
	join_thread(helper_tid);
	unsigned post_mode = _MM_GET_ROUNDING_MODE();
	if(!ok1(pre_mode == post_mode)) {
		diag("mode=%#x mode'=%#x", pre_mode, post_mode);
	}
#endif
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


START_TEST(sse_smoketest)
{
#ifndef __SSE__
	plan_skip_all("no SSE support at compile-time");
#else
	plan_tests(2);

	union {
		float f[4];
		__m128 m;
	} a, b, out;
	for(int i=0; i < 4; i++) {
		a.f[i] = 1.234 * i;
		b.f[i] = 8.9 - (i * 3.778);
	}
	out.m = _mm_add_ps(a.m, b.m);
	pass("didn't die");

	bool still_ok = true;
	for(int i=0; i < 4; i++) {
		float expect = a.f[i] + b.f[i];
		if(fabsf(out.f[i] - expect) > 0.0003) {
			still_ok = false;
			diag("at i=%d, diff > 0.0003", i);
		}
	}
	ok(still_ok, "results consistent");
#endif
}
END_TEST


struct ex_ctx
{
	int last_int, trace_count;
	bool running;
};


static struct ex_ctx *ex_ctx(void)
{
	static int ex_key = -1;

	if(ex_key == -1) tsd_key_create(&ex_key, &free);
	void *ptr = tsd_get(ex_key);
	if(ptr == NULL) {
		struct ex_ctx *ctx = malloc(sizeof(*ctx));
		ptr = ctx;
		*ctx = (struct ex_ctx){ .running = true, .last_int = -1 };
		tsd_set(ex_key, ptr);
	}
	return ptr;
}


static int32_t x86_exh_last_interrupt(void) {
	return ex_ctx()->last_int;
}


static int32_t x86_exh_get_trace_count(void) {
	return ex_ctx()->trace_count;
}


static void x86_exh_quit(void) {
	ex_ctx()->running = false;
}


static void x86_exh_handle_exn(
	L4_Word_t *eip_p,
	L4_Word_t *eflags_p,
	L4_Word_t *exception_no_p,
	L4_Word_t *errorcode_p,
	L4_Word_t *edi_p, L4_Word_t *esi_p, L4_Word_t *ebp_p, L4_Word_t *esp_p,
	L4_Word_t *ebx_p, L4_Word_t *edx_p, L4_Word_t *ecx_p, L4_Word_t *eax_p)
{
	struct ex_ctx *ctx = ex_ctx();
	const uint8_t *insn = (uint8_t *)*eip_p;

	ctx->last_int = *errorcode_p >> 3;

	switch(ctx->last_int) {
		default:
			/* assume INT imm8 */
			diag("exception at ip=%#lx, errorcode=%#lx", *eip_p, *errorcode_p);
			fail_unless(insn[0] == 0xcd, "insn=%#02x", insn[0]);
			*eip_p += 2;
			break;

		case 0x24:
			/* enable tracing. */
			diag("enabling tracing (old tf=%s)",
				btos(CHECK_FLAG(*eflags_p, 1 << 8)));
			*eflags_p |= 1 << 8;	/* set TF */
			*eip_p += 2;
			break;

		case 0x25:
			/* disable it. */
			diag("disabling tracing (old tf=%s)",
				btos(CHECK_FLAG(*eflags_p, 1 << 8)));
			*eflags_p &= ~(1 << 8);
			*eip_p += 2;
			break;

		case 1:
			/* the tracing debug vector! */
			ctx->trace_count++;
			diag("trace, eip'=%#lx", *eip_p);
			/* eip advanced by tracing mechanism. */
			break;
	}
}


static const struct x86ex_handler_vtable exh_vtable = {
	.last_interrupt = &x86_exh_last_interrupt,
	.get_trace_count = &x86_exh_get_trace_count,
	.sys_exception = &x86_exh_handle_exn,
	.arch_exception = &x86_exh_handle_exn,
	.quit = &x86_exh_quit,
};

IDL_FIXTURE(x86_exh, x86ex_handler, &exh_vtable, !ex_ctx()->running);


/* tests interrupt delivery to an exception handler thread. */
START_TEST(int_exn)
{
	plan_tests(3);

	/* part 1: there should not be a previous exception at the start. */
	int last_int, n = __x86_last_interrupt(x86_exh_tid, &last_int);
	fail_if(n != 0, "n=%d", n);
	ok1(last_int < 0);

	/* part 2: raising exception 0x23 should change last_int to 0x23. */
	L4_ThreadId_t old_exh = L4_ExceptionHandler();
	L4_Set_ExceptionHandler(x86_exh_tid);
	asm volatile ("int $0x23");
	n = __x86_last_interrupt(x86_exh_tid, &last_int);
	if(!ok1(last_int == 0x23)) diag("last_int=%#x", last_int);

	/* part 3: similarly, raising exception 0x69 should do the same. */
	asm volatile ("int $0x69");
	n = __x86_last_interrupt(x86_exh_tid, &last_int);
	if(!ok1(last_int == 0x69)) diag("last_int=%#x", last_int);

	L4_Set_ExceptionHandler(old_exh);
}
END_TEST


START_TEST(trace_exn)
{
	plan_tests(2);

	/* part 1: no tracing events, yet. */
	int tct, n = __x86_get_trace_count(x86_exh_tid, &tct);
	fail_if(n != 0, "n=%d", n);
	ok1(tct == 0);

	/* part 2: there should be two tracing events, as there are two
	 * instructions between the controlling interrupts.
	 */
	L4_ThreadId_t old_exh = L4_ExceptionHandler();
	L4_Set_ExceptionHandler(x86_exh_tid);
	asm volatile ("int $0x24");		/* enable tracing */
	asm volatile ("xorl %%eax, %%eax" ::: "eax");
	asm volatile ("xorl %%ebx, %%ebx" ::: "ebx");
	asm volatile ("int $0x25");		/* disable it */
	tct = 0; n = __x86_get_trace_count(x86_exh_tid, &tct);
	fail_if(n != 0, "n=%d", n);
	diag("tct=%d", tct);
	ok1(tct > 0);

	L4_Set_ExceptionHandler(old_exh);
}
END_TEST


struct Suite *x86_suite(void)
{
	Suite *s = suite_create("x86");

	/* smoke testing */
	{
		TCase *tc = tcase_create("smoke");
		tcase_add_test(tc, mmx_smoketest);
		tcase_add_test(tc, sse_smoketest);
		suite_add_tcase(s, tc);
	}

	/* FPU/SSE context tests */
	{
		TCase *tc = tcase_create("hwctx");
		tcase_add_test(tc, fpu_cw_restore);
		tcase_add_test(tc, sse_cw_restore);
		suite_add_tcase(s, tc);
	}

	/* exception handling tests */
	{
		TCase *tc = tcase_create("exn");
		ADD_IDL_FIXTURE(tc, x86_exh);
		tcase_add_test(tc, int_exn);
		tcase_add_test(tc, trace_exn);
		suite_add_tcase(s, tc);
	}

	return s;
}
