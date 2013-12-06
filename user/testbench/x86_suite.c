
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

/* returned from INT3 */
#define INT3_EAX_FILLER 0xf00dcafe


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

	int32_t exn_count;
	L4_Word_t last_ip, last_sp, last_eax, last_ec;
	L4_ThreadId_t last_exn_tid;
};


static struct ex_ctx *ex_ctx(void)
{
	static int ex_key = -1;

	if(ex_key == -1) tsd_key_create(&ex_key, &free);
	void *ptr = tsd_get(ex_key);
	if(ptr == NULL) {
		struct ex_ctx *ctx = malloc(sizeof(*ctx));
		ptr = ctx;
		*ctx = (struct ex_ctx){
			.running = true, .last_int = -1, .exn_count = -1,
		};
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


static int32_t x86_exh_last_exn_detail(
	L4_Word_t *tid_p, L4_Word_t *ec_p,
	L4_Word_t *ip_p, L4_Word_t *sp_p,
	L4_Word_t *eax_p)
{
	struct ex_ctx *c = ex_ctx();
	*tid_p = c->last_exn_tid.raw;
	*ec_p = c->last_ec;
	*ip_p = c->last_ip;
	*sp_p = c->last_sp;
	*eax_p = c->last_eax;
	return c->exn_count;
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

	ctx->last_ip = *eip_p;
	ctx->last_sp = *esp_p;
	ctx->last_eax = *eax_p;
	ctx->last_exn_tid = muidl_get_sender();
	ctx->last_ec = *errorcode_p;
	ctx->exn_count++;

	switch(ctx->last_int) {
		case 0x6:
			/* #UD; stop the task */
			diag("got #UD at ip=%#lx", *eip_p);
			muidl_raise_no_reply();
			break;

		default:
			diag("exception at ip=%#lx, errorcode=%#lx", *eip_p, *errorcode_p);
			if(insn[0] == 0xcd) {
				/* INT imm8 */
				*eip_p += 2;
			} else if(insn[0] == 0xcc) {
				/* INT3 */
				(*eip_p)++;
				*eax_p = INT3_EAX_FILLER;
			} else {
				diag("unhandled insn=%#02x, not replying", insn[0]);
				muidl_raise_no_reply();
			}
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
	.last_exn_detail = &x86_exh_last_exn_detail,
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


/* fork a child, have it start a thread that calls @exn_fn, and sync with the
 * child process after three A_SHORT_NAP sleeps. cleanup is performed on pain
 * of failure.
 *
 * returns the thread ID of the forked process.
 */
static L4_ThreadId_t child_exn_test(
	void (*exn_fn)(void *param),
	void *param_ptr)
{
	L4_ThreadId_t parent_tid = L4_Myself(), child_tid = L4_nilthread;
	int child = fork_tid(&child_tid);
	if(child == 0) {
		L4_ThreadId_t failer = xstart_thread(exn_fn, param_ptr);

		/* sync exit with parent */
		L4_MsgTag_t tag = L4_Receive_Timeout(parent_tid, TEST_IPC_DELAY);
		if(L4_IpcFailed(tag)) {
			diag("child failed, ec=%#lx", L4_ErrorCode());
			exit(1);
		}

		kill_thread(failer);
		exit(0);
	} else {
		/* let the child execute, then sync & wait. */
		for(int i=0; i < 3; i++) L4_Sleep(A_SHORT_NAP);

		L4_LoadMR(0, 0);
		L4_MsgTag_t tag = L4_Send_Timeout(child_tid, TEST_IPC_DELAY);
		IPC_FAIL(tag);

		int st = 0, dead = wait(&st);
		fail_if(dead != child, "expected dead=%d, got %d", child, dead);
	}

	return child_tid;
}


struct exn_dets {
	L4_ThreadId_t tid;
	L4_Word_t ec, ip, sp, eax;
};


static int32_t get_exn_dets(struct exn_dets *out)
{
	int exct = -666;
	int n = __x86_last_exn_detail(x86_exh_tid, &exct, &out->tid.raw,
		&out->ec, &out->ip, &out->sp, &out->eax);
	fail_if(n != 0, "n=%d", n);
	fail_if(exct == -666);

	return exct;
}


static void sigill_fn(void *param)
{
	uint32_t *magic = param;
	L4_Set_ExceptionHandler(x86_exh_tid);
	asm volatile ("ud2" :: "a" (*magic));
	diag("%s: still active after UD2?", __func__);
}


/* illegal instruction exception test. */
START_TEST(illegal_insn_exn)
{
	plan_tests(3);
	const uint32_t magic = 0xb00bfee7;	/* outright bizarre */

	int32_t exct = -666;
	L4_ThreadId_t extid;
	L4_Word_t exec, exip, exsp, exeax;
	int n = __x86_last_exn_detail(x86_exh_tid, &exct, &extid.raw,
		&exec, &exip, &exsp, &exeax);
	fail_if(n != 0, "n=%d", n);
	fail_if(exct == -666);
	fail_if(exeax == magic);

	uint32_t *m_copy = malloc(sizeof(uint32_t));
	*m_copy = magic;
	L4_ThreadId_t child_tid = child_exn_test(&sigill_fn, m_copy);
	free(m_copy);

	const int32_t prev_exct = exct;
	n = __x86_last_exn_detail(x86_exh_tid, &exct, &extid.raw,
		&exec, &exip, &exsp, &exeax);
	fail_if(n != 0, "n=%d", n);

	if(!ok1(exct == prev_exct + 1)) {
		diag("exct=%d, prev_exct=%d", exct, prev_exct);
	}
	if(!ok1(exeax == magic)) {
		diag("exeax=%#x, magic=%#x", exeax, magic);
	}
	/* forkserv assigns the same version to threads within the same process
	 * image. this verifies that the exception came from where it's supposed
	 * to have.
	 */
	if(!ok1(L4_Version(child_tid) == L4_Version(extid))) {
		diag("child_tid=%lu:%lu, extid=%lu:%lu",
			L4_ThreadNo(child_tid), L4_Version(child_tid),
			L4_ThreadNo(extid), L4_Version(extid));
	}
}
END_TEST


static void ineffective_kdb_enter(void *param UNUSED)
{
	L4_Word_t eax_ret = 0;

	L4_Set_ExceptionHandler(x86_exh_tid);
	diag("look ma, no hands");
	asm volatile (
		"/* hand-written test code in x86_suite.c */\n"
		"\ttestb 1f, %%al\n"
		"\tint3\n"
		"\tjmp 2f\n"
		"\tmovl $1f, %%eax\n"
		".section .rodata\n"
		"1:\t.ascii \"mung has a feature where this will do nothing\"\n"
		"\t.byte 0\n"
		".previous\n"
		"2:\n"
		: "=a" (eax_ret)
		:: "cc");
	diag("WE OUT");
	fail_if(eax_ret == INT3_EAX_FILLER, "eax_ret=%#lx", eax_ret);

	/* pop another exception to signify correct return from the previous. */
	asm volatile ("int $0xbb" :: "a" (0xbeefc0de));

	/* FIXME: see FIXME below */
	L4_Sleep(L4_Never);
}


START_TEST(valid_kdb_exn)
{
	plan_tests(3);

	struct exn_dets d;
	const int32_t first_exct = get_exn_dets(&d);

	L4_ThreadId_t child_tid = child_exn_test(&ineffective_kdb_enter, NULL);
	int32_t exct = get_exn_dets(&d);
	if(!ok(exct == first_exct + 1, "one exception occurred")) {
		diag("exct=%d, first_exct=%d", (int)exct, (int)first_exct);
	}
	ok1(L4_Version(child_tid) == L4_Version(d.tid));
	if(!ok1(d.ec == 0xbb * 8 + 2)) {
		diag("ec=%#lx", d.ec);
	}
}
END_TEST


static void invalid_kdb_enter(void *param UNUSED)
{
	L4_Word_t eax_ret;

	L4_Set_ExceptionHandler(x86_exh_tid);
	diag("super happy invalid KDB entry!!!!");
	asm volatile (
		"int3\n"
		"\txorl %%ecx, %%ecx\n"
		: "=a" (eax_ret)
		:: "ecx");
	diag("returned from INT3");
	fail_unless(eax_ret == INT3_EAX_FILLER, "eax_ret=%#lx", eax_ret);

	/* FIXME: remove this sleep once kill_thread() works on threads that've
	 * already exited. *sigh* *paperbag*
	 */
	L4_Sleep(L4_Never);
}


/* execute INT3 that isn't followed by a movl $straddr, %eax. this should
 * cause a #GP as though int $3 had been executed instead.
 */
START_TEST(invalid_kdb_exn)
{
	plan_tests(3);

	struct exn_dets d;
	const int32_t first_exct = get_exn_dets(&d);

	L4_ThreadId_t child_tid = child_exn_test(&invalid_kdb_enter, NULL);
	int32_t exct = get_exn_dets(&d);
	ok1(L4_Version(child_tid) == L4_Version(d.tid));
	if(!ok(exct == first_exct + 1, "exception occurred")) {
		diag("exct=%d, first_exct=%d", (int)exct, (int)first_exct);
	}
	if(!ok(d.ec == 8 * 3 + 2, "errorcode was for int3")) {
		diag("d.ec=%#lx", d.ec);
	}
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
		tcase_add_test(tc, illegal_insn_exn);
		tcase_add_test(tc, valid_kdb_exn);
		tcase_add_test(tc, invalid_kdb_exn);
		suite_add_tcase(s, tc);
	}

	return s;
}
