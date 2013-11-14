
/* libtap-like interface. basically a rewritten version of libtap's tap.c,
 * which carries the following license statement:
 *
 *-
 * Copyright (c) 2004 Nik Clayton
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *-
 *
 * the aggregate work is hereby licensed with identical terms to the ones
 * above, excepting the copyright statement.
 */

/* FIXME: replace calls to abort() to something that returns control to the
 * test harness (i.e. main()) thread.
 */

#if !defined(__KERNEL__) || defined(ENABLE_SELFTEST)

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <stdarg.h>
#include <ccan/likely/likely.h>

#ifndef __KERNEL__
#include "defs.h"
#include "test.h"
#else
#include <ukernel/ktest.h>
#define flush_log(...)
#endif


#define TRY_INIT do { \
		if(unlikely(!tap_init_done)) { \
			tap_reset(); \
			tap_init_done = true; \
		} \
	} while(0)


static bool no_plan, have_plan, skip_all, todo, test_died;
static int num_tests_run, expected_tests, failed_tests;
static char *todo_msg = NULL;		/* must be valid on first reset */

static bool tap_init_done = false;


void tap_reset(void)
{
	/* (yeah, I know) */
	no_plan = have_plan = skip_all = todo = test_died = false;
	num_tests_run = expected_tests = failed_tests = 0;

	free(todo_msg);
	todo_msg = NULL;
}


#ifndef __KERNEL__
void _fail_unless(
	int result,
	const char *file,
	int line,
	const char *expr,
	...)
{
	if(unlikely(!result)) {
		va_list ap;
		char buf[512];
		va_start(ap, expr);
		const char *msg = va_arg(ap, char *);
		if(msg == NULL) msg = expr;
		vsnprintf(buf, sizeof(buf), msg, ap);
		va_end(ap);
		printf("Bail out!  %s (`%s' in %s:%d)\n", buf, expr, file, line);
		flush_log(true);

		exit_on_fail();
	}
}
#endif


int _gen_result(
	bool ok,
	const char *func,
	const char *file,
	unsigned int line,
	const char *test_name_fmt,
	...)
{

	TRY_INIT;
	num_tests_run++;

	char name_buf[512];	/* that's six lines of 80 bytes each. */
	const char *test_name = NULL;
	if(test_name_fmt != NULL) {
		va_list al;
		va_start(al, test_name_fmt);
		vsnprintf(name_buf, sizeof(name_buf), test_name_fmt, al);
		va_end(al);
		test_name = name_buf;

		bool found_ok = false;
		for(int i=0; test_name[i] != '\0'; i++) {
			char c = test_name[i];
			if((c < '0' || c > '9') && c != ' ' && c != '\t'
				&& c != '\n' && c != '\r')
			{
				found_ok = true;
				break;
			}
		}
		if(!found_ok) {
			diag("    You named your test `%s'. You shouldn't use numbers for your test names.",
				test_name);
			diag("    Very confusing.");
		}
	}

	if(!ok) {
		printf("not ");
		failed_tests++;
	}
	printf("ok %d", num_tests_run);

	if(test_name != NULL) {
		int len = strlen(test_name);
		char escbuf[len * 2 + 1];
		for(int i=0, p=0; i < len; i++) {
			if(test_name[i] != '#') escbuf[p++] = test_name[i];
			else {
				escbuf[p++] = '\\';
				escbuf[p++] = '#';
			}
			if(i + 1 == len) escbuf[p++] = '\0';
		}
		printf(" - %s", escbuf);
	}

	if(todo) {
		printf(" # TODO %s", todo_msg);
		if(!ok) failed_tests--;
	}

	printf("\n");

	if(!ok) {
		diag("    Failed %stest (%s:%s() at line %d)",
			todo ? "(TODO) " : "", file, func, line);
	}

	return ok ? 1 : 0;
}


void plan_no_plan(void)
{
	TRY_INIT;
	if(have_plan) {
		fprintf(stderr, "You tried to plan twice!\n");
		test_died = true;
		abort();
	}

	have_plan = true;
	no_plan = true;
}


void plan_skip_all(const char *reason)
{
	TRY_INIT;
	if(have_plan) {
		fprintf(stderr, "You tried to plan twice!\n");
		test_died = true;
		abort();
	}

	have_plan = true;
	skip_all = true;

	printf("1..0");
	if(reason != NULL) printf(" # Skip %s", reason);
	printf("\n");

	/* FIXME: do a longjmp-equivalent to the top level. or exit the thread. or
	 * something.
	 */
}


void plan_tests(unsigned int num_tests)
{
	TRY_INIT;
	if(have_plan) {
		fprintf(stderr, "You tried to plan twice!\n");
		test_died = true;
		abort();
	}

	if(num_tests == 0) {
		fprintf(stderr, "You said to run 0 tests! You've got to run something.\n");
		test_died = true;
		abort();
	}

	have_plan = 1;
	printf("1..%u\n", num_tests);
	expected_tests = num_tests;
}


int diag(const char *fmt, ...)
{
	fprintf(stderr, "# ");
	va_list al;
	va_start(al, fmt);
	char msg[512];
	vsnprintf(msg, sizeof(msg), fmt, al);
	fprintf(stderr, "%s", msg);
	va_end(al);
	fprintf(stderr, "\n");

	return 0;
}


int skip(unsigned int num_skip, const char *reason, ...)
{
	char msg[512];
	va_list al;
	va_start(al, reason);
	vsnprintf(msg, sizeof(msg), reason, al);
	va_end(al);
	for(unsigned int i = 0; i < num_skip; i++) {
		num_tests_run++;
		printf("ok %d # skip %s\n", num_tests_run, msg);
	}

	return 1;
}


void todo_start(const char *fmt, ...)
{
	char msg[512];
	va_list al;
	va_start(al, fmt);
	int len = vsnprintf(msg, sizeof(msg), fmt, al);
	va_end(al);

	/* FIXME: this may leave todo_msg NULL. it should be replaced with a
	 * "tap.c malloc() issue" type message, as in libtap.
	 */
	free(todo_msg);
	todo_msg = malloc(len + 1);
	if(todo_msg != NULL) {
		memcpy(todo_msg, msg, len + 1);
	}

	todo = true;
}


void todo_end(void)
{
	todo = false;
	free(todo_msg);
	todo_msg = NULL;
}


int exit_status(void)
{
	if(no_plan || !have_plan) return failed_tests;
	else if(expected_tests < num_tests_run) {
		/* ran too many. return number of unplanned tests */
		return num_tests_run - expected_tests;
	} else {
		/* failed + didn't run */
		return failed_tests + expected_tests - num_tests_run;
	}
}

#endif
