
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
#include <stdint.h>
#include <string.h>
#include <stdbool.h>
#include <stdarg.h>
#include <assert.h>
#include <ccan/likely/likely.h>
#include <ccan/htable/htable.h>

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


struct saved_ctx
{
	struct saved_ctx *next;
	int depth;

	bool no_plan, have_plan, skip_all, todo, test_died, no_plan_closed;
	int num_tests_run, expected_tests, failed_tests;
	char *todo_msg;

	char testmsg[];
};


struct subtest_msg {
	pid_t pid;
	char *msg;
};


static size_t rehash_subtest_msg(const void *ptr, void *unused);
extern uint32_t int_hash(int x);	/* FIXME: get from header */


static bool tap_init_done = false;

/* these are reset in tap_reset(). */
static struct htable subtest_msg_ht = HTABLE_INITIALIZER(
	subtest_msg_ht, &rehash_subtest_msg, NULL);
static struct saved_ctx *save_stack = NULL;		/* for subtests */

/* these are saved and restored by the subtest mechanism. */
static bool no_plan, have_plan, skip_all, todo, test_died, no_plan_closed;
static int num_tests_run, expected_tests, failed_tests;
static char *todo_msg = NULL;		/* must be valid on first reset */


static void reset_state(void)
{
	/* (yeah, I know) */
	no_plan = have_plan = skip_all = todo = test_died = false;
	no_plan_closed = false;
	num_tests_run = expected_tests = failed_tests = 0;
}


static size_t rehash_subtest_msg(const void *ptr, void *unused) {
	const struct subtest_msg *msg = ptr;
	return int_hash(msg->pid);
}


void tap_reset(void)
{
	reset_state();
	while(save_stack != NULL) {
		struct saved_ctx *next = save_stack->next;
		free(save_stack->todo_msg);
		free(save_stack);
		save_stack = next;
	}

	free(todo_msg);
	todo_msg = NULL;

	struct htable_iter it;
	for(struct subtest_msg *ptr = htable_first(&subtest_msg_ht, &it);
		ptr != NULL; ptr = htable_next(&subtest_msg_ht, &it))
	{
		free(ptr->msg);
		free(ptr);
	}
	htable_clear(&subtest_msg_ht);
}


void subtest_start(const char *fmt, ...)
{
	TRY_INIT;
	va_list al;
	va_start(al, fmt);
	int msglen = vsnprintf(NULL, 0, fmt, al) + 1;
	va_end(al);

	struct saved_ctx *c = malloc(sizeof *c + msglen);
	va_start(al, fmt);
	vsnprintf(c->testmsg, msglen, fmt, al);
	va_end(al);

	c->depth = save_stack == NULL ? 1 : save_stack->depth + 1;
	c->no_plan = no_plan;
	c->have_plan = have_plan;
	c->skip_all = skip_all;
	c->todo = todo;
	c->test_died = test_died;
	c->num_tests_run = num_tests_run;
	c->expected_tests = expected_tests;
	c->failed_tests = failed_tests;
	c->todo_msg = todo_msg;
	c->no_plan_closed = no_plan_closed;
	todo_msg = NULL;	/* subtests get a clean slate */

	c->next = save_stack;
	save_stack = c;

	reset_state();
}


char *subtest_pop(int *rc_p, void **freeptr_p)
{
	if(save_stack == NULL) {
		diag("%s: save_stack == NULL", __func__);
		abort();
	}

	close_no_plan();
	int exst = exit_status();
	struct saved_ctx *c = save_stack;
	save_stack = c->next;

	/* (there's likely a nicer way to do this, but, meh.) */
	no_plan = c->no_plan;
	have_plan = c->have_plan;
	skip_all = c->skip_all;
	todo = c->todo;
	test_died = c->test_died;
	num_tests_run = c->num_tests_run;
	expected_tests = c->expected_tests;
	failed_tests = c->failed_tests;
	no_plan_closed = c->no_plan_closed;
	free(todo_msg); todo_msg = c->todo_msg;

	if(exst != 0) diag("subtest exit_status=%d", exst);
	if(rc_p != NULL) *rc_p = exst;
	if(freeptr_p != NULL) *freeptr_p = c;
	return c->testmsg;
}


int subtest_end(void)
{
	int rc;
	void *freeptr;
	char *msg = subtest_pop(&rc, &freeptr);
	/* "rc" is value of exit_status(), 0 when all tests passed. */
	ok(rc == 0, "%s", msg);
	free(freeptr);
	return rc;
}


void stash_subtest_msg(pid_t pid, char *msgstr)
{
	struct subtest_msg *msg = malloc(sizeof *msg);
	assert(msg != NULL);
	msg->pid = pid;
	msg->msg = msgstr;
	bool ok = htable_add(&subtest_msg_ht, rehash_subtest_msg(msg, NULL), msg);
	assert(ok);
}


const char *fetch_subtest_msg(pid_t pid, const char *orelse)
{
	size_t hash = int_hash(pid);
	struct htable_iter it;
	for(struct subtest_msg *cand = htable_firstval(&subtest_msg_ht, &it, hash);
		cand != NULL; cand = htable_nextval(&subtest_msg_ht, &it, hash))
	{
		if(cand->pid == pid) return cand->msg;
	}
	return orelse;
}


static void print_sub_prefix(void)
{
	if(save_stack != NULL) {
		for(int i=0; i < save_stack->depth; i++) printf("    ");
	}
}


#ifndef __KERNEL__
void _fail_unless( int result,
	const char *file, int line,
	const char *expr, ...)
{
	if(unlikely(!result)) {
		va_list ap;
		char buf[512];
		va_start(ap, expr);
		const char *msg = va_arg(ap, char *);
		if(msg == NULL) msg = expr;
		vsnprintf(buf, sizeof(buf), msg, ap);
		va_end(ap);
		/* NOTE: bailouts don't get a subtest prefix. */
		printf("Bail out!  %s (`%s' in %s:%d)\n", buf, expr, file, line);
		flush_log(true);

		exit_on_fail();
	}
}
#endif


int _gen_result(bool ok,
	const char *func, const char *file, unsigned int line,
	const char *test_name_fmt, ...)
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

	print_sub_prefix();
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


void plan_skip_all(const char *reason, ...)
{
	TRY_INIT;
	if(have_plan) {
		fprintf(stderr, "You tried to plan twice!\n");
		test_died = true;
		abort();
	}

	have_plan = true;
	skip_all = true;

	print_sub_prefix();
	printf("1..0");
	if(reason != NULL) {
		char rbuf[200];
		va_list args; va_start(args, reason);
		int l = vsnprintf(rbuf, sizeof rbuf, reason, args);
		va_end(args);
		while(rbuf[--l] == '\n') rbuf[l] = '\0';
		printf(" # Skip %s", rbuf);
	}
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
	print_sub_prefix();
	printf("1..%u\n", num_tests);
	expected_tests = num_tests;
}


int diag(const char *fmt, ...)
{
	TRY_INIT;
	print_sub_prefix();
	va_list al;
	va_start(al, fmt);
	char msg[512];
	vsnprintf(msg, sizeof(msg), fmt, al);
	fprintf(stderr, "# %s\n", msg);
	va_end(al);

	return 0;
}


int skip(unsigned int num_skip, const char *reason, ...)
{
	TRY_INIT;
	char msg[512];
	va_list al;
	va_start(al, reason);
	vsnprintf(msg, sizeof(msg), reason, al);
	va_end(al);
	for(unsigned int i = 0; i < num_skip; i++) {
		num_tests_run++;
		print_sub_prefix();
		printf("ok %d # skip %s\n", num_tests_run, msg);
	}

	return 1;
}


void todo_start(const char *fmt, ...)
{
	TRY_INIT;
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
	TRY_INIT;
	if(no_plan || !have_plan) return failed_tests;
	else if(expected_tests < num_tests_run) {
		/* ran too many. return number of unplanned tests */
		return num_tests_run - expected_tests;
	} else {
		/* failed + didn't run */
		return failed_tests + expected_tests - num_tests_run;
	}
}


void close_no_plan(void)
{
	if(no_plan_closed) return;
	if(!have_plan || no_plan) {
		print_sub_prefix();
		printf("1..%d\n", num_tests_run);
		expected_tests = num_tests_run;
		if(!have_plan) {
			/* discourage an implicit lazy plan */
			diag("    No plan until end of test?");
			have_plan = true;	/* shouldn't plan again. */
		}
	}
	no_plan_closed = true;
}


#endif
