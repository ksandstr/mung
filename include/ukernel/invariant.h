
#ifndef SEEN_UKERNEL_INVARIANT_H
#define SEEN_UKERNEL_INVARIANT_H

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <stdbool.h>

#include <ccan/list/list.h>
#include <ccan/likely/likely.h>

/* utility macros and so forth for invariant testing. everything here defines
 * to empty when NDEBUG is set.
 */

struct _inv_ctx
{
#ifndef NDEBUG
	struct list_head lines;
#endif
};

struct _inv_line {
#ifndef NDEBUG
	struct list_node link;
	char text[];
#endif
};


static inline void _inv_ctx_push(struct _inv_ctx *ctx, const char *fmt, ...)
	__attribute__((format(printf, 2, 3)));

static inline void _inv_ctx_push(struct _inv_ctx *ctx, const char *fmt, ...)
{
#ifndef NDEBUG
	va_list al;
	va_start(al, fmt);
	int length = vsnprintf(NULL, 0, fmt, al);
	va_end(al);
	struct _inv_line *line = malloc(sizeof(struct _inv_line) + length + 1);
	if(line == NULL) {
		printf("%s: can't allocated %d bytes!\n", __func__,
			sizeof(struct _inv_line) + length + 1);
		return;
	}
	va_start(al, fmt);
	vsnprintf(line->text, length + 1, fmt, al);
	va_end(al);
	list_add(&ctx->lines, &line->link);
#endif
}


static inline void _inv_ctx_pop(struct _inv_ctx *ctx)
{
#ifndef NDEBUG
	struct _inv_line *line = list_top(&ctx->lines, struct _inv_line, link);
	if(line != NULL) {
		list_del_from(&ctx->lines, &line->link);
		free(line);
	} else {
		printf("%s: unmatched (returns to %p)\n", __func__,
			__builtin_return_address(0));
	}
#endif
}

static inline void _inv_report(
	struct _inv_ctx *ctx,
	const char *file,
	int line,
	const char *condition,
	const char *fmt,
	...)
	__attribute__((format(printf, 5, 6)));

static inline void _inv_report(
	struct _inv_ctx *ctx,
	const char *file,
	int line,
	const char *condition,
	const char *fmt,
	...)
{
	va_list al;
	va_start(al, fmt);
	int length = vsnprintf(NULL, 0, fmt, al);
	va_end(al);
	char buf[length + 1];
	va_start(al, fmt);
	vsnprintf(buf, length + 1, fmt, al);
	va_end(al);
	printf("invariant `%s' failed in %s:%d: %s\n", condition,
		file, line, buf);

	/* earliest-to-latest means back-to-front. */
	struct _inv_line *cur, *next;
	list_for_each_rev(&ctx->lines, cur, link) {
		printf("  %s\n", cur->text);
	}
	list_for_each_safe(&ctx->lines, cur, next, link) {
		list_del_from(&ctx->lines, &cur->link);
		free(cur);
	}
}


#ifndef NDEBUG
#define INV_CTX struct _inv_ctx _ictx; list_head_init(&_ictx.lines)
#define inv_push(fmt, ...) _inv_ctx_push(&_ictx, (fmt), ##__VA_ARGS__)
#define inv_pop() _inv_ctx_pop(&_ictx)
#define inv_ok1(cond) do { \
		if(unlikely(!(cond))) { \
			_inv_report(&_ictx, __FILE__, __LINE__, #cond, NULL); \
			goto inv_fail; \
		} \
	} while(false)
#define inv_ok(cond, fmt, ...) do { \
		if(unlikely(!(cond))) { \
			_inv_report(&_ictx, __FILE__, __LINE__, #cond, fmt, ##__VA_ARGS); \
			goto inv_fail; \
		} \
	} while(false)
#else
#define inv_ctx
#define inv_push(...)
#define inv_pop()
#define inv_ok1(cond)
#define inv_ok(cond, fmt...)
#endif


#endif
