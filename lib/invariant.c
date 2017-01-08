
/* function implementations for <ukernel/invariant.h> */

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <stdbool.h>

#include <ccan/list/list.h>

#include <ukernel/invariant.h>


void _inv_ctx_push(struct _inv_ctx *ctx, bool stop, const char *fmt, ...)
{
	if(ctx->first) return;

	va_list al;
	va_start(al, fmt);
	int length = vsnprintf(NULL, 0, fmt, al);
	va_end(al);
	struct _inv_line *line = malloc(sizeof(struct _inv_line) + length + 1);
	if(line == NULL) {
		printf("%s: can't allocate %d bytes!\n", __func__,
			sizeof(struct _inv_line) + length + 1);
		return;
	}
	va_start(al, fmt);
	vsnprintf(line->text, length + 1, fmt, al);
	va_end(al);
	line->stop = stop;
	list_add(&ctx->lines, &line->link);
}


void _inv_ctx_pop(struct _inv_ctx *ctx)
{
	if(ctx->first) return;

	struct _inv_line *line = list_top(&ctx->lines, struct _inv_line, link);
	if(line != NULL) {
		bool stop = line->stop;
		list_del_from(&ctx->lines, &line->link);
		free(line);
		if(!stop) _inv_ctx_pop(ctx);
	}
}


void _inv_report(
	struct _inv_ctx *ctx,
	const char *file, int line,
	const char *condition,
	const char *fmt, ...)
{
	if(ctx->first) {
		longjmp(ctx->redo, 1);
		printf("%s: shouldn't be reached!\n", __func__);
		return;
	}

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
