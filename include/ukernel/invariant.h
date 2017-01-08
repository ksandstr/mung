
#ifndef SEEN_UKERNEL_INVARIANT_H
#define SEEN_UKERNEL_INVARIANT_H

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <stdbool.h>
#include <setjmp.h>

#include <ccan/list/list.h>
#include <ccan/likely/likely.h>



/* support for invariant checking. does nothing when NDEBUG is set. */

struct _inv_ctx {
	jmp_buf redo;
	struct list_head lines;
	bool first;
};

struct _inv_line {
	struct list_node link;
	bool stop;
	char text[];
};


extern void _inv_ctx_push(
	struct _inv_ctx *ctx,
	bool stop,
	const char *fmt, ...)
		__attribute__((format(printf, 3, 4)));

extern void _inv_ctx_pop(struct _inv_ctx *ctx);

extern void _inv_report(
	struct _inv_ctx *ctx,
	const char *file, int line,
	const char *condition,
	const char *fmt, ...)
	__attribute__((format(printf, 5, 6)));


#ifndef NDEBUG

#define INV_CTX struct _inv_ctx _icmem, *_ictx = &_icmem; \
	_ictx->first = true; \
	list_head_init(&_ictx->lines); \
	if(unlikely(setjmp(_ictx->redo) != 0)) { \
		printf("%s: re-running failed invariant check\n", __func__); \
		_ictx->first = false; \
	}
#define inv_push(fmt, ...) _inv_ctx_push(_ictx, true, (fmt), ##__VA_ARGS__)
#define inv_log(fmt, ...) _inv_ctx_push(_ictx, false, (fmt), ##__VA_ARGS__)
#define inv_pop() _inv_ctx_pop(_ictx)
#define inv_ok1(cond) do { \
		if(unlikely(!(cond))) { \
			_inv_report(_ictx, __FILE__, __LINE__, #cond, NULL); \
			goto inv_fail; \
		} \
	} while(false)
#define inv_ok(cond, fmt, ...) do { \
		if(unlikely(!(cond))) { \
			_inv_report(_ictx, __FILE__, __LINE__, #cond, fmt, ##__VA_ARGS__); \
			goto inv_fail; \
		} \
	} while(false)
#define inv_iff1(a, b) inv_ok((a) == (b), "%s iff %s", #a, #b)
#define inv_imply1(a, b) inv_ok(!(a) || (b), "%s --> %s", #a, #b)

#define INVCTX_ARG struct _inv_ctx *_ictx
#define INV_CHILD _ictx

#else

#define INV_CTX
#define inv_push(...)
#define inv_log(...)
#define inv_pop()
#define inv_ok1(cond)
#define inv_ok(cond, fmt...)
#define inv_iff1(a, b)
#define inv_imply1(a, b)

#define INVCTX_ARG int __foo
#define INV_CHILD 0

#endif


#endif
