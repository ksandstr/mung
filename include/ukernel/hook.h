
#ifndef SEEN_UKERNEL_HOOK_H
#define SEEN_UKERNEL_HOOK_H

/* general stacked hook mechanism. */

#include <stdint.h>
#include <stdbool.h>
#include <ccan/list/list.h>


struct hook;
struct hook_fn;

typedef void (*hook_call_t)(
	struct hook *hook,	/* useful for container_of() shenanigans */
	void *param,
	uintptr_t code,
	void *dataptr);		/* per call. per-hook is under @hook */


struct hook
{
	void *dataptr;				/* per hook, general access */

	/* private bits below this line. */
	struct list_head fn_list;	/* of struct hook_fn via @link */
	struct hook_fn *current;
};


#define hook_empty(h) list_empty(&(h)->fn_list)


/* there's no hook_destroy(). instead, do hook_call_{front,back}(...) with a
 * @code or @param that causes the hooks to detach themselves.
 */
extern void hook_init(struct hook *h, void *dataptr);


/* "prepend" */
extern void hook_push_front(struct hook *h, hook_call_t fn, void *dataptr);

/* "append" */
extern void hook_push_back(struct hook *h, hook_call_t fn, void *dataptr);

/* calls all hooks in a front-to-back order, passing @code and @param as
 * given. returns number of hooks called.
 */
extern int hook_call_front(struct hook *h, void *param, uintptr_t code);
/* same, but back-to-front */
extern int hook_call_back(struct hook *h, void *param, uintptr_t code);

/* called from within a hook: removes the current hook even if @remove wasn't
 * true.
 */
extern void hook_detach(struct hook *h);

#endif
