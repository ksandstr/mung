
#ifndef SEEN_UKERNEL_HOOK_H
#define SEEN_UKERNEL_HOOK_H

/* general stacked hooks for state machines and the like. main use is in IPC
 * for running a hook when an IPC operation completes such as for processor
 * exceptions, page faults, and string transfer faults.
 */

#include <stdint.h>
#include <stdbool.h>

#include <ccan/list/list.h>
#include <ccan/typesafe_cb/typesafe_cb.h>


struct hook;
struct hook_fn;

/* @h is for container_of() stuff and for accessing @h->private; @param and
 * @code are those given to hook_call_{front,back}(), and @priv is from the
 * call to hook_push_{front,back}() according to modern idiom.
 */
typedef void (*hook_call_t)(
	struct hook *h, void *param, uintptr_t code, void *priv);


struct hook
{
	void *dataptr;				/* per hook */
	struct list_head fn_list;	/* of struct hook_fn via @link */
	struct hook_fn *current;
};


#define hook_empty(h) list_empty(&(h)->fn_list)


/* there's no hook_destroy(). instead, do hook_call_{front,back}(...) with a
 * @code or @param that causes the hooks to detach themselves.
 */
#define HOOK_INIT(h, dataptr_) \
	(struct hook){ .fn_list = LIST_HEAD_INIT((h).fn_list), \
		.dataptr = (dataptr_) }
static inline void hook_init(struct hook *h, void *dataptr) {
	*h = HOOK_INIT(*h, dataptr);
}


/* "prepend" */
#define hook_push_front(h, fn, priv) \
	_hook_push_front((h), \
		typesafe_cb_cast3(hook_call_t, \
			void (*)(struct hook *, void *, uintptr_t, typeof(*(priv)) *), \
			void (*)(struct hook *, void *, uintptr_t, const typeof (*(priv)) *), \
			void (*)(struct hook *, void *, uintptr_t, const void *), (fn)), \
		(priv))
extern struct hook_fn *_hook_push_front(
	struct hook *h, hook_call_t fn, const void *priv);

/* "append" */
#define hook_push_back(h, fn, priv) \
	_hook_push_back((h), \
		typesafe_cb_cast3(hook_call_t, \
			void (*)(struct hook *, void *, uintptr_t, typeof(*(priv)) *), \
			void (*)(struct hook *, void *, uintptr_t, const typeof (*(priv)) *), \
			void (*)(struct hook *, void *, uintptr_t, const void *), (fn)), \
		(priv))
extern struct hook_fn *_hook_push_back(
	struct hook *h, hook_call_t fn, const void *priv);

/* calls all hooks in a front-to-back order, passing @code and @param as
 * given. returns number of hooks called.
 */
extern int hook_call_front(struct hook *h, void *param, uintptr_t code);
/* same, but back-to-front */
extern int hook_call_back(struct hook *h, void *param, uintptr_t code);

extern void hook_remove(struct hook *h, struct hook_fn *entry);

/* valid from within a hook routine */
#define hook_detach(h) hook_remove((h), (h)->current)

#endif
