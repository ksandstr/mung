/* a subset of the C11 <threads.h> interface.
 *
 * notably, neither the microkernel runtime of mung or the testbench runtime
 * implement _Thread_local storage.
 */

#ifndef SEEN_FAKE_CLIB_THREADS_H
#define SEEN_FAKE_CLIB_THREADS_H

#include <stdbool.h>
#include <stdatomic.h>
#include <l4/types.h>


typedef _Atomic volatile L4_Word_t mtx_t;

enum {
	mtx_plain = 0,
	mtx_recursive = 1,
	mtx_timed = 2,
};

enum {
	thrd_success = 0,
	thrd_timedout,
	thrd_busy,
	thrd_error,
	thrd_nomem,
};

struct timespec;	/* left undefined here */

typedef void (*tss_dtor_t)(void *);
typedef int tss_t;	/* may as well. */

typedef int (*thrd_start_t)(void *);
typedef int thrd_t;


#define ONCE_FLAG_INIT 0

typedef _Atomic int once_flag;
extern void call_once(once_flag *flag, void (*func)(void));


/* C11 threads. */

typedef int thrd_t;
extern thrd_t thrd_current(void);
extern int thrd_create(thrd_t *thr, thrd_start_t fn, void *arg);
extern void thrd_exit(int res);
extern int thrd_join(thrd_t thr, int *res_p);

/* non-portable outside the testbench runtime. */
extern int thrd_set_daemon_NP(thrd_t thread, bool is_daemon);


/* C11 mutexes. */

extern int mtx_init(mtx_t *mtx, int type);
extern void mtx_destroy(mtx_t *mtx);
extern int mtx_lock(mtx_t *mtx);
extern int mtx_trylock(mtx_t *mtx);
extern int mtx_timedlock(mtx_t *mtx, const struct timespec *ts);
extern int mtx_unlock(mtx_t *mtx);


/* C11 explicit per-thread data. (not implemented in mung or its testbench
 * runtime. declared here for completeness.)
 */

extern int tss_create(tss_t *key, tss_dtor_t dtor);
extern void tss_delete(tss_t key);
extern void *tss_get(tss_t key);
extern void tss_set(tss_t key, void *ptr);


#endif
