/* a subset of the C11 <threads.h> interface.
 *
 * notably, neither the microkernel runtime of mung or the testbench runtime
 * implement _Thread_local storage.
 */

#ifndef SEEN_FAKE_CLIB_THREADS_H
#define SEEN_FAKE_CLIB_THREADS_H

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


/* C11 mutexes. */

extern int mtx_init(mtx_t *mtx, int type);
extern void mtx_destroy(mtx_t *mtx);
extern int mtx_lock(mtx_t *mtx);
extern int mtx_trylock(mtx_t *mtx);
extern int mtx_timedlock(mtx_t *mtx, const struct timespec *ts);
extern int mtx_unlock(mtx_t *mtx);


#endif
