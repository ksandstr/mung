
#include <stdio.h>
#include <stdlib.h>
#include <stdatomic.h>
#include <threads.h>

#include <l4/types.h>
#include <l4/thread.h>


#define MTX_UNOWNED ((L4_Word_t)0)
#define MTX_DEAD (~MTX_UNOWNED)


/* public interface. */

int mtx_init(mtx_t *mtx, int type)
{
	if(type != mtx_plain) return thrd_error;

	*mtx = MTX_UNOWNED;
	return thrd_success;
}


void mtx_destroy(mtx_t *mtx)
{
	/* mutexes that aren't unowned, or owned by the local thread and not
	 * conflicted on, mustn't be destroyed.
	 */
	if(*mtx != MTX_UNOWNED && *mtx != (L4_MyLocalId().raw | 1)) {
		fprintf(stderr, "attempted to destroy busy mutex at %p\n", mtx);
		abort();
	}

	*mtx = MTX_DEAD;
}


int mtx_lock(mtx_t *mtx)
{
	L4_Word_t prev = MTX_UNOWNED, next = L4_MyLocalId().raw | 1;
	if(atomic_compare_exchange_strong(mtx, &prev, next)) {
		/* fastpath exit. */
		return thrd_success;
	}

	/* TODO: mark conflict and handle it. */

	return thrd_error;
}


int mtx_trylock(mtx_t *mtx)
{
	L4_Word_t prev = MTX_UNOWNED, next = L4_MyLocalId().raw | 1;
	return atomic_compare_exchange_strong(mtx, &prev, next)
		? thrd_success : thrd_busy;
}


int mtx_timedlock(mtx_t *mtx, const struct timespec *ts)
{
	/* invalid type, since mtx_init() never allows timed mutexes. */
	return thrd_error;
}


int mtx_unlock(mtx_t *mtx)
{
	L4_Word_t prev = L4_MyLocalId().raw | 1, next = MTX_UNOWNED;
	if(atomic_compare_exchange_strong(mtx, &prev, next)) {
		/* fastpath */
		return thrd_success;
	}

	/* TODO: resolve conflicted mutex atomically. */

	return thrd_error;
}
