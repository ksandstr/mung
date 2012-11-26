
/* simple threading for the purposes of the testbench personality. */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <assert.h>
#include <ccan/htable/htable.h>
#include <ccan/likely/likely.h>
#include <ccan/compiler/compiler.h>

#include <l4/types.h>
#include <l4/thread.h>
#include <l4/ipc.h>
#include <l4/schedule.h>
#include <l4/syscall.h>

#include <ukernel/util.h>

#include "defs.h"
#include "forkserv.h"


#define THREAD_STACK_SIZE (32 * 1024)
#define MAX_THREADS 12


struct thread
{
	uint8_t *stack;
	int version;
	bool alive;
	void *retval;
};


struct tsd_key
{
	int key;
	void (*destructor)(void *ptr);
};


struct tsd_item {
	int key;
	void *ptr;
};


struct tsd
{
	struct htable tsd_vals;
};


static size_t hash_tsd_key(const void *key, void *priv);


static struct thread threads[MAX_THREADS];
static int base_tnum, next_tsd_key = 1;
static L4_Word_t utcb_base;
static struct htable tsd_keys = HTABLE_INITIALIZER(tsd_keys,
	&hash_tsd_key, NULL);


static L4_ThreadId_t tid_of(int t) {
	return L4_GlobalId(base_tnum + t, abs(threads[t].version));
}


static size_t hash_tsd_key(const void *key, void *priv) {
	const struct tsd_key *k = key;
	return int_hash(k->key);
}


static size_t hash_int(const void *k, void *priv) {
	return int_hash(*(const int *)k);
}


static bool int_eq(const void *cand, void *ref) {
	return *(const int *)cand == *(int *)ref;
}


static COLD void init_threading(void)
{
	for(int i=0; i < MAX_THREADS; i++) {
		threads[i] = (struct thread){ };
	}

	base_tnum = L4_ThreadNo(L4_Myself());
	utcb_base = L4_MyLocalId().raw & ~511ul;	/* TODO: get mask from KIP */

	struct thread *self = &threads[0];
	self->stack = NULL;
	self->version = L4_Version(L4_Myself());
	self->alive = true;
}


int thread_self(void) {
	return L4_ThreadNo(L4_Myself()) - base_tnum;
}


int thread_on_fork(
	L4_ThreadId_t *caller_tid,
	L4_Word_t caller_ip,
	L4_Word_t caller_sp,
	int new_base_tnum)
{
	/* TODO: run atfork-style child-side hooks? */

	int caller = L4_ThreadNo(*caller_tid) - base_tnum;
	struct thread copy = threads[caller];
	base_tnum = new_base_tnum;
	for(int i=0; i < MAX_THREADS; i++) {
		if(!threads[i].alive || i == caller) continue;

		if(i != caller) {
			free(threads[i].stack);
		}
		threads[i].stack = NULL;
		threads[i].alive = false;
		threads[i].retval = NULL;
	}

	L4_LoadMR(0, (L4_MsgTag_t){ .X.label = FORKSERV_NEW_THREAD,
		.X.u = 4 }.raw);
	L4_LoadMR(1, ~(L4_Word_t)0);
	L4_LoadMR(2, caller_ip);
	L4_LoadMR(3, caller_sp);
	L4_LoadMR(4, caller);
	L4_MsgTag_t tag = L4_Call(L4_Pager());
	if(L4_IpcFailed(tag)) {
		return 1;		/* TODO: not a proper errno. */
	}

	L4_StoreMR(1, &caller_tid->raw);
	int new_caller = L4_ThreadNo(*caller_tid) - base_tnum;
	assert(new_caller == caller);	/* avoids cleaning threads[caller] */
	threads[new_caller] = copy;

	return 0;
}


static void thread_wrapper(void)
{
	L4_ThreadId_t parent = { .raw = L4_UserDefinedHandle() };
	L4_Set_UserDefinedHandle(0);
	L4_Set_ExceptionHandler(parent);

	L4_MsgTag_t tag = L4_Receive(parent);
	if(L4_IpcFailed(tag)) {
		printf("%s: initial IPC failed (ec %#lx), doing early exit\n",
			__func__, L4_ErrorCode());
		goto end;
	}
	L4_Word_t fn, param, tnum;
	L4_StoreMR(1, &fn);
	L4_StoreMR(2, &param);
	L4_StoreMR(3, &tnum);	/* TODO: not used -- remove. */

	(*(void (*)(void *))fn)((void *)param);

end:
	exit_thread(NULL);
}


void exit_thread(void *return_value)
{
	int tnum = L4_ThreadNo(L4_MyGlobalId()) - base_tnum;
	assert(tnum < MAX_THREADS);
	threads[tnum].retval = return_value;
	threads[tnum].version = -threads[tnum].version;

	/* toss the TSD values. */
	struct tsd *tsd = (void *)L4_UserDefinedHandle();
	if(tsd != NULL) {
		struct htable_iter it;
		for(struct tsd_item *val = htable_first(&tsd->tsd_vals, &it);
			val != NULL;
			val = htable_next(&tsd->tsd_vals, &it))
		{
			struct tsd_key *key = htable_get(&tsd_keys,
				int_hash(val->key), &int_eq, &val->key);
			if(key != NULL && key->destructor != NULL) {
				(*key->destructor)(val->ptr);
			}
			htable_delval(&tsd->tsd_vals, &it);
			free(val);
		}
		free(tsd);
		L4_Set_UserDefinedHandle(0);
	}

#if 0
	printf("testbench thread %d (%u:%u) terminating\n", (int)tnum,
		L4_ThreadNo(self), L4_Version(self));
#endif

	for(;;) {
		asm volatile ("int $1");
	}
}


L4_ThreadId_t start_thread(void (*fn)(void *param), void *param)
{
	/* default timeslice is 50 ms just to avoid preemptions. */
	return start_thread_long(fn, param, -1,
		L4_TimePeriod(50 * 1000), L4_Never);
}


L4_ThreadId_t start_thread_long(
	void (*fn)(void *param),
	void *param,
	int priority,
	L4_Time_t ts_len,
	L4_Time_t total_quantum)
{
	static bool first = true;
	if(unlikely(first)) {
		init_threading();
		first = false;
	}

	int t;
	for(t = 0; t < MAX_THREADS; t++) {
		if(!threads[t].alive) {
			assert(threads[t].version <= 0);
			threads[t].version = -threads[t].version + 1;
			if(threads[t].version >= 1 << 14) threads[t].version = 1;
			break;
		}
	}
	if(t == MAX_THREADS) return L4_nilthread;

	threads[t].alive = true;
	assert(threads[t].version > 0);

	L4_ThreadId_t self = L4_Myself(), tid = tid_of(t);
#if 0
	printf("%s: creating thread %u:%u, utcb at %#lx\n", __func__,
		L4_ThreadNo(tid), L4_Version(tid), utcb_base + t * 512);
#endif
	L4_Word_t r = L4_ThreadControl(tid, self, self, L4_Pager(),
		(void *)(utcb_base + t * 512));
	if(r == 0) {
		printf("%s: ThreadControl failed, ErrorCode %#lx\n", __func__,
			L4_ErrorCode());
		threads[t].version = -threads[t].version;
		assert(!threads[t].alive);
		assert(threads[t].version <= 0);
		return L4_nilthread;
	}

	/* let forkserv know this should be paged for testbench */
	add_fs_tid(1, tid);

	uint8_t *stack = malloc(THREAD_STACK_SIZE);
	if(stack == NULL) {
		printf("%s: can't allocate stack for thread!\n", __func__);
		abort();
	}
	threads[t].stack = stack;
	L4_Word_t stk_top = (L4_Word_t)stack + THREAD_STACK_SIZE - 16;
	L4_Set_UserDefinedHandleOf(tid, self.raw);
	if(priority != -1) {
		L4_Word_t r = L4_Set_Priority(tid, priority);
		if(r == L4_SCHEDRESULT_ERROR) {
			printf("%s: L4_Set_Priority() failed: errorcode %lu\n",
				__func__, L4_ErrorCode());
			/* TODO: cleanups? */
			return L4_nilthread;
		}
	}
	L4_Set_Timeslice(tid, ts_len, total_quantum);
	L4_Start_SpIp(tid, stk_top, (L4_Word_t)&thread_wrapper);
	L4_LoadMR(0, (L4_MsgTag_t){ .X.u = 3 }.raw);
	L4_LoadMR(1, (L4_Word_t)fn);
	L4_LoadMR(2, (L4_Word_t)param);
	L4_LoadMR(3, t);
	L4_MsgTag_t tag = L4_Send(tid);
	if(L4_IpcFailed(tag)) {
		printf("%s: initial IPC failed, ErrorCode %#lx\n", __func__,
			L4_ErrorCode());
		/* TODO: cleanups! */
		return L4_nilthread;
	}

	return tid;
}


void *join_thread(L4_ThreadId_t tid)
{
	if(L4_IsNilThread(tid)) return NULL;

	int t = L4_ThreadNo(tid) - base_tnum;
	assert(t < MAX_THREADS);
	assert(abs(threads[t].version) == L4_Version(tid));

	L4_MsgTag_t tag = L4_Receive(tid_of(t));
	if(L4_IpcFailed(tag)) {
		printf("%s: receive from thread failed, ec %#lx\n", __func__,
			L4_ErrorCode());
		return NULL;
	}
	/* TODO: verify the exception message (label, GP#) */

	/* destroy the thread. */
	L4_Word_t res = L4_ThreadControl(tid_of(t), L4_nilthread,
		L4_nilthread, L4_nilthread, (void *)-1);
	if(res == 0) {
		printf("%s: deleting ThreadControl failed, ec %#lx\n", __func__,
			L4_ErrorCode());
		return NULL;
	}

	threads[t].alive = false;
	threads[t].version = -abs(threads[t].version);
	free(threads[t].stack);
	threads[t].stack = NULL;
	void *rv = threads[t].retval;
	threads[t].retval = NULL;
	return rv;
}


void for_each_thread(void (*fn)(L4_ThreadId_t tid, void *ptr), void *ptr)
{
	for(int i=0; i < MAX_THREADS; i++) {
		if(!threads[i].alive) continue;
		L4_ThreadId_t tid = L4_GlobalId(base_tnum + i, threads[i].version);
		(*fn)(tid, ptr);
	}
}


void tsd_key_create(int *key_p, void (*destructor)(void *ptr))
{
	*key_p = next_tsd_key++;
	struct tsd_key *k = malloc(sizeof(struct tsd_key));
	k->key = *key_p;
	k->destructor = destructor;		/* TODO: use! */
	htable_add(&tsd_keys, hash_tsd_key(k, NULL), k);
}


void tsd_set(int key, void *ptr)
{
	if(key == 0) return;

	struct tsd *tsd = (void *)L4_UserDefinedHandle();
	if(tsd == NULL) {
		tsd = malloc(sizeof(struct tsd));
		htable_init(&tsd->tsd_vals, &hash_int, NULL);
		L4_Set_UserDefinedHandle((L4_Word_t)tsd);
	}

	size_t hv = int_hash(key);
	struct tsd_item *val = htable_get(&tsd->tsd_vals, hv, &int_eq, &key);
	if(val == NULL) {
		val = malloc(sizeof(struct tsd_item));
		val->key = key;
		val->ptr = NULL;
		htable_add(&tsd->tsd_vals, hv, val);
	}
	val->ptr = ptr;
}


void *tsd_get(int key)
{
	if(key == 0) return NULL;

	struct tsd *tsd = (void *)L4_UserDefinedHandle();
	if(tsd == NULL) return NULL;

	struct tsd_item *val = htable_get(&tsd->tsd_vals, int_hash(key),
		&int_eq, &key);
	return val != NULL ? val->ptr : NULL;
}
