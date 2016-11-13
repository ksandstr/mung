
/* simple threading for the purposes of the testbench personality. */

#define THREADMGR_IMPL_SOURCE 1

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <assert.h>
#include <errno.h>
#include <ccan/likely/likely.h>
#include <ccan/compiler/compiler.h>
#include <ccan/htable/htable.h>
#include <ccan/list/list.h>
#include <ccan/talloc/talloc.h>

#include <l4/types.h>
#include <l4/thread.h>
#include <l4/ipc.h>
#include <l4/schedule.h>
#include <l4/syscall.h>

#include <ukernel/util.h>

#include "defs.h"
#include "forkserv.h"
#include "threadmgr-defs.h"


#define THREAD_STACK_SIZE (32 * 1024)


struct thread
{
	uint8_t *stack;
	int version;
	bool alive, daemon;
	void *retval;
};


static struct thread threads[MAX_THREADS];
static int base_tnum = -1;		/* indicates "no init" */
static L4_Word_t utcb_base;
static L4_ThreadId_t mgr_tid;
static uint8_t *mgr_stk_base;


static void mgr_thread_fn(L4_ThreadId_t first_client);


static L4_ThreadId_t tid_of(int t) {
	return L4_GlobalId(base_tnum + t, abs(threads[t].version));
}


static COLD void init_threading(void)
{
	for(int i=0; i < MAX_THREADS; i++) {
		threads[i] = (struct thread){ .version = -1 };
	}

	base_tnum = L4_ThreadNo(L4_Myself());
	utcb_base = L4_MyLocalId().raw & ~511ul;	/* TODO: get mask from KIP */

	struct thread *self = &threads[0];
	self->stack = NULL;
	self->version = L4_Version(L4_Myself());
	self->alive = true;
}


static COLD void start_mgr_thread(L4_ThreadId_t given_tid)
{
	assert(L4_IsNilThread(mgr_tid));

	uint8_t *stk_base = malloc(THREAD_STACK_SIZE);
	if(stk_base == NULL) {
		printf("%s: can't allocate stack\n", __func__);
		abort();
	}
	mgr_stk_base = stk_base;
	uintptr_t stk_top = ((uintptr_t)stk_base + THREAD_STACK_SIZE - 16) & ~0xfu;
#ifdef __SSE__
	/* mystery alignment for SSE; see start_thread_long() for details */
	stk_top += 4;
#endif
	L4_Word_t *sptr = (L4_Word_t *)stk_top;
	*(--sptr) = L4_Myself().raw;	/* first managed thread */
	*(--sptr) = 0xbeadf00d;			/* return address (grains of sand) */
	stk_top = (L4_Word_t)sptr;

	int my_pri = find_own_priority(), t;
	if(!is_privileged()) {
		int want_tno = -1;
		if(!L4_IsNilThread(given_tid)) {
			want_tno = L4_ThreadNo(given_tid) - base_tnum;
		}
		int n = forkserv_new_thread(L4_Pager(), &mgr_tid.raw, ~0ul,
			(L4_Word_t)&mgr_thread_fn, stk_top, want_tno, L4_TimePeriod(50 * 1000),
			L4_Never, my_pri, my_pri, 2000);
		if(n != 0) {
			printf("%s: forkserv_new_thread() failed; n=%d\n", __func__, n);
			abort();
		}

		t = L4_ThreadNo(mgr_tid) - base_tnum;
		if(t < 0 || t >= MAX_THREADS) {
			printf("%s: invalid t=%d from forkserv\n", __func__, t);
			abort();
		}
		assert(want_tno == -1 || t == want_tno);

		n = forkserv_set_mgr_tid(L4_Pager(), mgr_tid.raw);
		if(n != 0) {
			printf("%s: couldn't set mgr_tid (n=%d)\n", __func__, n);
			abort();
		}
	} else {
		/* this is called before forkserv is up & running. so the manager
		 * thread will end up being paged by sigma0. its pager should be reset
		 * once the forkserv transfer is complete.
		 */
		assert(L4_IsNilThread(given_tid));
		t = L4_ThreadNo(L4_MyGlobalId()) - base_tnum + 1;
		assert(t > 0 && t < MAX_THREADS);
		mgr_tid = L4_GlobalId(base_tnum + t, L4_Version(L4_MyGlobalId()));
		printf("mgr t=%d, tid=%lu:%lu\n", t,
			L4_ThreadNo(mgr_tid), L4_Version(mgr_tid));
		L4_Word_t r = L4_ThreadControl(mgr_tid, L4_Myself(), L4_Myself(),
			L4_Pager(), (void *)(utcb_base + t * 512));
		if(r == 0) {
			printf("%s: ThreadControl failed, ec=%#lx\n", __func__,
				L4_ErrorCode());
			abort();
		}

		L4_Set_Priority(mgr_tid, my_pri);
		L4_Start_SpIp(mgr_tid, stk_top, (L4_Word_t)&mgr_thread_fn);
	}
	threads[t].version = L4_Version(mgr_tid);
	threads[t].alive = true;
}


L4_ThreadId_t get_mgr_tid(void)
{
	assert(base_tnum > 0);

	if(L4_IsNilThread(mgr_tid)) {
		start_mgr_thread(L4_nilthread);
		assert(!L4_IsNilThread(mgr_tid));
	}

	return mgr_tid;
}


int thread_self(void) {
	return L4_ThreadNo(L4_Myself()) - base_tnum;
}


thrd_t thrd_current(void) {
	return thread_self();
}


int thrd_set_daemon_NP(thrd_t tno, bool is_daemon)
{
	if(tno != thrd_current()) {
		errno = -EINVAL;
		return -1;
	}

	struct thread *t = &threads[tno];
	t->daemon = is_daemon;
	return 0;
}


int thread_on_fork(
	L4_ThreadId_t *caller_tid,
	L4_Word_t caller_ip,
	L4_Word_t caller_sp,
	int new_base_tnum)
{
	/* TODO: run atfork-style child-side hooks? */

	assert(L4_IsGlobalId(*caller_tid));
	int caller = L4_ThreadNo(*caller_tid) - base_tnum,
		old_mgr = L4_ThreadNo(mgr_tid) - base_tnum;
	assert(caller >= 0 && caller < MAX_THREADS);
	struct thread copy = threads[caller];
	base_tnum = new_base_tnum;
	for(int i=0; i < MAX_THREADS; i++) {
		if(!threads[i].alive) continue;

		if(i != caller) {
			free(threads[i].stack);
			threads[i].stack = NULL;
			threads[i].alive = false;
			threads[i].version = -abs(threads[i].version);
			threads[i].retval = NULL;

			/* TODO: destroy TSD bits for threads[i] */
		}
	}

	/* set up thread context for the child starter thread */
	int starter = L4_ThreadNo(L4_Myself()) - base_tnum;
	assert(starter < MAX_THREADS);
	threads[starter] = (struct thread){
		.version = L4_Version(L4_Myself()), .alive = true,
		/* (could use .stack to free the starter thread's param->stk_top. why
		 * bother though.)
		 */
	};

	/* restart the manager thread. */
	L4_ThreadId_t new_mgr_tid = L4_GlobalId(old_mgr + base_tnum,
		L4_Version(mgr_tid));
	free(mgr_stk_base);
	mgr_tid = L4_nilthread;
	start_mgr_thread(new_mgr_tid);

	/* TODO: instead figure out the caller's priority. */
	int pri = find_own_priority();
	int n = forkserv_new_thread(L4_Pager(), &caller_tid->raw, ~0ul,
		caller_ip, caller_sp, caller, L4_TimePeriod(10 * 10000), L4_Never,
		pri, pri, 0);
	if(n != 0) {
		printf("%s: new_thread failed, n=%d\n", __func__, n);
		abort();
	}
	int new_caller = L4_ThreadNo(L4_GlobalIdOf(*caller_tid)) - base_tnum;
	assert(new_caller == caller);	/* avoids cleaning threads[caller] */
	threads[caller] = copy;
	threads[caller].version = L4_Version(*caller_tid);

	n = __tmgr_add_thread(mgr_tid, caller_tid->raw);
	if(n != 0) {
		printf("%s: add_thread failed, n=%d\n", __func__, n);
		abort();
	}

	return 0;
}


static void thread_wrapper(L4_ThreadId_t parent)
{
	L4_Set_UserDefinedHandle(0);
	L4_Set_ExceptionHandler(mgr_tid);

	L4_LoadBR(0, 0);
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
	/* FIXME: move these into the ThreadMgr impl. */
	int tnum = L4_ThreadNo(L4_MyGlobalId()) - base_tnum;
	assert(tnum < MAX_THREADS);
	threads[tnum].retval = return_value;
	threads[tnum].version = -threads[tnum].version;

	tsd_clear();

	int n = __tmgr_exit_thread(get_mgr_tid(), (L4_Word_t)return_value);
	printf("%s: ThreadMgr::exit_thread() returned, n=%d\n", __func__, n);
	abort();
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
		first = false;
		init_threading();
	}
	L4_ThreadId_t mgr_tid = get_mgr_tid();

	int t;
	for(t = 0; t < MAX_THREADS; t++) {
		if(!threads[t].alive) {
			assert(threads[t].version <= 0);
			int newver = -threads[t].version + 1;
			if(newver >= 1 << 14) newver = 2;	/* wrap */
			else if((newver & 63) == 0) {
				/* avoid looking like a local thread ID */
				newver |= 1;
			}
			assert((newver & 63) != 0 && newver != 1);
			threads[t].version = newver;
			break;
		}
	}
	if(t == MAX_THREADS) return L4_nilthread;
	L4_ThreadId_t self = L4_Myself(), tid = tid_of(t);
	assert(L4_IsGlobalId(tid));

	threads[t].alive = true;
	assert(threads[t].version > 0);
	uint8_t *stack = malloc(THREAD_STACK_SIZE);
	if(stack == NULL) {
		printf("%s: can't allocate stack for thread!\n", __func__);
		abort();
	}
	threads[t].stack = stack;
	L4_Word_t stk_top = ((L4_Word_t)stack + THREAD_STACK_SIZE - 16) & ~0xfu;
#ifdef __SSE__
	/* align for the parameter. (FIXME: this is poorly understood. why does 4
	 * give the right alignment, but not 8?)
	 */
	stk_top += 4;
#endif
	L4_Word_t *sptr = (L4_Word_t *)stk_top;
	*(--sptr) = self.raw;
	*(--sptr) = 0xbabecafe;		/* >implying human trafficking */
	stk_top = (L4_Word_t)sptr;

	if(!is_privileged()) {
		/* FIXME: add setting of timeslice and priority, i.e. change forkserv
		 * to set the new_thread caller as the new thread's scheduler.
		 */
		L4_ThreadId_t out_tid;
		int n = forkserv_new_thread(L4_Pager(), &out_tid.raw, ~0ul,
			(L4_Word_t)&thread_wrapper, stk_top, t, ts_len, total_quantum,
			priority, priority, 0);
		if(n != 0 || L4_ThreadNo(out_tid) - base_tnum != t) {
			printf("%s: forkserv_new_thread() failed, n=%d, out_tid %lu:%lu (%d)\n",
				__func__, n, L4_ThreadNo(out_tid), L4_Version(out_tid),
				(int)L4_ThreadNo(out_tid) - base_tnum);
			/* TODO: problem, officer? */
			return L4_nilthread;
		}

		tid = out_tid;
		threads[t].version = L4_Version(tid);
	} else {
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

		/* let forkserv know this should be paged for testbench, for which
		 * space_id=1
		 */
		add_fs_tid(1, tid);

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
	}

	int n = __tmgr_add_thread(mgr_tid, tid.raw);
	if(n != 0) {
		printf("%s: local tmgr call failed (n=%d)\n", __func__, n);
		abort();
	}

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


static void *destroy_thread(L4_ThreadId_t tid, struct thread *th)
{
	if(is_privileged()) {
		L4_Word_t res = L4_ThreadControl(tid, L4_nilthread,
			L4_nilthread, L4_nilthread, (void *)-1);
		if(res == 0) {
			printf("%s: deleting ThreadControl failed, ec %#lx\n", __func__,
				L4_ErrorCode());
			return NULL;
		}
	} else {
		int n = forkserv_exit_thread(L4_Pager(), tid.raw);
		if(n != 0) {
			printf("%s: forkserv_exit_thread failed, n=%d\n", __func__, n);
			/* FIXME: ... and then what? */
		}
	}

	th->alive = false;
	th->version = -abs(th->version);
	free(th->stack);
	th->stack = NULL;
	void *rv = th->retval;
	th->retval = NULL;
	return rv;
}


void *join_thread_long(L4_ThreadId_t tid, L4_Time_t timeout, L4_Word_t *ec_p)
{
	if(L4_IsNilThread(tid)) return NULL;

	tid = L4_GlobalIdOf(tid);
	int t = L4_ThreadNo(tid) - base_tnum;
	assert(t < MAX_THREADS);
	assert(abs(threads[t].version) == L4_Version(tid));

	int32_t status;
	L4_Word_t result = 0;
	int n = __tmgr_join_thread_timeout(get_mgr_tid(), tid.raw,
		&status, &result, timeout);
	if(n > 0) {
		*ec_p = n;
		return NULL;
	} else {
		*ec_p = status;		/* hacky! */
		return (void *)result;
	}
}


void *join_thread(L4_ThreadId_t tid) {
	L4_Word_t dummy = 0;
	return join_thread_long(tid, L4_Never, &dummy);
}


void kill_thread(L4_ThreadId_t tid)
{
	tid = L4_GlobalIdOf(tid);
	int t = L4_ThreadNo(tid) - base_tnum;
	if(t < 0 || t >= MAX_THREADS) {
		printf("%s: tried to kill non-local TID %lu:%lu (base %d)\n",
			__func__, L4_ThreadNo(tid), L4_Version(tid), base_tnum);
	}
	assert(t < MAX_THREADS && t >= 0);
	assert(abs(threads[t].version) == L4_Version(tid));

	if(!threads[t].alive) return;

	int n = __tmgr_remove_thread(get_mgr_tid(), tid.raw);
	if(n != 0) {
		printf("%s: ipc fail, n=%d\n", __func__, n);
		abort();
	}

	destroy_thread(tid, &threads[t]);
	/* TODO: call Schedule to ensure that "tid" is truly gone */
}


void for_each_thread(void (*fn)(L4_ThreadId_t tid, void *ptr), void *ptr)
{
	for(int i=0; i < MAX_THREADS; i++) {
		if(!threads[i].alive) continue;
		L4_ThreadId_t tid = L4_GlobalId(base_tnum + i, threads[i].version);
		(*fn)(tid, ptr);
	}
}


/* ThreadMgr instance. */

struct mgrs {
	/* sleeper. */
	struct list_node link;
	L4_ThreadId_t tid;		/* local TID */
};

struct mgrp {
	/* preëmpt record. */
	struct list_node link;
	L4_Clock_t clock;
	int64_t msg_diff;
	L4_Word_t exno, ec;
	bool was_exn;
};

struct mgrt {
	L4_ThreadId_t tid;		/* global TID (for hash & cmp) */
	bool alive, segfault;
	L4_Word_t result;
	struct list_head sleepers, preempts;
};


static size_t hash_mgrt_ptr(const void *ptr, void *priv) {
	return int_hash(((const struct mgrt *)ptr)->tid.raw);
}

static bool mgrt_cmp(const void *cand, void *arg) {
	L4_ThreadId_t tid = *(L4_ThreadId_t *)arg;
	const struct mgrt *t = cand;
	return t->tid.raw == tid.raw;
}


static struct htable mgr_threads = HTABLE_INITIALIZER(
	mgr_threads, &hash_mgrt_ptr, NULL);
static int mgrt_alive = 0;


static struct mgrt *get_mgrt(L4_ThreadId_t tid)
{
	tid = L4_GlobalIdOf(tid);
	return htable_get(&mgr_threads, int_hash(tid.raw), &mgrt_cmp, &tid);
}


static void t_sys_exn(
	L4_Word_t *eip_p,
	L4_Word_t *eflags_p,
	L4_Word_t *exception_no_p,
	L4_Word_t *errorcode_p,
	L4_Word_t *edi_p, L4_Word_t *esi_p, L4_Word_t *ebp_p, L4_Word_t *esp_p,
	L4_Word_t *ebx_p, L4_Word_t *edx_p, L4_Word_t *ecx_p, L4_Word_t *eax_p)
{
	L4_ThreadId_t sender = L4_GlobalIdOf(muidl_get_sender());
	struct mgrt *t = get_mgrt(sender);
	if(t == NULL) {
		printf("%s: unknown sender=%lu:%lu\n", __func__,
			L4_ThreadNo(sender), L4_Version(sender));
		return;
	}

	struct mgrp *p = talloc(t, struct mgrp);
	p->clock = L4_SystemClock();
	p->exno = *exception_no_p;
	p->ec = *errorcode_p;
	p->was_exn = true;
	list_add_tail(&t->preempts, &p->link);
}


static void t_arch_exn(
	L4_Word_t *eip_p,
	L4_Word_t *eflags_p,
	L4_Word_t *exception_no_p,
	L4_Word_t *errorcode_p,
	L4_Word_t *edi_p, L4_Word_t *esi_p, L4_Word_t *ebp_p, L4_Word_t *esp_p,
	L4_Word_t *ebx_p, L4_Word_t *edx_p, L4_Word_t *ecx_p, L4_Word_t *eax_p)
{
	/* no idea what to do. previously we'd halt the sender, so let's do that
	 * here as well.
	 */
	L4_ThreadId_t sender = L4_GlobalIdOf(muidl_get_sender());
	printf("%s: exception from %lu:%lu; eip=%#lx, eflags=%#lx\n", __func__,
		L4_ThreadNo(sender), L4_Version(sender),
		*eip_p, *eflags_p);
	muidl_raise_no_reply();
}


static void t_preempt_fault(L4_Word_t clock_hi, L4_Word_t clock_lo)
{
	L4_ThreadId_t sender = L4_GlobalIdOf(muidl_get_sender());
	L4_Clock_t clock = { .raw = (L4_Word64_t)clock_hi << 32 | clock_lo };
#if 0
	printf("%s: sender=%lu:%lu, clock=%lu (%#lx)\n", __func__,
		L4_ThreadNo(sender), L4_Version(sender),
		(unsigned long)clock.raw, (L4_Word_t)clock.raw);
#endif

	struct mgrt *t = get_mgrt(sender);
	if(t == NULL) {
		printf("%s: unknown sender=%lu:%lu\n", __func__,
			L4_ThreadNo(sender), L4_Version(sender));
		return;
	}
	struct mgrp *p = talloc(t, struct mgrp);
	*p = (struct mgrp){
		.clock = clock, .was_exn = false,
		.msg_diff = (int64_t)L4_SystemClock().raw - clock.raw,
	};
	list_add_tail(&t->preempts, &p->link);
}


static bool t_get_preempt_record(
	L4_Word_t tid_raw,
	L4_Word_t *clock_hi_p, L4_Word_t *clock_lo_p,
	int64_t *msg_diff_p,
	bool *was_exn_p)
{
	L4_ThreadId_t tid = { .raw = tid_raw };
	struct mgrt *t = get_mgrt(tid);
	if(t == NULL) {
		printf("%s: unknown tid=%lu:%lu\n", __func__,
			L4_ThreadNo(tid), L4_Version(tid));
		return false;
	}

	struct mgrp *p = list_pop(&t->preempts, struct mgrp, link);
	if(p == NULL) return false;
	else {
		*clock_hi_p = p->clock.raw >> 32;
		*clock_lo_p = p->clock.raw & 0xffffffff;
		*msg_diff_p = p->msg_diff;
		*was_exn_p = p->was_exn;
		talloc_free(p);
		return true;
	}
}


static void t_add_thread(L4_Word_t arg_tid)
{
	L4_ThreadId_t sender;
	if(muidl_supp_get_context() != NULL		/* false iff called before dispatch */
		&& (sender = muidl_get_sender(), !L4_IsLocalId(sender))
		&& !L4_SameThreads(sender, L4_Pager()))
	{
		printf("%s: unknown sender %lu:%lu\n", __func__,
			L4_ThreadNo(sender), L4_Version(sender));
		return;
	}

	L4_ThreadId_t tid = { .raw = arg_tid };
	tid = L4_GlobalIdOf(tid);

	struct mgrt *t = talloc(NULL, struct mgrt);
	if(t == NULL) {
		printf("%s: talloc failed\n", __func__);
		abort();
	}
	*t = (struct mgrt){ .tid = tid, .alive = true };
	list_head_init(&t->sleepers);
	list_head_init(&t->preempts);
	htable_add(&mgr_threads, hash_mgrt_ptr(t, NULL), t);
	mgrt_alive++;
}


/* not a service call despite naming. */
static void t_end_thread(
	L4_ThreadId_t tid,
	int join_status,
	int exit_status,
	L4_Word_t result)
{
	tid = L4_GlobalIdOf(tid);
	assert(L4_ThreadNo(tid) - base_tnum < MAX_THREADS);
	struct mgrt *t = get_mgrt(tid);
	if(t == NULL) {
		printf("%s: tid %lu:%lu not found\n", __func__,
			L4_ThreadNo(tid), L4_Version(tid));
		return;
	}

	bool reply_ok = false;
	if(!list_empty(&t->sleepers)) {
		struct mgrs *s, *next;
		list_for_each_safe(&t->sleepers, s, next, link) {
			L4_LoadMR(0, (L4_MsgTag_t){ .X.u = 2 }.raw);
			L4_LoadMR(1, join_status);
			L4_LoadMR(2, result);
			L4_MsgTag_t tag = L4_Reply(s->tid);
			if(L4_IpcSucceeded(tag)) reply_ok = true;
			list_del_from(&t->sleepers, &s->link);
		}
		assert(list_empty(&t->sleepers));
	}
	if(reply_ok) {
		/* don't retain exit record on successful join */
		htable_del(&mgr_threads, hash_mgrt_ptr(t, NULL), t);
		talloc_free(t);
		destroy_thread(tid, &threads[L4_ThreadNo(tid) - base_tnum]);
	} else {
		t->alive = false;
		t->segfault = (join_status == 1);
		t->result = result;
	}

	mgrt_alive--;
	assert(mgrt_alive <= mgr_threads.elems);
	if(mgrt_alive == 0) {
		exit(exit_status);
	}
}


static void t_exit_thread(L4_Word_t result)
{
	L4_ThreadId_t sender = muidl_get_sender();
	if(!L4_IsLocalId(sender)) {
		printf("%s: non-local sender %lu:%lu ignored\n", __func__,
			L4_ThreadNo(sender), L4_Version(sender));
		return;
	}

	t_end_thread(sender, 0, 0, result);

	muidl_raise_no_reply();
}


static void t_segv(L4_Word_t a_dead_tid, L4_Word_t fault_addr)
{
	L4_ThreadId_t sender = muidl_get_sender();
	if(sender.raw != L4_Pager().raw && !L4_IsLocalId(sender)) {
		printf("%s: non-local, non-pager sender %lu:%lu ignored\n",
			__func__, L4_ThreadNo(sender), L4_Version(sender));
		return;
	}

	L4_ThreadId_t dead_tid = { .raw = a_dead_tid };
#if 0
	printf("%s: dead_tid=%lu:%lu, fault_addr=%#lx\n", __func__,
		L4_ThreadNo(dead_tid), L4_Version(dead_tid), fault_addr);
#endif

	/* 1 per ThreadMgr join_thread spec */
	t_end_thread(dead_tid, 1, (fault_addr & ~0xf) | 7, fault_addr);
}


static void t_rm_thread(L4_Word_t arg_tid)
{
	L4_ThreadId_t dead_tid = { .raw = arg_tid };

	L4_ThreadId_t sender = muidl_get_sender();
	if(!L4_IsLocalId(sender)) {
		printf("%s: non-local sender %lu:%lu ignored\n", __func__,
			L4_ThreadNo(sender), L4_Version(sender));
		return;
	}

	struct mgrt *t = get_mgrt(dead_tid);
	if(t != NULL) {
		htable_del(&mgr_threads, hash_mgrt_ptr(t, NULL), t);
		talloc_free(t);
	}

	/* FIXME: shouldn't this be inside the non-NULL test's block, above? */
	mgrt_alive--;
	assert(mgrt_alive > 0);
	assert(mgrt_alive <= mgr_threads.elems);
}


static void t_join_thread(
	L4_Word_t arg_join_tid,
	int32_t *status_p,
	L4_Word_t *result_p)
{
	L4_ThreadId_t join_tid = { .raw = arg_join_tid };

	L4_ThreadId_t sender = muidl_get_sender();
	if(!L4_IsLocalId(sender)) {
		printf("%s: non-local sender %lu:%lu ignored\n", __func__,
			L4_ThreadNo(sender), L4_Version(sender));
		return;
	}

	assert(L4_ThreadNo(join_tid) - base_tnum < MAX_THREADS);
	struct mgrt *t = get_mgrt(join_tid);
	if(t == NULL) {
		/* not found. */
		*status_p = -1;
		*result_p = 0;
	} else if(!t->alive) {
		/* immediate join. */
		assert(list_empty(&t->sleepers));
		*status_p = t->segfault ? 1 : 0;
		*result_p = t->result;
		htable_del(&mgr_threads, hash_mgrt_ptr(t, NULL), t);
		talloc_free(t);
		destroy_thread(join_tid,
			&threads[L4_ThreadNo(join_tid) - base_tnum]);
	} else {
		/* sleep. */
		struct mgrs *s = talloc(t, struct mgrs);
		if(s == NULL) {
			printf("%s: can't allocate mgrs\n", __func__);
			abort();
		}
		s->tid = sender;
		list_add(&t->sleepers, &s->link);
		muidl_raise_no_reply();
	}
}


static void mgr_thread_fn(L4_ThreadId_t first_client)
{
	static const struct thread_mgr_vtable vt = {
		.sys_exception = &t_sys_exn,
		.arch_exception = &t_arch_exn,
		.preempt_fault = &t_preempt_fault,
		.add_thread = &t_add_thread,
		.remove_thread = &t_rm_thread,
		.exit_thread = &t_exit_thread,
		.join_thread = &t_join_thread,
		.segv = &t_segv,
		.get_preempt_record = &t_get_preempt_record,
	};

	/* reset state (discard & release fork parent's things) */
	htable_clear(&mgr_threads);
	mgrt_alive = 0;
	assert(muidl_supp_get_context() == NULL);
	t_add_thread(first_client.raw);
	assert(mgrt_alive == 1);
	assert(mgr_threads.elems == 1);

	/* this one has no exit condition; t_exit_thread() destroys the process or
	 * leaves it hanging.
	 */
	for(;;) {
		L4_Word_t status = _muidl_thread_mgr_dispatch(&vt);
		if(status == MUIDL_UNKNOWN_LABEL) {
			printf("%s: unknown label in %lu:%lu (tag=%#lx)\n", __func__,
				L4_ThreadNo(L4_MyGlobalId()), L4_Version(L4_MyGlobalId()),
				muidl_get_tag().raw);
		}
	}

	assert(false);

	/* DEATH HAS APPEARED! */
	L4_Set_ExceptionHandler(L4_nilthread);
	for(;;) asm volatile ("int $23");
}
