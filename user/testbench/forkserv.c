
/* service that takes over the root task's memory, letting it fork
 * subprocesses (and subprocesses of those). this is useful for test cases
 * involving map operations in IPC, or the Unmap system call.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <ccan/list/list.h>
#include <ccan/htable/htable.h>
#include <ccan/likely/likely.h>
#include <ccan/container_of/container_of.h>

#include <l4/types.h>
#include <l4/ipc.h>
#include <l4/syscall.h>
#include <l4/space.h>
#include <l4/schedule.h>
#include <l4/kip.h>

/* for int_hash() */
#include <ukernel/util.h>

#include "defs.h"
#include "forkserv.h"


#define TPS_SHIFT 3
#define THREADS_PER_SPACE (1 << TPS_SHIFT)


#define FSHELPER_CHOPCHOP	0x6343	/* "cC", moar liek a wakeup IPC */

/* from helper to forkserv */
#define FSHELPER_CHOPCHOP_DONE 0x4364	/* "Cd" */
#define FSHELPER_CHOPCHOP_REPLY 0x4372	/* "Cr" */


struct fs_thread;


struct fs_space
{
	L4_Word_t id, parent_id, prog_brk;
	struct htable pages;
	struct fs_thread *threads[THREADS_PER_SPACE];
	L4_Fpage_t utcb_area, kip_area;
	struct list_head dead_children;
	struct list_head waiting_threads;
	struct list_node dead_link;		/* in parent's dead_children */
	int exit_status;
};


struct fs_phys_page
{
	L4_Word_t local_addr;	/* address in forkserv */
	int refcount;			/* 1 for exclusive, 0 for dead */
};


struct fs_vpage
{
	L4_Word_t address;		/* key in "pages" */
	int access;
	struct fs_phys_page *page;
};


struct fs_thread
{
	L4_ThreadId_t tid;
	struct fs_space *space;
	struct list_node wait_link;	/* in space's waiting_threads */
};


/* queued to helper thread */
struct helper_work {
	struct helper_work *next;
	L4_ThreadId_t from;
	L4_Word_t mrs[];
};


static size_t hash_word(const void *, void *);
static size_t hash_threadno(const void *, void *);


/* thread_hash is hashed by threadno, but compared for equality with full TID
 * (i.e. word_cmp().) this is for handle_add_tid()'s overwrite function.
 */
static struct htable thread_hash = HTABLE_INITIALIZER(thread_hash,
	&hash_threadno, NULL);
static struct htable space_hash = HTABLE_INITIALIZER(space_hash,
	&hash_word, NULL);

static L4_ThreadId_t console_tid, fpager_tid, helper_tid, forkserv_tid;
static L4_Word_t map_range_pos = 0, next_space_id = 100, next_async_id = 1;
static struct helper_work *helper_queue = NULL;


static size_t hash_word(const void *elem, void *priv) {
	L4_Word_t val = *(const L4_Word_t *)elem;
	return int_hash(val);
}


static bool word_cmp(const void *a, void *b) {
	return *(const L4_Word_t *)a == *(L4_Word_t *)b;
}


static size_t hash_threadno(const void *elem, void *priv) {
	L4_ThreadId_t val = { .raw = *(const L4_Word_t *)elem };
	return int_hash(L4_ThreadNo(val));
}


static struct fs_thread *get_thread(L4_ThreadId_t tid) {
	return htable_get(&thread_hash, int_hash(L4_ThreadNo(tid)),
		&word_cmp, &tid);
}


static struct fs_space *get_space(L4_Word_t id)
{
	struct fs_space *sp;
	void *ptr = htable_get(&space_hash, int_hash(id), &word_cmp, &id);
	if(ptr != NULL) sp = container_of(ptr, struct fs_space, id);
	else {
		sp = malloc(sizeof(*sp));
		*sp = (struct fs_space){
			.id = id, .parent_id = 0, .prog_brk = find_phys_mem_top(),
			.utcb_area = L4_Fpage(0x30000, UTCB_SIZE * THREADS_PER_SPACE),
			.kip_area = L4_FpageLog2(0x2f000, 12),
		};
		list_head_init(&sp->dead_children);
		list_head_init(&sp->waiting_threads);
		htable_init(&sp->pages, &hash_word, NULL);
		htable_add(&space_hash, int_hash(id), &sp->id);
	}

	return sp;
}


static struct fs_space *get_space_by_tid(L4_ThreadId_t tid)
{
	struct fs_thread *t = get_thread(tid);
	return t == NULL ? NULL : t->space;
}


static struct fs_phys_page *alloc_new_page(void)
{
	struct fs_phys_page *phys = malloc(sizeof(*phys));
	void *ptr = valloc(PAGE_SIZE);
	memset(ptr, 0, PAGE_SIZE);
	phys->local_addr = (L4_Word_t)ptr;
	phys->refcount = 1;
	return phys;
}


/* if removed, a serial I/O con_putstr() will be linked in from under lib/ . */
#if 0
/* FIXME: copypasta'd from user/sigma0.c . merge these. */
void con_putstr(const char *str)
{
	size_t len = strlen(str);
	L4_LoadMR(0, (L4_MsgTag_t){
		.X.label = 0x5370, /* "pS" */
		.X.u = (len + 3) / 4,
	}.raw);
	for(int i=0; i * 4 < len; i++) {
		L4_LoadMR(i + 1, *(L4_Word_t *)&str[i * 4]);
	}
	L4_MsgTag_t tag = L4_Call(console_tid);
	if(L4_IpcFailed(tag)) {
		asm volatile ("int $23");
	}
}
#endif


void __assert_failure(
	const char *condition,
	const char *file,
	unsigned int line,
	const char *function)
{
	printf("assert(%s) failed in `%s' (%s:%u)\n", condition, function,
		file, line);
	for(;;) { asm volatile ("int $1"); }
}


void abort(void)
{
	printf("forkserv: abort() called from %p!\n",
		__builtin_return_address(0));
	for(;;) { asm volatile ("int $1"); }
}


void malloc_panic(void) {
	printf("%s: called!\n", __func__);
	abort();
}


int sched_yield(void)
{
	L4_ThreadSwitch(L4_nilthread);
	return 0;
}


static bool handle_send_page(L4_ThreadId_t from)
{
	L4_Word_t phys_addr, space_id;
	L4_StoreMR(1, &phys_addr);
	L4_StoreMR(2, &space_id);

	L4_Fpage_t window;
	struct fs_space *sp = get_space(space_id);
	if(space_id == 0) {
		window = L4_Fpage(phys_addr, PAGE_SIZE);
	} else {
		if(unlikely(map_range_pos == 0)) {
			map_range_pos = find_phys_mem_top() + 1;
		}

		window = L4_Fpage(map_range_pos, PAGE_SIZE);
	}

	L4_LoadMR(0, 0);
	L4_MsgTag_t tag = L4_Reply(from);
	if(L4_IpcFailed(tag)) goto ipcfail;

	L4_LoadBR(0, window.raw);
	tag = L4_Receive_Timeout(from, L4_TimePeriod(100 * 1000));
	if(L4_IpcFailed(tag)) goto ipcfail;
	L4_LoadBR(0, 0);

	L4_MapItem_t mi;
	L4_StoreMRs(1, 2, mi.raw);
#if 0
	printf("got page %#lx:%#lx, offset %#lx, phys_addr %lu:%#lx, local %#lx\n",
		L4_Address(L4_MapItemSndFpage(mi)), L4_Size(L4_MapItemSndFpage(mi)),
		L4_MapItemSndBase(mi), space_id, phys_addr, L4_Address(window));
#endif
	if(space_id != 0) map_range_pos += L4_Size(window);

	struct fs_phys_page *phys = malloc(sizeof(*phys));
	*phys = (struct fs_phys_page){
		.local_addr = L4_Address(window),
		.refcount = 1,
	};
	struct fs_vpage *p = malloc(sizeof(*p));
	*p = (struct fs_vpage){
		.address = phys_addr,
		.page = phys,
		.access = L4_FullyAccessible,
	};
	assert(htable_get(&sp->pages, int_hash(p->address),
		&word_cmp, &p->address) == NULL);
	htable_add(&sp->pages, int_hash(p->address), &p->address);

	L4_LoadMR(0, 0);
	return true;

ipcfail:
	printf("%s: IPC failed, ec %#lx\n", __func__,
		L4_ErrorCode());
	abort();
	return false;
}


/* it'd be nice if this part could communicate with the outside world without
 * a pager thread in testbench. such as via a serial port.
 */
static bool handle_pf(
	L4_ThreadId_t from,
	L4_Word_t addr,
	L4_Word_t ip,
	L4_Word_t fault_access)
{
#if 0
	printf("forkserv: pf [%c%c%c] in %d:%d (ip %#lx, addr %#lx)\n",
		CHECK_FLAG(fault_access, L4_Readable) ? 'r' : '-',
		CHECK_FLAG(fault_access, L4_Writable) ? 'w' : '-',
		CHECK_FLAG(fault_access, L4_eXecutable) ? 'x' : '-',
		from.global.X.thread_no, from.global.X.version,
		ip, addr);
#endif

	static int rep_count = 0;
	static L4_Word_t last_addr = 0;
	if(last_addr == addr && ++rep_count == 10) {
		printf("*** all work and no play makes jack a dull boy\n");
		rep_count = 0;
		last_addr = 0;
		return false;
	} else if(last_addr != addr) {
		rep_count = 0;
		last_addr = addr;
	}

	struct fs_space *sp = get_space_by_tid(from);
	if(sp == NULL) {
		printf("source %#lx isn't known\n", from.raw);
		return false;
	}

	L4_Word_t page_addr = addr & ~PAGE_MASK;
	struct fs_vpage *page = htable_get(&sp->pages,
		int_hash(page_addr), &word_cmp, &page_addr);
	if(page == NULL && page_addr >= sp->prog_brk) {
		/* moar ramz pls */
		page = malloc(sizeof(*page));
		page->address = page_addr;
		page->page = alloc_new_page();
		page->access = L4_FullyAccessible;
#if 0
		printf("%s: new page %#lx at address %#lx\n", __func__,
			page->page->local_addr, page->address);
#endif
		htable_add(&sp->pages, int_hash(page_addr), &page->address);
	} else if(page == NULL) {
#if 0
		printf("segfault in thread %#lx, space %lu (brk %#lx)\n", from.raw,
			sp->id, sp->prog_brk);
#endif
		return false;
	} else if(page->page->refcount == 1
		&& !CHECK_FLAG_ALL(page->access, fault_access))
	{
		/* always give access. */
#if 0
		printf("%s: expand access at %#lx: old %#x -> new %#x\n", __func__,
			page->address, (unsigned)page->access,
			(unsigned)(page->access | fault_access));
#endif
		page->access |= fault_access;
	} else if(page->page->refcount > 1
		&& CHECK_FLAG(fault_access, L4_Writable))
	{
		/* duplicate, drop reference, and remap on top of the old page. */
#if 0
		printf("%s: duplicating rc %d phys page %#lx for write into %#lx\n",
			__func__, page->page->refcount, page->page->local_addr,
			page_addr);
#endif
		assert(!CHECK_FLAG(page->access, L4_Writable));
		struct fs_phys_page *copy = alloc_new_page();
		memcpy((void *)copy->local_addr, (void *)page->page->local_addr,
			PAGE_SIZE);
		page->page->refcount--;
		page->page = copy;
		page->access |= L4_Writable;
	} else {
#if 0
		printf("%s: remap old page at %#lx (access %#x)\n", __func__,
			page->page->local_addr, (unsigned)page->access);
#endif
	}

	L4_Fpage_t fp = L4_Fpage(page->page->local_addr, PAGE_SIZE);
	L4_Set_Rights(&fp, page->access & fault_access);
	L4_MapItem_t mi = L4_MapItem(fp, page->address);
	L4_LoadMR(0, (L4_MsgTag_t){ .X.t = 2 }.raw);
	L4_LoadMRs(1, 2, mi.raw);

	return true;
}


/* FIXME: threads added like this have an implicit UTCB allocated for them, but
 * use some other UTCB address configured by their actual creator. this may
 * cause fuckups when forkserv's thread creation function is called from the
 * original testbench space.
 */
static bool handle_add_tid(L4_Word_t space_id, L4_ThreadId_t tid)
{
	size_t tno_hash = int_hash(L4_ThreadNo(tid));
	struct fs_thread *t;

	/* just find the matching thread number for overwrite. */
	struct htable_iter it;
	for(t = htable_firstval(&thread_hash, &it, tno_hash);
		t != NULL;
		t = htable_nextval(&thread_hash, &it, tno_hash))
	{
		if(L4_ThreadNo(t->tid) == L4_ThreadNo(tid)) break;
	}

	if(t != NULL) {
		t->tid = tid;
		t->space = get_space(space_id);
		if(t->tid.raw != tid.raw) {
			htable_delval(&thread_hash, &it);
			t->tid = tid;
			htable_add(&thread_hash, int_hash(tid.raw), &t->tid.raw);
		}
	} else {
		t = malloc(sizeof(struct fs_thread));
		*t = (struct fs_thread){ .tid = tid, .space = get_space(space_id) };
		htable_add(&thread_hash, tno_hash, &t->tid);

		int slot;
		for(slot = THREADS_PER_SPACE - 1; slot >= 0; --slot) {
			if(t->space->threads[slot] == NULL) break;
		}
		if(slot < 0) {
			printf("%s: no slots left in space %lu!\n", __func__,
				space_id);
			return false;		/* (breaks unit testing, which is OK) */
		}
		t->space->threads[slot] = t;
	}

	L4_LoadMR(0, 0);
	return true;
}


static bool handle_sbrk(L4_ThreadId_t from, L4_Word_t new_break)
{
	struct fs_space *sp = get_space_by_tid(from);
	if(sp == NULL) {
		printf("%s: can't find for tid %#lx's fs_space\n", __func__, from.raw);
		return false;
	}

	sp->prog_brk = MIN(L4_Word_t, sp->prog_brk, new_break);

	L4_LoadMR(0, 0);
	return true;
}


/* returns 1 on success, 0 on syscall failure, and L4_IpcFailed(*tag_out),
 * L4_ErrorCode() on ipc failure.
 */
static L4_Word_t fpager_threadctl(
	L4_MsgTag_t *tag_out,
	L4_Word_t *ec_out,
	L4_ThreadId_t dest,
	L4_ThreadId_t space,
	L4_ThreadId_t scheduler,
	L4_ThreadId_t pager,
	void *utcb_location)
{
	L4_LoadBR(0, 0);
	L4_LoadMR(0, (L4_MsgTag_t){ .X.label = FPAGER_THREADCTL, .X.u = 5 }.raw);
	L4_LoadMR(1, dest.raw);
	L4_LoadMR(2, space.raw);
	L4_LoadMR(3, scheduler.raw);
	L4_LoadMR(4, pager.raw);
	L4_LoadMR(5, (L4_Word_t)utcb_location);
	*tag_out = L4_Call(fpager_tid);
	if(L4_IpcFailed(*tag_out)) return 2;
	if(tag_out->X.u != 2 || tag_out->X.t != 0) {
		printf("%s: weird reply tag; u %d, t %d, label %#lx\n", __func__,
			tag_out->X.u, tag_out->X.t, (L4_Word_t)tag_out->X.label);
	}

	L4_Word_t retval;
	L4_StoreMR(1, &retval);
	L4_StoreMR(2, ec_out);
	return retval;
}


/* see comment above fpager_threadctl() */
static L4_Word_t fpager_spacectl(
	L4_MsgTag_t *tag_out,
	L4_Word_t *ec_out,
	L4_ThreadId_t spacespec,
	L4_Word_t control,
	L4_Fpage_t kip_area,
	L4_Fpage_t utcb_area,
	L4_ThreadId_t redirector,
	L4_Word_t *ctl_out)
{
	L4_LoadBR(0, 0);
	L4_LoadMR(0, (L4_MsgTag_t){ .X.label = FPAGER_SPACECTL, .X.u = 5 }.raw);
	L4_LoadMR(1, spacespec.raw);
	L4_LoadMR(2, control);
	L4_LoadMR(3, kip_area.raw);
	L4_LoadMR(4, utcb_area.raw);
	L4_LoadMR(5, redirector.raw);
	*tag_out = L4_Call(fpager_tid);
	if(L4_IpcFailed(*tag_out)) return 2;
	if(tag_out->X.u != 3 || tag_out->X.t != 0) {
		printf("%s: weird reply tag; u %d, t %d, label %#lx\n", __func__,
			tag_out->X.u, tag_out->X.t, (L4_Word_t)tag_out->X.label);
	}

	L4_Word_t retval;
	L4_StoreMR(1, &retval);
	L4_StoreMR(2, ec_out);
	L4_StoreMR(3, ctl_out);
	return retval;
}


/* TODO: this function just plain doesn't clean up on failure.
 *
 * FIXME: it should also execute somewhere besides the pager's main thread, as
 * it involves calls to a thread that's paged by the former, therefore call
 * recursion and infinite fuckery.
 */
static bool handle_new_thread(
	L4_ThreadId_t from,
	L4_Word_t space_id,
	L4_Word_t instptr,
	L4_Word_t stkptr,
	int req_threadnum)
{
	const char *what;

	struct fs_space *sp;
	if(space_id == ~(L4_Word_t)0) {
		sp = get_space_by_tid(from);
		space_id = sp->id;
	} else {
		sp = get_space(space_id);
	}

	/* forkserv's initial pager will do privileged operations on forkserv's
	 * behalf.
	 */
	int t;
	if(req_threadnum >= 0 && req_threadnum < THREADS_PER_SPACE
		&& sp->threads[req_threadnum] == NULL)
	{
		t = req_threadnum;
	} else {
		for(t = 0; t < THREADS_PER_SPACE; t++) {
			if(sp->threads[t] == NULL) break;
		}
		if(t >= THREADS_PER_SPACE) goto fail;
	}

	L4_ThreadId_t new_tid = L4_GlobalId((space_id << TPS_SHIFT) + t, space_id + 1),
		space_tid = new_tid;
	for(int i=0; i < THREADS_PER_SPACE; i++) {
		if(sp->threads[i] != NULL) {
			space_tid = sp->threads[i]->tid;
			break;
		}
	}
	L4_MsgTag_t tag;
	L4_Word_t syscall_ec, retval = fpager_threadctl(&tag, &syscall_ec,
		new_tid, space_tid, L4_Myself(), L4_Myself(), (void *)-1);
	if(retval != 1) goto tc_fail;

	if(space_tid.raw == new_tid.raw) {
		/* also initialize the new address space. */
		L4_Word_t ctl_out;
		retval = fpager_spacectl(&tag, &syscall_ec,
			space_tid, 0, sp->kip_area, sp->utcb_area, L4_nilthread,
			&ctl_out);
		if(retval != 1) goto sc_fail;
	}

	/* set a lower priority so that the thread doesn't start up and cause
	 * pagefaults before the second ThreadControl changes its pager TCR.
	 */
	L4_Word_t timectl_out;
	retval = L4_Schedule(new_tid, ~0ul, ~0ul, 80, ~0ul, &timectl_out);
	if(retval == L4_SCHEDRESULT_ERROR) {
		printf("%s: L4_Schedule() failed: ec %#lx\n", __func__,
			L4_ErrorCode());
		abort();
	}

	retval = fpager_threadctl(&tag, &syscall_ec,
		new_tid, space_tid, L4_nilthread, L4_nilthread,
		(void *)L4_Address(sp->utcb_area) + t * UTCB_SIZE);
	if(retval != 1) goto tc_fail;

	/* finally, a breath-of-life IPC.
	 *
	 * TODO: strictly speaking this should be sent via forkserv_tid.
	 */
	L4_LoadMR(0, (L4_MsgTag_t){ .X.u = 2 }.raw);
	L4_LoadMR(1, instptr);
	L4_LoadMR(2, stkptr);
	tag = L4_Send(new_tid);
	if(L4_IpcFailed(tag)) {
		printf("%s: breath of life to %lu:%lu failed, ec %#lx\n",
			__func__, L4_ThreadNo(new_tid), L4_Version(new_tid),
			L4_ErrorCode());
		abort();
	}

	/* and a final pager switch. */
	retval = fpager_threadctl(&tag, &syscall_ec,
		new_tid, space_tid, forkserv_tid, forkserv_tid, (void *)-1);
	if(retval != 1) {
		printf("%s: pager reset failed, ec %#lx\n", __func__,
			L4_ErrorCode());
		abort();
	}

	sp->threads[t] = malloc(sizeof(struct fs_thread));
	*sp->threads[t] = (struct fs_thread){ .space = sp, .tid = new_tid };
	htable_add(&thread_hash, int_hash(L4_ThreadNo(new_tid)),
		&sp->threads[t]->tid);

	L4_LoadMR(0, (L4_MsgTag_t){ .X.u = 1 }.raw);
	L4_LoadMR(1, new_tid.raw);
	return true;

tc_fail:
	what = "threadctl";
	goto syscall_fail;

sc_fail:
	what = "spacectl";

syscall_fail:
	if(retval == 2) {
		printf("%s: ipc fail in %s: ec %#lx\n", __func__, what,
			L4_ErrorCode());
	} else if(retval == 0) {
		printf("%s: %s fail, ec %#lx\n", __func__, what, syscall_ec);
	} else {
		printf("%s: odd retval %lu\n", __func__, retval);
		assert(false);
	}

fail:
	L4_LoadMR(0, (L4_MsgTag_t){ .X.u = 1 }.raw);
	L4_LoadMR(1, L4_nilthread.raw);
	return true;
}


static bool handle_fork(L4_ThreadId_t from)
{
	L4_Word_t copy_id;

	struct fs_space *sp = get_space_by_tid(from);
	if(sp == NULL) {
		printf("%s: can't find fs_space of tid %#lx\n", __func__, from.raw);
		copy_id = ~(L4_Word_t)0;
		goto end;
	}

	do {
		copy_id = next_space_id++;
	} while(htable_get(&space_hash, int_hash(copy_id), &word_cmp,
		&copy_id) != NULL);
	struct fs_space *copy_space = malloc(sizeof(*copy_space));
	copy_space->id = copy_id;
	copy_space->parent_id = sp->id;
	copy_space->prog_brk = sp->prog_brk;
	copy_space->utcb_area = sp->utcb_area;
	copy_space->kip_area = sp->kip_area;
	list_head_init(&copy_space->dead_children);
	list_head_init(&copy_space->waiting_threads);
	htable_init(&copy_space->pages, &hash_word, NULL);
	htable_add(&space_hash, int_hash(copy_space->id), &copy_space->id);

	/* do the thing. with the pages and so on. */
	L4_Fpage_t unmapbuf[64];
	int num_unmap = 0;
	struct htable_iter it;
	for(struct fs_vpage *ovp = htable_first(&sp->pages, &it);
		ovp != NULL;
		ovp = htable_next(&sp->pages, &it))
	{
		struct fs_vpage *cvp = malloc(sizeof(*cvp));
		cvp->address = ovp->address;
		cvp->access = L4_Readable;
		cvp->page = ovp->page;
		cvp->page->refcount++;
		ovp->access = L4_Readable;
		htable_add(&copy_space->pages, int_hash(cvp->address),
			&cvp->address);

		unmapbuf[num_unmap] = L4_Fpage(ovp->page->local_addr, PAGE_SIZE);
		/* XXX would only need L4_Writable, but until unmap works, ... */
		L4_Set_Rights(&unmapbuf[num_unmap], L4_FullyAccessible);
		num_unmap++;

		if(num_unmap == 64) {
			L4_UnmapFpages(64, unmapbuf);
			num_unmap = 0;
		}
	}
	if(num_unmap > 0) L4_UnmapFpages(num_unmap, unmapbuf);
	assert(!CHECK_FLAG(copy_id, 0x80000000ul));

end:
	L4_LoadMR(0, (L4_MsgTag_t){ .X.u = 1 }.raw);
	L4_LoadMR(1, copy_id);
	return true;
}


static bool handle_as_cfg(
	L4_ThreadId_t from,
	L4_Word_t space_id,
	L4_Fpage_t kip_area,
	L4_Fpage_t utcb_area)
{
	struct fs_space *sp = get_space(space_id);
	if(!L4_IsNilFpage(kip_area)) sp->kip_area = kip_area;
	if(!L4_IsNilFpage(utcb_area)) sp->utcb_area = utcb_area;

#if 0
	printf("%s: from %#lx, space_id %#lx, kip_area %#lx:%#lx, utcb_area %#lx:%#lx\n",
		__func__, from.raw, space_id, L4_Address(kip_area), L4_Size(kip_area),
		L4_Address(utcb_area), L4_Size(utcb_area));
#endif

	return true;
}


static bool handle_unmap(
	L4_ThreadId_t from,
	L4_Word_t *n_p,
	L4_Fpage_t *pages)
{
	struct fs_space *sp = get_space_by_tid(from);
	if(sp == NULL) {
		printf("%s: unknown space for %#lx\n", __func__, from.raw);
		return false;
	}

	/* holy nested loops, batman! */
	L4_Fpage_t local[63], output[63];
	int outpos = 0;
	for(int i=0, in_len = *n_p; i < in_len && outpos < 63; i++) {
		for(L4_Word_t fp_addr = L4_Address(pages[i]),
					  fp_max = fp_addr + L4_Size(pages[i]);
			fp_addr < fp_max && outpos < 63;
			fp_addr += PAGE_SIZE)
		{
			struct fs_vpage *vp = htable_get(&sp->pages,
				int_hash(fp_addr), &word_cmp, &fp_addr);
			if(vp == NULL) continue;

			local[outpos] = L4_FpageLog2(vp->page->local_addr, 12);
			output[outpos] = L4_FpageLog2(fp_addr, 12);
			L4_Set_Rights(&local[outpos], L4_Rights(pages[i]));
			outpos++;
		}
	}
	L4_UnmapFpages(outpos, local);
	for(int i=0; i < outpos; i++) {
		L4_Set_Rights(&output[outpos], L4_Rights(local[outpos]));
	}
	*n_p = outpos;

	return true;
}


static void destroy_space(struct fs_space *sp)
{
	struct fs_space *cur, *next, *parent = get_space(sp->parent_id);
	assert(parent != NULL);
	assert(parent != sp);
	list_for_each_safe(&sp->dead_children, cur, next, dead_link) {
		list_del(&cur->dead_link);
		list_add_tail(&parent->dead_children, &cur->dead_link);
	}
	free(sp);
}


static bool handle_wait(L4_ThreadId_t ipc_from)
{
	struct fs_space *sp = get_space_by_tid(ipc_from);
	if(sp == NULL) return false;

	if(list_empty(&sp->dead_children)) {
		struct fs_thread *t = get_thread(ipc_from);
		list_add_tail(&sp->waiting_threads, &t->wait_link);
		return false;
	} else {
		struct fs_space *dead = list_top(&sp->dead_children,
			struct fs_space, dead_link);
		list_del_from(&sp->dead_children, &dead->dead_link);
		L4_Word_t wait_id = dead->id;
		int status = dead->exit_status;
		destroy_space(dead);

		L4_LoadMR(0, (L4_MsgTag_t){ .X.u = 2 }.raw);
		L4_LoadMR(1, wait_id);
		L4_LoadMR(2, status);
		return true;
	}
}


static bool handle_exit(L4_ThreadId_t *ipc_from, int status)
{
	struct fs_thread *t = get_thread(*ipc_from);
	if(t == NULL) {
		printf("forkserv/exit: unknown TID %lu:%lu\n",
			L4_ThreadNo(*ipc_from), L4_Version(*ipc_from));
		return false;
	}

	struct fs_space *sp = t->space;
	int tnum = L4_ThreadNo(t->tid) - (sp->id << TPS_SHIFT);
	assert(sp->threads[tnum] == t);
	sp->threads[tnum] = NULL;

	L4_MsgTag_t tag;
	L4_Word_t ec, res = fpager_threadctl(&tag, &ec, *ipc_from, L4_nilthread,
		L4_nilthread, L4_nilthread, (void *)-1);
	if(res != 1) {
		printf("forkserv/exit: threadctl failed, %s ec %#lx\n",
			L4_IpcFailed(tag) ? "ipc" : "syscall", ec);
		/* can't do much else. */
	}
	htable_del(&thread_hash, int_hash(L4_ThreadNo(t->tid)), &t->tid);
	free(t);

	/* stop here if other threads remain. */
	for(int i=0; i < THREADS_PER_SPACE; i++) {
		if(sp->threads[i] != NULL) return false;
	}

	/* destroy the virtual memory bits and zombify the address space */
	assert(list_empty(&sp->waiting_threads));
	htable_del(&space_hash, int_hash(sp->id), &sp->id);
	struct htable_iter it;
	for(struct fs_vpage *vp = htable_first(&sp->pages, &it);
		vp != NULL;
		vp = htable_next(&sp->pages, &it))
	{
		if(vp->page != NULL) {
			if(--vp->page->refcount == 0) {
				/* FIXME: add to free page list */
			}
		}
		free(vp);
	}
	htable_clear(&sp->pages);
	sp->utcb_area = L4_Nilpage;
	L4_Word_t dead_id = sp->id;

	/* activate waiting thread, or go to eternal sleep */
	struct fs_space *parent = get_space(t->space->parent_id);
	assert(parent != NULL);
	assert(sp != parent);		/* space 0 cannot die */
	if(!list_empty(&parent->waiting_threads)) {
		struct fs_thread *wakeup = list_top(&parent->waiting_threads,
			struct fs_thread, wait_link);
		list_del_from(&parent->waiting_threads, &wakeup->wait_link);
		/* move own dead children over to parent */
		destroy_space(sp);

		L4_LoadMR(0, (L4_MsgTag_t){ .X.u = 2 }.raw);
		L4_LoadMR(1, dead_id);
		L4_LoadMR(2, status);
		*ipc_from = wakeup->tid;
		return true;
	} else {
		/* the dead walk */
		list_add_tail(&parent->dead_children, &sp->dead_link);
		sp->exit_status = status;
		return false;
	}
}


/* used by forkserv_dispatch_loop to track async operation id to sender
 * thread.
 */
struct async_op {
	L4_Word_t id;
	L4_ThreadId_t from;
};


/* (not quite certain if this'll work with just a single thread. more RAM can
 * be had from sigma0, though, so there's no dependency there.)
 */
static void forkserv_dispatch_loop(void)
{
	struct htable async_hash = HTABLE_INITIALIZER(async_hash,
		&hash_word, NULL);
	for(;;) {
		L4_ThreadId_t from;
		L4_MsgTag_t tag = L4_Wait(&from);
		for(;;) {
			if(L4_IpcFailed(tag)) {
				printf("IPC failed: ec %#lx\n", L4_ErrorCode());
				break;
			}

			bool reply = true;
			switch(tag.X.label) {
				case FORKSERV_AS_CFG: {
					L4_Word_t space_id;
					L4_Fpage_t kip_area, utcb_area;
					L4_StoreMR(1, &space_id);
					L4_StoreMR(2, &kip_area.raw);
					L4_StoreMR(3, &utcb_area.raw);
					reply = handle_as_cfg(from, space_id,
						kip_area, utcb_area);
					break;
				}

				case FORKSERV_SEND_PAGE:
					reply = handle_send_page(from);
					break;

				case FORKSERV_ADD_TID: {
					L4_Word_t space_id;
					L4_ThreadId_t tid;
					L4_StoreMR(1, &space_id);
					L4_StoreMR(2, &tid.raw);
					reply = handle_add_tid(space_id, tid);
					break;
				}

				case FORKSERV_SBRK: {
					L4_Word_t new_break;
					L4_StoreMR(1, &new_break);
					reply = handle_sbrk(from, new_break);
					break;
				}

				case FORKSERV_FORK:
					reply = handle_fork(from);
					break;

				case FSHELPER_CHOPCHOP_REPLY: {
					L4_Word_t async_id;
					L4_MsgTag_t reply_tag;
					L4_StoreMR(1, &async_id);
					L4_StoreMR(2, &reply_tag.raw);
					L4_Word_t words[62];
					L4_StoreMRs(3, reply_tag.X.u + reply_tag.X.t, words);
					size_t hash = int_hash(async_id);
					struct async_op *op = htable_get(&async_hash, hash,
						&word_cmp, &async_id);
					if(op == NULL) {
						printf("%s: unknown async op %#lx\n", __func__,
							async_id);
						reply = false;
						break;
					}
					htable_del(&async_hash, hash, op);
					from = op->from;
					free(op);
					L4_LoadMR(0, reply_tag.raw);
					L4_LoadMRs(1, reply_tag.X.u + reply_tag.X.t, words);
					reply = true;
					break;
				}

				case FSHELPER_CHOPCHOP_DONE:
					/* check the queue for our armless friend. */
					if(helper_queue != NULL) {
						L4_LoadMR(0, (L4_MsgTag_t){
								.X.u = 1, .X.label = FSHELPER_CHOPCHOP,
							}.raw);
						L4_LoadMR(1, 0);	/* byoq */
						L4_Reply(helper_tid);
					}
					reply = false;
					break;

				case FORKSERV_NEW_THREAD: {
					L4_Word_t space_id, ip, sp, req_tnum;
					L4_StoreMR(1, &space_id);
					L4_StoreMR(2, &ip);
					L4_StoreMR(3, &sp);
					L4_StoreMR(4, &req_tnum);
					/* pass it to the queued handler */
					L4_Word_t aid = next_async_id++;
					struct async_op *op = malloc(sizeof(*op));
					op->id = aid;
					op->from = from;
					htable_add(&async_hash, int_hash(op->id), &op->id);
					struct helper_work *w = malloc(sizeof(struct helper_work)
						+ 7 * sizeof(L4_Word_t));
					w->from = from;
					/* FIXME: this format is bizarre. */
					w->mrs[0] = (L4_MsgTag_t){
							.X.u = 6, .X.label = FSHELPER_CHOPCHOP,
						}.raw;
					w->mrs[1] = aid;
					w->mrs[2] = tag.raw;
					w->mrs[3] = space_id;
					w->mrs[4] = ip;
					w->mrs[5] = sp;
					w->mrs[6] = req_tnum;
					L4_LoadMR(0, (L4_MsgTag_t){
							.X.u = 1, .X.label = FSHELPER_CHOPCHOP,
						}.raw);
					L4_LoadMR(1, (L4_Word_t)w);
					L4_MsgTag_t pass_tag = L4_Send_Timeout(helper_tid,
						L4_ZeroTime);
					if(L4_IpcFailed(pass_tag) && L4_ErrorCode() == 2) {
						do {
							w->next = *(struct helper_work *volatile *)&helper_queue;
						} while(!__sync_bool_compare_and_swap(&helper_queue,
							w->next, w));
						L4_Time_t x, y;
						L4_Word_t sr = L4_Timeslice(helper_tid, &x, &y);
						printf("recursion recurs! sched result %lu\n", sr);
						reply = false;
					} else if(L4_IpcFailed(tag)) {
						printf("helper_tid call failed: ec %#lx\n",
							L4_ErrorCode());
						free(w);
						reply = false;
					} else {
						assert(L4_IpcSucceeded(tag));
						reply = false;
					}
					break;
				}

				case FORKSERV_UNMAP: {
					L4_Fpage_t pages[63];
					L4_Word_t n = tag.X.u;
					L4_StoreMRs(1, n, &pages[0].raw);
					reply = handle_unmap(from, &n, pages);
					if(reply) {
						L4_LoadMR(0, (L4_MsgTag_t){ .X.u = n }.raw);
						L4_LoadMRs(1, n, &pages[0].raw);
					}
					break;
				}

				case FORKSERV_WAIT: reply = handle_wait(from); break;

				case FORKSERV_EXIT: {
					L4_Word_t status;
					L4_StoreMR(1, &status);
					reply = handle_exit(&from, status);
					break;
				}

				default:
					if((tag.X.label & 0xfff0) == 0xffe0
						&& tag.X.u == 2 && tag.X.t == 0)
					{
						/* pagefault */
						L4_Word_t ip, addr;
						L4_StoreMR(1, &addr);
						L4_StoreMR(2, &ip);
						reply = handle_pf(from, addr, ip, tag.X.label & 0xf);
					} else {
						printf("label %#x not recognized\n",
							(unsigned)tag.X.label);
						reply = false;
					}
					break;
			}
			if(!reply) break;

			tag = L4_ReplyWait(from, &from);
		}
	}
}


static struct helper_work *reverse_work(struct helper_work *w)
{
	struct helper_work *prev = NULL;
	while(w != NULL) {
		struct helper_work *next = w->next;
		w->next = prev;
		prev = w;
		w = next;
	}

	return prev;
}


static void helper_chopchop(L4_ThreadId_t reply_tid, struct helper_work *w)
{
	L4_Word_t async_id = w->mrs[1], space_id = w->mrs[3],
		ip = w->mrs[4], sp = w->mrs[5], req_tnum = w->mrs[6];
	L4_MsgTag_t tag = { .raw = w->mrs[2] };
	L4_ThreadId_t from = w->from;
	free(w);
	bool reply;
	switch(tag.X.label) {
		case FORKSERV_NEW_THREAD: {
			reply = handle_new_thread(from, space_id, ip, sp, req_tnum);
			break;
		}

		default:
			printf("%s: unknown label %#lx\n", __func__,
				(L4_Word_t)tag.X.label);
			reply = false;
	}

	if(reply) {
		L4_Word_t tmp[64];
		L4_StoreMR(0, &tag.raw);
		int n_words = tag.X.u + tag.X.t;
		if(n_words > 62) {
			n_words = 62;
			printf("%s: reply too large (truncated to %d words)\n", __func__,
				n_words);
		}
		L4_StoreMRs(1, n_words, tmp);
		L4_LoadMR(0, (L4_MsgTag_t){
				.X.label = FSHELPER_CHOPCHOP_REPLY, .X.u = n_words + 2,
			}.raw);
		/* chopchop reply metadata. */
		L4_LoadMR(1, async_id);
		L4_LoadMR(2, tag.raw);
		/* reply body (typed items transferred as untyped) */
		L4_LoadMRs(3, n_words, tmp);
		tag = L4_Send(reply_tid);
		if(L4_IpcFailed(tag)) {
			printf("%s: reply failed; ec %#lx\n", __func__, L4_ErrorCode());
		}
	}
}


static void helper_thread_fn(void)
{
	L4_Set_ExceptionHandler(L4_Pager());

	for(;;) {
		L4_ThreadId_t from;
		L4_MsgTag_t tag;

		/* polling, polling, polling, polling... */
		L4_LoadMR(0, (L4_MsgTag_t){ .X.label = FSHELPER_CHOPCHOP_DONE }.raw);
		tag = L4_Ipc(forkserv_tid, L4_anythread,
			L4_Timeouts(L4_Never, L4_Never), &from);

		for(;;) {
			if(L4_IpcFailed(tag)) {
				printf("forkserv helper IPC failed: ec %#lx\n",
					L4_ErrorCode());
				break;
			}

			bool reply;
			switch(tag.X.label) {
				case FSHELPER_CHOPCHOP: {
					L4_Word_t first;
					L4_StoreMR(1, &first);
					if(first != 0) {
						helper_chopchop(from, (struct helper_work *)first);
					}

					struct helper_work *w;
					do {
						w = *(struct helper_work *volatile *)&helper_queue;
					} while(w != NULL
						&& !__sync_bool_compare_and_swap(&helper_queue,
							w, NULL));

					if(w != NULL) {
						w = reverse_work(w);
						while(w != NULL) {
							struct helper_work *next = w->next;
							helper_chopchop(from, w);
							w = next;
						}
					}

					reply = false;
					break;
				}

				default:
					printf("forkserv helper got weird IPC label %#lx from %#lx\n",
						(L4_Word_t)tag.X.label, from.raw);
					reply = false;
			}

			if(reply) {
				L4_Reply(from);
			}
			/* fold race condition in the producer. */
			L4_LoadMR(0, (L4_MsgTag_t){
					.X.label = FSHELPER_CHOPCHOP_DONE,
				}.raw);
			/* "SendWait" */
			tag = L4_Ipc(forkserv_tid, L4_anythread,
				L4_Timeouts(L4_Never, L4_Never), &from);
		}
	}
}


static void start_helper_thread(void)
{
	L4_ThreadId_t self = L4_Myself();
	helper_tid = L4_GlobalId(L4_ThreadNo(self) + 1, 23);
	L4_MsgTag_t tag = { .raw = 0 };
	L4_Word_t my_utcb = (L4_Word_t)__L4_Get_UtcbAddress();	/* FIXME: nonportable! */
	L4_Word_t syscall_ec = 0,
		retval = fpager_threadctl(&tag, &syscall_ec,
			helper_tid, self, self, L4_Pager(),
			/* FIXME: no idea how this is properly supposed to go. the UTCB
			 * pointer goes to the middle of the memory region, though.
			 */
			(void *)((my_utcb + UTCB_SIZE) & ~(UTCB_SIZE - 1)));
	if(retval != 1) {
		printf("%s: retval %lu, ec %#lx, own ec %#lx\n", __func__,
			retval, syscall_ec, L4_ErrorCode());
		abort();
	}

	void *helper_stack = malloc(16 * 1024);
	L4_Word_t helper_sp = (L4_Word_t)helper_stack + 16 * 1024 - 32;
	L4_Start_SpIp(helper_tid, helper_sp, (L4_Word_t)&helper_thread_fn);
}


int main(void)
{
	L4_Set_ExceptionHandler(L4_Pager());
	console_tid = L4_Pager();
	fpager_tid = L4_Pager();
	forkserv_tid = L4_Myself();

	heap_init(0x80000);		/* leave 512 KiB for testbench */
	start_helper_thread();
	forkserv_dispatch_loop();

	return 0;
}
