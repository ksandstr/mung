
/* service that takes over the root task's memory, letting it fork
 * subprocesses (and subprocesses of those). this is useful for test cases
 * involving map operations in IPC, or the Unmap system call.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <ccan/htable/htable.h>
#include <ccan/likely/likely.h>
#include <ccan/container_of/container_of.h>

#include <l4/types.h>
#include <l4/ipc.h>
#include <l4/syscall.h>
#include <l4/kip.h>

/* for int_hash() */
#include <ukernel/util.h>

#include "defs.h"
#include "forkserv.h"


/* FIXME: get this from KIP... */
#define PAGE_SIZE 4096


struct fs_space
{
	L4_Word_t id, prog_brk;
	struct htable pages;
};


struct fs_page
{
	L4_Word_t address;		/* key in "pages" */
	L4_Word_t local_addr;	/* address in forkserv */
};


struct tid_to_space {
	L4_ThreadId_t tid;
	struct fs_space *space;
};


static size_t hash_word(const void *, void *);


static struct htable space_hash = HTABLE_INITIALIZER(space_hash,
	&hash_word, NULL);
static struct htable tid_to_space_hash = HTABLE_INITIALIZER(tid_to_space_hash,
	&hash_word, NULL);

static L4_ThreadId_t console_tid;
static L4_Word_t map_range_pos = 0;


static size_t hash_word(const void *elem, void *priv) {
	L4_Word_t val = *(const L4_Word_t *)elem;
	return int_hash(val);
}


static bool word_cmp(const void *a, void *b) {
	return *(const L4_Word_t *)a == *(L4_Word_t *)b;
}


static struct fs_space *get_space(L4_Word_t id)
{
	struct fs_space *sp;
	void *ptr = htable_get(&space_hash, int_hash(id), &word_cmp, &id);
	if(ptr != NULL) sp = container_of(ptr, struct fs_space, id);
	else {
		sp = malloc(sizeof(*sp));
		sp->id = id;
		sp->prog_brk = find_phys_mem_top();
		htable_init(&sp->pages, &hash_word, NULL);
		htable_add(&space_hash, int_hash(id), &sp->id);
	}

	return sp;
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

	struct fs_page *p = malloc(sizeof(*p));
	p->address = phys_addr;
	p->local_addr = L4_Address(window);
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
static bool handle_pf(L4_ThreadId_t from, L4_Word_t addr, L4_Word_t ip)
{
#if 0
	printf("pf in %d:%d (ip %#lx, addr %#lx)\n",
		from.global.X.thread_no, from.global.X.version,
		ip, addr);
#endif

	struct tid_to_space *map = htable_get(&tid_to_space_hash,
		int_hash(from.raw), &word_cmp, &from.raw);
	if(map == NULL) {
		printf("source %#lx isn't known\n", from.raw);
		return false;
	}

	L4_Word_t page_addr = addr & ~PAGE_MASK;
	struct fs_page *page = htable_get(&map->space->pages,
		int_hash(page_addr), &word_cmp, &page_addr);
	if(page == NULL && page_addr >= map->space->prog_brk) {
		/* moar ramz pls */
		void *ptr = valloc(PAGE_SIZE);
		page = malloc(sizeof(*page));
		page->address = page_addr;
		page->local_addr = (L4_Word_t)ptr;
		htable_add(&map->space->pages, int_hash(page_addr), &page->address);
	} else if(page == NULL) {
		printf("segfault in thread %#lx, space %lu (brk %#lx)\n", from.raw,
			map->space->id, map->space->prog_brk);
		return false;
	}

	L4_Fpage_t fp = L4_Fpage(page->local_addr, PAGE_SIZE);
	L4_Set_Rights(&fp, L4_FullyAccessible);
	L4_MapItem_t mi = L4_MapItem(fp, page->address);
	L4_LoadMR(0, (L4_MsgTag_t){ .X.t = 2 }.raw);
	L4_LoadMRs(1, 2, mi.raw);

	return true;
}


static bool handle_add_tid(L4_Word_t space_id, L4_ThreadId_t tid)
{
	struct tid_to_space *map = htable_get(&tid_to_space_hash,
		int_hash(tid.raw), &word_cmp, &tid.raw);
	if(map == NULL) {
		map = malloc(sizeof(*map));
		map->tid = tid;
		htable_add(&tid_to_space_hash, int_hash(tid.raw), &map->tid.raw);
	}
	map->space = get_space(space_id);

	L4_LoadMR(0, 0);
	return true;
}


static bool handle_sbrk(L4_ThreadId_t from, L4_Word_t new_break)
{
	struct tid_to_space *map = htable_get(&tid_to_space_hash,
		int_hash(from.raw), &word_cmp, &from.raw);
	if(map == NULL) {
		printf("%s: no space for tid %#lx\n", __func__, from.raw);
		return false;
	}

	map->space->prog_brk = MIN(L4_Word_t, map->space->prog_brk, new_break);

	L4_LoadMR(0, 0);
	return true;
}


static bool handle_new_thread(
	L4_ThreadId_t from,
	L4_Word_t space_id,
	L4_Word_t ip,
	L4_Word_t sp)
{
	/* yeah, no. */
	printf("forkserv: new_thread not implemented\n");
	return false;
}


static bool handle_fork(L4_ThreadId_t from)
{
	/* not quite yet. */
	printf("forkserv: would've forked for tid %#lx\n", from.raw);
	L4_LoadMR(0, (L4_MsgTag_t){ .X.u = 1 }.raw);
	L4_LoadMR(1, ~(L4_Word_t)0);
	return true;
}


/* (not quite certain if this'll work with just a single thread. more RAM can
 * be had from sigma0, though, so there's no dependency there.)
 */
static void forkserv_dispatch_loop(void)
{
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

				case FORKSERV_NEW_THREAD: {
					L4_Word_t space_id, ip, sp;
					L4_StoreMR(1, &space_id);
					L4_StoreMR(2, &ip);
					L4_StoreMR(3, &sp);
					reply = handle_new_thread(from, space_id, ip, sp);
					break;
				}

				case FORKSERV_EXIT:
					printf("label %#x not implemented yet\n",
						(unsigned)tag.X.label);
					reply = false;
					break;

				default:
					if((tag.X.label & 0xfff0) == 0xffe0
						&& tag.X.u == 2 && tag.X.t == 0)
					{
						/* pagefault */
						L4_Word_t ip, addr;
						L4_StoreMR(1, &addr);
						L4_StoreMR(2, &ip);
						reply = handle_pf(from, addr, ip);
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


int main(void)
{
	L4_Set_ExceptionHandler(L4_Pager());
	console_tid = L4_Pager();

	heap_init(0x80000);		/* leave 512 KiB for testbench */
	forkserv_dispatch_loop();

	return 0;
}
