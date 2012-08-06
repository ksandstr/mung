#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <ccan/compiler/compiler.h>
#include <ccan/htable/htable.h>

#include <l4/types.h>
#include <l4/thread.h>
#include <l4/ipc.h>
#include <l4/kip.h>
#include <l4/syscall.h>
#include <l4/bootinfo.h>

#include <ukernel/util.h>

#include "defs.h"
#include "forkserv.h"
#include "test.h"
#include "elf.h"


/* FIXME: derive these from the kernel interface page at runtime */
#define PAGE_SIZE 4096
#define PAGE_MASK 0xfff


/* pages tracked by the forkserv pager. mappings are granted to forkserv as
 * faults occur. presence of a forkserv_page means that a page was already
 * received from sigma0.
 */
struct forkserv_page {
	L4_Word_t address;
};


static size_t hash_forkserv_page(const void *key, void *priv);


static L4_ThreadId_t forkserv_pager, forkserv_tid;
static L4_Fpage_t forkserv_utcb_area;
static struct htable forkserv_pages = HTABLE_INITIALIZER(forkserv_pages,
	&hash_forkserv_page, NULL);
static L4_Word_t forkserv_start, forkserv_end;


/* runtime bits (TODO: move into a library?) */

void abort(void)
{
	printf("testbench abort() called!\n");
	L4_ThreadId_t dummy;
	for(;;) {
		L4_Ipc(L4_nilthread, L4_nilthread, L4_Timeouts(L4_Never, L4_Never),
			&dummy);
	}
}


void malloc_panic(void) {
	printf("%s: called!\n", __func__);
	abort();
}


int sched_yield(void) {
	L4_ThreadSwitch(L4_nilthread);
	return 0;
}


void __assert_failure(
	const char *condition,
	const char *file,
	unsigned int line,
	const char *function)
{
	printf("testbench %s(`%s', `%s', %u, `%s')\n", __func__,
		condition, file, line, function);
	abort();
	for(;;) { asm volatile("int $1"); }
}


static size_t hash_forkserv_page(const void *key, void *priv) {
	return int_hash(((struct forkserv_page *)key)->address);
}


static bool forkserv_page_cmp(const void *cand, void *key) {
	return ((struct forkserv_page *)cand)->address == *(L4_Word_t *)key;
}


static void add_forkserv_pages(L4_Word_t start, L4_Word_t end)
{
	for(L4_Word_t addr = start & ~PAGE_MASK;
		addr <= (end | PAGE_MASK);
		addr += PAGE_SIZE)
	{
		size_t hash = int_hash(addr);
		void *ptr = htable_get(&forkserv_pages, hash,
			&forkserv_page_cmp, &addr);
		if(ptr == NULL) {
			struct forkserv_page *p = malloc(sizeof(*p));
			p->address = addr;
			if(!htable_add(&forkserv_pages, hash, p)) {
				fprintf(stderr, "htable_add() failed\n");
				abort();
			}
		}
	}
}


/* FIXME: move this into a generic pager mechanism. the loop has been written
 * and copypasta'd often enough.
 */
static void forkserv_pager_fn(void *param UNUSED)
{
	for(;;) {
		L4_ThreadId_t from;
		L4_MsgTag_t tag = L4_Wait(&from);

		for(;;) {
			if(L4_IpcFailed(tag)) {
				// diag("reply/wait failed, ec %#lx", L4_ErrorCode());
				break;
			}

			if(tag.X.label >> 4 == 0xffe
				&& tag.X.u == 2 && tag.X.t == 0)
			{
				L4_Word_t faddr, fip;
				L4_StoreMR(1, &faddr);
				L4_StoreMR(2, &fip);
				// diag("%s: pf ip %#lx, addr %#lx\n", __func__, fip, faddr);
				int rwx = tag.X.label & 0x000f;

				/* look it up. */
				L4_Word_t page_addr = faddr & ~PAGE_MASK;
				struct forkserv_page *fp = htable_get(&forkserv_pages,
					int_hash(page_addr), &forkserv_page_cmp, &page_addr);
				if(fp != NULL) {
					/* map it without sigma0 */
					L4_Fpage_t map = L4_Fpage(fp->address, PAGE_SIZE);
					L4_Set_Rights(&map, L4_FullyAccessible);
					L4_MapItem_t mi = L4_MapItem(map, page_addr);
					L4_LoadMR(0, (L4_MsgTag_t){ .X.t = 2 }.raw);
					L4_LoadMR(1, mi.raw[0]);
					L4_LoadMR(2, mi.raw[1]);
				} else {
					/* pass the fault up to our pager (sigma0). */
					L4_LoadMR(0, (L4_MsgTag_t){ .X.label = 0xffe0 | rwx,
						.X.u = 2 }.raw);
					L4_LoadMR(1, faddr);
					L4_LoadMR(2, fip);
					L4_LoadBR(0, L4_CompleteAddressSpace.raw);
					tag = L4_Call(L4_Pager());
					if(L4_IpcFailed(tag)) {
						//diag("stats-to-pager IPC failed, ec %lu",
						//	L4_ErrorCode());
						break;
					} else if(tag.X.t != 2 || tag.X.u != 0) {
						//diag("stats-to-pager IPC returned weird tag %#lx",
						//	tag.raw);
						break;
					}
				}
			} else if((tag.X.label & 0xfff0) == 0xff80
				&& tag.X.u == 2 && tag.X.t == 0)
			{
				/* I/O faults (ia32, amd64) */
				L4_Fpage_t iofp;
				L4_StoreMR(1, &iofp.raw);
				if(!L4_IsIoFpage(iofp)) {
					printf("I/O fault didn't deliver I/O fpage? what.\n");
					break;
				}
				printf("iopf in %#lx, port range %#lx:%lu\n", from.raw,
					L4_IoFpagePort(iofp), L4_IoFpageSizeLog2(iofp));
				/* ... could forward the fault to sigma0, but why bother?
				 * forkserv won't do anything more than a debug printf()
				 * anyway.
				 */
				L4_Set_Rights(&iofp, L4_FullyAccessible);
				L4_MapItem_t map = L4_MapItem(iofp, 0);
				L4_Set_Rights(&map.X.snd_fpage, L4_FullyAccessible);
				L4_LoadMR(0, (L4_MsgTag_t){ .X.t = 2 }.raw);
				L4_LoadMR(1, map.raw[0]);
				L4_LoadMR(2, map.raw[1]);
			} else if(tag.X.label == 0x5370) {
				/* sigma0's con_putstr() protocol.
				 *
				 * NOTE: near-copypasta'd from kmain.c!
				 */
				char buf[257];
				for(int i=0; i < tag.X.u; i++) {
					L4_Word_t val;
					L4_StoreMR(i + 1, &val);
					memcpy(&buf[i * 4], &val, sizeof(L4_Word_t));
				}
				buf[tag.X.u * 4] = '\0';
				int len = strlen(buf);
				while(len > 0 && buf[len - 1] == '\n') buf[--len] = '\0';
				printf("[forkserv]: %s\n", buf);

				L4_LoadMR(0, 0);
			} else {
				printf("forkserv's pager got weird IPC from %#lx (label %#lx)\n",
					from.raw, (L4_Word_t)tag.X.label);
				break;
			}

			/* reply. */
			tag = L4_ReplyWait(from, &from);
		}
	}
}


static void start_forkserv(void)
{
	L4_KernelInterfacePage_t *kip = L4_GetKernelInterface();
	L4_BootInfo_t *bootinfo = (L4_BootInfo_t *)L4_BootInfo(kip);

	L4_BootRec_t *rec = L4_BootInfo_FirstEntry(bootinfo);
	bool found = false;
	for(L4_Word_t i = 0;
		i < L4_BootInfo_Entries(bootinfo);
		i++, rec = L4_BootRec_Next(rec))
	{
		if(rec->type != L4_BootInfo_Module) {
			printf("rec at %p is not module\n", rec);
			continue;
		}

		char *cmdline = L4_Module_Cmdline(rec);
		const char *slash = strrchr(cmdline, '/');
		if(slash != NULL && strcmp(slash + 1, "forkserv") == 0) {
			found = true;
			break;
		}
	}
	if(!found) {
		printf("can't find forkserv module; was it loaded?\n");
		abort();
	}

	forkserv_start = L4_Module_Start(rec);
	forkserv_end = forkserv_start + L4_Module_Size(rec) - 1;
	forkserv_pager = start_thread(&forkserv_pager_fn, NULL);
	if(L4_IsNilThread(forkserv_pager)) {
		printf("forkserv_pager_fn launch failed\n");
		abort();
	}

	/* parse and load the ELF32 binary. */
	const Elf32_Ehdr *ee = (void *)forkserv_start;
	if(memcmp(ee->e_ident, ELFMAG, SELFMAG) != 0) {
		printf("incorrect forkserv ELF magic\n");
		abort();
	}
	uintptr_t phoff = ee->e_phoff;
	for(int i=0; i < ee->e_phnum; i++, phoff += ee->e_phentsize) {
		const Elf32_Phdr *ep = (void *)(forkserv_start + phoff);
		if(ep->p_type != PT_LOAD) continue;	/* skip the GNU stack thing */
#if 0
		printf("program header at %p: type %u, offset %u, vaddr %#x, paddr %#x, filesz %#x, memsz %#x\n",
			ep, ep->p_type, ep->p_offset, ep->p_vaddr, ep->p_paddr, ep->p_filesz,
			ep->p_memsz);
#endif

		/* map that shit! */
		memcpy((void *)ep->p_vaddr, (void *)(forkserv_start + ep->p_offset),
			ep->p_filesz);
		if(ep->p_filesz < ep->p_memsz) {
			memset((void *)ep->p_vaddr + ep->p_filesz, 0,
				ep->p_memsz - ep->p_filesz);
		}
		add_forkserv_pages(ep->p_vaddr, ep->p_vaddr + ep->p_memsz - 1);
	}

	/* set up the address space & start the main thread. */
	forkserv_tid = L4_GlobalId(1366, 768);	/* yeah. so what? */
	L4_Word_t res = L4_ThreadControl(forkserv_tid, forkserv_tid,
		forkserv_pager, forkserv_pager, (void *)-1);
	if(res != 1) {
		fprintf(stderr, "%s: ThreadControl failed, ec %lu\n",
			__func__, L4_ErrorCode());
		abort();
	}
	forkserv_utcb_area = L4_FpageLog2(0x100000, 14);
	L4_Word_t old_ctl;
	res = L4_SpaceControl(forkserv_tid, 0, L4_FpageLog2(0xff000, 12),
		forkserv_utcb_area, L4_anythread, &old_ctl);
	if(res != 1) {
		fprintf(stderr, "%s: SpaceControl failed, ec %lu\n",
			__func__, L4_ErrorCode());
		abort();
	}
	res = L4_ThreadControl(forkserv_tid, forkserv_tid, forkserv_pager,
		forkserv_pager, (void *)L4_Address(forkserv_utcb_area));
	if(res != 1) {
		fprintf(stderr, "%s: ThreadControl failed, ec %lu\n",
			__func__, L4_ErrorCode());
		abort();
	}

	/* propagated breath of life. */
	L4_Set_VirtualSender(forkserv_pager);
	L4_LoadMR(0, (L4_MsgTag_t){ .X.u = 2, .X.flags = 1 }.raw);
	L4_LoadMR(1, ee->e_entry);
	L4_LoadMR(2, 0xdeadbeef);
	L4_MsgTag_t tag = L4_Send_Timeout(forkserv_tid,
		L4_TimePeriod(50 * 1000));
	if(L4_IpcFailed(tag)) {
		fprintf(stderr, "%s: breath-of-life to forkserv failed: ec %lu\n",
			__func__, L4_ErrorCode());
		abort();
	}
}


/* space_id 0 means forkserv's own pages. */
static void send_one_page(L4_Word_t address, L4_Word_t space_id)
{
	/* "hey, prepare to receive." */
	L4_LoadMR(0, (L4_MsgTag_t){ .X.label = FORKSERV_SEND_PAGE,
		.X.u = 2 }.raw);
	L4_LoadMR(1, address);
	L4_LoadMR(2, space_id);
	L4_MsgTag_t tag = L4_Call(forkserv_tid);
	if(L4_IpcFailed(tag)) goto ipcfail;

	L4_Fpage_t page = L4_Fpage(address, PAGE_SIZE);
	L4_Set_Rights(&page, L4_FullyAccessible);
	L4_GrantItem_t gi = L4_GrantItem(page, 0);
	L4_LoadBR(0, L4_CompleteAddressSpace.raw);
	L4_LoadMR(0, (L4_MsgTag_t){ .X.label = FORKSERV_SEND_PAGE_2,
		.X.t = 2 }.raw);
	L4_LoadMR(1, gi.raw[0]);
	L4_LoadMR(2, gi.raw[1]);
	tag = L4_Call(forkserv_tid);
	if(L4_IpcFailed(tag)) goto ipcfail;

	return;

ipcfail:
	printf("IPC failed: ec %#lx\n", L4_ErrorCode());
	abort();
}


void add_fs_tid(L4_Word_t space_id, L4_ThreadId_t tid)
{
	L4_LoadMR(0, (L4_MsgTag_t){ .X.label = FORKSERV_ADD_TID,
		.X.u = 2 }.raw);
	L4_LoadMR(1, space_id);
	L4_LoadMR(2, tid.raw);
	L4_MsgTag_t tag = L4_Call(forkserv_tid);
	if(L4_IpcFailed(tag)) {
		printf("%s: IPC failed: ec %#lx\n", __func__, L4_ErrorCode());
		abort();
	}
}


static void transfer_to_forkserv(void)
{
	add_fs_tid(0, forkserv_tid);
	struct htable_iter it;
	for(struct forkserv_page *fp = htable_first(&forkserv_pages, &it);
		fp != NULL;
		fp = htable_next(&forkserv_pages, &it))
	{
		send_one_page(fp->address, 0);
	}

	/* switch it over to sigma0 paging. */
	L4_Word_t res = L4_ThreadControl(forkserv_tid, forkserv_tid,
		L4_nilthread, L4_Pager(), (void *)-1);
	if(res != 1) {
		printf("can't set forkserv to s0 paging: ec %lu\n", L4_ErrorCode());
		abort();
	}

	/* TODO: iterate over the hash table again and free all the forkserv_page
	 * structs, then clear it
	 */

	/* transfer testbench's own pages over to forkserv. this is a bit
	 * tricky.
	 */
	extern char _start, _end;
	const int max_pages = 2048;
	L4_Word_t *page_addrs = malloc(sizeof(L4_Word_t) * max_pages);
	printf("page_addrs is %lu bytes at %p\n",
		(unsigned long)(sizeof(L4_Word_t) * max_pages), page_addrs);
	int num_pages = 0;
	/* first, fault them in. */
	volatile uint8_t foo = 0;
	/* ELF pages */
	for(L4_Word_t addr = (L4_Word_t)&_start & ~PAGE_MASK;
		addr < (((L4_Word_t)&_end + PAGE_MASK) & ~PAGE_MASK);
		addr += PAGE_SIZE)
	{
		foo ^= *(const uint8_t *)addr;	/* fault it in. */
		assert(num_pages < max_pages);
		page_addrs[num_pages++] = addr;
	}
	/* malloc heap */
	for(L4_Word_t addr = (L4_Word_t)sbrk(0);
		addr < get_heap_top();
		addr += PAGE_SIZE)
	{
		foo ^= *(const uint8_t *)addr;
		assert(num_pages < max_pages);
		page_addrs[num_pages++] = addr;
	}
	/* then the send. */
	add_fs_tid(1, L4_MyGlobalId());
	L4_Set_Pager(forkserv_tid);
	L4_Set_PagerOf(forkserv_pager, forkserv_tid);
	use_forkserv_sbrk = true;
	for(int i=num_pages - 1; i >= 0; --i) send_one_page(page_addrs[i], 1);
	free(page_addrs);

	/* TODO: terminate forkserv pager & release its stack */

	printf("%s: at an end\n", __func__);
}


int main(void)
{
	printf("hello, world!\n");
	calibrate_delay_loop();

	/* FIXME: add option to _not_ activate forkserv. */
	start_forkserv();
	transfer_to_forkserv();

	/* proper test suite */
	static Suite *(* const suites[])(void) = {
		/* selftests */
		&process_suite,

		/* microkernel tests */
		&thread_suite,
		&space_suite,
		&sched_suite,
	};
	SRunner *run = srunner_create(NULL);
	for(int i=0; i < sizeof(suites) / sizeof(suites[0]); i++) {
		Suite *s = (*suites[i])();
		srunner_add_suite(run, s);
	}
	srunner_run_all(run, 0);

	printf("*** legacy tests follow\n");
	legacy_tests();

	printf("*** testbench completed.\n");

	return 0;
}
