
#include <stdio.h>
#include <stdlib.h>
#include <stdnoreturn.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>
#include <ccan/compiler/compiler.h>
#include <ccan/hash/hash.h>
#include <ccan/htable/htable.h>
#include <ccan/minmax/minmax.h>

#include <l4/types.h>
#include <l4/thread.h>
#include <l4/ipc.h>
#include <l4/kip.h>
#include <l4/sigma0.h>
#include <l4/syscall.h>
#include <l4/bootinfo.h>
#include <l4/kdebug.h>

#include <ukernel/util.h>
#include <ukernel/ioport.h>

#include "defs.h"
#include "forkserv.h"
#include "test.h"
#include "elf.h"


/* pages tracked by the forkserv pager. mappings are granted to forkserv as
 * faults occur. presence of a forkserv_page means that a page was already
 * received from sigma0.
 */
struct forkserv_page {
	L4_Word_t address;		/* what forkserv sees */
	L4_Word_t phys_addr;	/* what sigma0 granted */
};


/* command-line options (passed in the forkserv module's cmdline field). */
struct cmd_opt {
	char *value;
	char key[];		/* has internal null. value = &key[nullpos + 1]. */
};


static size_t hash_forkserv_page(const void *key, void *priv);


L4_KernelInterfacePage_t *the_kip;

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


long sysconf(int name)
{
	switch(name) {
		case _SC_PAGESIZE: return PAGE_SIZE;
		case _SC_NPROCESSORS_ONLN: return 1;	/* FIXME: get from KIP */
		default:
			// errno = EINVAL;
			return -1;
	}
}


void malloc_panic(void) {
	printf("testbench: %s called in %lu:%lu, returns %p, %p, %p!\n", __func__,
		L4_ThreadNo(L4_Myself()), L4_Version(L4_Myself()),
		__builtin_return_address(0), __builtin_return_address(1),
		__builtin_return_address(2));
	abort();
}


int sched_yield(void) {
	L4_ThreadSwitch(L4_nilthread);
	return 0;
}


noreturn void __assert_failure(
	const char *condition,
	const char *file, int line, const char *func)
{
	if(in_test()) {
		printf("Bail out!  %s:%d assert failure `%s'\n",
			file, line, condition);
		exit_on_fail();
	} else {
		printf("testbench %lu:%lu %s(`%s', `%s', %d, `%s')\n",
			L4_ThreadNo(L4_Myself()), L4_Version(L4_Myself()),
			__func__, condition, file, line, func);
		abort();
		for(;;) { asm volatile("int $1"); }
	}
}


static size_t hash_forkserv_page(const void *key, void *priv) {
	return int_hash(((struct forkserv_page *)key)->address);
}


static bool forkserv_page_cmp(const void *cand, void *key) {
	return ((struct forkserv_page *)cand)->address == *(L4_Word_t *)key;
}


/* TODO: move this into a generic pager mechanism. the loop has been written
 * and copypasta'd often enough.
 */
static void forkserv_pager_fn(void *param UNUSED)
{
	for(;;) {
		L4_ThreadId_t from;
		L4_MsgTag_t tag = L4_Wait(&from);

		for(;;) {
			if(L4_IpcFailed(tag)) {
				printf("forkserv pager: reply/wait failed, ec=%#lx\n",
					L4_ErrorCode());
				break;
			}

			if(tag.X.label >> 4 == 0xffe
				&& tag.X.u == 2 && tag.X.t == 0)
			{
				L4_Word_t faddr, fip;
				L4_StoreMR(1, &faddr);
				L4_StoreMR(2, &fip);
#ifdef DEBUG_ME_HARDER
				printf("forkserv[%lu:%lu] pf; fip=%#lx, faddr=%#lx\n",
					L4_ThreadNo(from), L4_Version(from), fip, faddr);
#endif
				/* look it up. */
				L4_Word_t vaddr = faddr & ~PAGE_MASK;
				struct forkserv_page *fp = htable_get(&forkserv_pages,
					int_hash(vaddr), &forkserv_page_cmp, &vaddr);
				if(fp != NULL) {
					/* map it from local memory. */
					L4_Fpage_t map = L4_FpageLog2(fp->phys_addr, PAGE_BITS);
					L4_Set_Rights(&map, L4_FullyAccessible);
					assert(fp->address == vaddr);
					L4_MapItem_t mi = L4_MapItem(map, fp->address);
					L4_LoadMR(0, (L4_MsgTag_t){ .X.t = 2 }.raw);
					L4_LoadMR(1, mi.raw[0]);
					L4_LoadMR(2, mi.raw[1]);
				} else if(vaddr == 0) {
					printf("fpager: null page access from %lu:%lu ip %#lx\n",
						L4_ThreadNo(from), L4_Version(from), fip);
					break;
				} else {
					printf("forkserv[%lu:%lu] SEGFAULT at fip=%#lx, faddr=%#lx\n",
						L4_ThreadNo(from), L4_Version(from), fip, faddr);
					break;
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
				assert(L4_IsGlobalId(from));
				printf("iopf from=%lu:%lu range=%#lx:%#lx\n",
					L4_ThreadNo(from), L4_Version(from),
					L4_IoFpagePort(iofp), L4_IoFpageSize(iofp));
				/* forward the fault to sigma0 because forkserv does the same
				 * through us.
				 */
				L4_LoadMR(0, tag.raw);
				L4_LoadMR(1, iofp.raw);
				L4_LoadMR(2, 0xdeadbeef);
				L4_Accept(L4_MapGrantItems(L4_IoFpageLog2(0, 16)));
				L4_MsgTag_t tt = L4_Call(L4_Pager());
				if(L4_IpcFailed(tt)) {
					printf("can't forward iopf: ec=%lu\n", L4_ErrorCode());
					break;
				}
				L4_Accept(L4_UntypedWordsAcceptor);
				/* now reply to our client. */
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
			} else if(tag.X.label == FPAGER_THREADCTL) {
				L4_ThreadId_t dest, space, scheduler, pager;
				L4_Word_t utcb_loc;
				L4_StoreMR(1, &dest.raw);
				L4_StoreMR(2, &space.raw);
				L4_StoreMR(3, &scheduler.raw);
				L4_StoreMR(4, &pager.raw);
				L4_StoreMR(5, &utcb_loc);
				L4_Word_t retval = L4_ThreadControl(dest, space, scheduler,
					pager, (void *)utcb_loc);
				L4_LoadMR(0, (L4_MsgTag_t){ .X.u = 2 }.raw);
				L4_LoadMR(1, retval);
				L4_LoadMR(2, L4_ErrorCode());
			} else if(tag.X.label == FPAGER_SPACECTL) {
				L4_ThreadId_t spacespec, redirector;
				L4_Word_t control, ctl_out;
				L4_Fpage_t kip_area, utcb_area;
				L4_StoreMR(1, &spacespec.raw);
				L4_StoreMR(2, &control);
				L4_StoreMR(3, &kip_area.raw);
				L4_StoreMR(4, &utcb_area.raw);
				L4_StoreMR(5, &redirector.raw);
				L4_Word_t retval = L4_SpaceControl(spacespec, control,
					kip_area, utcb_area, redirector, &ctl_out);
				L4_LoadMR(0, (L4_MsgTag_t){ .X.u = 3 }.raw);
				L4_LoadMR(1, retval);
				L4_LoadMR(2, L4_ErrorCode());
				L4_LoadMR(3, ctl_out);
			} else {
				printf("forkserv's pager got weird IPC from=%lu:%lu, tag=%#lx\n",
					L4_ThreadNo(from), L4_Version(from), tag.raw);
				break;
			}

			/* reply. */
			tag = L4_ReplyWait(from, &from);
		}
	}
}


static L4_Fpage_t next_s0_page(void)
{
	static L4_Word_t next_addr = 8 * 1024 * 1024;
	L4_Fpage_t fp;
	do {
		/* allocate only until the 256M point since at that point there's
		 * honestly no more RAM around, or the kernel reservation has grown
		 * right out of control.
		 */
		if(next_addr >= 256 * 1024 * 1024) {
			printf("next_s0_page: won't go past 256M!\n");
			abort();
		}
		fp = L4_Sigma0_GetPage(L4_nilthread,
			L4_FpageLog2(next_addr, PAGE_BITS));
		next_addr += PAGE_SIZE;
	} while(L4_IsNilFpage(fp));
	return fp;
}


/* returns the raw command line. */
static const char *start_forkserv(void)
{
	L4_KernelInterfacePage_t *kip = the_kip;
	L4_BootInfo_t *bootinfo = (L4_BootInfo_t *)L4_BootInfo(kip);

	L4_BootRec_t *rec = L4_BootInfo_FirstEntry(bootinfo);
	bool found = false;
	const char *cmdline_rest = NULL;
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
		if(slash != NULL && memcmp(slash + 1, "forkserv", 8) == 0) {
			found = true;
			cmdline_rest = strchr(slash, ' ');
			break;
		}
	}
	if(!found) {
		printf("can't find forkserv module; was it loaded?\n");
		abort();
	}

	fault_pages_in(L4_Module_Start(rec),
		L4_Module_Start(rec) + L4_Module_Size(rec));
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
		const Elf32_Phdr *ep = (void *)forkserv_start + phoff;
		if(ep->p_type != PT_LOAD) continue;	/* skip the GNU stack thing */
#if 0
		printf("phdr=%p: type=%u offset=%u vaddr=%#x paddr=%#x filesz=%#x, memsz=%#x\n",
			ep, ep->p_type, ep->p_offset, ep->p_vaddr, ep->p_paddr,
			ep->p_filesz, ep->p_memsz);
#endif

		/* map that shit! */
		for(L4_Word_t off = 0; off < ep->p_memsz; off += PAGE_SIZE) {
			/* prefer idempotent maps */
			L4_Fpage_t pg = L4_Sigma0_GetPage(L4_nilthread,
				L4_FpageLog2(ep->p_vaddr + off, PAGE_BITS));
			if(L4_IsNilFpage(pg)) {
				/* ... but in their absence "any old" will do. forkserv
				 * doesn't care about the physical locations of where its
				 * process image was loaded, as long as those pages don't turn
				 * up again in the sigma0 pump which must land idempotently.
				 */
				pg = next_s0_page();
			}
			void *phys = (void *)L4_Address(pg);
			if(off < ep->p_filesz) {
				size_t tail = ep->p_filesz - off;
				memcpy(phys, (void *)forkserv_start + ep->p_offset + off,
					min_t(size_t, PAGE_SIZE, tail));
				if(tail < PAGE_SIZE) {
					memset(phys + off + tail, '\0', PAGE_SIZE - tail);
				}
			} else {
				memset(phys, '\0', PAGE_SIZE);
			}
			struct forkserv_page *f = malloc(sizeof *f);
			*f = (struct forkserv_page){
				.address = ep->p_vaddr + off, .phys_addr = L4_Address(pg),
			};
			assert((f->address & PAGE_MASK) == 0);
			htable_add(&forkserv_pages, int_hash(f->address), f);
		}
	}

	/* set up the address space & start the main thread. */
	forkserv_tid = L4_GlobalId(23069, 42);		/* distinctive. */
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

	return cmdline_rest;
}


/* space_id 0 means forkserv's own pages. */
static void send_one_page(L4_Word_t phys, L4_Word_t virt, int space_id)
{
	assert(phys == virt || space_id != 1);

	/* first the kiss */
	int n = forkserv_send_page_timeout(forkserv_tid,
		virt, space_id, L4_Never);
	if(n != 0) goto ipcfail;

	/* then the grant */
	L4_Fpage_t page = L4_Fpage(phys, PAGE_SIZE);
	L4_Set_Rights(&page, L4_FullyAccessible);
	L4_GrantItem_t gi = L4_GrantItem(page, 0);
	L4_LoadBR(0, L4_CompleteAddressSpace.raw);
	L4_LoadMR(0, (L4_MsgTag_t){ .X.label = FORKSERV_SEND_PAGE_2,
		.X.u = 1, .X.t = 2 }.raw);
	L4_LoadMR(1, phys);	/* needed because IPC rewrites SndPage address */
	L4_LoadMRs(2, 2, gi.raw);
	L4_MsgTag_t tag = L4_Call(forkserv_tid);
	if(L4_IpcSucceeded(tag)) return;

	n = L4_ErrorCode();

ipcfail:
	printf("%s: IPC failed, n=%d\n", __func__, n);
	abort();
}


void add_fs_tid(L4_Word_t space_id, L4_ThreadId_t tid)
{
#if 0
	printf("%s: in %lu:%lu (%lu, %lu:%lu)\n", __func__,
		L4_ThreadNo(L4_Myself()), L4_Version(L4_Myself()),
		space_id, L4_ThreadNo(tid), L4_Version(tid));
#endif
	if(!L4_IsNilThread(forkserv_tid)) {
		int n = forkserv_add_tid(forkserv_tid, space_id, tid.raw);
		if(n != 0) {
			printf("%s: IPC failed, n=%d, ec=%#lx\n", __func__, n,
				L4_ErrorCode());
			abort();
		}
	}
}


static void transfer_to_forkserv(void)
{
	add_fs_tid(0, forkserv_tid);
	struct htable_iter it;
	for(struct forkserv_page *fp = htable_first(&forkserv_pages, &it);
		fp != NULL; fp = htable_next(&forkserv_pages, &it))
	{
		send_one_page(fp->phys_addr, fp->address, 0);
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
	int num_pages = 0;
	volatile uint8_t foo = 0;
	/* ELF pages */
	for(L4_Word_t addr = (L4_Word_t)&_start & ~PAGE_MASK;
		addr < (((L4_Word_t)&_end + PAGE_MASK) & ~PAGE_MASK);
		addr += PAGE_SIZE)
	{
		foo ^= *(const uint8_t *)addr;	/* fault them in as we go. */
		assert(num_pages < max_pages);
		page_addrs[num_pages++] = addr;
	}
	/* malloc heap */
	for(L4_Word_t a = (L4_Word_t)sbrk(0); a < get_heap_top(); a += PAGE_SIZE) {
		foo ^= *(const uint8_t *)a;		/* that, too. */
		assert(num_pages < max_pages);
		page_addrs[num_pages++] = a;
	}
	/* then the send. */
	add_fs_tid(1, L4_MyGlobalId());
	add_fs_tid(1, forkserv_pager);
	L4_Set_Pager(forkserv_tid);
	L4_Set_PagerOf(forkserv_pager, forkserv_tid);
	use_forkserv_sbrk = true;
	for(int i=num_pages - 1; i >= 0; --i) {
		send_one_page(page_addrs[i], page_addrs[i], 1);
	}
	free(page_addrs);

	/* inform forkserv of the shape of testbench's address space, i.e. the KIP
	 * and UTCB locations.
	 *
	 * space=1 is the testbench space's ID; s=16 means room for 128 threads.
	 * the latter is assumed rather than determined, but it's also the size
	 * used by mung kmain.c .
	 */
	L4_KernelInterfacePage_t *kip = the_kip;
	int n = forkserv_as_cfg(forkserv_tid, 1,
		get_mgr_tid().raw,
		L4_FpageLog2((L4_Word_t)kip, kip->KipAreaInfo.X.s),
		L4_FpageLog2((L4_Word_t)__L4_Get_UtcbAddress(), 16));
	if(n != 0) {
		printf("%s: A/S config with forkserv failed, n=%d\n", __func__, n);
		abort();
	}

	/* forkserv's pager is left behind to do privileged syscalls on its
	 * behalf.
	 */
}


static size_t rehash_cmd_opt(const void *data, void *priv) {
	const struct cmd_opt *opt = data;
	return hash_string(opt->key);
}


static bool cmd_opt_eq_keystr(const void *cand_ptr, void *key)
{
	const struct cmd_opt *opt = cand_ptr;
	return streq(opt->key, (const char *)key);
}


static const char *cmd_opt(struct htable *ht, const char *key)
{
	struct cmd_opt *opt = htable_get(ht,
		hash_string(key), &cmd_opt_eq_keystr, key);
	return opt != NULL ? opt->value : NULL;
}


static void parse_cmd_opts(struct htable *ht, const char *boot_cmdline)
{
	if(boot_cmdline == NULL) return;

	while(*boot_cmdline != '\0') {
		while(isspace(*boot_cmdline)) boot_cmdline++;
		if(*boot_cmdline == '\0') break;
		const char *eqpos = strchr(boot_cmdline, '=');
		if(eqpos == NULL) {
			printf("*** malformed command-line at `%s'\n", boot_cmdline);
			abort();
		}
		const char *valptr = eqpos + 1, *val_end = valptr;
		while(!isspace(*val_end) && *val_end != '\0') val_end++;
		int val_len = val_end - valptr, opt_len = eqpos - boot_cmdline;

		struct cmd_opt *opt = malloc(sizeof(struct cmd_opt)
			+ val_len + opt_len + 2);
		memcpy(opt->key, boot_cmdline, opt_len);
		opt->key[opt_len] = '\0';
		opt->value = &opt->key[opt_len + 1];
		memcpy(opt->value, valptr, val_len);
		opt->value[val_len] = '\0';
		/* NOTE: this can add multiple values for a single option. that's by
		 * design!
		 */
		htable_add(ht, rehash_cmd_opt(opt, NULL), opt);

		boot_cmdline = val_end;
	}
}


/* IA-PC style keyboard interrupt handling. */
static void keyboard_thread(void *param UNUSED)
{
	printf("keyboard thread running!\n");

	/* direct IRQ1 to this thread. */
	L4_ThreadId_t irq_tid = L4_GlobalId(1, 1);
	L4_Word_t res = L4_ThreadControl(irq_tid, irq_tid, L4_nilthread,
		L4_MyGlobalId(), (void *)-1);
	if(res == 0) {
		printf("IRQ1 ThreadControl failed, ec %#lx\n", L4_ErrorCode());
		goto end;
	}

	for(;;) {
		L4_MsgTag_t tag = L4_Receive(irq_tid);
		if(L4_IpcFailed(tag)) {
			printf("IRQ1 receive failed, ec %#lx\n", L4_ErrorCode());
			continue;
		}

        printf("i'm a keyboard, beepin ur macaronis\n");
#define KBD_STATUS_REG 0x64
#define KBD_DATA_REG 0x60
#define KBD_STAT_OBF 0x01
        for(;;) {
            uint8_t st = inb(KBD_STATUS_REG);
            if((st & KBD_STAT_OBF) == 0) break;
            inb(KBD_DATA_REG);  /* and throw it away */
        }

		L4_LoadMR(0, 0);
		L4_Reply(irq_tid);

#if 0
		printf("[keyb] sleeping for 1 second...\n");
		L4_Sleep(L4_TimePeriod(1 * 1000 * 1000));
		printf("[keyb] woke up hale & hearty\n");
#endif
	}

end:
	printf("keyboard thread exiting!\n");
}


static void invoke_ktest(bool describe)
{
	/* kernel tests are executed in the int3 handler in response to a special
	 * KDB cookie from a privileged space (this one). this works when
	 * ENABLE_SELFTEST is active and prints the message otherwise.
	 *
	 * TODO: check for a kernel extension on the KIP to this effect instead.
	 */
	char *cookie = "(with describe option) "
		"tried to run mung self-tests, but they weren't enabled";
	while(!describe && *cookie != ')') {
		cookie++;
		assert(*cookie != '\0');
	}

	/* copying the cookie ensures it's been mapped. this is important. */
	char buf[256];
	strscpy(buf, cookie, sizeof(buf));
	L4_KDB_PrintString(buf);
}


static void print_boot_info(void)
{
	L4_KernelInterfacePage_t *kip = the_kip;
	if(kip == NULL || memcmp(&kip->magic, "L4\346K", 4) != 0) {
		printf("L4.X2 microkernel not found!\n");
		return;
	}
	char api_buf[256];
	const char *api_str = api_buf;
	switch(kip->ApiVersion.X.version) {
		case 0x2: api_str = "Version 2"; break;
		case 0x83:
			if(kip->ApiVersion.X.subversion == 0x80) {
				api_str = "Experimental Version X.0";
			} else if(kip->ApiVersion.X.subversion == 0x81) {
				api_str = "Experimental Version X.1";
			} else {
				snprintf(api_buf, sizeof(api_buf),
					"Experimental [subver=%#02x]",
					kip->ApiVersion.X.subversion);
			}
			break;
		case 0x84:
			snprintf(api_buf, sizeof(api_buf),
				"Experimental Version X.2 (Revision %u)",
				kip->ApiVersion.X.subversion);
			break;
		default:
			snprintf(api_buf, sizeof(api_buf),
				"L4 version=%#02x subversion=%#02x",
				kip->ApiVersion.X.version,
				kip->ApiVersion.X.subversion);
	}

	printf("microkernel is L4, API `%s' (%s endian, %d-bit)\n", api_str,
		kip->ApiFlags.X.ee == 0 ? "little" : "big",
		kip->ApiFlags.X.ww == 0 ? 32 : 64);
	L4_KernelDesc_t *kdesc = (void *)kip + kip->KernelVerPtr;
	printf("  kernel ID %#02x:%#02x\n", kdesc->KernelId.X.id,
		kdesc->KernelId.X.subid);
	printf("  generated on %04d-%02d-%02d\n",
		(int)kdesc->KernelGenDate.X.year + 2000,
		(int)kdesc->KernelGenDate.X.month,
		(int)kdesc->KernelGenDate.X.day);
	printf("  kernel version %d.%d.%d\n",
		(int)kdesc->KernelVer.X.ver,
		(int)kdesc->KernelVer.X.subver,
		(int)kdesc->KernelVer.X.subsubver);
	char supl[5]; memcpy(supl, &kdesc->Supplier, 4); supl[4] = '\0';
	printf("  supplier `%s'\n", supl);
	printf("  version string `%s'\n", kdesc->VersionString);
	int vpos = strlen(kdesc->VersionString) + 1;
	if(kdesc->VersionString[vpos] != '\0') {
		printf("  features:");
		do {
			char *str = &kdesc->VersionString[vpos];
			printf(" %s", str);
			vpos += strlen(str) + 1;
		} while(kdesc->VersionString[vpos] != '\0');
		printf("\n");
	}
}


static void add_to_fs_and_stop(L4_ThreadId_t tid, void *ptr)
{
	if(L4_SameThreads(tid, L4_Myself())) return;
	add_fs_tid(1, tid);
	L4_Stop(tid);
}


static void set_forkserv_pager_and_start(L4_ThreadId_t tid, void *ptr)
{
	if(L4_SameThreads(tid, L4_Myself())) return;
	L4_Set_PagerOf(tid, forkserv_tid);
	L4_Start(tid);
}


static int suite_spec_by_priority_fn(const void *ap, const void *bp) {
	const struct suite_spec *a = ap, *b = bp;
	return a->priority - b->priority;
}


int main(void)
{
	the_kip = L4_GetKernelInterface();
	printf("hello, world!\n");
	print_boot_info();
	calibrate_delay_loop();
	fault_own_pages();

	const char *boot_cmdline = start_forkserv();
	struct htable opts;
	htable_init(&opts, &rehash_cmd_opt, NULL);
	parse_cmd_opts(&opts, boot_cmdline);

	/* start manager thread lest it be unknown to forkserv. */
	get_mgr_tid();
	for_each_thread(&add_to_fs_and_stop, NULL);
	transfer_to_forkserv();
	for_each_thread(&set_forkserv_pager_and_start, NULL);

	suite_ctor *suites = NULL;
	size_t n_suites = 0;
	/* choose between the userspace test suites, the in-kernel test suite, or
	 * the meta suite, which produces various results to test both generation
	 * thereof and their capture by the reporting script.
	 *
	 * also "notest" can be specified to e.g. run just the benchmarks, if
	 * enabled.
	 */
	bool notest = false;
	if(cmd_opt(&opts, "notest") != NULL) {
		printf("*** notest specified, tests skipped.\n");
		notest = true;
	} else if(cmd_opt(&opts, "meta") != NULL) {
		static suite_ctor meta_plan[] = { &meta_suite };
		suites = meta_plan;
		n_suites = NUM_ELEMENTS(meta_plan);
	} else if(cmd_opt(&opts, "ktest") != NULL) {
		invoke_ktest(cmd_opt(&opts, "describe") != NULL);
		n_suites = 0;
	} else {
		/* actual microkernel test suite */
		struct suite_spec **specs_orig = autodata_get(
			testsuites, &n_suites), specs[n_suites];
		for(int i=0; i < n_suites; i++) specs[i] = *specs_orig[i];
		qsort(specs, n_suites, sizeof specs[0], &suite_spec_by_priority_fn);
		suites = malloc(n_suites * sizeof *suites);
		for(int i=0; i < n_suites; i++) suites[n_suites - 1 - i] = specs[i].fn;
	}
	SRunner *run = srunner_create(NULL);
	for(int i=0; i < n_suites; i++) {
		Suite *s = (*suites[i])();
		srunner_add_suite(run, s);
	}
	if(cmd_opt(&opts, "describe") != NULL) {
		srunner_describe(run);
	}
	const char *only = cmd_opt(&opts, "runonly");
	if(!notest && only != NULL) {
		char *copy = strdup(only), *pos = copy;
		for(;;) {
			char *sep = strchr(pos, '+');
			if(sep != NULL) *sep = '\0';
			if(streq(pos, "@")) {
				/* the special "stop" symbol. */
				break;
			}
			if(!streq(pos, "")) srunner_run_path(run, pos, 0);
			if(sep == NULL) break; else pos = sep + 1;
		}
		free(copy);
	} else if(!notest) {
		srunner_run_all(run, 0);
	}

	if(cmd_opt(&opts, "bench") != NULL) {
		printf("*** benchmarks follow\n");
		run_benchmarks();
	}

	printf("*** testbench completed.\n");

	if(cmd_opt(&opts, "keyboard") != NULL) {
		printf("*** starting keyboard test thread\n");
		L4_ThreadId_t keyb_tid = start_thread(&keyboard_thread, NULL);
		join_thread(keyb_tid);
	}

	return 0;
}
