
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdnoreturn.h>
#include <string.h>
#include <assert.h>
#include <ccan/minmax/minmax.h>
#include <ccan/array_size/array_size.h>

#include <l4/types.h>
#include <l4/kcp.h>
#include <l4/kip.h>
#include <l4/bootinfo.h>

#include <ukernel/x86.h>
#include <ukernel/16550.h>
#include <ukernel/vgacon.h>
#include <ukernel/mm.h>
#include <ukernel/memdesc.h>
#include <ukernel/util.h>

#include "defs.h"
#include "multiboot.h"
#include "elf.h"


/* TODO: add dynamic memory allocation? */
#define MAX_BOOT_MODS 32
#define MAX_MMAP_ENTS 512


/* when @end == 0, module is invalid. */
struct boot_module
{
	L4_Word_t start, end;		/* [start, end) as set by bootloader */
	L4_Word_t load_start, load_end;
	L4_Word_t sp, ip;
	char cmdline[128];
} __attribute__((packed));


static int num_boot_mods = 0;
static struct boot_module boot_mods[MAX_BOOT_MODS];
static uintptr_t heap_low = 0;


void noreturn panic(const char *message)
{
	printf("PANIC: %s\n", message);
	while(true) {
		asm("cli; hlt");
	}
}


#ifndef NDEBUG
noreturn void __assert_failure(
	const char *condition,
	const char *file, int line, const char *func)
{
	printf("assert(%s) failed in `%s' (%s:%u)\n", condition, func,
		file, line);
	panic("*** assertion failure");
}
#endif


long con_write(void *cookie, const char *buf, size_t size)
{
	for(size_t i=0; i < size; i++) computchar(buf[i]);
	vgacon_write((void *)VGA_TEXT_BASE, buf, size);
	return size;
}


void *sbrk(intptr_t increment)
{
	if(heap_low == 0) {
		panic("sbrk called in pre-heap part");
	}

	void *prev = (void *)heap_low;
	heap_low += increment;
	return prev;
}


/* primitive page-grain malloc via an equally primitive sbrk(). */
void *malloc(size_t size)
{
	int shift = max_t(int, 12, size_to_shift(size));
	heap_low = (heap_low + PAGE_SIZE - 1) & ~PAGE_MASK;
	return sbrk(1 << shift);
}


void free(void *ptr) {
	/* stubbity stubbing stub */
}


void abort(void) {
	panic("mbiloader abort() called!");
}


/* construct a 32-bit kernel configuration page.
 * TODO: handle kdebug also.
 *
 * @plat_mmap_fn receives the memdescbuf after fill_kcp() has communicated the
 * MBI memory maps and boot-module dedicated segments in it. the idea is that
 * e.g. VGA video memory gets dedicated 
 */
static void fill_kcp(
	uint8_t *kcp_base,
	struct boot_module *kernel_mod,
	struct boot_module *s0_mod,
	struct boot_module *s1_mod,
	struct boot_module *roottask_mod,
	const struct multiboot_mmap_entry *mm, int mmap_count,
	bool (*plat_mmap_fn)(struct memdescbuf *mdb, void *priv), void *fn_priv)
{
	memset(kcp_base, 0, PAGE_SIZE);

	/* set the bytes one at a time to avoid confusing the KCP-finding
	 * loop.
	 */
	kcp_base[0] = 'L';
	kcp_base[1] = '4';
	kcp_base[2] = 230;		// 'µ' in iso-8859-1 (?)
	kcp_base[3] = 'K';
	*(L4_Word_t *)&kcp_base[0x04] = 66642;	/* API version */
	*(L4_Word_t *)&kcp_base[0x10] = 0;		/* kdebug.init */
	*(L4_Word_t *)&kcp_base[0x14] = 0;		/* kdebug.entry */
	*(L4_Word_t *)&kcp_base[0x18] = 0;		/* kdebug.low */
	*(L4_Word_t *)&kcp_base[0x1c] = 0;		/* kdebug.high */
	*(L4_Word_t *)&kcp_base[0x20] = s0_mod->sp;
	*(L4_Word_t *)&kcp_base[0x24] = s0_mod->ip;
	*(L4_Word_t *)&kcp_base[0x28] = s0_mod->load_start;
	*(L4_Word_t *)&kcp_base[0x2c] = s0_mod->load_end | PAGE_MASK;
	struct boot_module null_mod = { };
	if(s1_mod == NULL) s1_mod = &null_mod;
	*(L4_Word_t *)&kcp_base[0x30] = s1_mod->sp;
	*(L4_Word_t *)&kcp_base[0x34] = s1_mod->ip;
	*(L4_Word_t *)&kcp_base[0x38] = s1_mod->load_start;
	*(L4_Word_t *)&kcp_base[0x3c] = s1_mod != &null_mod
		? (s1_mod->load_end | PAGE_MASK) : 0;
	if(roottask_mod == NULL) roottask_mod = &null_mod;
	*(L4_Word_t *)&kcp_base[0x40] = roottask_mod->sp;
	*(L4_Word_t *)&kcp_base[0x44] = roottask_mod->ip;
	*(L4_Word_t *)&kcp_base[0x48] = roottask_mod->load_start;
	*(L4_Word_t *)&kcp_base[0x4c] = roottask_mod != &null_mod
		? (roottask_mod->load_end | PAGE_MASK) : 0;

	*(L4_Word_t *)&kcp_base[0x58] = 0;		/* kdebug.config0 */
	*(L4_Word_t *)&kcp_base[0x5c] = 0;		/* kdebug.config1 */

	int resv_pos = 0x100;	/* reserved after the defined KCP bits. */

	/* memory descriptors. the bootloader passes only non-virtual conventional
	 * and reserved memory; other memory types, and the virtual address
	 * space's shape, come from per-platform code.
	 *
	 * this bumps resv_pos after mdbuf has been filled.
	 */
	const int md_pos = resv_pos;
	struct memdescbuf mdb = {
		.ptr = (L4_MemoryDesc_t *)&kcp_base[md_pos],
		.size = (PAGE_SIZE - md_pos) / sizeof(L4_MemoryDesc_t),
	};
	for(int i=0; i < mmap_count; i++) {
		bool ok;
		if(mm[i].type != MULTIBOOT_MEMORY_AVAILABLE) {
			/* bootloader specific, pass type as seen */
			ok = mdb_set(&mdb, mm[i].addr, mm[i].addr + mm[i].len - 1,
				false, L4_BootLoaderSpecificMemoryType, mm[i].type);
		} else {
			/* available memory. pass as conventional. */
			ok = mdb_set(&mdb, mm[i].addr, mm[i].addr + mm[i].len - 1,
				false, L4_ConventionalMemoryType, 0);
		}
		if(!ok) panic("ran out of MemoryDesc space!");
	}
	/* platform details */
	if(!(*plat_mmap_fn)(&mdb, fn_priv)) {
		/* TODO: dump mdb contents so far */
		panic("platform memory map couldn't be constructed");
	}

	/* dedicate memory for the idempotently mapped kernel/root servers */
	struct boot_module *bms[] = {
		s0_mod, s1_mod, roottask_mod, kernel_mod
	};
	for(int i=0; i < sizeof(bms) / sizeof(bms[0]); i++) {
		struct boot_module *m = bms[i];
		if(m != NULL && m->end > 0) {
			/* FIXME: is load_end inclusive or exclusive? this assumes
			 * exclusive.
			 */
			printf("dedicating %#lx .. %#lx for boot module %d\n",
				m->load_start, m->load_end, i);
			if(!mdb_set(&mdb, m->load_start, m->load_end, false,
				L4_DedicatedMemoryType, 0))
			{
				panic("ran out of MemoryDesc space!");
			}
		}
	}

	/* and for other boot modules. (also collects those boot modules that
	 * should be passed to the root server.)
	 */
	const struct boot_module *list_mods[num_boot_mods];
	int num_list = 0;
	for(int i=0; i < num_boot_mods; i++) {
		const struct boot_module *m = &boot_mods[i];
		bool found = false;
		for(int j=0; j < sizeof(bms) / sizeof(bms[0]); j++) {
			if(m == bms[j]) {
				found = true;
				break;
			}
		}
		if(!found) {
			L4_Word_t lo = m->start & ~PAGE_MASK,
				hi = (m->end - 1) | PAGE_MASK;
			printf("dedicating [%#lx, %#lx] for module `%s'\n",
				lo, hi, m->cmdline);
			if(!mdb_set(&mdb, lo, hi, false, L4_DedicatedMemoryType, 0)) {
				panic("ran out of MemoryDesc space!");
			}
			list_mods[num_list++] = m;
		}
	}

	/* MemoryInfo, resv_pos bump */
	mdb_normalize(&mdb);
	mdb_sort(&mdb);
	assert(offsetof(L4_KernelConfigurationPage_t, MemoryInfo) == 0x54);
	*(L4_Word_t *)&kcp_base[0x54] = md_pos << 16 | mdb.len;
	resv_pos += mdb.len * sizeof(L4_MemoryDesc_t);
#if 0
	printf("KCP MemoryDesc dump:\n");
	for(int i=0; i < mdb.len; i++) {
		printf("i=%02d\t%s range=[%#lx, %#lx], type=%#lx\n",
			i, L4_IsMemoryDescVirtual(&mdb.ptr[i]) ? "virt" : "phys",
			L4_MemoryDescLow(&mdb.ptr[i]), L4_MemoryDescHigh(&mdb.ptr[i]),
			L4_MemoryDescType(&mdb.ptr[i]));
	}
#endif

	/* BootInfo and BootRecs */
	L4_BootInfo_t *binf = (L4_BootInfo_t *)&kcp_base[resv_pos];
	*(L4_Word_t *)&kcp_base[0xb8] = (L4_Word_t)binf;	/* BootInfo */
	resv_pos += sizeof(L4_BootInfo_t);
	*binf = (L4_BootInfo_t){
		.magic = L4_BOOTINFO_MAGIC,
		.version = L4_BOOTINFO_VERSION,
		.size = sizeof(L4_BootInfo_t),
		.num_entries = num_list,
	};
	L4_BootRec_t *prev = NULL;
	for(int i=0; i < num_list; i++) {
		const struct boot_module *m = list_mods[i];
		L4_Boot_Module_t *mod = (L4_Boot_Module_t *)&kcp_base[resv_pos];
		resv_pos += sizeof(*mod);
		/* the command line, too. */
		int cllen = strlen(m->cmdline), cl_resv = (cllen + 4) & ~3;
		char *cmdline_buf = (char *)&kcp_base[resv_pos];
		memcpy(cmdline_buf, m->cmdline, cllen);
		memset(&cmdline_buf[cllen], 0, cl_resv - cllen);
		resv_pos += cl_resv;

		*mod = (L4_Boot_Module_t){
			.type = L4_BootInfo_Module, .version = 1,
			.start = m->start,
			.size = m->end - m->start,
			.cmdline_offset = (uint8_t *)cmdline_buf - (uint8_t *)mod,
		};

		/* linkage */
		if(prev == NULL) {
			binf->first_entry = (uint8_t *)mod - (uint8_t *)binf;
		} else {
			prev->offset_next = (uint8_t *)mod - (uint8_t *)prev;
		}
		binf->size += sizeof *mod + cl_resv;
		assert(offsetof(L4_Boot_Module_t, offset_next)
			== offsetof(L4_BootRec_t, offset_next));
		prev = (L4_BootRec_t *)mod;
	}
	assert(binf->num_entries == 0 || binf->first_entry != 0);
	assert((void *)binf + binf->size == (void *)&kcp_base[resv_pos]);
}


static void parse_elf_range(struct boot_module *mod)
{
	const void *elf = (const void *)mod->start;
	const Elf32_Ehdr *eh = elf;
	uintptr_t phoff = eh->e_phoff;
	uintptr_t m_start = ~0ul, m_end = 0;
	for(int i=0; i < eh->e_phnum; i++, phoff += eh->e_phentsize) {
		const Elf32_Phdr *ph = elf + phoff;
		if(ph->p_type != PT_LOAD) continue;

		m_start = min_t(uintptr_t, m_start, ph->p_vaddr & ~PAGE_MASK);
		uintptr_t past = (ph->p_vaddr + ph->p_memsz + PAGE_SIZE - 1) & ~PAGE_MASK;
		m_end = max_t(uintptr_t, m_end, past - 1);
	}
	mod->load_start = m_start;
	mod->load_end = m_end;
}


static void load_elf_module(struct boot_module *mod)
{
	const void *elf = (const void *)mod->start;
	const Elf32_Ehdr *eh = elf;
	uintptr_t phoff = eh->e_phoff;
	for(int i=0; i < eh->e_phnum; i++, phoff += eh->e_phentsize) {
		const Elf32_Phdr *ph = elf + phoff;
		if(ph->p_type != PT_LOAD) continue;

		size_t copysize = ph->p_filesz;
		memcpy((void *)ph->p_vaddr, (void *)mod->start + ph->p_offset,
			copysize);
		if(copysize < ph->p_memsz) {
			memset((void *)ph->p_vaddr + copysize, 0,
				ph->p_memsz - ph->p_filesz);
		}
	}

	mod->sp = 0;
	mod->ip = eh->e_entry;

	/* blank the memory just to be safe. */
	memset((void *)mod->start, 0, mod->end - mod->start);
}


/* verify that the ELF load ranges of selected boot modules (i.e. kernel,
 * sigma0, sigma1, roottask) don't overlap with one another, or with a
 * non-boot module's MBI load address, or with mbiloader's own range.
 *
 * in the second case affected modules are moved to malloc()'d sections.
 */
static void check_boot_modules(struct boot_module **mods, int num_mods)
{
	extern char _start, _end;
	const L4_Word_t this_start = (L4_Word_t)&_start,
		this_end = (L4_Word_t)&_end;
	for(int i=0; i < num_mods; i++) {
		struct boot_module *m = mods[i];
		if(m == NULL) continue;

		/* check for overlap with mbiloader. */
		if(RANGE_OVERLAP(m->load_start, m->load_end, this_start, this_end)) {
			printf("mbiloader: module %d's load range [%#lx..%#lx] overlaps mbiloader!\n"
				"  (mbiloader is at [%#lx..%#lx])\n",
				i, m->load_start, m->load_end, this_start, this_end);
			panic("mbiloader: boot module overlaps with loader");
		}

		/* verify that the _load addresses_ don't overlap. */
		for(int j=0; j < i; j++) {
			struct boot_module *o = mods[j];
			if(RANGE_OVERLAP(m->load_start, m->load_end,
				o->load_start, o->load_end))
			{
				printf("mbiloader: modules %d and %d overlap per ELF headers:\n",
					i, j);
				printf("  load ranges [0x%lx .. 0x%lx] and [0x%lx .. 0x%lx] respectively\n",
					m->load_start, m->load_end, o->load_start, o->load_end);
				panic("boot module ranges overlap!");
			}
		}

		/* now check if load addresses overlap with the modules' data, and
		 * move those modules if so.
		 */
		for(int j=i + 1; j < num_boot_mods; j++) {
			if(RANGE_OVERLAP(m->load_start, m->load_end,
				boot_mods[j].start, boot_mods[j].end - 1))
			{
				size_t length = boot_mods[j].end - boot_mods[j].start + 1;
				void *reloc_addr = sbrk(length);
				printf("moving module %d (`%s') to %p\n",
					j, boot_mods[j].cmdline, reloc_addr);
				memcpy(reloc_addr, (void *)boot_mods[j].start, length);
				boot_mods[j].start = (uintptr_t)reloc_addr;
				boot_mods[j].end = (uintptr_t)reloc_addr + length - 1;
			}
		}
	}
}


/* crawls over the MBI mmap entries & figures out how many KiB of low (< 640Ki)
 * and extended (> 1Mi) memory the computer has.
 */
static void scan_mbi_mmaps(
	size_t *low_p, size_t *high_p,
	const struct multiboot_mmap_entry *ents,
	int num_ents)
{
	*low_p = *high_p = 0;
	for(int i=0; i < num_ents; i++) {
		const struct multiboot_mmap_entry *e = &ents[i];
		if(e->type != MULTIBOOT_MEMORY_AVAILABLE) continue;

		if(e->addr >= 0x100000) *high_p += e->len / 1024;
		else if(e->addr < 640 * 1024) {
			uintptr_t end = min_t(uintptr_t, e->addr + e->len, 640 * 1024);
			*low_p += (end - e->addr) / 1024;
		}
		/* NOTE: skips over entries that straddle the 640k..1m high-mem
		 * boundary.
		 */
	}
}


int bootmain(multiboot_info_t *mbi, uint32_t magic)
{
	printf("mbiloader says hello!\n");

	/* find top of physical memory after 1 MiB. */
	size_t mem_before_640k = 0, mem_after_1m = 0;
	int mmap_count = 0;
	static struct multiboot_mmap_entry mmap_ents[MAX_MMAP_ENTS];
	if(CHECK_FLAG(mbi->flags, MULTIBOOT_INFO_MEM_MAP)) {
		mmap_count = min_t(int, MAX_MMAP_ENTS,
			mbi->mmap_length / sizeof(struct multiboot_mmap_entry));
		/* duplicate them for private use ;) */
		memcpy(mmap_ents, (void *)mbi->mmap_addr,
			mmap_count * sizeof(struct multiboot_mmap_entry));
		scan_mbi_mmaps(&mem_before_640k, &mem_after_1m,
			mmap_ents, mmap_count);
	} else {
		printf("multiboot memory-map info not present!\n");
	}

	/* get parameters for "*P". fall back to the conservative sum from
	 * before. only use the values if they're provided; qemu-kvm 1.7.0 seems
	 * to put 0 in mbi->mem_upper.
	 */
	if(CHECK_FLAG(mbi->flags, MULTIBOOT_INFO_MEMORY)) {
		printf("MBI mem_lower=%#lx, mem_upper=%#lx%s\n",
			(L4_Word_t)mbi->mem_lower, (L4_Word_t)mbi->mem_upper,
			mbi->mem_upper == 0 || mbi->mem_lower == 0
				? " (bogus values ignored)" : "");
		if(mbi->mem_lower > 0) mem_before_640k = mbi->mem_lower;
		if(mbi->mem_upper > 0) mem_after_1m = mbi->mem_upper;
	}

	printf("seeing %u KiB of low memory + %u KiB of high memory\n",
		(unsigned)mem_before_640k, (unsigned)mem_after_1m);

	/* scan boot modules, noting their load-ranges. */
	uintptr_t r_start = ~0ul, r_end = 0;
	if(CHECK_FLAG(mbi->flags, MULTIBOOT_INFO_MODS)
		&& mbi->mods_count > 0)
	{
		multiboot_module_t *m_base = (multiboot_module_t *)mbi->mods_addr;
		num_boot_mods = min_t(int, mbi->mods_count, MAX_BOOT_MODS);
		for(int i=0; i < num_boot_mods; i++) {
			struct boot_module *bm = &boot_mods[i];
			bm->start = m_base[i].mod_start;
			bm->end = m_base[i].mod_end;
			strncpy(bm->cmdline, (const char *)m_base[i].cmdline,
				sizeof(bm->cmdline));
			/* TODO: allow for non-ELF modules (initrd, etc) */
			parse_elf_range(bm);
			printf("  mod %d = [%#lx .. %#lx] -> [%#lx .. %#lx] `%s'\n", i,
				bm->start, bm->end, bm->load_start, bm->load_end,
				bm->cmdline);
			r_start = min_t(uintptr_t, bm->start, r_start);
			r_end = max_t(uintptr_t, bm->end - 1, r_end);
		}
	} else {
		panic("no multiboot modules found!");
	}
	printf("multiboot modules are between [%#x, %#x]\n", r_start, r_end);

	heap_low = r_end;
	for(int i=0; i < num_boot_mods; i++) {
		heap_low = max_t(uintptr_t, heap_low, boot_mods[i].load_end + 1);
	}
	heap_low = (heap_low + PAGE_SIZE - 1) & ~PAGE_MASK;
	printf("sbrk() heap starts at %#x\n", heap_low);

	/* locate kernel, sigma0, sigma1, roottask modules.
	 *
	 * the kernel is identified by being the first module. sigma0 and sigma1
	 * are identified by name. roottask follows after sigma1, or after sigma0
	 * if sigma1 isn't present.
	 */
	struct boot_module *kernel_mod = &boot_mods[0],
		*s0_mod = NULL, *s1_mod = NULL, *roottask_mod;
	for(int i=1; i < num_boot_mods; i++) {
		char *slash = strrchr(boot_mods[i].cmdline, '/');
		if(slash == NULL) slash = boot_mods[i].cmdline; else slash++;
		if(s0_mod == NULL && strcmp(slash, "sigma0") == 0) {
			s0_mod = &boot_mods[i];
		} else if(s1_mod == NULL && strcmp(slash, "sigma1") == 0) {
			s1_mod = &boot_mods[i];
		}
	}
	if(s0_mod == NULL) panic("cannot find module for sigma0!");
	roottask_mod = (s1_mod != NULL ? s1_mod : s0_mod) + 1;
	if(roottask_mod >= &boot_mods[num_boot_mods]) {
		printf("no roottask found, leaving KCP slot empty\n");
		roottask_mod = NULL;
	}
	struct boot_module *mods[] = { kernel_mod, s0_mod, s1_mod, roottask_mod };
	check_boot_modules(mods, ARRAY_SIZE(mods));

	/* load kernel, s0, s1, roottask */
	load_elf_module(kernel_mod);
	load_elf_module(s0_mod);
	if(s1_mod != NULL) load_elf_module(s1_mod);
	if(roottask_mod != NULL) load_elf_module(roottask_mod);

	/* designate and fill a kernel configuration page. */
	void *kcp_base = sbrk(PAGE_SIZE);
	assert(!CHECK_FLAG_ANY((uintptr_t)kcp_base, PAGE_MASK));
	fill_kcp(kcp_base, kernel_mod, s0_mod, s1_mod, roottask_mod,
		mmap_ents, mmap_count, &plat_pc_mmap, NULL);

	printf("mbiloader heap ends at %#x\n", heap_low);

	/* the x86 boot parameter "*P"; we'll recycle some unspecified KCP fields
	 * for this.
	 */
	void *bigp = kcp_base + 0x60;
	*(L4_Word_t *)(bigp + 0x00) = 1;
	*(L4_Word_t *)(bigp + 0x04) = mem_before_640k;
	*(L4_Word_t *)(bigp + 0x08) = mem_after_1m;
	printf("entering L4.X2 kernel...\n");
	asm volatile (
		"\tjmp *%%ecx\n"
		:: "a" (0x2BADB002), "b" (bigp), "c" (kernel_mod->ip));

	printf("mbiloader: shouldn't get here! halting.\n");

	return 0;
}
