
/* dealing with the global descriptor table. */

#include <stdio.h>
#include <stdint.h>
#include <assert.h>

#include <ukernel/mm.h>
#include <ukernel/tss.h>
#include <ukernel/misc.h>
#include <ukernel/gdt.h>


struct gdt_entry {
	uint16_t limit_0;
	uint16_t base_0;
	uint8_t base_1;
	uint8_t access;
	uint8_t flags_limit1;		/* upper 4 bits = flags, lower 4 = limit 16-19 */
	uint8_t base_2;
} __attribute__((packed));


/* the descriptor structure that LGDT eats */
struct gdt_desc {
	uint16_t limit;
	uint32_t base;
} __attribute__((packed));


/* type fields for code/data */
#define DESC_A_ACCESSED (1 << 0)
#define DESC_A_RW (1 << 1)		/* readable / read-write */
#define DESC_A_DC (1 << 2)		/* direction/conforming bit */
#define DESC_A_EX (1 << 3)		/* executable bit */

/* type values for system descriptors (well, just the one) */
#define DESC_A_TSS_32BIT 0x09

#define DESC_A_SYSTEM (1 << 4)	/* clear for system, set for code/data */
#define DESC_A_PRIV_MASK ((1 << 5) | (1 << 6))
#define DESC_A_PRESENT (1 << 7)

#define DESC_F_SZ (1 << 2)		/* 0 = 16 bit, 1 = 32 bit */
#define DESC_F_GR (1 << 3)		/* 0 = bytes, 1 = 4k pages */

#define GDT_ENTRY(base, limit, access_, flags) \
	((struct gdt_entry){ \
		.base_0 = (base) & 0xffff, \
		.base_1 = ((base) >> 16) & 0xff, \
		.base_2 = ((base) >> 24) & 0xff, \
		.access = (access_), \
		.limit_0 = (limit) & 0xffff, \
		.flags_limit1 = (flags) << 4 | (((limit) >> 16) & 0xf), \
	})


void dump_gdt(struct gdt_desc *gd)
{
	printf("gdt_desc: base 0x%x, limit %u\n", gd->base, gd->limit);
	for(int i=0; i < ((int)gd->limit + 1) / 8; i++) {
		const struct gdt_entry *ge = (void *)gd->base + i * 8;
		if(!CHECK_FLAG(ge->access, DESC_A_PRESENT)) {
			printf("GDT entry %d not present\n", i);
			continue;
		}
		printf("GDT entry %d (selector 0x%x, access 0x%x, flags 0x%x):\n",
			i, (unsigned)i * 8, ge->access, ge->flags_limit1 & 0xf0);
		printf("  base 0x%x, limit 0x%x (%s)",
			(uint32_t)ge->base_0 | (uint32_t)ge->base_1 << 16
				| (uint32_t)ge->base_2 << 24,
			(uint32_t)ge->limit_0 | ((uint32_t)ge->flags_limit1 & 0xf) << 16,
			CHECK_FLAG(ge->flags_limit1 >> 4, DESC_F_GR) ? "pages" : "bytes");
		printf(", %s, %s\n",
			CHECK_FLAG(ge->access, DESC_A_EX) ? "code" : "data",
			CHECK_FLAG(ge->flags_limit1 >> 4, DESC_F_SZ) ? "32-bit" : "16-bit");
	}
}


/* create a nice, friendly global descriptor table. */
void setup_gdt(void)
{
	assert(sizeof(struct gdt_entry) == 8);

	static struct gdt_entry gdt_array[N_KERNEL_SEGS] PAGE_ALIGN;
	for(int i=0; i < N_KERNEL_SEGS; i++) {
		gdt_array[i] = (struct gdt_entry){ };
	}

	gdt_array[0] = GDT_ENTRY(0, 0, 0, 0);
	gdt_array[SEG_KERNEL_CODE] = GDT_ENTRY(0, 0xfffff,
		DESC_A_PRESENT | DESC_A_RW | DESC_A_SYSTEM | DESC_A_EX,
		DESC_F_SZ | DESC_F_GR);
	gdt_array[SEG_KERNEL_DATA] = GDT_ENTRY(0, 0xfffff,
		DESC_A_PRESENT | DESC_A_RW | DESC_A_SYSTEM, DESC_F_SZ | DESC_F_GR);
	gdt_array[SEG_KERNEL_TSS] = GDT_ENTRY((intptr_t)&kernel_tss,
		sizeof(kernel_tss), DESC_A_PRESENT | DESC_A_TSS_32BIT, DESC_F_SZ);

	/* special segments that make kernel code and data appear at low
	 * addresses, even though they are at the top of the linear address space.
	 */
	gdt_array[SEG_KERNEL_CODE_HIGH] = GDT_ENTRY(KERNEL_SEG_START,
		KERNEL_SEG_SIZE >> PAGE_BITS,
		DESC_A_PRESENT | DESC_A_RW | DESC_A_SYSTEM | DESC_A_EX,
		DESC_F_SZ | DESC_F_GR);
	gdt_array[SEG_KERNEL_DATA_HIGH] = GDT_ENTRY(KERNEL_SEG_START,
		KERNEL_SEG_SIZE >> PAGE_BITS,
		DESC_A_PRESENT | DESC_A_RW | DESC_A_SYSTEM, DESC_F_SZ | DESC_F_GR);

	static struct gdt_desc gd = {
		.limit = sizeof(gdt_array) - 1,
		.base = (intptr_t)gdt_array,
	};

#if 0
	printf("about to load (gdt_array at 0x%x):\n", (unsigned)&gdt_array[0]);
	dump_gdt(&gd);
#endif

	asm volatile ("lgdt %0" :: "m" (gd) : "memory");
	asm volatile ("ltr %%ax" :: "a" (SEG_KERNEL_TSS * 8) : "memory");
	asm volatile (
		"\tljmp %0,$1f\n"
		"1:\n"
		:: "i" (SEG_KERNEL_CODE * 8));
	asm volatile (
		"\tmov %0, %%ds\n"
		"\tmov %0, %%es\n"
		"\tmov %0, %%fs\n"
		"\tmov %0, %%gs\n"
		"\tmov %0, %%ss\n"
		:: "r" (SEG_KERNEL_DATA * 8)
		: "memory");
}


void go_high(void)
{
	printf("%s: kernel seg at [0x%x .. 0x%x], length 0x%x (%d MiB)\n",
		__func__, KERNEL_SEG_START, KERNEL_SEG_START + KERNEL_SEG_SIZE - 1,
		KERNEL_SEG_SIZE, KERNEL_SEG_SIZE / (1024 * 1024));

	const int data_sel = SEG_KERNEL_DATA_HIGH << 3;

#if 0
	kernel_tss.ss0 = data_sel;
	asm volatile ("ltr %%ax" :: "a" (SEG_KERNEL_TSS << 3) : "memory");
	printf("new TSS stack segment installed\n");
#endif
	asm volatile (
		"\tljmp %0,$1f\n"
		"1:\n"
		:: "i" (SEG_KERNEL_CODE_HIGH << 3));
	asm volatile (
		"\tmov %0, %%ds\n"
		"\tmov %0, %%es\n"
		"\tmov %0, %%fs\n"
		"\tmov %0, %%gs\n"
		"\tmov %0, %%ss\n"
		:: "r" (data_sel)
		: "memory");
}
