
/* support for the IA-PC IOAPIC architecture, i.e. LAPICs and I/O APICs */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include <assert.h>

#include <ccan/compiler/compiler.h>
#include <ccan/darray/darray.h>

#include <ukernel/ioport.h>
#include <ukernel/interrupt.h>
#include <ukernel/x86.h>
#include <ukernel/acpi.h>
#include <ukernel/mm.h>
#include <ukernel/cpu.h>
#include <ukernel/util.h>
#include <ukernel/misc.h>


/* fields in IA32_APIC_BASE */
#define AB_BOOTSTRAP_CPU (1 << 8)
#define AB_GLOBAL_ENABLE (1 << 11)
/* base address is bits 31..12, i.e. 32-bit ~PAGE_MASK */


/* LAPIC MMIO registers (at 16-byte offsets, of which the low 32 bits can be
 * read (perhaps also written) with the 32-bit MMIO functions.)
 */
#define APIC_LAPIC_ID	0x20	/* local APIC ID (RO [partially RW]) */
#define APIC_LAPIC_VER	0x30	/* LAPIC version (RO) */
#define APIC_TPR		0x80	/* task priority (RW) */
#define APIC_APR		0x90	/* arbitrarion priority (RO, >p4) */
#define APIC_PPR		0xa0	/* CPU priority (RO) */
#define APIC_EOI		0xb0	/* EOI register (WO) */
#define APIC_RRD		0xc0	/* remote read register (RO, >p4) */
#define APIC_SIVR		0xf0	/* spurious-interrupt vector (RW) */
#define APIC_ERRSTATUS	0x280	/* error status register (RO) */
/* long registers. valid indexes are 0..7, low to high dword; except for
 * APIC_ICR(), which only has 64 bits.
 */
#define APIC_ISR(ix) (0x100 + (ix) * 0x10)	/* in-service register (RO) */
#define APIC_TMR(ix) (0x180 + (ix) * 0x10)	/* trigger mode reg (RO) */
#define APIC_IRR(ix) (0x200 + (ix) * 0x10)	/* interrupt request reg (RO) */
#define APIC_ICR(ix) (0x300 + (ix) * 0x10)	/* 0..1: interrupt cmd reg (RW) */
#define APIC_LVT_TIMER	0x320	/* local timer vector */
#define APIC_LVT_THERM	0x330	/* same, but thermal */
#define APIC_LVT_PERF	0x340	/* same, performance monitoring */
#define APIC_LVT_LINT0	0x350	/* same, LINT0 */
#define APIC_LVT_LINT1	0x360	/* same, LINT1 */
#define APIC_LVT_ERROR	0x370	/* same, error vector */
#define APIC_TIMER_INIT	0x380	/* LAPIC timer initial count register */
#define APIC_TIMER_CUR	0x390	/* LAPIC timer current count register (RO) */
#define APIC_TIMER_DCR	0x3e0	/* LAPIC timer divide cfg register */
/* etc... (add as needed) */

/* IOAPIC registers (via ioapic_{read,write}()) */
#define IOAPICID	0x00
#define IOAPICVER	0x01
#define IOAPICARB	0x02
#define IOREDTBL(n)	(0x10 + 2 * (n))


/* (this is intended as per-CPU. as there is no malloc in apic_init(), it'll
 * be allocated on a "max CPUs allowed by this kernel" basis in the BSS
 * section.)
 */
struct lapic_info
{
	uint32_t base_addr, phys_base;
	bool enabled, bootstrap;
	uint8_t max_vector;	/* uppermost vector scanned in isr_apic_bottom() */

	/* filled by interrupt when no ISR bit is set */
	uint32_t num_spurious;
};


struct ioapic_info
{
	uint32_t base_addr, phys_base, gsi_base;
	uint8_t apic_id, max_redir;
};


bool apic_enabled = false, apic_disable_opt = false;
struct lapic_info cpu_apics[1];		/* still an UP kernel. */

struct ioapic_info *global_ioapics = NULL;
int num_global_ioapics = 0;


#if 0
/* TODO: export this somewhere */
void apic_send_self_ipi(int vector)
{
	assert(vector > 0xf && vector <= 0xff);

	struct lapic_info *apic = &cpu_apics[0];
	/* self-interrupt destination shorthand, vector as given */
	mm_outl(apic->base_addr + APIC_ICR(0), vector | (1 << 18));
}
#endif


/* there are at most 256 32-bit registers in a pre-2013 IOAPIC. */
static inline void ioapic_write(
	struct ioapic_info *ioa,
	uint8_t address,
	uint32_t value)
{
	assert(!x86_irq_is_enabled());
	mm_outb(ioa->base_addr + 0, address);	/* IOREGSEL */
	mm_outl(ioa->base_addr + 0x10, value);	/* IOWIN */
}


static inline uint32_t ioapic_read(struct ioapic_info *ioa, uint8_t address)
{
	assert(!x86_irq_is_enabled());
	mm_outb(ioa->base_addr + 0, address);
	return mm_inl(ioa->base_addr + 0x10);
}


/* EOI is actually sent to the local APIC, not the I/O APIC as the function's
 * name would indicate.
 */
static void ioapic_send_eoi(int irq)
{
	assert(!x86_irq_is_enabled());
	struct lapic_info *apic = &cpu_apics[0];
	/* the specs say that 0 is the right thing to write here. */
	mm_outl(apic->base_addr + APIC_EOI, 0);
}


static void ioapic_mask_irq(int irq)
{
	assert(!x86_irq_is_enabled());
	struct ioapic_info *ioa = &global_ioapics[irq / 24];
	int vec = irq % 24;

	uint32_t ctl = ioapic_read(ioa, IOREDTBL(vec) + 0);
	ctl |= 1 << 16;
	ioapic_write(ioa, IOREDTBL(vec) + 0, ctl);
}


static void ioapic_unmask_irq(int irq, unsigned flags)
{
	struct ioapic_info *ioa = &global_ioapics[irq / 24];
	int vec = irq % 24;

	/* transfer the act-low/level-trig pattern into the APIC setting word
	 * directly.
	 */
	assert(IHF_ACT_LOW == (1 << 0));
	assert(IHF_LEVEL_TRIG == (1 << 2));
	int trig_pol = (flags & (IHF_ACT_LOW | IHF_LEVEL_TRIG)) << 13;

	assert(CHECK_FLAG(trig_pol, 1 << 13)
		== CHECK_FLAG(flags, IHF_ACT_LOW));
	assert(CHECK_FLAG(trig_pol, 1 << 15)
		== CHECK_FLAG(flags, IHF_LEVEL_TRIG));

	/* bit 16 = interrupt mask bit.
	 *     15 = trigger mode (0 = edge, 1 = level)
	 *     13 = pin polarity (0 = active high, 1 = active low)
	 *     7..0 = interrupt vector (0x10..0xff)
	 *
	 * other fields select fixed (regular) delivery to a physical LAPIC (CPU
	 * 0, since mung is still not a SMP kernel).
	 */
	uint32_t ctl = ioapic_read(ioa, IOREDTBL(vec) + 0);
	ctl &= ~((1 << 16) | (1 << 13) | (1 << 15) | 0xff);
	ctl |= trig_pol | (0x20 + irq);
	ioapic_write(ioa, IOREDTBL(vec) + 0, ctl);
}


void isr_apic_bottom(struct x86_exregs *regs)
{
	struct lapic_info *apic = &cpu_apics[0];
#if 0
	printf("%s: frame at %p, exregs=%p, frame_len=%u (size=%u)\n",
		__func__, &apic, regs, (unsigned)x86_frame_len(regs),
		(unsigned)sizeof(*regs));
#endif

	/* figure out which interrupt this is from the ISRs. */
	int vecnum;
	bool vec_found = false;
	for(int i=0; i <= apic->max_vector; i += 32) {
		uint32_t isr_limb = mm_inl(apic->base_addr + APIC_ISR(i >> 5));
		if(i == 0) isr_limb &= ~0xffffu;
		if(isr_limb != 0) {
			vecnum = i + ffsl(isr_limb) - 1;
			vec_found = true;
			break;
		}
	}
	if(!vec_found) {
		apic->num_spurious++;
		ioapic_send_eoi(0);		/* just in case. */
		return;
	}

#if 0
	if(vecnum == 0x21) {
		printf("i'm a keyboard, toot toot\n");
#define KBD_STATUS_REG 0x64
#define KBD_DATA_REG 0x60
#define KBD_STAT_OBF 0x01
		for(;;) {
			uint8_t st = inb(KBD_STATUS_REG);
			if((st & KBD_STAT_OBF) == 0) break;
			inb(KBD_DATA_REG);	/* and throw it away */
		}
	} else {
		printf("APIC interrupt vector %d\n", vecnum);
	}
#endif

	regs->reason = vecnum;
	isr_irq_bottom(regs);
	return;

#if 0
	/* TODO: move this into an x86_print_exregs() function */
	printf("exregs: reason=%#lx, es=%#lx, ds=%#lx, cs=%#lx, ss=%#lx\n"
		"\tedi=%#lx, esi=%#lx, ebp=%#lx, __esp=%#lx\n"
		"\tebx=%#lx, edx=%#lx, ecx=%#lx, eax=%#lx\n"
		"\terror=%#lx, eip=%#lx, eflags=%#lx, esp=%#lx\n",
		regs->reason, regs->es, regs->ds, regs->cs, regs->ss,
		regs->edi, regs->esi, regs->ebp, regs->__esp,
		regs->ebx, regs->edx, regs->ecx, regs->eax,
		regs->error, regs->eip, regs->eflags, regs->esp);
#endif
}


int ioapic_route_legacy_irq(int irqnum, int ioa_vector)
{
	uint32_t global_int = irqnum;
	int polarity = 0;	/* active high */
	int level_sens = 0;	/* edge sensitive */
	if(irqnum < 16) {
		/* adjust per MADT source override (ISA bus) */
		for(int offs = 0, len = 0,
				max_offs = acpi_madt->h.length - sizeof(acpi_madt->h) - 1;
			offs <= max_offs;
			offs += len)
		{
			assert(offs >= 0);
			const uint8_t *b = &acpi_madt->data[offs];
			const int devtype = b[0];
			len = b[1];
			if(len == 0 || offs + len - 1 > max_offs) break;

			if(devtype != 2) continue;
			uint8_t bus = b[2], source = b[3];
			if(bus == 0 && source == irqnum) {
				memcpy(&global_int, &b[4], 4);
				uint16_t flags;
				memcpy(&flags, &b[8], 2);
				switch(flags & 0x3) {
					case 0:	/* bus default, for ISA it's active-low */
					case 3:	/* explicit active-low */
						polarity = 1;
						break;

					case 1:	/* active high */
						polarity = 0;
						break;

					default:
						panic("unknown polarity attribute in MADT!");
				}
				switch((flags >> 2) & 0x3) {
					case 0: case 1:
						/* default = edge sensitive */
						level_sens = 0;
						break;

					case 3:
						/* level */
						level_sens = 1;
						break;

					default:
						panic("unknown sensitivity attribute in MADT!");
				}
				printf("IOAPIC: override IRQ%d -> int# %u (act-%s, %s)\n",
					irqnum, global_int, polarity == 1 ? "low" : "high",
					level_sens ? "edge" : "level");
				break;
			}
		}
	}

	/* find the relevant IOAPIC. */
	struct ioapic_info *ioa = NULL;
	int redir;
	for(int i=0, base=0; i < num_global_ioapics; i++) {
		struct ioapic_info *cand = &global_ioapics[i];
		if(base <= global_int && base + cand->max_redir >= global_int) {
			ioa = cand;
			redir = global_int - base;
			break;
		}
		base += cand->max_redir + 1;
	}
	if(ioa == NULL) {
		printf("I/O APIC for int# %u not found!\n", global_int);
		panic("error return goes here");
	}

	ioapic_write(ioa, IOREDTBL(redir) + 1, 0);	/* dest APIC 0 (UP mode) */
	/* not masked, physical dest mode, fixed delivery, vector=@vector */
	ioapic_write(ioa, IOREDTBL(redir) + 0,
		(level_sens << 15) | (polarity << 13) | ioa_vector);

	return 0;
}


/* per-CPU LAPIC init. */
static COLD void lapic_init(void)
{
	struct lapic_info *apic = &cpu_apics[0];

	uint64_t base_msr = x86_rdmsr(IA32_APIC_BASE);
	printf("APIC_BASE=%#08x:%08x",
		(unsigned)(base_msr >> 32), (unsigned)base_msr);
	*apic = (struct lapic_info){
		/* FIXME: handle 36 bits, or as indicated by CPUID leaf 80000008h */
		.phys_base = (uint32_t)base_msr & ~0xfffu,
		.bootstrap = CHECK_FLAG(base_msr, AB_BOOTSTRAP_CPU),
		.enabled = CHECK_FLAG(base_msr, AB_GLOBAL_ENABLE),
	};
	printf(": addr=%#08x, bootstrap=%s, enabled=%s\n",
		apic->base_addr, btos(apic->bootstrap), btos(apic->enabled));
	/* flip the enable bit */
	base_msr |= AB_GLOBAL_ENABLE;
	x86_wrmsr(IA32_APIC_BASE, base_msr);

	/* map the APIC in wherever. its MMIO space is, conveniently enough,
	 * exactly 4k big.
	 */
	uintptr_t resv = reserve_heap_page();
	/* FIXME: set a flag that makes the APIC range uncacheable. */
	put_supervisor_page(resv, apic->phys_base >> PAGE_BITS);
	apic->base_addr = resv;
	apic->max_vector = 0xff;		/* TODO: use for something */

	uint32_t lapic_id = mm_inl(apic->base_addr + APIC_LAPIC_ID),
		lapic_ver = mm_inl(apic->base_addr + APIC_LAPIC_VER);
	printf("LAPIC_id=%u, LAPIC_ver=%#08x\n", lapic_id, lapic_ver);

	uint32_t tpr = mm_inl(apic->base_addr + APIC_TPR);
	printf("  TPR=%#08x", tpr);

	uint32_t sivr = mm_inl(apic->base_addr + APIC_SIVR);
	printf("  SIVR=%#08x\n", sivr);

#if 0
	printf("  LVT: timer=%#08x, therm=%#08x, perf=%#08x, l0=%#08x, l1=%#08x\n",
		mm_inl(apic->base_addr + APIC_LVT_TIMER),
		mm_inl(apic->base_addr + APIC_LVT_THERM),
		mm_inl(apic->base_addr + APIC_LVT_PERF),
		mm_inl(apic->base_addr + APIC_LVT_LINT0),
		mm_inl(apic->base_addr + APIC_LVT_LINT1));
	printf("       error=%#08x\n", mm_inl(apic->base_addr + APIC_LVT_ERROR));
#endif

	/* enable local APIC. */
	mm_orl(apic->base_addr + APIC_SIVR, 1 << 8);
}


COLD int apic_probe(void)
{
	apic_enabled = false;

	if(!CHECK_FLAG(get_features()->edx, X86_FEATURE_D_APIC)) {
		printf("APIC not detected in CPUID flags.\n");
		return -1;
	}

	if(apic_disable_opt) {
		printf("APIC found, but instructed to disable it\n");
		return -1;
	}

	lapic_init();	/* per CPU */
	apic_enabled = true;

	return 0;
}


COLD int ioapic_init(struct pic_ops *ops)
{
	if(acpi_madt == NULL) {
		/* TODO: crawl over MPS tables instead */
		printf("Can't find IOAPIC without ACPI MADT!\n");
		return -1;
	}

	/* scan the MADT for I/O APIC entries (type 1) */
	darray(struct ioapic_info) ioas = darray_new();
	for(int offs = 0, len = 0,
			max_offs = acpi_madt->h.length - sizeof(acpi_madt->h) - 1;
		offs <= max_offs;
		offs += len)
	{
		assert(offs >= 0);
		const uint8_t *b = &acpi_madt->data[offs];
		const int devtype = b[0];
		len = b[1];
		if(len == 0 || offs + len - 1 > max_offs) break;

		if(devtype != 1) continue;
		struct ioapic_info info;
		info.base_addr = 0;
		info.apic_id = b[2];
		memcpy(&info.phys_base, &b[4], 4);
		memcpy(&info.gsi_base, &b[8], 4);
		darray_push(ioas, info);
	}
	if(darray_empty(ioas)) {
		printf("No I/O APIC found in MADT!\n");
		darray_free(ioas);
		return -1;
	}

	num_global_ioapics = ioas.size;
	global_ioapics = realloc(ioas.item,
		sizeof(struct ioapic_info) * ioas.size);
	if(global_ioapics == NULL) {
		/* wasting some RAM, in ur punani */
		global_ioapics = ioas.item;
	}
	printf("... seeing %d global I/O APICs.\n", num_global_ioapics);

	/* TODO: could also sort them so that this invariant is true, but
	 * whatever.
	 */
	for(int i=0; i < num_global_ioapics; i++) {
		if(global_ioapics[i].apic_id != i) {
			printf("Weird I/O APIC order: offset=%d, id=%d!\n",
				i, global_ioapics[i].apic_id);
			panic("foo!");
		}
	}

	/* map them in & dump some info. */
	for(int i=0; i < num_global_ioapics; i++) {
		uintptr_t resv = reserve_heap_page();
		struct ioapic_info *ioa = &global_ioapics[i];
		/* TODO: disable caching for MMIO maps! */
		put_supervisor_page(resv, ioa->phys_base >> 12);
		ioa->base_addr = resv;

		printf("I/O APIC %d:\n", (int)ioa->apic_id);
		uint32_t ver = ioapic_read(ioa, IOAPICVER);
		printf("  IOAPICID=%#08x, IOAPICVER=%#08x\n",
			ioapic_read(ioa, IOAPICID), ver);
		ioa->max_redir = (ver >> 16) & 0xff;
	}

	/* mask all vectors and set them to a 1:1 mapping, with exceptions made by
	 * ioapic_route_legacy_irq() later.
	 *
	 * FIXME: this is likely not good enough. Bochs maps the interval timer to
	 * IRQ#2 (which is the unused cascade line for XT-PIC), which is fine for
	 * the microkernel, but other things are a bit iffy.
	 */
	for(int i=0; i < num_global_ioapics; i++) {
		struct ioapic_info *ioa = &global_ioapics[i];
		for(int line=0; line < 24; line++) {
			uint32_t destctl = ioapic_read(ioa, IOREDTBL(line) + 1),
				intctl = ioapic_read(ioa, IOREDTBL(line) + 0);
			destctl = 0;	/* dest = physical 0 (UP mode) */
			/* physical destination mode, fixed delivery mode, also reset the
			 * vector field
			 */
			intctl &= ~((1 << 11) | 0x700 | 0xff);
			intctl |= 0x10000;	/* mask */
			intctl |= i * 24 + line;
			ioapic_write(ioa, IOREDTBL(line) + 1, destctl);
			ioapic_write(ioa, IOREDTBL(line) + 0, intctl);
		}
	}

	*ops = (struct pic_ops){
		.send_eoi = &ioapic_send_eoi,
		.mask_irq = &ioapic_mask_irq,
		.unmask_irq = &ioapic_unmask_irq,
	};

	/* preallocate all vectors, permit defer, and direct to userspace. */
	int num_ints = num_global_ioapics * 24 - 1;
	set_irq_handler(num_ints, NULL, 0);
	return num_ints;
}
