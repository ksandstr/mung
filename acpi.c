/* ACPI support. */

#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include <ukernel/misc.h>
#include <ukernel/acpi.h>


const struct acpi_fadt *acpi_fadt = NULL;
const struct acpi_madt *acpi_madt = NULL;
const struct acpi_hpet *acpi_hpet = NULL;


/* scans for the Root System Description Pointer (RSDP) between 0x000e0000 and
 * 0x000fffff. returns the position where the signature starts.
 */
static struct rsdp_v20 *find_rsdp(void)
{
	struct rsdp_v20 *p = NULL;
	for(uintptr_t addr = 0x000e0000; addr <= 0x000fffff; addr += 16) {
		if(memcmp((void *)addr, "RSD PTR ", 8) == 0) {
			p = (void *)addr;
			assert(memcmp(p->signature, "RSD PTR ", 8) == 0);
			break;
		}
	}
	if(p == NULL) goto end;

	/* validate its checksum. */
	uint32_t sum = 0;
	for(int i=0; i < 20; i++) sum += ((uint8_t *)p)[i];
	if((sum & 0xff) != 0) {
		printf("ACPI RDSP checksum mismatch!\n");
		return NULL;
	}

end:
	return p;
}


static bool sdt_valid(const struct sdt_header *sdt)
{
	if(sdt->length > 1024 * 128) {
		printf("NOTE: SDT header too long! (length=%u)\n", sdt->length);
		printf("  (sig=`%4s', oemid=`%6s', oemtableid=`%8s')\n",
			sdt->signature, sdt->oemid, sdt->oemtableid);
		return false;
	}

	uint32_t sum = 0;
	for(size_t i=0; i < sdt->length; i++) sum += ((const uint8_t *)sdt)[i];
	return (sum & 0xff) == 0;
}


static void print_sdt(const struct sdt_header *sdt)
{
	printf(" signature `%4s', length %u, revision %u, checksum %#02x\n",
		sdt->signature, sdt->length, sdt->revision, sdt->checksum);
#if 0
	/* who gives a fuck about these, anyway. */
	printf("  oemid `%6s', oemtableid `%8s', oemrevision %u\n",
		sdt->oemid, sdt->oemtableid, sdt->oemrevision);
#endif
}


static void dump_madt(const struct acpi_madt *madt)
{
	size_t offs = 0, max_offs = madt->h.length - sizeof(madt->h);
	while(offs < max_offs) {
		const uint8_t *b = &madt->data[offs];
		const int devtype = b[0], len = b[1];
		if(len == 0) {
			printf("  explicit end of MADT\n");
			break;
		}
		printf("  [%u] devtype=%d, len=%d: ", (unsigned)offs, devtype, len);
		if(offs + len > max_offs) {
			printf("    (falls out!)\n");
			break;
		}
		switch(devtype) {
			case 0: {
				assert(len == 8);
				int acpi_cpuid = b[2], apic_id = b[3];
				uint32_t flags;
				memcpy(&flags, &b[4], sizeof(flags));
				printf("processor-local APIC: cpu %d, apic %d, flags %#x\n",
					acpi_cpuid, apic_id, flags);
				/* TODO: this is actually a CPU declaration. bit 0 of flags
				 * being on indicates that it's a valid, usable CPU. record
				 * these somewhere; they're relevant for the KIP and so forth.
				 */
				break;
			}

			case 1: {
				uint8_t apic_id = b[2];
				uint32_t addr, gsi_base;
				memcpy(&addr, &b[4], 4);
				memcpy(&gsi_base, &b[8], 4);
				printf("I/O APIC; apic_id=%u, addr=%#08x, gsi_base=%#x\n",
					apic_id, addr, gsi_base);
				assert(len == 12);
				break;
			}

			case 2: {
				uint8_t bus = b[2], source = b[3];
				uint32_t global_int;
				uint16_t flags;
				memcpy(&global_int, &b[4], 4);
				memcpy(&flags, &b[8], 2);
				printf("Int Src Override; bus=%u, source=%u, global_int=%u, flags=%#04x\n",
					bus, source, global_int, flags);
				int pol = flags & 0x3, trigger = (flags >> 2) & 0x3;
				printf("    flags={polarity=%d, trigger=%d}\n", pol, trigger);
				assert(len == 10);
				break;
			}

			case 3: {
				printf("NMI source\n");
				break;
			}

			case 4: {
				uint8_t cpu_id = b[2], lapic_lint = b[5];
				uint16_t flags;
				memcpy(&flags, &b[3], 2);
				printf("Local APIC NMI; cpu_id=%u, flags=%#04x, lapic_lint=%u\n",
					cpu_id, flags, lapic_lint);
				assert(len == 6);
				break;
			}

			case 5: {
				printf("Local APIC Address Override\n");
				break;
			}

			case 6: {
				printf("I/O SAPIC\n");
				break;
			}

			case 7: {
				printf("Local SAPIC\n");
				break;
			}

			case 8: {
				printf("Platform Interrupt Sources\n");
				break;
			}

			case 9: {
				printf("Local x2APIC\n");
				break;
			}

			case 0xa: {
				printf("Local x2APIC NMI\n");
				break;
			}

			case 0xb: {
				printf("GIC\n");
				break;
			}

			case 0xc: {
				printf("GICD\n");
				break;
			}

			default:
				printf("unknown APIC device type=%d len=%d\n",
					devtype, len);
		}
		offs += len;
	}
}


/* NOTE: this function runs either in a 1:1 mapping between physical and
 * virtual memory, or with paging disabled. caveat lector.
 */
int acpi_init(void)
{
	struct rsdp_v20 *rsdp = find_rsdp();
	if(rsdp == 0) {
		printf("ACPI RSDP not found!\n");
		return -1;
	}

	printf("ACPI RSDP structure %p\n", rsdp);
	printf("  checksum %#02x, OEM ID `%6s', revision %u, RSDT @ %#08x\n",
		rsdp->checksum, rsdp->oemid, rsdp->revision, rsdp->rsdtaddress);

	const struct sdt_header *rsdt_hdr = (void *)rsdp->rsdtaddress;
	if(!sdt_valid(rsdt_hdr)) {
		printf("RSDT checksum not valid!\n");
		return -1;
	}
	printf("RSDT header:"); print_sdt(rsdt_hdr);
	const bool is_v2 = IS_V20_COMPAT(rsdp);

	int num_sdt_ptrs = (rsdt_hdr->length - sizeof(struct sdt_header)) / 4;
	const uint32_t *sdt_ptrs = (void *)(rsdt_hdr + 1);
	uint64_t dsdt_ptr = 0, facs_ptr = 0;
	printf("%d SDT pointers:\n", num_sdt_ptrs);
	for(int i=0; i < num_sdt_ptrs; i++) {
		printf("  [%d] %#x", i, sdt_ptrs[i]);
		const struct sdt_header *hdr = (void *)sdt_ptrs[i];
		if(!sdt_valid(hdr)) {
			printf(" [not valid]\n");
			continue;
		}
		print_sdt(hdr);

		if(memcmp(hdr->signature, "FACP", 4) == 0) {
			acpi_fadt = (void *)hdr;
			dsdt_ptr = acpi_fadt->dsdt;
			if(dsdt_ptr == 0 && is_v2) dsdt_ptr = acpi_fadt->x_dsdt;
			facs_ptr = acpi_fadt->firmwarectrl;
			if(facs_ptr == 0 && is_v2) facs_ptr = acpi_fadt->x_firmwarecontrol;
		} else if(memcmp(hdr->signature, "APIC", 4) == 0) {
			acpi_madt = (void *)hdr;
			printf("MADT: len %u, local controller at %#08x, flags %#x\n",
				acpi_madt->h.length, acpi_madt->lc_addr, acpi_madt->flags);
			dump_madt(acpi_madt);
		} else if(memcmp(hdr->signature, "SSDT", 4) == 0) {
			printf("SSDT (ptr=%p) has %u bytes of code.\n", hdr,
				hdr->length - sizeof(struct sdt_header));
		} else if(memcmp(hdr->signature, "HPET", 4) == 0) {
			acpi_hpet = (void *)hdr;
			printf("HPET: block=%#08x, id=%u, min_tick=%u, prot=%#02x\n",
				acpi_hpet->block_id, acpi_hpet->hpet_id, acpi_hpet->min_tick,
				acpi_hpet->prot);
		}
	}

	if(dsdt_ptr != 0) {
		const struct sdt_header *dsdt = (void *)(uintptr_t)dsdt_ptr;
		if(!sdt_valid(dsdt)) {
			printf("ACPI DSDT not valid!\n");
		} else {
			printf("DSDT (ptr=%p) has %u bytes of code.\n", dsdt,
				dsdt->length - sizeof(struct sdt_header));
		}
	}

#if 0
	panic("in development!");
#endif

	return 0;
}