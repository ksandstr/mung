
#ifndef SEEN_UKERNEL_ACPI_H
#define SEEN_UKERNEL_ACPI_H

#include <stdint.h>


#define IS_V20_COMPAT(rsdp) ((rsdp)->revision >= 1)


struct rsdp_v20
{
	char signature[8];
	uint8_t checksum;
	char oemid[6];
	uint8_t revision;
	uint32_t rsdtaddress;

	/* version 2.0 fields */
	uint32_t length;
	uint64_t xsdtaddress;
	uint8_t extendedchecksum;
	uint8_t reserved[3];
} __attribute__((packed));


struct sdt_header
{
	char signature[4];
	uint32_t length;
	uint8_t revision;
	uint8_t checksum;
	char oemid[6];
	char oemtableid[8];
	uint32_t oemrevision;
	uint32_t creatorid;
	uint32_t creatorrevision;
} __attribute__((packed));


struct acpi_gas {
	uint8_t addressspace;
	uint8_t bitwidth;
	uint8_t bitoffset;
	uint8_t accesssize;
	uint64_t address;
} __attribute__((packed));


struct acpi_fadt
{
	struct sdt_header h;
	uint32_t firmwarectrl;
	uint32_t dsdt;

	// field used in acpi 1.0; no longer in use, for compatibility only
	uint8_t reserved;

	uint8_t  preferredpowermanagementprofile;
	uint16_t sci_interrupt;
	uint32_t smi_commandport;
	uint8_t acpienable;
	uint8_t acpidisable;
	uint8_t s4bios_req;
	uint8_t pstate_control;
	uint32_t pm1aeventblock;
	uint32_t pm1beventblock;
	uint32_t pm1acontrolblock;
	uint32_t pm1bcontrolblock;
	uint32_t pm2controlblock;
	uint32_t pmtimerblock;
	uint32_t gpe0block;
	uint32_t gpe1block;
	uint8_t pm1eventlength;
	uint8_t pm1controllength;
	uint8_t pm2controllength;
	uint8_t pmtimerlength;
	uint8_t gpe0length;
	uint8_t gpe1length;
	uint8_t gpe1base;
	uint8_t cstatecontrol;
	uint16_t worstc2latency;
	uint16_t worstc3latency;
	uint16_t flushsize;
	uint16_t flushstride;
	uint8_t dutyoffset;
	uint8_t dutywidth;
	uint8_t dayalarm;
	uint8_t monthalarm;
	uint8_t century;

	// reserved in acpi 1.0; used since acpi 2.0+
	uint16_t bootarchitectureflags;

	uint8_t reserved2;
	uint32_t flags;

	// 12 byte structure; see below for details
	struct acpi_gas resetreg;

	uint8_t resetvalue;
	uint8_t reserved3[3];

	// 64bit pointers - available on acpi 2.0+
	uint64_t x_firmwarecontrol;
	uint64_t x_dsdt;

	struct acpi_gas x_pm1aeventblock;
	struct acpi_gas x_pm1beventblock;
	struct acpi_gas x_pm1acontrolblock;
	struct acpi_gas x_pm1bcontrolblock;
	struct acpi_gas x_pm2controlblock;
	struct acpi_gas x_pmtimerblock;
	struct acpi_gas x_gpe0block;
	struct acpi_gas x_gpe1block;
} __attribute__((packed));


/* "multiple APIC description table" */
struct acpi_madt
{
	struct sdt_header h;
	uint32_t lc_addr;	/* local controller address */
	uint32_t flags;
	uint8_t data[];
} __attribute__((packed));


/* HPET entry */
struct acpi_hpet
{
	struct sdt_header h;
	uint32_t block_id;		/* event timer block ID */
	struct acpi_gas base;	/* base address (lower 32 bit) */
	uint8_t hpet_id;		/* number of HPET described */
	uint16_t min_tick;		/* minimum # of clock ticks in periodic mode */
	uint8_t prot;			/* page protection & OEM bits */
} __attribute__((packed));


/* globals from acpi.c */

/* (copied into kernel heap by acpi_init()) */
extern const struct acpi_fadt *acpi_fadt;
extern const struct acpi_madt *acpi_madt;
extern const struct acpi_hpet *acpi_hpet;


/* 0 on success, negative on failure */
extern int acpi_init(void);


#endif
