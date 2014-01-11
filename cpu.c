
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

#include <ccan/compiler/compiler.h>

#include <ukernel/util.h>
#include <ukernel/x86.h>
#include <ukernel/cpu.h>


struct feature_bit {
	int bit;
	const char *name;
};


static struct x86_features features;


static COLD void print_features(uint32_t mask, const struct feature_bit *fs)
{
	bool first = true;
	for(int i=0; fs[i].name != NULL; i++) {
		if(CHECK_FLAG(mask, 1 << fs[i].bit)) {
			printf("%s%s", first ? "" : " ", fs[i].name);
			first = false;
		}
	}
}


/* x86 CPUID parsing (only for features on the first page, and those things
 * that're used by the kernel)
 */
COLD void scan_cpuid(void)
{
	struct cpuid_out id;
	char cpuname[13];
	x86_cpuid(&id, 0, 0, 0, 0);
	memcpy(&cpuname[0], &id.ebx, sizeof(uint32_t));
	memcpy(&cpuname[4], &id.edx, sizeof(uint32_t));
	memcpy(&cpuname[8], &id.ecx, sizeof(uint32_t));
	cpuname[12] = '\0';
	int max_page = id.eax;
	printf("CPU is a `%s' (with %d info pages)\n", cpuname, max_page);

	if(1 <= max_page) {
		/* intel page 01h */
		printf("page 01h:\n");
		x86_cpuid(&id, 1, 0, 0, 0);
		features.ecx = id.ecx;
		features.edx = id.edx;
		int stepping = id.eax & 0xf, model = (id.eax >> 4) & 0xf,
			family_id = (id.eax >> 8) & 0xf, proc_type = (id.eax >> 12) & 0x3,
			ext_model = (id.eax >> 16) & 0xf, ext_family = (id.eax >> 20) & 0xff;
		if(family_id == 0x6 || family_id == 0xf) model |= ext_model << 4;
		if(family_id == 0xf) family_id += ext_family;
		printf("  type %d, model %d, family %d, stepping %d\n",
			proc_type, model, family_id, stepping);
		int brand_ix = id.ebx & 0xff, clflush_sz = ((id.ebx >> 8) & 0xff) << 3,
			max_local_cpus = (id.ebx >> 16) & 0xff,
			init_apic_id = (id.ebx >> 24) & 0xff;
		printf("  brand_ix %d, clflush_sz %d, max_local_cpus %d, apic_id %d\n",
			brand_ix, clflush_sz, max_local_cpus, init_apic_id);
		static const struct feature_bit p01_ecx[] = {
			{ 0, "sse3" }, { 1, "pclmulqdq" }, { 2, "dtes64" }, { 3, "monitor" },
			{ 4, "dscpl" }, { 5, "vmx" }, { 6, "smx" }, { 7, "eist" },
			{ 8, "tm2" }, { 9, "ssse3" }, { 10, "cnxtid" }, { 12, "fma" },
			{ 13, "cmpxchg16b" }, { 14, "xtpruc" }, { 15, "pdcm" }, { 17, "pcid" },
			{ 18, "dca" }, { 19, "sse41" }, { 20, "sse42" }, { 21, "x2apic" },
			{ 22, "movbe" }, { 23, "popcnt" }, { 24, "tscdl" }, { 25, "aesni" },
			{ 26, "xsave" }, { 27, "osxsave" }, { 28, "avx" }, { 29, "f16c" },
			{ 30, "rdrand" },
			{ .name = NULL },
		}, p01_edx[] = {
			{ 0, "fpu" }, { 1, "vme" }, { 2, "de" }, { 3, "pse" },
			{ 4, "tsc" }, { 5, "msr" }, { 6, "pae" }, { 7, "mce" },
			{ 8, "cx8" }, { 9, "apic" }, { 11, "sysenter" }, { 12, "mtrr" },
			{ 13, "pge" }, { 14, "mca" }, { 15, "cmov" }, { 16, "pat" },
			{ 17, "pse36" }, { 18, "psn" }, { 19, "clflush" }, { 21, "ds" },
			{ 22, "acpi" }, { 23, "mmx" }, { 24, "fxsr" }, { 25, "sse" },
			{ 26, "sse2" }, { 27, "ss" }, { 28, "htt" }, { 29, "tm" },
			{ 31, "pbe" },
			{ .name = NULL }
		};
		printf("  features: ");
		print_features(id.edx, p01_edx);	/* trad first. */
		print_features(id.ecx, p01_ecx);
		printf("\n");
	}

	if(2 <= max_page) {
		/* intel page 02h */
		printf("page 02h:\n");
		x86_cpuid(&id, 2, 0, 0, 0);
		int id_iters = CHECK_FLAG(id.eax, 0x80000000u) ? 1 : id_iters, iter_id = 0;
		do {
			uint32_t descs[3] = { id.ebx, id.ecx, id.edx };
			for(int i=0; i < 3; i++) {
				int d = descs[i] & 0xff;
				if(CHECK_FLAG(descs[i], 0x80000000u) || d == 0) continue;
				switch(d) {
					case 0x7d:
						printf("  L2d: 2M, 64B line, 8-way set assoc\n");
						break;

					default:
						printf("  descriptor %#02x\n", descs[i] & 0xff);
				}
			}
			x86_cpuid(&id, 2, 0, 0, 0);
		} while(++iter_id < id_iters);
	}
}


/* TODO: for smp, get this from the per-CPU spot */
const cpu_features *get_features(void) {
	return &features;
}


SYSCALL L4_Word_t sys_processorcontrol(
	L4_Word_t proc_no,
	L4_Word_t internal_freq,
	L4_Word_t external_freq,
	L4_Word_t voltage)
{
	printf("%s: called\n", __func__);
	return 1;
}
