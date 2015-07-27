
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

#include <ukernel/memdesc.h>

#include "defs.h"
#include "multiboot.h"


bool plat_pc_probe(multiboot_info_t *mbi)
{
	return true;	/* damn the torpedoes! */
}


#define SET(buf, lo, hi, v, t, subt) do { \
		if(!mdb_set((buf), (lo), (hi), (v), (t), (subt))) return false; \
	} while(false)

bool plat_pc_mmap(struct memdescbuf *mdb, void *priv)
{
	/* the x86 virtual address space without the kernel reservation. */
	SET(mdb, 0, ~0ul, true, L4_ConventionalMemoryType, 0);

	/* PC-BIOS realmode IVT (0..0x3ff) and data area (0x400..0x4ff). */
	SET(mdb, 0, 0x7ff, false,
		L4_ArchitectureSpecificMemoryType, L4_BIOSRegion);
	/* OS boot sector (what.) */
	SET(mdb, 0x7c00, 0x7fff, false,
		L4_ArchitectureSpecificMemoryType, L4_BIOSRegion);

	/* IBM-PC VGA RAM. shared between OS and a hardware device. */
	SET(mdb, 0xa0000, 0xbffff, false, L4_SharedMemoryType, 0);
	/* VGA BIOS. */
	SET(mdb, 0xc0000, 0xc7fff, false,
		L4_ArchitectureSpecificMemoryType, L4_ReadOnlyBIOSRegion);
	/* misc BIOS area: ROMs and unusable space. */
	SET(mdb, 0xc8000, 0xeffff, false,
		L4_ArchitectureSpecificMemoryType, L4_ReadOnlyBIOSRegion);
	/* motherboard BIOS. */
	SET(mdb, 0xf0000, 0xfffff, false,
		L4_ArchitectureSpecificMemoryType, L4_ReadOnlyBIOSRegion);

	return true;
}
