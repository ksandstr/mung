
/* inter-module defs for mbiloader */

#ifndef SEEN_MBILOADER_DEFS_H
#define SEEN_MBILOADER_DEFS_H

#include <stdbool.h>
#include <ukernel/memdesc.h>

#include "multiboot.h"


/* returns true if the platform is suitable for booting mung at all. */
extern bool plat_pc_probe(multiboot_info_t *mbi);

/* returns true if all calls to mdb_set() succeeded. */
extern bool plat_pc_mmap(struct memdescbuf *mdb, void *priv);


#endif
