/* utility routines for L4_MemoryDesc_t handling. used to construct the KCP
 * and KIP structures in mbiloader and the kernel, respectively.
 */

#ifndef SEEN_UKERNEL_MEMDESC_H
#define SEEN_UKERNEL_MEMDESC_H

#include <stdint.h>
#include <stdbool.h>

#include <l4/types.h>
#include <l4/kip.h>


/* contains a number of MemoryDesc items in a buffer of at most 64k items,
 * allowing easy querying and modification.
 *
 * modification makes sure that conflicting memory descs won't be output.
 * instead conflicts are resolved as last-wins. querying handles conflicting
 * memory descs with a "least permissive wins" policy.
 *
 * this means that generators should output memdescs permissive-first to
 * guarantee exact results from the query side.
 */
struct memdescbuf {
	L4_MemoryDesc_t *ptr;
	uint16_t size, len;
};


/* picks through @mdb, searching for a @virtual (or not) descriptor with
 * L4_MemoryDescType() @t in the query range, which isn't overlapped by a
 * reserved (unless @t is the reserved memory type) or @dedicated (optional)
 * memorydesc.
 *
 * returns leftmost flexpage subset of the query range for which the condition
 * is true, or nilpage when nothing was matched. as the subset result may not
 * cover the entire valid result-range, the caller may wish to probe forward
 * until there's a nil result (or its range is out).
 *
 * note that return values may have sizes smaller than PAGE_SIZE.
 *
 * this function is suitable for calling in an early-boot context: it
 * allocates only constant stack space.
 *
 * does not alter @mdb. it's non-const because the L4.X2 MemoryDesc accessors
 * aren't const-clean.
 */
extern L4_Fpage_t mdb_query(
	struct memdescbuf *mdb,
	L4_Word_t start, L4_Word_t end,		/* query range is [start, end] */
	bool virtual, bool dedicated, L4_Word_t t);

#endif
