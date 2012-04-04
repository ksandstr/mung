#ifndef SEEN_UKERNEL_MAPDB_H
#define SEEN_UKERNEL_MAPDB_H

#include <stdint.h>
#include <assert.h>
#include <ccan/htable/htable.h>

#include <l4/types.h>
#include <ukernel/mm.h>


/* zero is not a valid space ID. therefore a mapdb_ref of just 0 is a good
 * null value.
 */

#define MAPDB_REF(spaceid, addr) (((spaceid) & PAGE_MASK) | ((addr) & ~page_mask))
#define REF_DEFINED(ref) (REF_SPACE((ref)) != 0)
#define REF_SPACE(ref) ((ref) & PAGE_MASK)
#define REF_ADDR(ref) ((ref) & ~PAGE_MASK)

#define MAX_ENTRIES_PER_GROUP 1024	/* 4 MiB in 4 KiB pages */


struct space;


/* "range" is L4_nilpage if entry is empty */
struct map_entry
{
	L4_Fpage_t range;		/* incl. L4 permission bits */
	uint32_t first_page_id;
	/* there may be fewer actual children of this map_entry; num_children only
	 * tracks whether "child" or "children" is to be used, and how many words
	 * are allocated under "children" when num_children > 1.
	 *
	 * empties may show up as L4_nilpage, or a reference to a child space that
	 * either doesn't exist, has no mapping at that address, or the mapping
	 * doesn't reference a physical page in this map_entry. (the latter are
	 * compacted by Unmap and the large-page splitting function [which divvies
	 * the larger page's children appropriately].)
	 *
	 * "children" is also used to verify that the previous mapping was made
	 * from this entry at this address, as required by the L4.X2 criteria on
	 * when a map/grant extends the privileges of an existing mapping.
	 */
	uint16_t num_children;	/* (upper 4 bits redundant) */
	union {
		L4_Word_t child;		/* when num_children <= 1 */
		L4_Word_t *children;	/* otherwise (malloc()'d) */
	};
};


struct map_group
{
	uintptr_t start;			/* virtual address */
	uint16_t num_entries;
	uint16_t num_alloc;			/* always a power of two, or 0; never 1 */
	uint32_t occ[8];			/* occupancy bitmap, 4 entries per bit */
	struct map_entry *entries;	/* at most MAX_ENTRIES_PER_GROUP */
};


struct map_db
{
	uint32_t ref_id;

	/* keyed by int_hash(start), entered as <struct map_group *>.
	 *
	 * TODO: change to a word_hash() function.
	 */
	struct htable groups;

	struct space *space;
};


extern void init_mapdb(void);

extern void mapdb_init(struct map_db *ptr, struct space *space);
extern void mapdb_destroy(struct map_db *ptr);

/* returns OR mask of rights that would've been granted by this mapping
 * operation (which doesn't happen in the rights extension case, but is
 * ignored. [TODO: consider what difference this makes.])
 */
extern int mapdb_map_pages(
	struct map_db *from,
	struct map_db *to,
	L4_Fpage_t src_page,
	L4_Word_t dest_addr);

/* access from the pagefault handler. returns NULL when the entry doesn't
 * exist.
 */
extern struct map_entry *mapdb_probe(
	struct map_db *db,
	uintptr_t addr);

/* map_entry accessor */
static inline uint32_t mapdb_page_id_in_entry(
	const struct map_entry *m,
	uintptr_t addr)
{
	assert(!L4_IsNilFpage(m->range));
	return m->first_page_id + ((addr - L4_Address(m->range)) >> PAGE_BITS);
}


/* kernel-mode initialization */
extern void mapdb_init_range(
	struct map_db *ptr,
	uintptr_t start_addr,
	const uint32_t *page_ids,
	unsigned int num_pages,
	int entry_flags);

#endif
