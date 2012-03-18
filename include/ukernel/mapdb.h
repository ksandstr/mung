#ifndef SEEN_UKERNEL_MAPDB_H
#define SEEN_UKERNEL_MAPDB_H

#include <stdint.h>
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

#define ENTRIES_PER_GROUP 128		/* half a megabyte in 4k pages */


struct space;


/* represents a hole when page_id == 0 . */
struct map_entry
{
	uintptr_t parent;
	uint32_t page_id;
	uint16_t flags;			/* 0-2: L4 access bits */
	uint16_t num_children;	/* (upper 6 bits redundant) */
	union {
		uintptr_t child;		/* when num_children <= 1 */
		uintptr_t *children;	/* otherwise (malloc()'d) */
	};
};


/* TODO: this format eats up 16 MiB of memory for a whole 4 GiB of mappings.
 * that's clearly not OK, so add a sparse representation as well (& a
 * conversion criteria). that'll permit allocation of map groups on a slab,
 * too.
 */
struct map_group
{
	uintptr_t start;
	struct map_entry entries[ENTRIES_PER_GROUP];
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

/* access from the pagefault handler. returns NULL when the entry doesn't
 * exist.
 */
extern const struct map_entry *mapdb_probe(
	struct map_db *db,
	uintptr_t addr);

/* kernel-mode initialization */
extern void mapdb_init_range(
	struct map_db *ptr,
	uintptr_t start_addr,
	const uint32_t *page_ids,
	unsigned int num_pages);

#endif
