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

#define MAPDB_REF(spaceid, addr) (((spaceid) & PAGE_MASK) | ((addr) & ~PAGE_MASK))
#define REF_DEFINED(ref) (REF_SPACE((ref)) != 0)
#define REF_SPACE(ref) ((ref) & PAGE_MASK)
#define REF_ADDR(ref) ((ref) & ~PAGE_MASK)
#define REF_SPECIAL(addr) MAPDB_REF(1, addr)

/* tombstone in map_entry->children */
#define REF_TOMBSTONE MAPDB_REF(0, 1 << PAGE_BITS)

#define LAST_PAGE_ID(ent) ((ent)->first_page_id + (L4_Size((ent)->range) >> PAGE_BITS) - 1)

#define MAX_ENTRIES_PER_GROUP 1024	/* 4 MiB in 4 KiB pages */


struct space;


/* L4_SizeLog2(@range) <= L4_SizeLog2(parent->range).
 *
 * toplevel mappings, such as those granted by sigma0, have an invalid
 * @parent.
 *
 * @range == L4_nilpage is used to indicate an empty slot in map_group.
 */
struct map_entry
{
	L4_Fpage_t range;		/* incl. L4 permission bits */
	uint32_t first_page_id;

	/* addr = address in parent space, may be in the middle of a larger range.
	 * space = parent space ID.
	 *
	 * special values:
	 *   - when space == 0 (i.e. !defined), there is no parent. this appears
	 *   at boot in sigma0, and entries whose immediate last parent has been
	 *   granted away (e.g. as granted by sigma0)
	 *   - when space == 1, the entry represents a kernel special range such
	 *   as the UTCB or KIP area, and is not subject to unmapping, access
	 *   querying, mapping on top of, or map/granting out of.
	 */
	L4_Word_t parent;

	/* this field is OR'd with the page table queries to give the access bits
	 * returned by Unmap. it's written to when a parent recursively
	 * queries-and-resets the page table's access bits, and when a child
	 * resets its own access bits. it's cleared by Unmap.
	 *
	 * NOTE: there's some loss of precision where a parent entry is larger
	 * than the child; i.e. some physical pages' entries end up reporting for
	 * a larger range than their size.
	 */
	uint16_t access;
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
	L4_Word_t start;			/* virtual address */
	uint16_t num_entries;
	uint16_t num_alloc;			/* always a power of two, or 0; never 1 */
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
};


extern void init_mapdb(void);

/* returns 0 on success, or -ENOMEM */
extern int mapdb_init(struct map_db *ptr);
extern void mapdb_destroy(struct map_db *ptr);

/* on success, returns OR mask of rights that would've been granted by this
 * mapping operation (which doesn't happen in the rights extension case, but
 * is ignored). special ranges may not be mapped from, or over, using this
 * function; instead, such maps are ignored and the return value set to 0.
 *
 * on failure, returns negative errno.
 */
extern int mapdb_map_pages(
	struct map_db *from,
	struct map_db *to,
	L4_Fpage_t src_page,
	L4_Word_t dest_addr);

/* revokes access rights for bits given and in the mappings covered by @fpage.
 * if a covered mapping ends up with no rights at all, it is removed and its
 * children are moved to its parent (if any remain after recursion).
 *
 * if @immediate is true, the mappings in @db covered by @fpage will be
 * affected. thus pairs of (@immediate, @recursive) correspond to L4.X2 unmap
 * operations like this:
 *   (true, false) = grant
 *   (true, true) = flush
 *   (false, true) = unmap
 *   (false, false) = one-level status read (not used)
 *
 * returns the inclusive mask of access bits of the physical pages actually
 * covered by @fpage since the last call to mapdb_unmap_fpage(). if @recursive
 * is set, this also covers child mappings' access bits. destructive
 * examination of page tables is compensated by storing access bits in struct
 * map_entry as appropriate to recursion. (see comment above the field
 * declaration.)
 *
 * special mappings (KIP, UTCB pages) aren't affected by mapdb_unmap_fpage()
 * in its regular forms. however, when (@fpage.address & 0xc00) == 0x800 &&
 * !@recursive && @fpage.rights == full, special mappings are removed if they
 * overlap at all with @fpage. regardless of this setting, the access bits of
 * special mappings don't contribute to the return value. (TODO: this needs
 * more testing.) when the special form isn't used, the denormal bits in
 * @fpage.address must be zero.
 *
 * if @clear_stored_access is set, the stored access bits retrieved for @fpage
 * will be cleared in the immediate entry. for recursion it is always passed
 * as false.
 */
extern int mapdb_unmap_fpage(
	struct map_db *db,
	L4_Fpage_t fpage,
	bool immediate,
	bool recursive,
	bool clear_stored_access);

static inline void mapdb_erase_special(struct map_db *db, L4_Fpage_t fpage) {
	fpage = L4_FpageLog2(L4_Address(fpage) | 0x800, L4_SizeLog2(fpage));
	L4_Set_Rights(&fpage, L4_FullyAccessible);
	mapdb_unmap_fpage(db, fpage, true, false, false);
}


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

/* writes 1 << PT_UPPER_WIDTH entries into the page table using pt_*()
 * primitives. sets @force to true, but doesn't allocate memory if no entries
 * are found for the range.
 */
extern int mapdb_fill_page_table(struct map_db *db, uintptr_t addr);


/* NOTE: this doesn't communicate whether the affected address range should be
 * flushed or not. a simple 0/1 boolean thing would suffice.
 */
extern int mapdb_add_map(
	struct map_db *db,
	L4_Word_t parent,		/* as in struct map_entry */
	L4_Fpage_t fpage,
	uint32_t first_page_id);


/* kernel-mode initialization */
extern void mapdb_init_range(
	struct map_db *ptr,
	uintptr_t start_addr,
	const uint32_t *page_ids,
	unsigned int num_pages,
	int entry_flags);		/* L4 Fpage access bits */


#endif
