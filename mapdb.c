
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <assert.h>

#include <ccan/htable/htable.h>
#include <ccan/alignof/alignof.h>
#include <ccan/container_of/container_of.h>

#include <ukernel/misc.h>
#include <ukernel/mapdb.h>


#define GROUP_SIZE (PAGE_SIZE * ENTRIES_PER_GROUP)
#define GROUP_ADDR(addr) ((addr) & ~(GROUP_SIZE - 1))


static size_t rehash_ref_hash(const void *, void *);


static uint32_t next_ref_id = 1;
static struct htable ref_hash = HTABLE_INITIALIZER(ref_hash,
	rehash_ref_hash, NULL);


/* returns true if the entry was cleared. */
static bool flush_entry(struct map_entry *e, int access)
{
	const uintptr_t *children = e->num_children > 1 ? e->children : &e->child;
	for(int j=0; j < e->num_children && REF_DEFINED(children[j]); j++) {
		/* TODO: deref children[j], recur */
	}

	e->flags &= ~access;
	if((e->flags & 0x7) == 0) {
		/* everything was revoked. */
		e->page_id = 0;
		if(e->num_children > 1) free(e->children);
		e->num_children = 0;
		return true;
	} else {
		return false;
	}
}


/* returns true if the group now has 0 pages in it. */
static bool flush_group(struct map_db *db, struct map_group *g, int access)
{
	int active_maps = 0;
	for(int i=0; i < ENTRIES_PER_GROUP; i++) {
		struct map_entry *e = &g->entries[i];
		if(e->page_id == 0) continue;
		if(!flush_entry(e, access)) active_maps++;
	}

	return active_maps == 0;
}


static bool cmp_group_addr(const void *cand, void *keyptr)
{
	uintptr_t key = *(uintptr_t *)keyptr;
	const struct map_group *g = cand;
	return g->start == key;
}


static size_t rehash_map_group(const void *ptr, void *priv)
{
	const struct map_group *g = ptr;
	return int_hash(g->start);
}


static size_t rehash_ref_hash(const void *elem, void *priv) {
	const struct map_db *db = elem;
	return int_hash(db->ref_id);
}


void mapdb_init(struct map_db *ptr, struct space *space)
{
	htable_init(&ptr->groups, &rehash_map_group, NULL);
	ptr->space = space;
	ptr->ref_id = next_ref_id++;
	htable_add(&ref_hash, int_hash(ptr->ref_id), ptr);
}


void mapdb_destroy(struct map_db *db)
{
	htable_del(&ref_hash, int_hash(db->ref_id), db);

	struct htable_iter it;
	for(struct map_group *g = htable_first(&db->groups, &it);
		g != NULL;
		g = htable_next(&db->groups, &it))
	{
		flush_group(db, g, 0x7);
		free(g);
	}

	htable_clear(&db->groups);
	db->space = NULL;
}


static struct map_group *group_for_addr(struct map_db *db, uintptr_t addr)
{
	uintptr_t key = GROUP_ADDR(addr);
	return htable_get(&db->groups, int_hash(key), &cmp_group_addr, &key);
}


const struct map_entry *mapdb_probe(
	struct map_db *db,
	uintptr_t addr)
{
	struct map_group *g = group_for_addr(db, addr);
	if(g == NULL) return NULL;

	int g_offset = (addr - g->start) >> PAGE_BITS;
	assert(g_offset < ENTRIES_PER_GROUP);
	return &g->entries[g_offset];
}


COLD void init_mapdb(void)
{
	/* ... */
}


COLD void mapdb_init_range(
	struct map_db *db,
	uintptr_t start_addr,
	const uint32_t *page_ids,
	unsigned int num_pages,
	int entry_flags)
{
	uintptr_t g_first = GROUP_ADDR(start_addr),
		g_last = GROUP_ADDR(start_addr + num_pages * PAGE_SIZE - 1);

#ifndef NDEBUG
	int done = 0;
#endif
	for(uintptr_t i = g_first; i <= g_last; i += GROUP_SIZE) {
		struct map_group *g = group_for_addr(db, i);
		if(g == NULL) {
			g = calloc(1, sizeof(struct map_group));
			g->start = i;
			htable_add(&db->groups, int_hash(i), g);
		}

		uintptr_t start = MAX(uintptr_t, start_addr, g->start),
			end = MIN(uintptr_t, start_addr + num_pages * PAGE_SIZE - 1,
				g->start + GROUP_SIZE - 1);
		int id_offset = (start - start_addr) >> PAGE_BITS,
			g_offset = (start - g->start) >> PAGE_BITS,
			length = (end - start + 1) >> PAGE_BITS;
		assert(id_offset >= 0);
		assert(id_offset < num_pages);
		assert(id_offset + length <= num_pages);
		assert(g_offset >= 0);
		assert(g_offset < ENTRIES_PER_GROUP);
		assert(g_offset + length <= ENTRIES_PER_GROUP);
		for(int j=0; j < length; j++) {
			struct map_entry *e = &g->entries[j + g_offset];
			assert(e->page_id == 0);
			*e = (struct map_entry){
				.page_id = page_ids[j + id_offset],
				.flags = entry_flags,
			};
#ifndef NDEBUG
			done++;
#endif
		}
	}
	assert(done == num_pages);
}
