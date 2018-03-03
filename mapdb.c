
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <errno.h>

#include <ccan/htable/htable.h>
#include <ccan/likely/likely.h>
#include <ccan/minmax/minmax.h>

#include <l4/types.h>

#include <ukernel/bug.h>
#include <ukernel/misc.h>
#include <ukernel/util.h>
#include <ukernel/trace.h>
#include <ukernel/space.h>
#include <ukernel/mapdb.h>
#include <ukernel/ptab.h>


#define TRACE(fmt, ...) TRACE_MSG(TRID_MAPDB, fmt, ##__VA_ARGS__)


/* mapdb's child/parent reference format is made out of four types of entry in
 * a 32-bit unsigned integer. the lowest bits indicate entry type:
 *   - ....1 = reference
 *   - ..000 = null (other bits zero)
 *   - ..010 = tombstone (other bits zero, up=1)
 *   - ..100 = no-parent or root mapping indicator
 *   - ..110 = child bucket pointer (index set, up=6)
 *
 * empties and tombstones only appear in the child array as part of its
 * hashing mechanism.
 *
 * references appear in child and parent tables. its subfields are:
 *   - bits 31..11: 21 bits of group ID.
 *   - bits 10..1: 10 bits of index into group.
 *
 * no-parent indicators appear only in the parent table. its subfields are:
 *   - bits 10..8: L4.X2 rights mask
 *   - bits 13..11: "up" access accumulator
 *   - bit 14 is exempt flag
 *   - the rest are zero.
 *
 * child bucket pointers appear in the child table. its value, shifted to the
 * right by 3 bits, identifies a <struct child_bucket> within child_bucket_ra.
 * a child bucket pointer's corresponding c_aux[] slot contains the parent
 * index and a zero rights mask; its "up" accumulator is ignored.
 */
/* constants */
#define REF_NULL 0
#define REF_TOMBSTONE 2
#define REF_NO_PARENT 4
#define REF_BUCKET 6
#define REF_TYPE_MASK 7
#define REF_EXEMPT_BIT 0x4000
/* subfield ctors */
#define REF(ix, grp) (1 | (ix) << 1 | (grp) << 11)
#define REF_ROOT(rights) (REF_NO_PARENT | (rights) << 8)
/* subfield access */
#define REF_INDEX(ref) (((ref) >> 1) & 0x3ff)
#define REF_GROUP_ID(ref) (((ref) >> 11) & 0x1fffff)
#define REF_ADDR(ref) (REF_INDEX((ref)) * PAGE_SIZE)
#define REF_ROOT_RIGHTS(x) (((x) >> 8) & 0x7)
#define REF_ROOT_UP(x) (((x) >> 11) & 0x7)
/* format predicates */
#define IS_REF(x) ((x) & 1)
#define IS_NULL(x) ((x) == 0)
#define IS_ROOT(x) (((x) & REF_TYPE_MASK) == REF_NO_PARENT)
#define IS_EXEMPT(x) (IS_ROOT(x) && ((x) & REF_EXEMPT_BIT))
#define IS_BUCKET(x) (((x) & REF_TYPE_MASK) == REF_BUCKET)

/* the c_aux[] format. */
#define AUX(index, rights, up) (((index) & 0x3ff) \
	| ((rights) & 0x7) << 10 | ((up) & 0x7) << 13)
#define AUX_INDEX(x) ((x) & 0x3ff)
#define AUX_RIGHTS(x) (((x) >> 10) & 0x7)
#define AUX_UP(x) (((x) >> 13) & 0x7)
#define AUX_IS_AVAIL(x) (AUX_UP((x)) == 0)
/* ctors */
#define AUX_TOMBSTONE AUX(0, 0, 1)
#define AUX_BUCKETPTR(ix) AUX((ix), 0, 6)

/* maximum probe depth in childref storage. */
#define MAX_PROBE_DEPTH 15

/* maximum # of items sharing the same parent index in childref storage
 * before being placed in bucket.
 */
#define MAX_SHARE_CHAIN 3

/* mode bits for unmap_page() on top of the ones for mapdb_unmap(). */
#define UM_RIGHTS 0x70	/* L4_Rights() in mask */
#define UM_CHILD 0x80	/* not a primary Unmap target */


/* usage:
 *
 * struct ref_iter it;
 * for_each_child(&it, grp, parent_ix, c, aux) {
 *     if(predicate(c, aux)) ref_del(&it, grp);
 * }
 */
#define for_each_child(it, grp, p_ix, c, aux) \
	for(uint32_t aux, c = ref_first(&(aux), (it), (grp), (p_ix)); \
		(c) != 0; \
		(c) = ref_next(&(aux), (it), (grp)))


struct ref_iter {
	int ix, pos, lim, mask;

	uint32_t *b_cs;
	uint16_t *b_aux;
	int bpos, blim;
};


/* ->cs and ->aux are a childref array just like ->children and ->c_aux in
 * map_group, having allocated space for 1 << ->alloc_log2 entries. ->max is
 * one past the last valid index, i.e. 0 for empty. there are no holes;
 * ref_del() moves the last entry back.
 */
struct child_bucket {
	int alloc_log2, max;
	uint32_t *cs;
	uint16_t *aux;	/* alloc'd in same chunk w/ ->cs */
};


struct rangealloc *map_group_ra = NULL;
static struct rangealloc *child_bucket_ra = NULL;


/* tests a known-valid pointer for whether the group is also valid. this is
 * ensured by the group ctor and dtor.
 */
static inline bool is_group_valid(struct map_group *grp) {
	return grp->space != NULL;
}


/* never fails. */
static struct map_group *group_from_id(long id)
{
	assert(id > 0);

#ifndef NDEBUG
	if(unlikely(catch_pf() != 0)) {
		BUG_ON(true, "lookup of map_group id=%ld faulted!", id);
	}
#endif
	/* TODO: use constant shifts instead as in find_group() */
	struct map_group *g = ra_id2ptr(map_group_ra, id);
	BUG_ON(!is_group_valid(g), "group id=%ld isn't alive!", id);
#ifndef NDEBUG
	uncatch_pf();
#endif

	return g;
}


static inline void ref_set_aux(
	struct ref_iter *it, struct map_group *g, int newval)
{
	assert(newval != 0);	/* use ref_del() instead */

	if(it->b_aux != NULL) {
		it->b_aux[it->bpos] = newval;
	} else {
		g->c_aux[it->pos & it->mask] = newval;
	}
}


static void ref_enter_bucket(
	struct ref_iter *it, struct map_group *g, uint32_t bptr)
{
	assert(IS_BUCKET(bptr));

	struct child_bucket *b = ra_id2ptr(child_bucket_ra, bptr >> 3);
	assert(b->cs != NULL);
	assert(b->aux == (uint16_t *)&b->cs[1 << b->alloc_log2]);
	it->b_cs = b->cs;
	it->b_aux = b->aux;
	it->bpos = -1;
	it->blim = b->max + 1;
}


static uint32_t ref_next(
	uint32_t *aux_p,
	struct ref_iter *it, struct map_group *g)
{
	if(it->b_cs != NULL) {
bucketmode:
		if(++it->bpos < it->blim) {
			*aux_p = it->b_aux[it->bpos];
			return it->b_cs[it->bpos];
		} else {
			/* exit the bucket. */
			it->b_cs = NULL;
			it->b_aux = NULL;
		}
	}

	while(it->pos + 1 < it->lim) {
		int slot = ++it->pos & it->mask;
		uint32_t c = g->children[slot];
		int aux = g->c_aux[slot];
		if(IS_REF(c) && AUX_INDEX(aux) == it->ix) {
			*aux_p = aux;
			return c;
		}
		if(c == 0) break;
		if(IS_BUCKET(c)) {
			ref_enter_bucket(it, g, c);
			assert(it->b_cs != NULL);
			goto bucketmode;
		}
		assert(c == REF_TOMBSTONE
			|| (IS_REF(c) && AUX_INDEX(aux) != it->ix));
	}
	return 0;
}


/* NOTE: @it only becomes valid if the return value is not 0. */
static uint32_t ref_first(
	uint32_t *aux_p,
	struct ref_iter *it, struct map_group *g, int ix)
{
	if(MG_N_ALLOC_LOG2(g) == 0) return 0;

	it->ix = ix;
	it->mask = (1 << MG_N_ALLOC_LOG2(g)) - 1;
	it->pos = int_hash(ix) & it->mask;
	it->lim = it->pos + min(1 << MG_N_ALLOC_LOG2(g), MAX_PROBE_DEPTH);

	uint32_t c = g->children[it->pos];
	if(IS_BUCKET(c)) ref_enter_bucket(it, g, c);
	else {
		it->b_cs = NULL;
		it->b_aux = NULL;
		if(IS_REF(c) && AUX_INDEX(*aux_p = g->c_aux[it->pos]) == ix) return c;
	}
	return ref_next(aux_p, it, g);
}


/* set tombstone, clear c_aux. */
static void ref_del(struct ref_iter *it, struct map_group *g)
{
	if(it->b_cs != NULL) {
		int slot = it->pos & it->mask;
		uint32_t c = g->children[slot];
		assert(IS_BUCKET(c));
		struct child_bucket *b = ra_id2ptr(child_bucket_ra, c >> 3);
		assert(b->cs != NULL);
		assert(b->aux == (uint16_t *)&b->cs[1 << b->alloc_log2]);
		assert(b->max > 0);
		it->b_cs[it->bpos] = it->b_cs[b->max];
		it->b_aux[it->bpos] = it->b_aux[b->max];
		if(--b->max == 0) {
			/* dispose of the bucket. */
			g->children[slot] = b->cs[0];
			g->c_aux[slot] = b->aux[0];
			free(b->cs);
			ra_free(child_bucket_ra, b);
			it->b_cs = NULL; it->b_aux = NULL;
		} else {
			/* visit the item that hopped back, or terminate normally. */
			it->bpos--;
			it->blim--;
		}
	} else {
		int ix = it->pos & it->mask;
		g->children[ix] = REF_TOMBSTONE;
		g->c_aux[ix] = AUX_TOMBSTONE;
	}
}


/* find the parent group and child array index matching @parent_ref and
 * @child_ref. when found, returns the group and  leaves *@it at the child
 * reference; when not found, returns NULL.
 *
 * usage: @parent_ref is g->parent[ix]; @child_ref is REF(ix, g_id).
 */
static inline struct map_group *find_child(
	struct ref_iter *it, int *aux_p,
	uint32_t parent_ref, uint32_t child_ref)
{
	assert(IS_REF(parent_ref));
	assert(IS_REF(child_ref));

	struct map_group *pg = group_from_id(REF_GROUP_ID(parent_ref));
	for_each_child(it, pg, REF_INDEX(parent_ref), c, aux) {
		if(c == child_ref) {
			*aux_p = aux;
			return pg;
		}
	}

	return NULL;
}


/* find_child(), but never returns NULL. */
static struct map_group *get_child(
	struct ref_iter *it, int *aux_p,
	uint32_t p, uint32_t c)
{
	struct map_group *g = find_child(it, aux_p, p, c);
#ifndef NDEBUG
	BUG_ON(g == NULL, "%s: called from %p; cref={gid=%#x,ix=%#x}", __func__,
		__builtin_return_address(0), REF_GROUP_ID(c), REF_INDEX(c));
#endif
	return g;
}


#if 0
static void dump_map_group(struct map_group *g)
{
#ifndef NDEBUG
	L4_ThreadId_t space_id = space_name(g->space);
	TRACE("%s: group %#lx..%#lx in %lu:%lu (#children<=%lu, #maps=%u):\n",
		__func__, MG_START(g), MG_START(g) + GROUP_SIZE - 1,
		L4_ThreadNo(space_id), L4_Version(space_id),
		1ul << MG_N_ALLOC_LOG2(g), MG_N_MAPS(g));
	for(int i=0; i < PAGES_PER_GROUP; i++) {
		uint32_t pgid = 0;
		int rights = mapdb_query(&pgid, g->space, NULL,
			MG_START(g) + i * PAGE_SIZE);
		if(rights == 0) continue;
		TRACE("  %d: %#lx:%#lx rights=[%c%c%c] parent=%#x pgid=%u\n", i,
			(L4_Word_t)MG_START(g) + i * PAGE_SIZE, (L4_Word_t)PAGE_SIZE,
			CHECK_FLAG(rights, L4_Readable) ? 'r' : '-',
			CHECK_FLAG(rights, L4_Writable) ? 'w' : '-',
			CHECK_FLAG(rights, L4_eXecutable) ? 'x' : '-',
			g->parent[i], pgid);
	}
#endif
}
#endif


#ifndef DEBUG_ME_HARDER
#define check_map_group(g) true
#else
#include <ukernel/invariant.h>

static bool check_map_group(struct map_group *g)
{
	INV_CTX;
	inv_push("g=%p (id=%u): start=%#lx, n_alloc=%d, n_maps=%lu",
		g, ra_ptr2id(map_group_ra, g),
		MG_START(g), 1 << MG_N_ALLOC_LOG2(g), MG_N_MAPS(g));
	int map_count = 0;
	for(int i=0; i < PAGES_PER_GROUP; i++) {
		uint32_t p = g->parent[i];
		inv_push("g->parent[%d]=%#x (ix=%u group_id=%u)",
			i, p, REF_INDEX(p), REF_GROUP_ID(p));
		if(p != 0) map_count++;
		inv_imply1(!IS_NULL(p), IS_ROOT(p) || IS_REF(p));
		/* (the reason behind the second clause is that when unmap_page() is
		 * about to destroy a map_group after removing its last root page,
		 * REF_ROOT_RIGHTS() for that entry won't satisfy this condition.
		 * rather than remove the call to check_map_group(), we'll weaken the
		 * condition.)
		 */
		inv_imply1(IS_ROOT(p),
			REF_ROOT_RIGHTS(p) > 0 || MG_N_MAPS(g) == 1);

		if(IS_REF(p)) {
			if(unlikely(catch_pf() != 0)) {
				inv_ok(false, "caught pagefault on parent group");
			}
			struct map_group *pg = ra_id2ptr(map_group_ra, REF_GROUP_ID(p));
			inv_ok1(is_group_valid(pg));
			uint32_t pp = pg->parent[REF_INDEX(p)];
			uncatch_pf();
			inv_ok1(IS_ROOT(pp) || IS_REF(pp));
			struct ref_iter it;
			int aux;
			pg = find_child(&it, &aux, p, REF(i, ra_ptr2id(map_group_ra, g)));
			inv_ok1(pg != NULL);
			inv_ok1(AUX_RIGHTS(aux) > 0);
			inv_ok1(AUX_INDEX(aux) == REF_INDEX(p));
		}

		/* TODO: check child references etc. */
		inv_pop();
	}
	inv_log("map_count=%d", map_count);
	inv_ok1(map_count == MG_N_MAPS(g));

	inv_pop();
	return true;

inv_fail:
	return false;
}

#endif


static void destroy_group(struct map_group *g)
{
	htable_del(&g->space->ptab_groups, int_hash(MG_START(g)), g);
	free(g->parent);
	free(g->children);	/* also releases g->c_aux */
	g->children = NULL; g->c_aux = NULL; g->parent = NULL;
	x86_free_ptab(g);
	g->space = NULL;
	assert(!is_group_valid(g));
	ra_free(map_group_ra, g);
}


static struct map_group *add_group(struct space *sp, uintptr_t addr)
{
	struct map_group *g = ra_alloc(map_group_ra, -1);
	if(unlikely(g == NULL)) return NULL;
	assert(!is_group_valid(g));
	*g = (struct map_group){
		.space = sp, .addr = addr & ~GROUP_MASK,
		.parent = calloc(GROUP_SIZE / PAGE_SIZE, sizeof(uint32_t)),
	};
	TRACE("%s: sp=%p, g=%p (id=%u), ->addr=%#lx\n", __func__, sp, g,
		ra_ptr2id(map_group_ra, g), g->addr);
	int n = x86_alloc_ptab(g);
	bool ok = htable_add(&sp->ptab_groups, int_hash(MG_START(g)), g);
	if(g->parent == NULL || n < 0 || !ok) {
		destroy_group(g);
		return NULL;
	}

	return g;
}


static inline bool _cmp_group_addr(const void *cand, void *keyptr) {
	const struct map_group *g = cand;
	return MG_START(g) == *(uintptr_t *)keyptr;
}


struct map_group *find_group(struct space *sp, uintptr_t addr) {
	uintptr_t key = addr & ~GROUP_MASK;
	return htable_get(&sp->ptab_groups, int_hash(key), &_cmp_group_addr, &key);
}


/* turn @addr within @g into a group-id + index pair. */
static inline L4_Word_t addr_to_ref(struct map_group *g, uintptr_t addr)
{
	assert(addr >= MG_START(g) && addr < MG_START(g) + GROUP_SIZE);
	assert(map_group_ra->id_shift == 5);
	uintptr_t grp_bits = ((uintptr_t)g & map_group_ra->and_mask) << 6,
		ix = (addr - MG_START(g)) >> (PAGE_BITS - 1);

	L4_Word_t ref = grp_bits | ix;
	assert(REF_ADDR(ref) + MG_START(g) == (addr & ~PAGE_MASK));
	assert(group_from_id(REF_GROUP_ID(ref)) == g);
	return ref;
}


static size_t rehash_map_group(const void *ptr, void *priv)
{
	const struct map_group *g = ptr;
	return int_hash(MG_START(g));
}


/* find an empty or bucket slot within c_aux[], determined by that entry
 * granting no rights. return -1 if chain length was exceeded.
 *
 * if @shares != NULL, fill in *@n_shares_p and @shares[0..*@n_shares_p-1]
 * with slot offsets referencing the same parent index through the entire hash
 * chain for @parent_ix.
 */
static int probe_add(
	const uint16_t *c_aux, size_t sz, int parent_ix,
	int *shares, int *n_shares_p)
{
	assert(shares == NULL || *n_shares_p == 0);
	size_t mask = sz - 1, hash = int_hash(parent_ix);
	int pos = hash & mask, first = -1;
	for(int end = pos + min_t(int, sz, MAX_PROBE_DEPTH); pos < end; pos++) {
		int slot = pos & mask,
			rights = AUX_RIGHTS(c_aux[slot]);
		if(rights == 0) {
			if(shares == NULL) return slot;
			if(first < 0) first = slot;
			if(AUX_IS_AVAIL(c_aux[slot])) return first;	/* end of chain */
		} else if(shares != NULL && AUX_INDEX(c_aux[slot]) == parent_ix) {
			assert(*n_shares_p < MAX_SHARE_CHAIN);
			shares[(*n_shares_p)++] = slot;
		}
	}
	TRACE("%s: max depth hit w/ sz=%u, hash=%#x\n", __func__,
		(unsigned)sz, (unsigned)hash);
	return -1;
}


/* rehash existing children into a larger table. returns false on ENOMEM, true
 * otherwise.
 */
static bool grow_children_array(struct map_group *g)
{
	unsigned new_scale, old_size;
	if(MG_N_ALLOC_LOG2(g) == 0) {
		/* allocate a cache line and a half iff @g has any maps. */
		new_scale = MG_N_MAPS(g) > PAGES_PER_GROUP / 33 ? 4 : 1;
		old_size = 0;
	} else {
		new_scale = MG_N_ALLOC_LOG2(g) + 1;
		old_size = 1u << MG_N_ALLOC_LOG2(g);
	}
	unsigned new_size = 1u << new_scale;

	uint32_t *new_children = calloc(new_size,
		sizeof(uint32_t) + sizeof(uint16_t));
	if(new_children == NULL) return false;
	uint16_t *new_c_aux = (uint16_t *)&new_children[new_size];

	for(int i=0; i < old_size; i++) {
		if(!IS_REF(g->children[i]) && !IS_BUCKET(g->children[i])) continue;
		int slot = probe_add(new_c_aux, new_size,
			AUX_INDEX(g->c_aux[i]), NULL, NULL);
		/* NOTE: if the hashing gets randomized at some point, and the random
		 * seed is recomputed for every table, it might be that insert will
		 * fail with an overlong hash chain. for now this'll never happen
		 * because the hash values used for both tables are the same, so
		 * chains will at most become shorter.
		 */
		BUG_ON(slot < 0, "hash chain overflow in grow_children_array()");

		assert(!IS_BUCKET(new_children[slot]));
		new_c_aux[slot] = g->c_aux[i];
		new_children[slot] = g->children[i];
	}

	free(g->children);
	g->children = new_children;
	g->c_aux = new_c_aux;
	g->addr = (g->addr & ~(0x1f << 10)) | (new_scale << 10);
	assert(MG_N_ALLOC_LOG2(g) == new_scale);

	return true;
}


static struct child_bucket *make_bucket(
	struct map_group *g, int *shares, int n_shares,
	uint32_t childref, int aux)
{
	struct child_bucket *b = ra_alloc(child_bucket_ra, -1);
	BUG_ON(b == NULL, "out of child buckets!");	/* TODO: share RA space */
	b->alloc_log2 = size_to_shift(n_shares + 1);
	int sz = 1 << b->alloc_log2;
	assert(sz >= n_shares + 1);
	b->cs = calloc(sz, sizeof(uint32_t) + sizeof(uint16_t));
	BUG_ON(b->cs == NULL, "out of memory!");
	b->aux = (uint16_t *)&b->cs[sz];
	for(int i=0; i < n_shares; i++) {
		int slot = shares[i];
		b->cs[i] = g->children[slot];
		b->aux[i] = g->c_aux[slot];
		g->children[slot] = REF_TOMBSTONE;
		g->c_aux[slot] = AUX_TOMBSTONE;
	}
	b->cs[n_shares] = childref;
	b->aux[n_shares] = aux;
	b->max = n_shares;
	assert(b->max > 0);
	return b;
}


static bool add_to_bucket(struct child_bucket *b, uint32_t cref, int aux)
{
	int sz = 1 << b->alloc_log2;
	if(unlikely(b->max + 1 == sz)) {
		uint32_t *cs = realloc(b->cs,
			sz * 2 * (sizeof(uint32_t) + sizeof(uint16_t)));
		if(cs == NULL) return false;
		uint16_t *old_aux = (uint16_t *)&cs[sz],
			*new_aux = (uint16_t *)&cs[sz * 2];
		memcpy(new_aux, old_aux, sizeof(uint16_t) * sz);
		memset(&new_aux[sz], 0, sizeof(uint16_t) * sz);
		memset(&cs[sz], 0, sizeof(uint32_t) * sz);
		b->cs = cs;
		b->aux = new_aux;
		b->alloc_log2++;
	}

	int ix = ++b->max;
	b->cs[ix] = cref;
	b->aux[ix] = aux;

	return true;
}


static bool add_child(
	struct map_group *g, uint32_t childref,
	int parent_ix, int rights)
{
	assert((rights & L4_FullyAccessible) == rights);
	assert(rights > 0);
	assert(parent_ix >= 0 && parent_ix < PAGES_PER_GROUP);
	assert(IS_REF(childref));

	if(unlikely(g->children == NULL) && !grow_children_array(g)) {
		return false;
	}

	int slot, aux = AUX(parent_ix, rights, 0);
	do {
		size_t sz = 1u << MG_N_ALLOC_LOG2(g);
		int shares[MAX_SHARE_CHAIN], n_shares = 0;
		slot = probe_add(g->c_aux, sz, parent_ix, shares, &n_shares);
		if(n_shares == MAX_SHARE_CHAIN) {
			struct child_bucket *b = make_bucket(g, shares, n_shares,
				childref, aux);
			slot = shares[0];
			g->children[slot] = ra_ptr2id(child_bucket_ra, b) << 3 | REF_BUCKET;
			g->c_aux[slot] = AUX(parent_ix, 0, 7);
		} else if(slot >= 0 && IS_BUCKET(g->children[slot])) {
			struct child_bucket *b = ra_id2ptr(child_bucket_ra,
				g->children[slot] >> 3);
			if(!add_to_bucket(b, childref, aux)) return false;
		} else if(slot >= 0) {
			g->children[slot] = childref;
			g->c_aux[slot] = AUX(parent_ix, rights, 0);
			assert(probe_add(g->c_aux, sz, parent_ix, NULL, NULL) != slot);
		} else {
			if(!grow_children_array(g)) return false;
		}
	} while(slot < 0);

	return true;
}


/* moves children of @g:@ix to be children of @pg, updating their parent
 * pointers. if @pg is not NULL, children will be assigned to the map
 * indicated by @g->parent[@ix]; if @pg is NULL, children will be moved to
 * root.
 *
 * returns 0 on success, or -ENOMEM on failure, leaving a partially-completed
 * state behind. calling the function again with the exact same parameters
 * will resume the operation and complete into the same final state.
 */
static int reparent_children(
	struct map_group *g, int ix, struct map_group *pg)
{
	assert((pg == NULL) == IS_ROOT(g->parent[ix]));
	if(MG_N_ALLOC_LOG2(g) == 0) return 0;

	TRACE("%s: g=%p, ix=%#x, pg=%p\n", __func__, g, ix, pg);

	/* loop over the children of @g:@ix. */
	int parent_ix = REF_INDEX(g->parent[ix]);
	struct ref_iter it;
	for_each_child(&it, g, ix, c, aux) {
		int rights_up = (aux >> 10) & 0x3f;
		ref_del(&it, g);
		struct map_group *cg = group_from_id(REF_GROUP_ID(c));
		if(pg == NULL) {
			cg->parent[REF_INDEX(c)] = REF_ROOT(rights_up);
			TRACE("%s: made cg=%p,ix=%#x a root map\n", __func__,
				cg, REF_INDEX(c));
		} else {
			TRACE("%s: adding child c=%#x to pg=%p,ix=%#x\n", __func__,
				c, pg, parent_ix);
			if(!add_child(pg, c, parent_ix, rights_up & 0x7)) return -ENOMEM;
			cg->parent[REF_INDEX(c)] = REF(parent_ix,
				ra_ptr2id(map_group_ra, pg));
		}
	}
	TRACE("%s: done!\n", __func__);

	return 0;
}


/* returns negative errno on failure. */
int mapdb_put(
	struct space *sp, L4_Fpage_t fpage, uint32_t first_page_id,
	bool is_exempt)
{
	assert(L4_Size(fpage) <= GROUP_SIZE);

	TRACE("%s: sp=%p, fpage=%#lx:%#lx, access=%c%c%c, fpi=%u, is_exempt=%s\n",
		__func__, sp, L4_Address(fpage), L4_Size(fpage),
		CHECK_FLAG(L4_Rights(fpage), L4_Readable) ? 'r' : '-',
		CHECK_FLAG(L4_Rights(fpage), L4_Writable) ? 'w' : '-',
		CHECK_FLAG(L4_Rights(fpage), L4_eXecutable) ? 'x' : '-',
		first_page_id, btos(is_exempt));

	/* mapdb_put() will only create unparented maps in sigma0_space, in any
	 * space while sigma0_space hasn't yet been defined, in @sp's UTCB area,
	 * and in @sp's KIP area. other causes of such entries are nonrecursive
	 * flushes: grants and mappings on top of.
	 */
	assert(fpage_overlap(fpage, sp->utcb_area)
		|| fpage_overlap(fpage, sp->kip_area)
		|| sigma0_space == NULL || sp == sigma0_space);

	bool g_is_new = false;
	struct map_group *g = find_group(sp, L4_Address(fpage));
	if(g == NULL) {
		g = add_group(sp, L4_Address(fpage));
		if(g == NULL) return -ENOMEM;
		g_is_new = true;
	}
	assert(g_is_new || check_map_group(g));

	struct pt_iter it;
	pt_iter_init_group(&it, g);
	int first_ix = (L4_Address(fpage) - MG_START(g)) / PAGE_SIZE;
	uint32_t parent = REF_ROOT(L4_Rights(fpage))
		| (is_exempt ? REF_EXEMPT_BIT : 0);
	for(int i=0; i < L4_Size(fpage) / PAGE_SIZE; i++) {
		if(g->parent[first_ix + i] == 0) {
			assert(MG_N_MAPS(g) < PAGES_PER_GROUP);
			if(!g_is_new) g->addr++; else g_is_new = false;
		}
		L4_Word_t addr = L4_Address(fpage) + i * PAGE_SIZE;
		pt_set_page(&it, addr, first_page_id + i, L4_Rights(fpage));
		g->parent[first_ix + i] = parent;
	}
	pt_iter_destroy(&it);
	assert(check_map_group(g));

	return 0;
}


/* TODO: should catch and return -ENOMEM from add_child() etc. */
int mapdb_map(
	struct space *from_space, L4_Fpage_t map_page,
	struct space *to_space, L4_Word_t dest_addr)
{
	assert((dest_addr & (L4_Size(map_page) - 1)) == 0);

	int given = 0;

	/* well this is a bit vile: the map_page > group_size case. recursion
	 * recurs, baby.
	 */
	int n_groups = L4_Size(map_page) / GROUP_SIZE;
	if(unlikely(n_groups > 1)) {
		for(int i=0; i < n_groups; i++) {
			L4_Fpage_t fp = L4_Fpage(
				L4_Address(map_page) + i * GROUP_SIZE,
				GROUP_SIZE);
			L4_Set_Rights(&fp, L4_Rights(map_page));
			int n = mapdb_map(from_space, fp, to_space,
				dest_addr + i * GROUP_SIZE);
			if(unlikely(n < 0)) return n;
			given |= n;
		}
		return given;
	}

	TRACE("%s: from_space=%p, map_page=%#lx:%#lx, to_space=%p, dest_addr=%#lx\n",
		__func__, from_space, L4_Address(map_page), L4_Size(map_page),
		to_space, dest_addr);
	/* smaller cases affecting a single group in @from_space. */
	L4_Word_t addr = L4_Address(map_page);
	int n_pages = L4_Size(map_page) / PAGE_SIZE;
	struct map_group *g_src = find_group(from_space, addr), *g_dst = NULL;
	struct pt_iter it_dst = { };
	uintptr_t g_dst_end = 0;
	int g_dst_id = 0, g_src_id = ra_ptr2id(map_group_ra, g_src);
	for(int i=0; i < n_pages; i++, addr += PAGE_SIZE, dest_addr += PAGE_SIZE) {
		int from_ix = (addr - MG_START(g_src)) / PAGE_SIZE;
		if(unlikely(IS_EXEMPT(g_src->parent[from_ix]))) continue;

		bool dst_is_new = false;
		if(g_dst_end <= dest_addr) {
			g_dst = find_group(to_space, dest_addr);
			if(g_dst == NULL) {
				g_dst = add_group(to_space, dest_addr);
				if(g_dst == NULL) {
					/* TODO: undo maps up to this point & return -ENOMEM */
					panic("mapdb_map() doesn't handle failing add_group()");
				}
				dst_is_new = true;
			}
			assert(dst_is_new || check_map_group(g_dst));
			g_dst_end = MG_START(g_dst) + GROUP_SIZE;
			g_dst_id = ra_ptr2id(map_group_ra, g_dst);
			if(i > 0) pt_iter_destroy(&it_dst);
			pt_iter_init_group(&it_dst, g_dst);
		}

		uint32_t s_pgid = 0, d_pgid = 0;
		TRACE("%s: mapping g_src=%p,addr=%#lx to g_dst=%p,dest_addr=%#lx\n",
			__func__, g_src, addr, g_dst, dest_addr);
		int to_ix = (dest_addr - MG_START(g_dst)) / PAGE_SIZE;
		if(unlikely(IS_EXEMPT(g_dst->parent[to_ix]))) continue;
		int s_rights = mapdb_query(&s_pgid, from_space, g_src, addr),
			d_rights = mapdb_query(&d_pgid, to_space, g_dst, dest_addr),
			map_rights = s_rights & L4_Rights(map_page);
		if(map_rights == 0 && s_rights != 0) {
			/* no-op: map_page identified no access that should be granted,
			 * but we're not trying to map from a hole in @from_space.
			 */
			continue;
		}
		uint32_t d_parent = g_dst->parent[to_ix];
		if(d_rights > 0 && (s_rights == 0 || s_pgid != d_pgid
			|| IS_ROOT(d_parent)
			|| REF_INDEX(d_parent) != from_ix
			|| REF_GROUP_ID(d_parent) != g_src_id))
		{
			/* remove dest map ahead of overwrite or clear. */
			TRACE("%s: tossing dest at to_ix=%d\n", __func__, to_ix);
			int n = reparent_children(g_dst, to_ix,
				IS_REF(d_parent) ? group_from_id(REF_GROUP_ID(d_parent)) : NULL);
			if(n < 0) {
				/* FIXME: handle it */
				panic("can't handle reparent_children() failure");
			}

			struct ref_iter opg_it;
			int dummy;
			struct map_group *opg = get_child(&opg_it, &dummy,
				d_parent, REF(to_ix, g_dst_id));
			ref_del(&opg_it, opg);

			d_parent = 0;
			d_rights = 0;
		}

		bool kill_dst = false;
		if(s_rights == 0) {
			/* clear. */
			assert(d_parent == 0);
			pt_clear_page(&it_dst, dest_addr);
			if(dst_is_new) {
				/* mapping a blank into a fresh empty group. */
				kill_dst = true;
			} else if(MG_N_MAPS(g_dst) == 1) {
				/* mapping a blank over the last page in g_dst. */
				kill_dst = true;
			} else {
				g_dst->addr--;
				assert(MG_N_MAPS(g_dst) < PAGES_PER_GROUP);
			}
		} else if(s_pgid == d_pgid && (s_rights & ~d_rights) > 0
			&& REF_INDEX(d_parent) == from_ix)
		{
			/* expand. */
			TRACE("%s: expanding rights from %#x to %#x\n", __func__,
				d_rights, d_rights | map_rights);
			struct ref_iter p_it;
			int aux;
			struct map_group *foo = get_child(&p_it, &aux, d_parent,
				REF(to_ix, g_dst_id));
			assert(foo == g_src);
			ref_set_aux(&p_it, g_src, aux | (map_rights & 0x7) << 10);
			pt_set_rights(&it_dst, dest_addr, d_rights | map_rights);
		} else if(d_rights == 0) {
			/* set. */
			TRACE("%s: insert/replace case at sp=%p dest_addr=%#lx\n",
				__func__, to_space, dest_addr);
			assert(s_pgid > 0);		/* good as true */

			bool ok = add_child(g_src, REF(to_ix, g_dst_id),
				from_ix, map_rights);
			if(!ok) {
				/* FIXME */
				panic("can't handle OOM from add_child()");
			}
			pt_set_page(&it_dst, dest_addr, s_pgid, map_rights);
			d_parent = REF(from_ix, g_src_id);
			if(g_dst->parent[to_ix] == 0) {
				assert(MG_N_MAPS(g_dst) < PAGES_PER_GROUP);
				if(!dst_is_new) g_dst->addr++;
			}
		} else {
			/* the goggles, they do nothing */
			TRACE("%s: no-op!\n", __func__);
			assert(d_parent != 0);
		}

		if(kill_dst) {
			g_dst_end = 0;
			pt_iter_destroy(&it_dst);
			destroy_group(g_dst);
		} else {
			g_dst->parent[to_ix] = d_parent;
			assert(check_map_group(g_dst));
		}
	}
	pt_iter_destroy(&it_dst);

	return given;
}


/* helper for deep_call() recursion */
struct unmap_param {
	struct map_group **g_p;
	int ix, mode, retval;
};

static void call_unmap(struct unmap_param *p);


/* the core of mapdb_unmap(). does one page at a time for convenience, letting
 * the upper interface handle group transitions and skip-or-remove exempt
 * maps. note that this function will destroy *@g_p and overwrite the pointer
 * with NULL if *@g_p becomes empty.
 *
 * TODO: could receive a pt_iter over @g since Unmap works first in a single
 * address space; this'd remove some derefs from repeated lookups of page
 * table data. but, benchmark first.
 */
static int unmap_page(struct map_group **g_p, int ix, int mode)
{
	assert(ix >= 0 && ix < PAGES_PER_GROUP);
	assert(*g_p != NULL);

	/* prevent stack-breaking recursion.
	 *
	 * [TODO: for recursion that's both deep and broad, layers where
	 * deep_call() comes into effect in the next call will end up always doing
	 * that, over and over; that'll also enter the heap allocator time and
	 * time again. it could be fixed, but it'll be complicated.]
	 */
	if(!is_stack_safe(0x400)) {
		struct unmap_param p = { .g_p = g_p, .ix = ix, .mode = mode };
		deep_call(&call_unmap, &p);
		return p.retval;
	}

	bool get_access = CHECK_FLAG(mode, UM_GET_ACCESS),
		recursive = CHECK_FLAG(mode, UM_RECURSIVE),
		immediate = CHECK_FLAG(mode, UM_IMMEDIATE);

	int unmap_rights = (mode & UM_RIGHTS) >> 4;
	assert(unmap_rights != L4_NoAccess || get_access);

	struct map_group *g = *g_p;
	TRACE("%s: g=%p (start=%#lx), ix=%#x, mode=%#x\n"
		"    immediate=%s, recursive=%s, get_access=%s\n",
		__func__, g, MG_START(g), ix, mode,
		btos(immediate), btos(recursive), btos(get_access));

	struct pt_iter it;
	pt_iter_init_group(&it, g);
	int rwx_seen = 0;	/* access query accumulator */
	if(g->children == NULL) recursive = false;
	bool destroy_g = false;

	/* dereference children, recur, and remove rights. */
	assert((g->children == NULL) == (MG_N_ALLOC_LOG2(g) == 0));
	int next_mode = mode | UM_IMMEDIATE | UM_CHILD;
	struct ref_iter g_it;
	for(uint32_t aux, c = ref_first(&aux, &g_it, g, ix);
		recursive && c != 0;
		c = ref_next(&aux, &g_it, g))
	{
		int c_rights = AUX_RIGHTS(aux),
			rm_rights = c_rights & unmap_rights;
		TRACE("%s: child in slot=%d: c=%#x, aux=%#x\n", __func__,
			g_it.pos & g_it.mask, c, aux);
		if(!get_access && rm_rights == 0) continue;

		struct map_group *c_grp = group_from_id(REF_GROUP_ID(c));
		int n = unmap_page(&c_grp, REF_INDEX(c), next_mode);
		if(unlikely(n < 0)) {
			assert(n == -ENOMEM);
			rwx_seen = n;
			goto end;
		}
		assert(c_grp != NULL);	/* because of UM_CHILD, it's never removed. */

		assert(n >= 0 && n < 8);
		/* return-and-clear "up", combine with "down". */
		rwx_seen |= n | AUX_UP(aux);
		aux &= ~(0x7 << 13);
		assert(AUX_UP(aux) == 0);

		if(rm_rights != 0) {
			assert(IS_REF(c_grp->parent[REF_INDEX(c)])); /* natch */
			int remain = c_rights & ~rm_rights;
			TRACE("  c_rights=%#x, rm_rights=%#x, remain=%#x\n",
				c_rights, rm_rights, remain);
			if(remain == 0 && MG_N_MAPS(c_grp) == 1) {
				destroy_group(c_grp);
				aux = 0;
			} else {
				struct pt_iter c_it; pt_iter_init_group(&c_it, c_grp);
				uintptr_t addr = MG_START(c_grp) + REF_INDEX(c) * PAGE_SIZE;
				if(remain != 0) {
					pt_set_rights(&c_it, addr, remain);
					aux &= ~(rm_rights << 10);
					assert((AUX_RIGHTS(aux) & rm_rights) == 0);
					assert(AUX_RIGHTS(aux) > 0);
				} else {
					c_grp->parent[REF_INDEX(c)] = 0;
					c_grp->addr--;
					assert(MG_N_MAPS(c_grp) < PAGES_PER_GROUP);
					pt_clear_page(&c_it, addr);
					aux = 0;
				}
				pt_iter_destroy(&c_it);
			}
		}

		if(AUX_RIGHTS(aux) == 0) ref_del(&g_it, g);
		else ref_set_aux(&g_it, g, aux);
	}

	/* process access data for the page at hand. */
	struct ref_iter p_it;
	struct map_group *pg = NULL;
	int p_aux;
	uintptr_t page_addr = MG_START(g) + ix * PAGE_SIZE;
	if(get_access) {
		int my = 0, side = 0;
		pt_probe(&it, &my, &side, page_addr,
			CHECK_FLAG(mode, UM_CHILD), rwx_seen);
		rwx_seen |= my;

		if(!CHECK_FLAG(mode, UM_CHILD) && !IS_ROOT(g->parent[ix])) {
			/* pass immediate and recursive access data up to parent. */
			pg = get_child(&p_it, &p_aux, g->parent[ix],
				REF(ix, ra_ptr2id(map_group_ra, g)));
			p_aux |= rwx_seen << 13;
			ref_set_aux(&p_it, pg, p_aux);
		}

		if(!CHECK_FLAG(mode, UM_CHILD)) {
			/* merge stored access in. */
			rwx_seen |= side;
		}
	}

	/* the flush case. */
	if(!CHECK_FLAG(mode, UM_CHILD) && immediate && unmap_rights > 0) {
		/* find our childptr (where applicable) & remove access. */
		TRACE("%s: flushing unmap_rights=%#x\n", __func__, unmap_rights);
		int remain;
		if(IS_ROOT(g->parent[ix])) {
			/* root maps are very, very common in Unmap. */
			g->parent[ix] &= ~(unmap_rights << 8);
			remain = REF_ROOT_RIGHTS(g->parent[ix]);
			pg = NULL;
		} else {
			if(pg == NULL) {
				pg = get_child(&p_it, &p_aux, g->parent[ix],
					REF(ix, ra_ptr2id(map_group_ra, g)));
			}
			p_aux &= ~(unmap_rights << 10);
			remain = AUX_RIGHTS(p_aux);
			if(remain != 0) ref_set_aux(&p_it, pg, p_aux); else ref_del(&p_it, pg);
		}
		TRACE("%s: after flush, remain=%#x\n", __func__, remain);

		if(remain != 0) pt_set_rights(&it, page_addr, remain);
		else {
			pt_clear_page(&it, page_addr);
			if(!recursive) {
				int n = reparent_children(g, ix, pg);
				if(n < 0) {
					/* FIXME: handle it */
					panic("can't handle ENOMEM from reparent_children()");
				}
			}
			if(MG_N_MAPS(g) == 1) {
				destroy_g = true;
			} else {
				g->parent[ix] = 0;
				g->addr--;
				assert(MG_N_MAPS(g) < PAGES_PER_GROUP);
			}
		}
	}

end:
	pt_iter_destroy(&it);
	if(destroy_g) {
		assert(check_map_group(g));
		destroy_group(g);
		*g_p = NULL;
	}
	return rwx_seen;
}


static void call_unmap(struct unmap_param *p) {
	p->retval = unmap_page(p->g_p, p->ix, p->mode);
}


int mapdb_unmap(struct space *sp, L4_Fpage_t range, unsigned arg_mode)
{
	const bool immediate = CHECK_FLAG(arg_mode, UM_IMMEDIATE),
		recursive = CHECK_FLAG(arg_mode, UM_RECURSIVE),
		get_access = CHECK_FLAG(arg_mode, UM_GET_ACCESS);
	assert(recursive || immediate);	/* disallows the one-level status read */
	TRACE("%s: range %#lx:%#lx, %simmediate, %srecursive, get_access=%s\n",
		__func__, L4_Address(range), L4_Size(range),
		!immediate ? "non-" : "", !recursive ? "non" : "",
		btos(get_access));

	int unmap_rights = L4_Rights(range);
	if(unmap_rights == L4_NoAccess && !get_access) return 0;	/* no-op */

	/* this, unlike the one in mapdb_map(), lets us write the rest for the
	 * easy case where @range lies in a single group.
	 */
	int n_groups = L4_Size(range) / GROUP_SIZE;
	if(unlikely(n_groups > 1)) {
		int acc = 0;
		for(int i=0; i < n_groups; i++) {
			/* (note: this should pass along the exempt range trigger.) */
			L4_Fpage_t fp = L4_Fpage(
				L4_Address(range) + i * GROUP_SIZE, GROUP_SIZE);
			L4_Set_Rights(&fp, L4_Rights(range));
			int n = mapdb_unmap(sp, fp, arg_mode);
			if(unlikely(n < 0)) return n;
			acc |= n;
		}
		return acc;
	}

	struct map_group *g = find_group(sp, L4_Address(range));
	if(g == NULL) return 0;		/* applies to nothing. */

	/* the "remove exempt maps" form */
	const bool drop_exempt = !recursive
		&& L4_Rights(range) == L4_FullyAccessible
		&& (range.raw & 0xc00) == 0x800;
	assert(drop_exempt || (L4_Address(range) & 0xc00) == 0);

	int rwx_seen = 0, mode = (arg_mode & 0x7) | (unmap_rights << 4),
		ix = (L4_Address(range) - MG_START(g)) / PAGE_SIZE,
		n_pages = L4_Size(range) / PAGE_SIZE;
	assert(!CHECK_FLAG(mode, UM_CHILD));
	for(int i=0; i < n_pages; i++, ix++) {
		uint32_t p = g->parent[ix];
		if(likely(IS_REF(p)) || (IS_ROOT(p) && !IS_EXEMPT(p))) {
			int n = unmap_page(&g, ix, mode);
			if(unlikely(n < 0)) return n;
			rwx_seen |= n;
			if(g == NULL) break;
		} else if(IS_ROOT(p) && IS_EXEMPT(p) && drop_exempt) {
			/* TODO: assert that it had no children, either. */
			if(MG_N_MAPS(g) == 1) {
				destroy_group(g);
				g = NULL;
			} else {
				struct pt_iter it; pt_iter_init_group(&it, g);
				pt_clear_page(&it, MG_START(g) + ix * PAGE_SIZE);
				pt_iter_destroy(&it);
				g->parent[ix] = 0;
				g->addr--;
				assert(MG_N_MAPS(g) < PAGES_PER_GROUP);
			}
		}
	}

	assert(g == NULL || check_map_group(g));
	return rwx_seen;
}


int mapdb_query(
	uint32_t *pgid_p,
	struct space *sp, struct map_group *g, uintptr_t address)
{
	assert(g == NULL
		|| (address >= MG_START(g) && address < MG_START(g) + GROUP_SIZE));
	if(g == NULL) {
		g = find_group(sp, address);
		if(unlikely(g == NULL)) return 0;
	}

	if(pgid_p != NULL) {
		/* grab page# under a possible htable_get() cost. */
		struct pt_iter it;
		pt_iter_init_group(&it, g);
		*pgid_p = pt_probe(&it, NULL, NULL, address, false, 0);
		pt_iter_destroy(&it);
	}

	int rights, ix = (address - MG_START(g)) >> PAGE_BITS;
	uint32_t p_ref = g->parent[ix];
	if(IS_ROOT(p_ref)) {
		rights = REF_ROOT_RIGHTS(p_ref);
	} else if(IS_REF(p_ref)) {
		struct ref_iter it;
		int aux;
		get_child(&it, &aux, p_ref, REF(ix, ra_ptr2id(map_group_ra, g)));
		rights = AUX_RIGHTS(aux);
	} else {
		assert(IS_NULL(p_ref));
		rights = 0;
	}

	return rights;
}


void mapdb_init(struct space *sp)
{
	htable_init(&sp->ptab_groups, &rehash_map_group, NULL);
}


void mapdb_destroy(struct space *sp)
{
	struct htable_iter it;
	for(struct map_group *g = htable_first(&sp->ptab_groups, &it);
		g != NULL;
		g = htable_next(&sp->ptab_groups, &it))
	{
		int g_id = ra_ptr2id(map_group_ra, g);

		/* remove existing child pointers and reparent our own maps. */
		for(int i=0; i < PAGES_PER_GROUP; i++) {
			uint32_t p = g->parent[i];
			if(!IS_REF(p) && !IS_ROOT(p)) continue;

			struct map_group *pg;
			if(IS_REF(p)) {
				struct ref_iter it;
				int dummy;
				pg = get_child(&it, &dummy, p, REF(i, g_id));
				ref_del(&it, pg);
			} else {
				assert(IS_ROOT(p));
				pg = NULL;
			}

			int n = reparent_children(g, i, pg);
			if(n < 0) {
				/* FIXME: release memory somewhere else & try again. */
				panic("can't handle ENOMEM at this point!");
			}
		}

		destroy_group(g);
	}
	htable_clear(&sp->ptab_groups);
}


COLD void init_mapdb(void)
{
	assert(map_group_ra == NULL);

	/* group IDs are 21 bits long. this allows for 1 << 21 distinct map_groups
	 * (less a few for special values) for 8 TiB (32-bit paging) or 4 TiB
	 * (PAE) of usermode virtual address space with at least a single page
	 * mapped per two meg. that's enough for over a thousand fully covered
	 * 3.5 GiB address spaces.
	 */
	map_group_ra = RA_NEW(struct map_group, 1 << 21);
	ra_disable_id_0(map_group_ra);

	/* child buckets are used for when there's a large number of child
	 * references from a single page. we'll reserve address space for a
	 * relatively low number of bucket structs because a larger allocation
	 * would sit idle far too much.
	 *
	 * TODO: extend rangealloc to support range sharing, so that buckets could
	 * be allocated in e.g. map_group_ra's range which is similarly
	 * underutilized in typical conditions.
	 */
	child_bucket_ra = RA_NEW(struct child_bucket, 1 << 15);
}
