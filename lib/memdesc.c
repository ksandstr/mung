
/* implements what's exported in <ukernel/memdesc.h>.
 *
 * the point of all this hideous rigamarole is that MemoryDescs are mostly
 * generated and consumed in early-boot and sigma0/1 environments, which
 * cannot allocate dynamic memory in proportion to the number of gaps in an
 * arbitrarily complex memory map. so this here is a trade-off between
 * execution time and the degree of space and infrastructure required;
 * earlyboot having precious little of the latter but scads of the former.
 */

#include <stdio.h>
#include <string.h>

#include <ukernel/mm.h>
#include <ukernel/util.h>
#include <ukernel/memdesc.h>


#define MDESC_OVERLAP(a, b) \
	RANGE_OVERLAP(L4_MemoryDescLow((a)), L4_MemoryDescHigh((a)), \
		L4_MemoryDescLow((b)), L4_MemoryDescHigh((b)))


/* range given as [start, end] */
static L4_Fpage_t normal_fpage(L4_Word_t start, L4_Word_t end)
{
	const int word_bits = sizeof(L4_Word_t) * 8;
	if(start > end) return L4_Nilpage;
	return L4_FpageLog2(start,
		MIN(int, start == 0 ? word_bits : ffsl(start) - 1,
			MAX(int, 10, MSB(end - start + 1))));
}


static bool is_desc_empty(const L4_MemoryDesc_t *md) {
	return md->x.low == md->x.high;
}


/* ranges given as for normal_fpage().
 *
 * this is inexact by design to let the accumulator only retain a single
 * range. therefore a series of three [s, e] that'd otherwise form a
 * contiguous length may produce 2 distinct results, depending on whether the
 * middle part was seen before the start and end parts.
 */
static void acc_left(
	L4_Word_t *acc_start, L4_Word_t *acc_end,
	L4_Word_t s, L4_Word_t e)
{
	if((*acc_start == 0 && *acc_end == 0) || e < *acc_start) {
		/* empty acc, or other ends before acc; replace. */
		*acc_start = s;
		*acc_end = e;
	} else if(s > *acc_end) {
		/* starts past acc end; skip. */
	} else {
		/* straightforward join */
		*acc_start = MIN(L4_Word_t, *acc_start, s);
		*acc_end = MAX(L4_Word_t, *acc_end, e);
	}
}


L4_Fpage_t mdb_query(
	struct memdescbuf *mdb,
	L4_Word_t q_start, L4_Word_t q_end,		/* [q_start, q_end] */
	bool virtual, bool dedicated, L4_Word_t t)
{
	q_start &= ~0x3ff;
	q_end |= 0x3ff;
	if(q_start > q_end) return L4_Nilpage;

	/* the way this works is, we'll accumulate leftmost positive and negative
	 * subsets of the query range within @mdb.
	 *
	 * then we'll decide that there's a length of positive result in there,
	 * that the function must recur from the end of the negative part, or that
	 * there isn't anything there at all.
	 */
	L4_Word_t pos_s = 0, pos_e = 0, neg_s = 0, neg_e = 0;
	for(int i=0; i < mdb->len; i++) {
		L4_MemoryDesc_t *md = &mdb->ptr[i];
		if(virtual != !!L4_IsMemoryDescVirtual(md)) continue;

		L4_Word_t s = L4_MemoryDescLow(md), e = L4_MemoryDescHigh(md);
		if(e < q_start || s > q_end) continue;	/* out of range */
		s = MAX(L4_Word_t, s, q_start);
		e = MIN(L4_Word_t, e, q_end);

		/* positive condition. */
		L4_Word_t mt = L4_MemoryDescType(md);
		if(mt == t) {
			acc_left(&pos_s, &pos_e, s, e);
		}
		/* negative condition. */
		if((!dedicated && mt == L4_DedicatedMemoryType)
			|| (t != L4_ReservedMemoryType && mt == L4_ReservedMemoryType))
		{
			acc_left(&neg_s, &neg_e, s, e);
		}
	}

	/* interpret the runes */
	if(pos_s == 0 && pos_e == 0) {
		/* no positive section. */
		return L4_Nilpage;
	} else if((neg_s == 0 && neg_e == 0) || neg_s > pos_e) {
		/* no negative section, or it starts after positive ends */
		return normal_fpage(pos_s, pos_e);
	} else if(pos_s < neg_s) {
		/* there's a positive slice that ends at a negative slice. */
		return normal_fpage(pos_s, neg_s - 1);
	} else if(neg_e >= q_end) {
		/* the negative section extends up to or beyond query range. */
		return L4_Nilpage;
	} else {
		/* the leftmost negative precedes the positive, but ends before query.
		 * recur to ensure forward progress despite acc_left()'s noted
		 * weakness.
		 */
		return mdb_query(mdb, neg_e + 1, q_end, virtual, dedicated, t);
	}
}


static int next_conflict(L4_MemoryDesc_t *ptr, int len, L4_MemoryDesc_t desc)
{
	for(int i=0; i < len; i++) {
		if(ptr[i].x.v == desc.x.v
			&& MDESC_OVERLAP(&desc, &ptr[i])
			&& L4_MemoryDescType(&ptr[i]) != L4_MemoryDescType(&desc))
		{
			return i;
		}
	}
	return -1;
}


static int num_conflicts(struct memdescbuf *mdb, L4_MemoryDesc_t desc)
{
	int i = 0, acc = 0;
	while(i < mdb->len) {
		int off = next_conflict(&mdb->ptr[i], mdb->len - i, desc);
		if(off < 0) break; else { acc++; i += off + 1; }
	}
	return acc;
}


/* returns 1 when *@space is altered, 0 when altering *@md was sufficient, and
 * -1 when *@md would be removed because of @newdesc taking priority.
 */
static int resolve_conflict(
	L4_MemoryDesc_t *md,		/* inout */
	L4_MemoryDesc_t *space,		/* out */
	L4_MemoryDesc_t newdesc)
{
	if(!MDESC_OVERLAP(md, &newdesc)) return 0;

	L4_Word_t md_lo = L4_MemoryDescLow(md), md_hi = L4_MemoryDescHigh(md),
		new_lo = L4_MemoryDescLow(&newdesc),
		new_hi = L4_MemoryDescHigh(&newdesc);
	if(md_lo < new_lo && md_hi > new_hi) {
		/* split case. */
		int v = md->x.v, type = md->x.type, t = md->x.t;
		*md = (L4_MemoryDesc_t){
			.x.v = v, .x.type = type, .x.t = t,
			.x.low = md_lo >> 10, .x.high = (new_lo >> 10) - 1,
		};
		*space = (L4_MemoryDesc_t){
			.x.v = v, .x.type = type, .x.t = t,
			.x.low = (new_hi >> 10) + 1, .x.high = md_hi >> 10,
		};
		return 1;
	} else if(md_lo >= new_lo && md_hi <= new_hi) {
		/* subset case */
		return -1;
	} else {
		/* edge cut case */
		*md = (L4_MemoryDesc_t){
			.x.v = md->x.v, .x.type = md->x.type, .x.t = md->x.t,
			.x.low = md->x.high > newdesc.x.high
				? newdesc.x.high + 1 : md->x.low,
			.x.high = md->x.low < newdesc.x.low
				? newdesc.x.low - 1 : md->x.high,
		};
		assert(L4_MemoryDescLow(md) < L4_MemoryDescHigh(md));
		return 0;
	}
}


/* this function does two things: remove or shrink conflicting memorydescs in
 * @mdb, and add the given memorydesc on top or join it with an existing
 * memorydesc. it must be atomic on failure.
 *
 * to do this, we'll first discover how many conflicting memorydescs there
 * are, then compute the number of output memorydescs required to resolve
 * these conflicts, check whether there's enough space in @mdb for the result,
 * and finally either fail or perform the changes.
 *
 * helper functions: next_conflict(), num_conflicts()
 */
bool mdb_set(
	struct memdescbuf *mdb,
	L4_Word_t start, L4_Word_t end,
	bool virtual, L4_Word_t type, L4_Word_t subtype)
{
	L4_MemoryDesc_t newdesc = {
		.x.type = type, .x.t = subtype, .x.v = virtual ? 1 : 0,
		.x.low = start >> 10, .x.high = end >> 10,
	};

	/* allocation stage. */
	int num_cs = num_conflicts(mdb, newdesc);
	/* naïve upper bound: assumes that every conflict will be resolved by a
	 * single split, and that additional memory will be required for @newdesc.
	 */
	if(mdb->len + num_cs + 1 >= mdb->size) {
		/* pretend-resolve conflicts to get the number of descs generated.
		 * this slows mdb_set() right the fuck down as its caller gets close
		 * to maxing the buffer out.
		 */
		int num_new = 1;
		for(int i=0, pos=0; i < num_cs; i++, pos++) {
			int off = next_conflict(&mdb->ptr[pos], mdb->len - pos, newdesc);
			if(off < 0) break;
			pos += off;
			L4_MemoryDesc_t conf = mdb->ptr[pos], tmp;
			num_new += resolve_conflict(&conf, &tmp, newdesc);
		}

		if(mdb->len + num_new >= mdb->size) return false;
	}

	/* conflict-modification stage.
	 * this may leave empties (.x.low == .x.high == 0) in the array.
	 */
	int first_empty = -1, num_dead = 0;
	for(int i=0, pos=0; i < num_cs; i++, pos++) {
		int off = next_conflict(&mdb->ptr[pos], mdb->len - pos, newdesc);
		if(off < 0) break;
		pos += off;
		assert(mdb->len + 1 < mdb->size);
		int n = resolve_conflict(&mdb->ptr[pos], &mdb->ptr[mdb->len],
			newdesc);
		if(n > 0) mdb->len += n;
		else if(n < 0) {
			num_dead++;
			mdb->ptr[pos] = (L4_MemoryDesc_t){ };
			if(first_empty < 0) first_empty = pos;
		}
	}

	/* pre-output stage; checks if newdesc can be merged into an existing
	 * descriptor. if so, extends newdesc accordingly, and recycles the first
	 * such descriptor to store newdesc; or removes descriptors after the
	 * first.
	 */
	int output_pos = -1;
	L4_Word_t full_type = (type & 0xf) | (subtype << 4);
	for(int i=0; i < mdb->len; i++) {
		L4_MemoryDesc_t *m = &mdb->ptr[i];
		if(!!L4_IsMemoryDescVirtual(m) != virtual) continue;
		if(L4_MemoryDescType(m) != full_type) continue;
		bool merged = false;
		if(m->x.low >= newdesc.x.low && m->x.high <= newdesc.x.high) {
			/* existing entry contained in new. */
			merged = true;
		} else {
			/* otherwise, including overlaps. */
			if(BETWEEN(m->x.low, m->x.high, newdesc.x.high + 1)) {
				newdesc.x.high = m->x.high;
				merged = true;
			}
			if(BETWEEN(m->x.low, m->x.high + 1, newdesc.x.low)) {
				newdesc.x.low = m->x.low;
				merged = true;
			}
		}
		if(merged) {
			if(output_pos < 0) {
				output_pos = i;
			} else {
				*m = (L4_MemoryDesc_t){ };
				num_dead++;
				if(first_empty < 0) first_empty = i;
			}
		}
	}

	/* compression stage. */
	if(num_dead > 0) {
		assert(first_empty >= 0);
		for(int i = 0, e = first_empty; i < num_dead; i++) {
			int span = 1;
			while(e + span < mdb->len && is_desc_empty(&mdb->ptr[e + span])) {
				span++;
			}
			if(e + span == mdb->len) {
				/* it's at the end, so we'll stop here. */
				break;
			}
			/* copy other items back. */
			for(int j = e + span; j < mdb->len; j++) {
				mdb->ptr[j - span] = mdb->ptr[j];
			}
			e += span;
			/* find next empty. */
			while(e < mdb->len && !is_desc_empty(&mdb->ptr[e])) e++;
			if(e >= mdb->len) break;
			i += span - 1;
		}
		mdb->len -= num_dead;
	}

	/* output. */
	if(output_pos < 0) {
		assert(mdb->len + 1 < mdb->size);
		output_pos = mdb->len++;
	}
	mdb->ptr[output_pos] = newdesc;

	return true;
}
