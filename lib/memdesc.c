
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


/* range given as [start, end] */
static L4_Fpage_t normal_fpage(L4_Word_t start, L4_Word_t end)
{
	const int word_bits = sizeof(L4_Word_t) * 8;
	if(start > end) return L4_Nilpage;
	return L4_FpageLog2(start,
		MIN(int, start == 0 ? word_bits : ffsl(start) - 1,
			MAX(int, 10, MSB(end - start + 1))));
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
	//printf("%s: query=[%#lx, %#lx]\n", __func__, q_start, q_end);
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
			//printf("%s: pos <+ [%#lx, %#lx]\n", __func__, s, e);
			acc_left(&pos_s, &pos_e, s, e);
			//printf("%s: pos'=[%#lx, %#lx]\n", __func__, pos_s, pos_e);
		}
		/* negative condition. */
		if((!dedicated && mt == L4_DedicatedMemoryType)
			|| (t != L4_ReservedMemoryType && mt == L4_ReservedMemoryType))
		{
			//printf("%s: neg <+ [%#lx, %#lx]\n", __func__, s, e);
			acc_left(&neg_s, &neg_e, s, e);
			//printf("%s: neg'=[%#lx, %#lx]\n", __func__, neg_s, neg_e);
		}
	}

	/* interpret the runes */
	if(pos_s == 0 && pos_e == 0) {
		/* no positive section. */
		//printf("%s: no positive section.\n", __func__);
		return L4_Nilpage;
	} else if((neg_s == 0 && neg_e == 0) || neg_s > pos_e) {
		/* no negative section, or it starts after positive ends */
		//printf("%s: only positive [%#lx, %#lx]\n", __func__, pos_s, pos_e);
		return normal_fpage(pos_s, pos_e);
	} else if(pos_s < neg_s) {
		/* there's a positive slice that ends at a negative slice. */
		//printf("%s: pos ends at neg [%#lx, %#lx]\n", __func__,
			//pos_s, neg_s - 1);
		return normal_fpage(pos_s, neg_s - 1);
	} else if(neg_e >= q_end) {
		/* the negative section extends up to or beyond query range. */
		//printf("%s: negative section covers query end\n", __func__);
		return L4_Nilpage;
	} else {
		/* the leftmost negative precedes the positive, but ends before query.
		 * recur to ensure forward progress despite acc_left()'s noted
		 * weakness.
		 */
		//printf("%s: recurse\n", __func__);
		return mdb_query(mdb, neg_e + 1, q_end, virtual, dedicated, t);
	}
}
