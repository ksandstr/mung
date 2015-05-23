/* quasi-runtime functions for the microkernel and associated programs. */

#ifndef SEEN_UKERNEL_UTIL_H
#define SEEN_UKERNEL_UTIL_H

#include <assert.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>

#include <l4/types.h>
#include <l4/message.h>


#define NUM_ELEMENTS(array) (sizeof((array)) / sizeof((array)[0]))

#define CHECK_FLAG(mask, bit) ({ \
		assert(__builtin_popcount((bit)) == 1); \
		((mask) & (bit)) != 0; \
	})
#define CHECK_FLAG_ANY(mask, bits) (((mask) & (bits)) != 0)
#define CHECK_FLAG_ALL(mask, bits) (((mask) & (bits)) == (bits))

#define PURE __attribute__((pure))

#define MIN(t, a, b) ({ t __a = (a), __b = (b); __a < __b ? (t)__a : (t)__b; })
#define MAX(t, a, b) ({ t __a = (a), __b = (b); __a > __b ? (t)__a : (t)__b; })
#define SWAP(t, a, b) do { t __tmp = (a); (a) = (b); (b) = __tmp; } while(0)

#define BETWEEN(low, high, x) ((low) <= (x) && (x) <= (high))

#define RANGE_OVERLAP(l0, h0, l1, h1) \
	(BETWEEN((l0), (h0), (l1)) || BETWEEN((l0), (h0), (h1)) \
		|| BETWEEN((l1), (h1), (l0)) || BETWEEN((l1), (h1), (h0)))

#define FPAGE_LOW(fp) (L4_Address((fp)))
#define FPAGE_HIGH(fp) (FPAGE_LOW((fp)) + L4_Size((fp)) - 1)

#define ADDR_IN_FPAGE(haystack, needle) \
	fpage_overlap((haystack), L4_FpageLog2((needle), 0))

#define MSB(x) (sizeof((x)) * 8 - __builtin_clzl((x)) - 1)
#define POPCOUNT(x) (__builtin_popcount((x)))

#define WORD_BITS (sizeof(L4_Word_t) * 8)

#define ALIGN_TO_SHIFT(addr, shift) ((addr) & ~((1 << (shift)) - 1))
#define ALIGN_TO_SIZE(addr, size) ALIGN_TO_SHIFT((addr), size_to_shift((shift)))

/* printf() utility macro */
#define btos(x) (!!(x) ? "true" : "false")


/* usage:
 *
 * int size_log2;
 * L4_Word_t address;
 * for_page_range(start, end, address, size_log2) {
 *       L4_Fpage_t page = L4_FpageLog2(address, size_log2);
 *       // carry on
 * }
 *
 * "start" and "end" define an exclusive range, i.e. [start .. end). The null
 * range (i.e. start == end) skips the loop body entirely.
 */
#define for_page_range(_start, _end, _addr, _sizelog2) \
	for(L4_Word_t _E = (_end), _A = (_addr) = (_start), \
			_S = (_sizelog2) = MIN(int, ffsl(_A) - 1, MSB(_E - _A)); \
		_A < _E; \
		(_addr) = (_A += (1 << _S)), \
			(_sizelog2) = _S = MIN(int, ffsl(_A) - 1, MSB(_E - _A)))


static inline int size_to_shift(size_t size) {
	int msb = MSB(size);
	return (1 << msb) < size ? msb + 1 : msb;
}


static inline uint64_t time_in_us(L4_Time_t t)
{
	/* only defined for periods. c'mon. that's what "in" means. */
	assert(L4_IsTimePeriod_NP(t));
	return L4_PeriodUs_NP(t);
}


/* NOTE: copypasta'd from type_suite.c! */
static inline bool pt_is_valid(L4_Clock_t base, L4_Time_t t)
{
	uint32_t max = 0x3ff << t.point.e,
		us = L4_PointClock_NP(base, t).raw - base.raw;
	return max >= us;
}


static inline bool fpage_overlap(L4_Fpage_t a, L4_Fpage_t b)
{
	L4_Word_t mask = ~((1ul << MAX(int, L4_SizeLog2(a), L4_SizeLog2(b))) - 1);
	return ((a.raw ^ b.raw) & mask) == 0;
}


/* from hash.c */
extern uint32_t int_hash(uint32_t key);
extern uint32_t ptr_hash(const void *ptr);

static inline bool int_eq(const void *elem, void *ref) {
	return *(const int *)elem == *(int *)ref;
}


/* from arch_x86.c */
/* deep_call() returns 0 on success, or -ENOMEM when the new stack couldn't be
 * allocated.
 */
extern int deep_call(void (*fn)(void *), void *paramptr);
extern bool is_stack_safe(size_t margin);


/* from l4util.c */
extern size_t stritemlen(L4_StringItem_t *si);


#endif
