/* quasi-runtime functions for the microkernel and associated programs. */

#ifndef SEEN_UKERNEL_UTIL_H
#define SEEN_UKERNEL_UTIL_H

#include <assert.h>
#include <stdint.h>
#include <string.h>

#include <l4/types.h>


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

#define MSB(x) (sizeof((x)) * 8 - __builtin_clzl((x)) - 1)
#define POPCOUNT(x) (__builtin_popcount((x)))

#define WORD_BITS (sizeof(L4_Word_t) * 8)


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


static inline uint64_t time_in_us(L4_Time_t t)
{
	/* only defined for periods. c'mon. that's what "in" means. */
	assert(t.period.a == 0);
	if(t.raw == L4_ZeroTime.raw) return 0;
	else return (uint32_t)t.period.m * (1u << t.period.e);
}


/* from hash.c */
extern uint32_t int_hash(uint32_t key);
extern uint32_t ptr_hash(const void *ptr);

#endif
