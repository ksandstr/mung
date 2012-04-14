/* quasi-runtime functions for the microkernel. */

#ifndef SEEN_UKERNEL_MISC_H
#define SEEN_UKERNEL_MISC_H

#include <stdint.h>
#include <string.h>
#include <ccan/compiler/compiler.h>

#include <l4/types.h>


#define CHECK_FLAG(mask, bit) (((mask) & (bit)) != 0)

#define PURE __attribute__((pure))

#define MIN(t, a, b) ({ t __a = (a), __b = (b); __a < __b ? (t)__a : (t)__b; })
#define MAX(t, a, b) ({ t __a = (a), __b = (b); __a > __b ? (t)__a : (t)__b; })
#define SWAP(t, a, b) do { t __tmp = (a); (a) = (b); (b) = __tmp; } while(0)

#define MSB(x) (sizeof((x)) * 8 - __builtin_clzl((x)) - 1)


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


struct page;


/* from kmain.c */

extern void *kip_mem;
extern uint8_t syscall_stack[];
/* should only be read with interrupts disabled! */
extern uint64_t *global_timer_count;

extern void NORETURN panic(const char *message);
extern uint64_t read_global_timer(void);


/* from hash.c */
extern uint32_t int_hash(uint32_t key);
extern uint32_t ptr_hash(const void *ptr);


/* from kip.c */
/* parameter is a pointer to the KCP, which gets reworked to be a proper
 * KIP. the range between kern_start and kern_end will be added to MemoryInfo
 * as a reserved range before the first dedicated range.
 */
extern void make_kip(void *mem, L4_Word_t kern_start, L4_Word_t kern_end);

#endif
