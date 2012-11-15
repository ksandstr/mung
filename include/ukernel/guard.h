/* guard areas for debugging damage to specific parts of structures, the
 * classic example being struct thread's rb_node field.
 *
 * synopsis:
 *
 * struct damage_prone_thing {
 *     GUARD_MEMBER(first);
 *     struct rb_node tree_link;
 *     GUARD_MEMBER(second);
 *     uint32_t something;
 *     uint32_t whatever;
 * };
 *
 * static void construct_thing(struct damage_prone_thing *t) {
 *     GUARD_INIT(t, first);
 *     GUARD_INIT(t, second);
 * }
 *
 * static bool per_thing_invariant(struct damage_prone_thing *t) {
 *     inv_ok1(GUARD_CHECK(t, first));
 *     inv_ok1(GUARD_CHECK(t, second));
 *     etc...
 * }
 */

#ifndef SEEN_UKERNEL_GUARD_H
#define SEEN_UKERNEL_GUARD_H

#include <stdint.h>
#include <stddef.h>
#include <string.h>

#include <ccan/alignof/alignof.h>

#include <ukernel/util.h>		/* for int_hash() */


/* configurables. */
#ifndef GUARD_SIZE
#ifdef DEBUG_ME_HARDER
#define GUARD_SIZE 256
#else
#define GUARD_SIZE 16
#endif
#endif

#ifndef GUARD_SEED
#define GUARD_SEED 0xb828054a		/* can be e.g. a module variable too */
#endif


#ifdef NDEBUG
#define GUARD_MEMBER(name)
#define GUARD_INIT(ptr, name)
#define GUARD_CHECK(ptr, name) true
#else
#define GUARD_MEMBER(name) uint8_t _guard_##name[GUARD_SIZE] \
	__attribute__((aligned(sizeof(uint32_t))))
#define GUARD_INIT(ptr, name) guard_init(ptr, offsetof(typeof(*ptr), _guard_##name))
#define GUARD_CHECK(ptr, name) guard_check(ptr, offsetof(typeof(*ptr), _guard_##name))
#endif


#ifndef NDEBUG
static inline uint32_t gval_for_ptr(uintptr_t ptr) {
	return int_hash((ptr >> 2) ^ GUARD_SEED);
}


static inline void guard_init(void *ptr, size_t offset)
{
	ptr += offset;
	int done = 0;
	while(done < GUARD_SIZE) {
		uint32_t gval = gval_for_ptr((uintptr_t)ptr + done);
		*(uint32_t *)(ptr + done) = gval;
		done += sizeof(uint32_t);
	}
}


static inline bool guard_check(const void *ptr, size_t offset)
{
	ptr += offset;
	for(int i=0; i < GUARD_SIZE; i += sizeof(uint32_t)) {
		uint32_t gval = gval_for_ptr((uintptr_t)ptr + i);
		if(*(const uint32_t *)(ptr + i) != gval) {
#if 0
			printf("guard fail: expected %#x at %p (i %d, off %u), but found %#x\n",
				gval, ptr + i, i, (unsigned)offset, *(const uint32_t *)(ptr + i));
#endif
			return false;
		}
	}
	return true;
}
#endif

#endif
