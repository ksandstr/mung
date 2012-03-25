
#include <stdint.h>


/* hash32shiftmult(); presumed to have been in the public domain. */
uint32_t int_hash(uint32_t key)
{
	uint32_t c2=0x27d4eb2du; // a prime or an odd constant
	key = (key ^ 61) ^ (key >> 16);
	key = key + (key << 3);
	key = key ^ (key >> 4);
	key = key * c2;
	key = key ^ (key >> 15);
	return key;
}


uint32_t ptr_hash(const void *ptr)
{
	uintptr_t i = (uintptr_t)ptr;
	if(sizeof(i) == sizeof(uint32_t)) return int_hash(i);
	else {
#if 1
		/* downshift by granularity, then hash those 32 bits. */
		return int_hash(i >> 5);
#else
		return int_hash(i & 0xffffffff) ^ int_hash((uint64_t)i >> 32);
#endif
	}
}
