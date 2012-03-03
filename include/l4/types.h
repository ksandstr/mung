
/* <l4/types.h> for this microkernel. follows L4Ka::Pistachio. */

#ifndef __L4__TYPES_H__
#define __L4__TYPES_H__

#include <stdint.h>


/* hardcoded for ia32. bitfields not properly endianized. */

typedef uint32_t L4_Word_t;
typedef uint8_t L4_Word8_t;
typedef uint16_t L4_Word16_t;
typedef uint32_t L4_Word32_t;
typedef uint64_t L4_Word64_t;


typedef L4_Word_t L4_Bool_t;


typedef union {
	L4_Word_t raw;
	struct {
		unsigned rwx:3;
		unsigned extended:1;
		unsigned s:6;
		unsigned b:22;
	} __attribute__((packed)) X;
} L4_Fpage_t;


#define L4_Nilpage ((L4_Fpage_t){ .raw = 0 })


static inline bool L4_IsNilFpage(L4_Fpage_t fp) {
	return fp.raw == 0;
}

static inline L4_Fpage_t L4_FpageLog2(L4_Word_t address, int shift) {
	return (L4_Fpage_t){ .X = { .s = shift, .b = address >> 10 } };
}

/* L4_Fpage_t accessors */

static inline L4_Word_t L4_Address(L4_Fpage_t fp) {
	return fp.X.b << 10;
}

static inline L4_Word_t L4_Size(L4_Fpage_t fp) {
	return fp.X.s == 0 ? 0 : 1ul << fp.X.s;
}

static inline L4_Word_t L4_SizeLog2(L4_Fpage_t fp) {
	return fp.X.s;
}


#endif
