
/* <l4/types.h> for this microkernel. follows L4Ka::Pistachio. */

#ifndef __L4__TYPES_H__
#define __L4__TYPES_H__

#include <stdint.h>
#include <stdbool.h>


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

/* from L4Ka::Pistachio */
#define L4_Readable		(0x04)
#define L4_Writable		(0x02)
#define L4_eXecutable		(0x01)
#define L4_FullyAccessible	(0x07)
#define L4_ReadWriteOnly	(0x06)
#define L4_ReadeXecOnly		(0x05)
#define L4_NoAccess		(0x00)

#define L4_Nilpage ((L4_Fpage_t){ .raw = 0 })
#define L4_CompleteAddressSpace ((L4_Fpage_t){ .X.s = 1 })

static inline L4_Word_t L4_Rights(L4_Fpage_t fp) {
	return fp.X.rwx;
}

static inline L4_Fpage_t L4_Set_Rights(L4_Fpage_t *fp, L4_Word_t rwx) {
	fp->X.rwx = rwx;
	return *fp;
}


typedef union {
	L4_Word_t raw;
	struct {
		unsigned version:14;
		unsigned thread_no:18;
	} __attribute__((packed)) X;
} L4_GthreadId_t;

typedef union {
	L4_Word_t raw;
	struct {
		unsigned zeros:6;
		unsigned local_id:26;
	} __attribute__((packed)) X;
} L4_LthreadId_t;

typedef union {
	L4_Word_t raw;
	L4_GthreadId_t global;
	L4_LthreadId_t local;
} L4_ThreadId_t;

#define L4_nilthread ((L4_ThreadId_t){ .raw = 0 })
#define L4_anythread ((L4_ThreadId_t){ .raw = ~0UL })
#define L4_anylocalthread ((L4_ThreadId_t){ .raw = ~0UL << 6 })


static inline bool L4_IsNilThread(L4_ThreadId_t tid) {
	return tid.raw == 0;
}

static inline bool L4_IsLocalId(L4_ThreadId_t tid) {
	return tid.local.X.zeros == 0;
}

static inline bool L4_IsGlobalId(L4_ThreadId_t tid) {
	return tid.local.X.zeros != 0;
}


static inline bool L4_IsNilFpage(L4_Fpage_t fp) {
	return fp.raw == 0;
}

static inline L4_Fpage_t L4_FpageLog2(L4_Word_t address, int shift) {
	return (L4_Fpage_t){ .X = { .s = shift, .b = address >> 10 } };
}

static inline L4_Fpage_t L4_Fpage(L4_Word_t address, L4_Word_t size) {
	/* GCC intrinsics. */
	int msb = sizeof(L4_Word_t) * 8 - __builtin_clzl(size) - 1,
		shift = (1ul << msb) <= size ? msb : msb + 1;
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


/* time. derived from L4Ka::Pistachio. */

typedef union {
	L4_Word16_t	raw;
	struct {
		L4_Word16_t m:10;
		L4_Word16_t e:5;
		L4_Word16_t a:1;
	} period;
	struct {
		L4_Word16_t m:10;
		L4_Word16_t c:1;
		L4_Word16_t e:4;
		L4_Word16_t a:1;
	} point;
} L4_Time_t;

#define L4_Never ((L4_Time_t){ .raw = 0 })
#define L4_ZeroTime ((L4_Time_t){ .period = { .e = 1 } })


static inline L4_Time_t L4_TimePeriod(L4_Word64_t microseconds)
{
#   define __L4_SET_TIMEPERIOD(exp, man) \
	do { time.period.m = man; time.period.e = exp; } while (0)
#   define __L4_TRY_EXPONENT(N) \
	else if (microseconds < (1UL << N)) \
	    __L4_SET_TIMEPERIOD (N - 10, microseconds >> (N - 10))

    L4_Time_t time;
    time.raw = 0;

    if (__builtin_constant_p (microseconds)) {
	if (0) {}
	__L4_TRY_EXPONENT (10); __L4_TRY_EXPONENT (11);
	__L4_TRY_EXPONENT (12); __L4_TRY_EXPONENT (13);
	__L4_TRY_EXPONENT (14); __L4_TRY_EXPONENT (15);
	__L4_TRY_EXPONENT (16); __L4_TRY_EXPONENT (17);
	__L4_TRY_EXPONENT (18); __L4_TRY_EXPONENT (19);
	__L4_TRY_EXPONENT (20); __L4_TRY_EXPONENT (21);
	__L4_TRY_EXPONENT (22); __L4_TRY_EXPONENT (23);
	__L4_TRY_EXPONENT (24); __L4_TRY_EXPONENT (25);
	__L4_TRY_EXPONENT (26); __L4_TRY_EXPONENT (27);
	__L4_TRY_EXPONENT (28); __L4_TRY_EXPONENT (29);
	__L4_TRY_EXPONENT (30); __L4_TRY_EXPONENT (31);
	else
	    return L4_Never;
    } else {
	L4_Word_t l4_exp = 0;
	L4_Word_t man = microseconds;
	while (man >= (1 << 10)) {
	    man >>= 1;
	    l4_exp++;
	}
	if (l4_exp <= 31)
	    __L4_SET_TIMEPERIOD (l4_exp, man);
	else
	    return L4_Never;
    }

    return time;
#   undef __L4_TRY_EXPONENT
#   undef __L4_SET_TIMEPERIOD
}


/* Clock, via L4Ka::Pistachio. */

typedef union {
	L4_Word64_t raw;
	struct {
		L4_Word32_t low;
		L4_Word32_t high;
	} __attribute__((packed)) X;
} L4_Clock_t;


#endif
