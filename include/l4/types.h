
/* <l4/types.h> for this microkernel. follows L4Ka::Pistachio. */

#ifndef __L4__TYPES_H__
#define __L4__TYPES_H__

#ifdef _L4_DEBUG_ME_HARDER
#include <assert.h>
#define _L4_ASSERT(x) assert((x))
#else
#define _L4_ASSERT(x)
#endif


typedef unsigned long L4_Word_t;
typedef unsigned char L4_Word8_t;
typedef unsigned short L4_Word16_t;
typedef unsigned int L4_Word32_t;
typedef unsigned long long L4_Word64_t;

typedef L4_Word_t L4_Bool_t;


/* hardcoded for ia32. bitfields not properly endianized, nor padded for
 * 64-bit targets.
 */

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

static inline L4_Fpage_t L4_FpageAddRights(L4_Fpage_t f, L4_Word_t rwx) {
	f.X.rwx |= rwx;
	return f;
}

static inline L4_Fpage_t L4_FpageAddRightsTo(L4_Fpage_t *f, L4_Word_t rwx) {
	f->X.rwx |= rwx;
	return *f;
}

static inline L4_Fpage_t L4_FpageRemoveRightsFrom(
	L4_Fpage_t *f, L4_Word_t rwx)
{
	f->X.rwx &= ~rwx;
	return *f;
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


static inline L4_ThreadId_t L4_GlobalId(L4_Word_t tnum, L4_Word_t vers) {
	return (L4_ThreadId_t){ .global = {
		.X.thread_no = tnum, .X.version = vers } };
}


static inline L4_Bool_t L4_IsNilThread(L4_ThreadId_t tid) {
	return tid.raw == 0;
}

static inline L4_Bool_t L4_IsLocalId(L4_ThreadId_t tid) {
	return tid.local.X.zeros == 0;
}

static inline L4_Bool_t L4_IsGlobalId(L4_ThreadId_t tid) {
	return tid.local.X.zeros != 0;
}

static inline L4_Word_t L4_ThreadNo(L4_ThreadId_t tid) {
	/* see below */
	return tid.raw >> 14;
}

static inline L4_Word_t L4_Version(L4_ThreadId_t tid) {
	/* NOTE: if this value is accessed as tid.global.X.version, gcc will call
	 * forth eldritch horrors from the dimension Öäy. so, like, don't do that.
	 */
	return tid.raw & 0x3fff;
}

static inline L4_Bool_t L4_IsThreadEqual(L4_ThreadId_t l, L4_ThreadId_t r) {
	return l.raw == r.raw;
}

static inline L4_Bool_t L4_IsThreadNotEqual(L4_ThreadId_t l, L4_ThreadId_t r) {
	return l.raw != r.raw;
}


static inline L4_Bool_t L4_IsNilFpage(L4_Fpage_t fp) {
	return fp.raw == 0;
}

static inline L4_Fpage_t L4_FpageLog2(L4_Word_t address, int shift)
{
	_L4_ASSERT((address & ((1 << shift) - 1)) == 0);
	return (L4_Fpage_t){ .X = { .s = shift, .b = address >> 10 } };
}

static inline L4_Fpage_t L4_Fpage(L4_Word_t address, L4_Word_t size) {
	/* GCC intrinsics. */
	int msb = sizeof(L4_Word_t) * 8 - __builtin_clzl(size) - 1,
		shift = (1ul << msb) <= size ? msb : msb + 1;
	_L4_ASSERT((address & ((1 << shift) - 1)) == 0);
	return (L4_Fpage_t){ .X = { .s = shift, .b = address >> 10 } };
}

/* L4_Fpage_t accessors */

static inline L4_Word_t L4_Address(L4_Fpage_t fp) {
	return fp.raw & ~((1ul << fp.X.s) - 1);
}

static inline L4_Word_t L4_Size(L4_Fpage_t fp) {
	return fp.X.s == 0 ? 0 : 1ul << fp.X.s;
}

static inline L4_Word_t L4_SizeLog2(L4_Fpage_t fp) {
	return fp.X.s;
}


/* Clock, via L4Ka::Pistachio. */

typedef union {
	L4_Word64_t raw;
	struct {
		L4_Word32_t low;
		L4_Word32_t high;
	} __attribute__((packed)) X;
} L4_Clock_t;

static inline L4_Bool_t L4_IsClockEarlier(L4_Clock_t a, L4_Clock_t b) {
	return a.raw < b.raw;
}

static inline L4_Bool_t L4_IsClockLater(L4_Clock_t a, L4_Clock_t b) {
	return a.raw > b.raw;
}

static inline L4_Bool_t L4_IsClockEqual(L4_Clock_t a, L4_Clock_t b) {
	return a.raw == b.raw;
}

static inline L4_Bool_t L4_IsClockNotEqual(L4_Clock_t a, L4_Clock_t b) {
	return a.raw != b.raw;
}

static inline L4_Clock_t L4_ClockAddUsec(L4_Clock_t a, L4_Word64_t us) {
	return (L4_Clock_t){ .raw = a.raw + us };
}

static inline L4_Clock_t L4_ClockSubUsec(L4_Clock_t a, L4_Word64_t us) {
	return (L4_Clock_t){ .raw = a.raw - us };
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


/* contrary to the spec as of 20120601, the maximum representable interval is
 * about 33½ seconds, or 0x3ff << 15 microseconds.
 *
 * or I've miscomprehended something along the way.
 *
 * anyway, this is like the L4Ka::Pistachio L4_TimePoint(), except with an
 * explicit time base for ease of testing.
 */
static inline L4_Time_t L4_TimePoint2_NP(L4_Clock_t base, L4_Clock_t at)
{
	_L4_ASSERT(base.raw < at.raw);

	if(at.raw - base.raw > (0x3ff << 15)) return L4_Never;

	L4_Word32_t us = at.raw - base.raw;
	int exp = 22 - __builtin_clz(us);
	if(exp < 0) exp = 0;
	_L4_ASSERT(exp <= 15);
	_L4_ASSERT((us >> exp) < 1024);
	return (L4_Time_t){ .point = { .a = 1,
		.e = exp, .m = (at.raw >> exp) & 0x3ff,
		.c = (at.raw >> (exp + 10)) & 1 } };
}


/* due to circular dependency between L4_Clock_t (here) and L4_SystemClock()
 * (in <l4/syscall.h>), in mung L4_TimePoint() is just a macro. caveat lector.
 */
#define L4_TimePoint(at) L4_TimePoint2_NP(L4_SystemClock(), (at))


/* these don't appear in L4Ka::Pistachio either. */
static inline L4_Bool_t L4_IsTimePoint_NP(L4_Time_t t) {
	return t.point.a == 1;
}


static inline L4_Bool_t L4_IsTimePeriod_NP(L4_Time_t t) {
	return t.period.a == 0;
}


static inline L4_Clock_t L4_PointClock_NP(L4_Clock_t base, L4_Time_t t)
{
	_L4_ASSERT(L4_IsTimePoint_NP(t));

	L4_Word64_t clk = base.raw >> (t.point.e + 10);
	clk += ((int)clk & 1) ^ t.point.c;
	return (L4_Clock_t){
		.raw = (clk << (t.point.e + 10)) | ((L4_Word32_t)t.point.m << t.point.e),
	};
}


/* (clearly a relative of Incontinentia's...) */
static inline L4_Word64_t L4_PeriodUs_NP(L4_Time_t t) {
	_L4_ASSERT(L4_IsTimePeriod_NP(t));
	return (L4_Word64_t)t.period.m << t.period.e;
}


/* (because of a circular dependency.) */
#include <l4/arch.h>

#endif
