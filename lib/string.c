/*
 * lib/string.c
 * Copyright 2008, 2009, 2010  Kalle A. Sandström <ksandstr@iki.fi>
 *
 * This file is part of µiX.
 *
 * µiX is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * µiX is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with µiX.  If not, see <http://www.gnu.org/licenses/>.
 */

#define IN_LIB_IMPL

#include <string.h>
#include <stdlib.h>
#include <stdint.h>
#include <assert.h>
#include <errno.h>
#include <ccan/endian/endian.h>
#include <ccan/minmax/minmax.h>

#include <ukernel/util.h>


/* TODO: make this conditional on those versions to which GCC bug #56888
 * applies.
 */
#pragma GCC optimize ("no-tree-loop-distribute-patterns")

static void *memcpy_forward(void *dst, const void *src, size_t len);


/* returns nonzero if there are zero bytes in @x. see caveat in zero_mask()
 * comment.
 *
 * via https://graphics.stanford.edu/~seander/bithacks.html#ZeroInWord and
 * others.
 */
static inline unsigned long haszero(unsigned long x) {
	return (x - 0x01010101ul) & ~x & 0x80808080ul;
}


/* sets the high bit for every byte in @x that's zero. generally haszero() is
 * used to _detect_ a zero byte in @x, however, we're interested not only in
 * that but its/their _location_, so there's a few more cycles of processing
 * afterward to exclude the 0x0100 -> 0x8080 case.
 */
static inline unsigned long zero_mask(unsigned long x) {
	return haszero(x) & (~(x & 0x01010101ul) << 7);
}


/* same, but for a given byte. */
static inline unsigned long byte_mask(unsigned long x, int c) {
	unsigned long ret = zero_mask(x ^ (~0ul / 255 * c));
#ifdef DEBUG_ME_HARDER
	unsigned long t = ret;
	while(t != 0) {
		int p = ffsl(t) / 8 - 1;
		assert(((x >> (p * 8)) & 0xff) == c);
		t &= ~(0xfful << (p * 8));
	}
#endif
	return ret;
}


/* compute bytes until end of page from @p. */
static inline int until_page(const void *ptr) {
	/* TODO: make prettier w/ PAGE_SIZE and so forth */
	return 0x1000 - ((uintptr_t)ptr & 0xfff);
}


#ifdef __i386__
/* via uClibc */
static void *memcpy_forward(void *dst, const void *src, size_t len)
{
	int32_t d0, d1, d2;
	if(__builtin_constant_p(len) && (len & 3) == 0) {
		asm volatile ("rep; movsl\n"
			: "=&c" (d0), "=&D" (d1), "=&S" (d2)
			: "0" (len / 4), "g" (len), "1" (dst), "2" (src)
			: "memory");
	} else {
		asm volatile (
			"rep; movsl\n"
			"movl %4, %%ecx\n"
			"andl $3, %%ecx\n"
			/* avoids rep;movsb with ecx==0, faster on post-2008 iron */
			"jz 1f\n"
			"rep; movsb\n"
			"1:\n"
			: "=&c" (d0), "=&D" (d1), "=&S" (d2)
			: "0" (len / 4), "g" (len), "1" (dst), "2" (src)
			: "memory");
	}
	return dst;
}

#else

/* generic memcpy_forward() for one-word transfers in a simple loop. */
static void *memcpy_forward(void *dst, const void *src, size_t len)
{
	uintptr_t *d = dst;
	const uintptr_t *s = src;
	size_t i = 0;
	while(len >= sizeof(uintptr_t)) {
		d[i] = s[i];
		i++;
		len -= sizeof(uintptr_t);
	}

	uint8_t *d8 = (uint8_t *)d;
	const uint8_t *s8 = (const uint8_t *)s;
	i *= sizeof(uintptr_t);
	while(len > 0) {
		d8[i] = s8[i];
		i++;
		len--;
	}

	return dst;
}
#endif


void *memcpy(void *dst, const void *src, size_t len) {
	return memcpy_forward(dst, src, len);
}


void *memmove(void *dst, const void *src, size_t len)
{
	if(dst + min_t(size_t, len, 32) < src || dst >= src + len
		|| dst + len < src)
	{
		/* strictly forward OK, or no overlap */
		return memcpy_forward(dst, src, len);
	} else {
		/* the brute force solution */
		void *buf = malloc(len);
		if(buf == NULL) abort();	/* FIXME, somehow */
		memcpy(buf, src, len);
		memcpy(dst, buf, len);
		free(buf);
		return dst;
	}
}


void *memset(void *start, int c, size_t len)
{
	size_t i = 0;

	uint32_t *dst = start;
	const uint8_t t = c;
	uint32_t c32 = 0x01010101u * (uint32_t)t;
	while(len >= 4) {
		dst[i++] = c32;
		len -= 4;
	}
	i *= 4;

	uint8_t *dst8 = start;
	while(len > 0) {
		dst8[i++] = t;
		len--;
	}

	return start;
}


int memcmp(const void *s1, const void *s2, size_t n)
{
	size_t major = n & ~3u;
	for(size_t i = 0, w = major / 4; i < w; i++) {
		unsigned long a = BE32_TO_CPU(*(unsigned long *)(s1 + i * 4)),
			b = BE32_TO_CPU(*(unsigned long *)(s2 + i * 4));
		long c = a - b;
		if(c != 0) return c;
	}

	const uint8_t *a = s1, *b = s2;
	for(size_t i = major; i < n; i++) {
		int c = (int)a[i] - b[i];
		if(c != 0) return c;
	}
	return 0;
}


void *memswap(void *a, void *b, size_t n)
{
	if(a == b || n == 0) return a;
	void *retval = a;

	if(n <= 16) {
		char *aa = a, *bb = b;
		for(int i=0; i < n; i++) {
			char t = aa[i];
			aa[i] = bb[i];
			bb[i] = t;
		}
		goto end;
	}

	char buf[256];
	size_t head = n % sizeof(buf);
	if(head > 0) {
		memcpy(buf, a, head);
		memcpy(a, b, head);
		memcpy(b, buf, head);
		a += head; b += head;
		if(n <= head) goto end;
	}

	/* i like the part where he said "sizeof(buf)" */
	for(size_t i = 0, l = n / sizeof(buf); i < l; i++) {
		memcpy(buf, a, sizeof(buf));
		memcpy(a, b, sizeof(buf));
		memcpy(b, buf, sizeof(buf));
		a += sizeof(buf); b += sizeof(buf);
	}

end:
	return retval;
}


int strcmp(const char *a, const char *b)
{
	if(a == b) return 0;

	/* the word-at-a-time optimization requires that long loads are valid,
	 * i.e. that they don't cross a 4k boundary in memory. so the loop
	 * consists of two parts: the one that runs before such a boundary in
	 * either operand, and one that runs over it.
	 */
	size_t pos = 0;
	for(;;) {
		/* NOTE: could add a second byte-at-a-time segment here to bump @pos
		 * such that at least one of a[pos] and b[pos] is aligned to 4. but
		 * that's a can of worms, so let's not.
		 */
		int bytes = min(until_page(&a[pos]), until_page(&b[pos])),
			words = bytes / 4;
		const unsigned long *ap = (const unsigned long *)(a + pos),
			*bp = (const unsigned long *)(b + pos);
		for(int i=0; i < words; i++) {
			unsigned long la = BE32_TO_CPU(ap[i]), lb = BE32_TO_CPU(bp[i]),
				za = zero_mask(la), zb = zero_mask(lb),
				m = ((1ul << MSB(za | zb | 1)) >> 7) * 0xff;
			if(m == 0) m = 0xff;
			m |= m << 8;
			m |= m << 16;
			long c = (la & m) - (lb & m);
			if(c != 0) return c;
			if((za | zb) != 0) return (za & m) - (zb & m);
		}
		pos += words * 4;

		/* then byte at a time. */
		for(int tail = bytes % sizeof(unsigned long); tail > 0; pos++, tail--) {
			int c = (int)a[pos] - b[pos];
			if(c != 0) return c;
			if(a[pos] == '\0') {
				assert(b[pos] == '\0');
				return 0;
			}
		}
	}
}


int strncmp(const char *a, const char *b, size_t n)
{
	size_t i = 0;
	while(i < n && a[i] != '\0' && b[i] != '\0') {
		int c = (int)a[i] - b[i];
		if(c != 0) return c;
		i++;
	}
	if(i == n || (a[i] == '\0' && b[i] == '\0')) return 0;
	else if(a[i] == '\0') return -1;
	else return 1;
}


char *strdup(const char *str)
{
	size_t n = strlen(str);
	char *buf = malloc(n + 1);
	if(buf != NULL) memcpy(buf, str, n + 1);
	return buf;
}


char *strndup(const char *str, size_t n)
{
	size_t len = strnlen(str, n);
	char *buf = malloc(len + 1);
	if(buf != NULL) {
		memcpy(buf, str, len);
		buf[len] = '\0';
	}
	return buf;
}


size_t strlen(const char *str)
{
	/* fancy-pants word-at-a-time algorithm w/ byte-at-a-time fallback for
	 * crossing 4k page boundaries safely under POSIX.
	 * NB: this, too, could pre-align @str to word boundary.
	 */
	size_t pos = 0;
	for(;;) {
		int bytes = until_page(&str[pos]), words = bytes / sizeof(uintptr_t);
		const uintptr_t *wp = (const uintptr_t *)(str + pos);
		for(int i=0; i < words; i++) {
			uintptr_t w = LE32_TO_CPU(wp[i]);
			if(haszero(w) != 0) {
				uintptr_t z = zero_mask(w);
				size_t len = pos + i * sizeof(uintptr_t) + ffsl(z) / 8 - 1;
				assert(str[len] == '\0');
				return len;
			}
		}
		pos += words * sizeof(uintptr_t);

		for(int tail = bytes % sizeof(uintptr_t); tail > 0; pos++, tail--) {
			if(str[pos] == '\0') return pos;
		}
	}
}


size_t strnlen(const char *str, size_t max) {
	size_t n = 0;
	while(n < max && str[n] != '\0') n++;
	return n;
}


char *strncpy(char *dest, const char *src, size_t n)
{
	size_t i = 0;
	while(i < n && src[i] != '\0') {
		dest[i] = src[i];
		i++;
	}
	memset(&dest[i], '\0', n - i);

	return dest;
}


int strscpy(char *dest, const char *src, size_t n)
{
	size_t pos = 0;
	if(labs(dest - src) < sizeof(uintptr_t)) goto rest;	/* , you pervert */

	/* again with the usual old WAAT: one loop per page boundary, inner loops
	 * for words and trailing bytes.
	 *
	 * NOTE: could try to align @src for the first segment, if it already
	 * isn't. unaligned loads are typically slower than unaligned stores (when
	 * there's any difference).
	 */
	while(pos < n) {
		int bytes = min(until_page(&dest[pos]), until_page(&src[pos])),
			words = bytes / sizeof(uintptr_t);
		for(int i=0; i < words; i++, pos += sizeof(uintptr_t)) {
			uintptr_t x = LE32_TO_CPU(*(const uintptr_t *)&src[pos]);
			if(haszero(x) == 0) {
				*(uintptr_t *)&dest[pos] = CPU_TO_LE32(x);
			} else {
				uintptr_t z = zero_mask(x);
				*(uintptr_t *)&dest[pos] = CPU_TO_LE32(x & ~((z >> 7) * 0xff));
				pos += ffsl(z) / 8 - 1;
				assert(src[pos] == '\0');
				goto end;
			}
		}
		assert(pos <= n);

		for(int tail = bytes % sizeof(uintptr_t); tail > 0; pos++, tail--) {
			assert(pos < n);
			if(src[pos] == '\0') goto end;
			dest[pos] = src[pos];
		}
		assert(pos <= n);
	}

rest:
	for(; pos < n && src[pos] != '\0'; pos++) dest[pos] = src[pos];
	if(pos >= n) {
		dest[n - 1] = '\0';
		return -E2BIG;
	} else {
end:
		dest[pos] = '\0';
		assert(strcmp(src, dest) == 0);
		return pos;
	}
}


char *strchr(const char *s, int c)
{
	if(c == '\0') return (char *)s + strlen(s);

	/* WAAT w/ fallback for page borders */
	size_t pos = 0;
	for(;;) {
		int left = until_page(&s[pos]), words = left / sizeof(long);
		const unsigned long *wp = (const unsigned long *)(s + pos);
		for(int i=0; i < words; i++) {
			unsigned long x = LE32_TO_CPU(wp[i]),
				found = byte_mask(x, c), zero = zero_mask(x);
			assert((found | zero) == 0 || found != zero);
			if(found != 0 && (zero == 0 || ffsl(found) < ffsl(zero))) {
				size_t len = pos + i * sizeof(long) + ffsl(found) / 8 - 1;
				assert(s[len] == c);
				return (char *)s + len;
			}
			if(zero != 0) return NULL;
		}
		pos += words * sizeof(long);

		int bytes = left - words * sizeof(long);
		for(int i=0; i < bytes; i++) {
			if(s[pos + i] == '\0') return NULL;
			if(s[pos + i] == c) return (char *)s + pos + i;
		}
		pos += bytes;
	}
}


char *strrchr(const char *s, int c)
{
	const char *t = s + strlen(s);
	while(*t != c && t != s) t--;
	return t != s ? (char *)t : NULL;
}


char *strstr(const char *haystack, const char *needle)
{
	if(*needle == '\0') goto end;	/* degenerate case */

	size_t n_len = 0;
	char diff = 0;
	while(needle[n_len] != '\0') {
		if(haystack[n_len] == '\0') return NULL;	/* #haystack < #needle */
		diff |= needle[n_len] ^ haystack[n_len];
		n_len++;
	}
	if(diff == 0) goto end;			/* prefix case */

	if(haystack[0] != needle[0]) goto forward;
	do {
		/* compare up to n_len bytes. return NULL if haystack is shorter than
		 * that. return haystack if needle is found. otherwise, advance until
		 * next instance of first letter in needle.
		 */
		bool found = true;
		for(size_t i=0; i < n_len; i++) {
			if(haystack[i] != needle[i]) {
				if(haystack[i] == '\0') return NULL;
				found = false;
				break;
			}
		}
		if(found) break;
forward:
		haystack = strchr(haystack + 1, needle[0]);
	} while(haystack != NULL);

end:
	return (char *)haystack;
}


/* fancy! and O(1) space! */
char *strpbrk(const char *s, const char *needles)
{
	const unsigned int long_bits = sizeof(unsigned long) * 8,
		present_len = (256 + long_bits - 1) / long_bits;
	unsigned long present[present_len];

	for(int i=0; i < present_len; i++) present[i] = 0;
	for(int i=0; needles[i] != '\0'; i++) {
		int c = (unsigned char)needles[i], o = c / long_bits,
			b = c & (long_bits - 1);
		present[o] |= 1ul << b;
	}

	while(*s != '\0') {
		int c = (unsigned char)(*s), o = c / long_bits,
			b = c & (long_bits - 1);
		if(present[o] & (1ul << b)) return (char *)s;
		s++;
	}

	return NULL;
}


#ifdef ffsl
#undef ffsl
#endif
int ffsl(long int __l)
{
	/* TODO: add bit magic version as well */
	return __builtin_ffsl(__l);
}
