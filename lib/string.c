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

#include <ukernel/util.h>


/* TODO: make this conditional on those versions to which GCC bug #56888
 * applies.
 */
#pragma GCC optimize ("no-tree-loop-distribute-patterns")

static void *memcpy_forward(void *dst, const void *src, size_t len);



#ifdef __i386__
/* via uClibc */
static void *memcpy_forward(void *dst, const void *src, size_t len)
{
	int32_t d0, d1, d2;
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
	if(dst + MIN(size_t, len, 32) < src || dst >= src + len
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
	const uint8_t *a = s1, *b = s2;
	size_t i = 0;
	while(i < n && a[i] == b[i]) i++;
	if(i == n) return 0;
	else if(a[i] < b[i]) return -1;
	else return 1;
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
	int i = 0;
	while(a[i] != '\0' && b[i] != '\0') {
		if(a[i] < b[i]) return -1;
		else if(a[i] > b[i]) return 1;
		i++;
	}
	if(a[i] == '\0' && b[i] == '\0') return 0;
	/* "a ends first" means it sorts first. */
	if(a[i] == '\0') return -1; else return 1;
}


int strncmp(const char *a, const char *b, size_t n)
{
	int i = 0;
	while(i < n && a[i] != '\0' && b[i] != '\0') {
		if(a[i] < b[i]) return -1;
		else if(a[i] > b[i]) return 1;
		i++;
	}
	if(i == n || (a[i] == '\0' && b[i] == '\0')) return 0;
	else if(a[i] == '\0') return -1;
	else return 1;
}


char *strdup(const char *str)
{
	size_t n = strlen(str);
	char *buf = malloc(n+1);
	if(buf != NULL) memcpy(buf, str, n+1);
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
	size_t n = 0;
	while(str[n] != '\0') n++;
	return n;
}


size_t strnlen(const char *str, size_t max) {
	size_t n = 0;
	while(n < max && str[n] != '\0') n++;
	return n;
}


char *strncpy(char *dest, const char *src, size_t n)
{
	size_t i=0;
	while(i < n && src[i] != '\0') {
		dest[i] = src[i];
		i++;
	}
	memset(&dest[i], '\0', n - i);

	return dest;
}


char *strchr(const char *s, int c)
{
	while(*s != '\0' && *s != c) s++;
	return *s == '\0' ? NULL : (char *)s;
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

	for(int i=0; i<present_len; i++) present[i] = 0;
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
