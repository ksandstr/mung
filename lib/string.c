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
	if(*needle == '\0') return (char *)haystack;

	size_t len = strlen(haystack), nlen = strlen(needle);
	for(size_t i=0; i <= len - nlen; i++) {
		if(memcmp(&haystack[i], needle, nlen) == 0) {
			return (char *)&haystack[i];
		}
	}

	return NULL;
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
