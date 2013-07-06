/*
 * include/clib/string.h
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

#ifndef SEEN_STRING_H
#define SEEN_STRING_H

/* FIXME: this is probably not the correct way of getting size_t around. */
#define __need_size_t
#include <stddef.h>
#undef __need_size_t


extern void *memcpy(void *dest, const void *src, size_t n);
extern void *memset(void *s, int c, size_t n);
extern int memcmp(const void *s1, const void *s2, size_t n);

/* note absence of strcpy(). */
extern char *strncpy(char *dest, const char *src, size_t n);

extern int strcmp(const char *a, const char *b)
	__attribute__((__pure__));
extern int strncmp(const char *a, const char *b, size_t n)
	__attribute__((__pure__));

extern char *strdup(const char *str);
extern char *strndup(const char *str, size_t n);

extern size_t strlen(const char *str)
	__attribute__((__pure__));
extern size_t strnlen(const char *str, size_t n)
	__attribute__((__pure__));

extern char *strchr(const char *s, int c)
	__attribute__((__pure__));
extern char *strrchr(const char *s, int c)
	__attribute__((__pure__));
extern char *strstr(const char *haystack, const char *needle)
	__attribute__((__pure__));

extern char *strpbrk(const char *s, const char *a)
	__attribute__((__pure__));


#ifdef _GNU_SOURCE
#if defined(__GNUC__) && defined(__OPTIMIZE__)
#define ffsl(__l) __builtin_ffsl((__l))
#else
extern int ffsl(long int __l) __attribute__((__const__));
#endif
#endif


#ifndef IN_LIB_IMPL

/* defines for GCC builtins */

#define memcpy(a, b, c) __builtin_memcpy((a), (b), (c))
#define memset(a, b, c) __builtin_memset((a), (b), (c))
#define strncpy(a, b, c) __builtin_strncpy((a), (b), (c))
#define strcmp(a, b) __builtin_strcmp((a), (b))
#define strncmp(a, b, n) __builtin_strncmp((a), (b), (n))
#define strlen(a) __builtin_strlen((a))


#endif /* IN_LIB_IMPL */


/* these are very much NON-STANDARD. they're defined in lib/strlcpy.c and
 * lib/strlcat.c, respectively.
 */
extern size_t strlcpy(char *dst, const char *src, size_t siz);
extern size_t strlcat(char *dst, const char *src, size_t siz);


#endif
