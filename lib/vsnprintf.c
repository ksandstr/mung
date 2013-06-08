/*
 * lib/vsnprintf.c -- implementation of vsnprintf(3)
 * Copyright 2008, 2009, 2010, 2011  Kalle A. Sandström <ksandstr@iki.fi>
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

#include <string.h>
#include <stdlib.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <limits.h>


static void convert_long(char *, long long, int, char, int *);
static void convert_ulong(char *, unsigned long long, int, char, int *);
static void convert_hex_long(
	char *, unsigned long long, int, char, const char *, int *);
static void convert_oct_long(char *, unsigned long long, int, char, int *);

static int parse_width(const char *, int *);


/*
 * NOTE: this is a limited subset of the functionality that real
 * C99 specifies. also, it doesn't always properly break apart and explode
 * when unimplemented features are used!
 *
 * ... also, it's a simple and likely-to-work thing that we've got here.
 * optimized for maintainability and for just work damnit, rather than
 * raw speed. (what kind of a dickhead depends on snprintf's speed anyway?)
 *
 * returns a hard -1 if there was an error in the format string, and
 * stores an error indicator in the output string.
 *
 * FIXME: field width acts as a _lower_ bound, when it should also act
 * as an _upper_ bound for e.g. strings.
 *
 * TODO: this is a bit stack-hungry. that's not good in e.g. a kernel
 * where thread stacks are limited to a few kilobytes.
 */
int vsnprintf(char *str, size_t size, const char *fmt, va_list arg_list)
{
	if(fmt == NULL) {
		strlcpy(str, "(null format)", size);
		return 0;
	}

	/* arg_list may be just a handle. make a proper copy for us. */
	va_list ap;
	va_copy(ap, arg_list);

	size_t outpos = 0;
	int out_total = 0;
	for(size_t i=0; fmt[i] != '\0'; i++) {
		if(fmt[i] != '%') {
			/* fastpath */
			if(outpos < size) str[outpos++] = fmt[i];
			out_total++;
			continue;
		}

		if(fmt[++i] == '\0') goto error;

		/* modifiers */
		/* (could track is_short also, but those are coerced to larger
		 * integers by the varargs mechanism.)
		 */
		// bool is_short = false;
		bool is_alternate = false, field_width_set = false;
		char pad_char = ' ';
		int field_width = 1, is_len = 0;

		bool was_modifier;
		do {
			was_modifier = true;
			switch(fmt[i]) {
				case 'l': is_len++; break;
				case 'h':
					/* forbid "short long long" conversions */
					if(is_len > 0) goto error;
					// is_short = true;
					break;
				case '#': is_alternate = true; break;
				case '0': pad_char = '0'; break;
				case '1' ... '9': {
					int len = 0, w = parse_width(&fmt[i], &len);
					if(w == 0) goto error;
					field_width = w;
					if(field_width < 0) field_width = 0;
					field_width_set = true;
					i += len - 1;
					break;
				}
				default: was_modifier = false; break;
			}
			if(was_modifier) {
				if(fmt[++i] == '\0') goto error;
			}
		} while(was_modifier);
		if(is_len > 2) {
			/* forbid "long long long" and longer conversions */
			goto error;
		}

		/* after modifiers, the actual conversion character. */

		/* for integral conversions, fetch the appropriate length of value.
		 *
		 * TODO: support size_t, ssize_t (i.e. the 'z', is_size, modifier)!
		 * TODO: support intmax_t, uintmax_t ('j', is_intmax)!
		 * TODO: support ptrdiff_t ('t', is_ptrdiff)!
		 */
		unsigned long long u_val = 0;
		long long s_val = 0;
		char c_val = 0;
		switch(fmt[i]) {
			case 'i': case 'd':
				/* (ignore shortness; shorts are coerced to ints anyway.) */
				if(is_len == 0) s_val = va_arg(ap, int);
				else if(is_len == 1) s_val = va_arg(ap, long);
				else if(is_len == 2) s_val = va_arg(ap, long long);
				break;

			case 'o': case 'u': case 'x': case 'X':
				if(is_len == 0) u_val = va_arg(ap, unsigned int);
				else if(is_len == 1) u_val = va_arg(ap, unsigned long);
				else if(is_len == 2) u_val = va_arg(ap, unsigned long long);
				break;

			case 'c':
				if(is_len == 0) c_val = va_arg(ap, int);
				else {
					/* TODO: do multibyte shit at some point (never) */
					goto error;
				}
				break;
		}

		/* (buf must have room for field width + 24 characters, since a 64-bit
		 * uintmax produces 22 characters and possibly a preceding zero. this
		 * will obviously fit base-10 and base-16 conversions too, since they
		 * would produce a shorter output. we'll include room for a terminator
		 * too, just to be safe. also one character in case the unsigned form is
		 * longer than the signed maximum. and a few characters for the alternate
		 * forms.)
		 */
		char buf[field_width + 32];
		const char *conv_src = buf;
		int conv_size;
		switch(fmt[i]) {
			case '%': conv_src = &fmt[i]; conv_size = 1; break;

			case 'n':
				*(va_arg(ap, int *)) = out_total;
				continue;

			case 'c':
				/* characters are pushed as ints. apparently.
				 * FIXME: this totally doesn't handle the wchar_t conversion.
				 */
				buf[0] = c_val;
				buf[1] = '\0';
				conv_size = 1;
				break;

			case 'i':
			case 'd':
				/* a 64-bit MAXINT would be 9223372036854775807-ish (19 digits),
				 * whereas a 32-bit MAXINT is 2147483647.
				 */
				convert_long(buf, s_val, field_width, pad_char, &conv_size);
				break;

			case 'u':
				convert_ulong(buf, u_val, field_width, pad_char, &conv_size);
				break;

			case 'x':
			case 'X': {
				/* 64 bits = 16 characters + terminator */
				const char *convtab =
					fmt[i] == 'x' ? "0123456789abcdef"
								  : "0123456789ABCDEF";
				int xpos;
				if(is_alternate) {
					buf[0] = '0';
					buf[1] = fmt[i];
					xpos = 2;
				} else {
					xpos = 0;
				}
				convert_hex_long(&buf[xpos], u_val, field_width, pad_char,
					convtab, &conv_size);
				conv_size += xpos;
				break;
			}

			case 'o': {
				int xpos = 0;
				if(is_alternate && u_val != 0) buf[xpos++] = '0';
				convert_oct_long(&buf[xpos], u_val, field_width, pad_char,
					&conv_size);
				conv_size += xpos;
				break;
			}

			case 'p':
				buf[0] = '0';
				buf[1] = 'x';
				convert_hex_long(&buf[2], (uintptr_t)va_arg(ap, void *),
					field_width, pad_char, "0123456789abcdef", &conv_size);
				conv_size += 2;
				break;

			case 's':
				/* FIXME: apply padding up to field_width when asked? */
				conv_src = va_arg(ap, const char *);
				if(conv_src != NULL) {
					if(field_width_set) {
						conv_size = strnlen(conv_src, field_width);
					} else {
						conv_size = strlen(conv_src);
					}
				} else {
					conv_src = "(null)";
					conv_size = 6;
				}
				break;

			default:
				goto error;
		}

		if(conv_size > 0) {
			out_total += conv_size;
			if(outpos + conv_size >= size) conv_size = size - outpos;
			memcpy(&str[outpos], conv_src, conv_size);
			outpos += conv_size;
		}
	}
	if(size > 0) {
		str[outpos >= size ? size - 1 : outpos] = '\0';
	}

	va_end(ap);
	return out_total;

error:
	strlcpy(str, "format string error", size);
	va_end(ap);
	return -1;
}

extern int kvsnprintf(char *, size_t, const char *, va_list)
	__attribute__((alias ("vsnprintf")));



/* FIXME: this hasn't been tested on a LP64 setup. there's no reason why
 * it wouldn't work there too, but.
 */
static void convert_long(
	char *buf,
	long long val,
	int width,
	char pad,
	int *count_p)
{
	if(width < 1) width = 1;
	if(val == 0) {
		for(int i=0; i<width-1; i++) buf[i] = pad;
		buf[width-1] = '0';
		buf[width] = '\0';
		*count_p = width;
		return;
	}

	const bool neg = val < 0;
	bool bump = false;
	if(neg) {
		if(val == LLONG_MIN) {
			bump = true;
			val++;
		}
		val = -val;
	}

	const int tmplen = width + 22;
	char tmp[tmplen];
	tmp[tmplen - 1] = '\0';
	int pos = tmplen - 1;
	while(val > 0) {
		int d = val % 10;
		val /= 10;
		if(bump) {
			d++;
			bump = false;
		}
		tmp[--pos] = "0123456789"[d];
	}
	/* pad until at field width, minus possible sign */
	while(tmplen - pos - 1 < width - (neg ? 1 : 0)) tmp[--pos] = pad;
	if(neg) tmp[--pos] = '-';

	int count = tmplen - pos;
	memcpy(buf, &tmp[pos], count);
	*count_p = count - 1;		/* exclude the terminator */
}


/* NOTE: same caveats apply as for convert_long(). oh, the power of
 * cut-and-paste.
 */
static void convert_ulong(
	char *buf,
	unsigned long long val,
	int width,
	char pad,
	int *count_p)
{
	if(width < 1) width = 1;
	if(val == 0) {
		for(int i=0; i<width-1; i++) buf[i] = pad;
		buf[width-1] = '0';
		buf[width] = '\0';
		*count_p = width;
		return;
	}

	const int tmplen = width + 22;
	char tmp[tmplen];
	tmp[tmplen - 1] = '\0';
	int pos = tmplen - 1;
	while(val > 0) {
		int d = val % 10;
		val /= 10;
		tmp[--pos] = "0123456789"[d];
	}
	/* pad until at field width */
	while(tmplen - pos - 1 < width) tmp[--pos] = pad;

	int count = tmplen - pos;
	memcpy(buf, &tmp[pos], count);
	*count_p = count - 1;		/* exclude the terminator */
}


static void convert_hex_long(
	char *buf,
	unsigned long long val,
	int width,
	char pad,
	const char *digits,
	int *count_p)
{
	if(width < 1) width = 1;
	if(val == 0) {
		for(int i=0; i<width-1; i++) buf[i] = pad;
		buf[width-1] = '0';
		buf[width] = '\0';
		*count_p = width;
		return;
	}

	const int tmplen = width + 22;
	char tmp[tmplen];
	tmp[tmplen-1] = '\0';
	int pos = tmplen - 1;
	while(val > 0) {
		tmp[--pos] = digits[val & 15];
		val >>= 4;
	}
	/* pad */
	while(tmplen - pos - 1 < width) tmp[--pos] = pad;

	int count = tmplen - pos;
	memcpy(buf, &tmp[pos], count);
	*count_p = count - 1;
}


static void convert_oct_long(
	char *buf,
	unsigned long long val,
	int width,
	char pad,
	int *count_p)
{
	if(width < 1) width = 1;
	if(val == 0) {
		for(int i=0; i<width-1; i++) buf[i] = pad;
		buf[width-1] = '0';
		buf[width] = '\0';
		*count_p = width;
		return;
	}

	const int tmplen = width + 23;
	char tmp[tmplen];
	tmp[tmplen-1] = '\0';
	int pos = tmplen - 1;
	while(val > 0) {
		tmp[--pos] = "0123456789"[val & 7];
		val >>= 3;
	}
	/* pad */
	while(tmplen - pos - 1 < width) tmp[--pos] = pad;
	
	int count = tmplen - pos;
	memcpy(buf, &tmp[pos], count);
	*count_p = count - 1;
}


static int parse_width(const char *str, int *len_p)
{
	int val = 0, len;
	for(len=0; str[len]!='\0'; len++) {
		if(str[len] < '0' || str[len] > '9') break;
		int digit = str[len] - '0';
		val = val * 10 + digit;
	}
	*len_p = len;
	return val;
}
