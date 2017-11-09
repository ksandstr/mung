
/* facsimiles of stdio routines for kernel-space. */

#ifndef SEEN_FAKE_CLIB_STDIO_H
#define SEEN_FAKE_CLIB_STDIO_H

#include <stddef.h>
#include <stdarg.h>


struct fake_clib_file {
	unsigned char foo;
};

typedef struct fake_clib_file FILE;


extern int printf(const char *fmt, ...)
	__attribute__((format(printf, 1, 2)));

extern int fprintf(FILE *stream, const char *fmt, ...)
	__attribute__((format(printf, 2, 3)));

extern int vfprintf(FILE *stream, const char *fmt, va_list args);

extern int snprintf(char *buf, size_t size, const char *fmt, ...)
	__attribute__((format(printf, 3, 4)));
extern int sprintf(char *buf, const char *fmt, ...)
	__attribute__((format(printf, 2, 3)));

extern int vsnprintf(
	char *buf,
	size_t size,
	const char *fmt,
	va_list arg_list);


extern FILE *stdout, *stderr;


/* interface for consumers of lib/fake_stdio.c */
extern void con_putstr(const char *string);

/* some compilers change a simple printf() to a puts(). likewise for other
 * things.
 */
extern int puts(const char *s);
extern int fputs(const char *s, FILE *stream);
extern int putchar(char c);
extern int fputc(char c, FILE *stream);
extern size_t fwrite(const void *ptr, size_t size, size_t nmemb, FILE *stream);

extern int fflush(FILE *stream);


#endif
