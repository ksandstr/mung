
/* some stdio routines for kernel-space. nowhere near standard-complete. */

#ifndef SEEN_MUNG_STDIO_H
#define SEEN_MUNG_STDIO_H

#include <stddef.h>
#include <stdarg.h>
#include <stdint.h>


#define EOF -1
#define BUFSIZ 256	/* very sufficient for line buffering */


typedef long (*cookie_read_function_t)(void *, char *, size_t);
typedef long (*cookie_write_function_t)(void *, const char *, size_t);
typedef int (*cookie_seek_function_t)(void *, int64_t *, int);
typedef int (*cookie_close_function_t)(void *);


typedef struct {
	cookie_read_function_t read;
	cookie_write_function_t write;
	cookie_seek_function_t seek;
	cookie_close_function_t close;
} cookie_io_functions_t;

typedef struct __clib_file {
	cookie_io_functions_t fn;
	void *cookie;
	/* there's no setbuf(3), so this is always line buffering,
	 * even on stderr.
	 */
	short bpos;
	char buffer[BUFSIZ];
} FILE;


extern FILE *fopencookie(void *cookie, const char *mode,
	cookie_io_functions_t io_funcs);

extern int printf(const char *fmt, ...)
	__attribute__((format(printf, 1, 2)));

extern int fprintf(FILE *stream, const char *fmt, ...)
	__attribute__((format(printf, 2, 3)));

extern int vfprintf(FILE *stream, const char *fmt, va_list args);

extern int snprintf(char *buf, size_t size, const char *fmt, ...)
	__attribute__((format(printf, 3, 4)));
extern int sprintf(char *buf, const char *fmt, ...)
	__attribute__((format(printf, 2, 3)));

extern int vsnprintf(char *buf, size_t size,
	const char *fmt, va_list arg_list);


/* no stdin because no input channel. */
extern FILE *stdout, *stderr;


/* GCC changes simple printf() to puts(), etc. */
extern int puts(const char *s);
extern int fputs(const char *s, FILE *stream);
extern int putchar(int c);
extern int fputc(int c, FILE *stream);
extern size_t fwrite(const void *ptr,
	size_t size, size_t nmemb,
	FILE *stream);

extern int fflush(FILE *stream);
extern int fclose(FILE *stream);


/* output to serial port one character at a time. implemented by lib/ser_io.c,
 * declared here for lack of a better file.
 */
extern void computchar(unsigned char ch);


/* weak symbol overridden by consumers of stdio.o to not output to the
 * default serial port.
 */
extern long con_write(void *cookie, const char *buf, size_t size);

#endif
