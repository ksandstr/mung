
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <stdbool.h>
#include <string.h>
#include <assert.h>
#include <alloca.h>


static FILE stdout_file = {
	.fn = { .write = &con_write },
}, stderr_file = {
	.fn = { .write = &con_write },
};

FILE *stdout = &stdout_file, *stderr = &stderr_file;


__attribute__((weak))
long con_write(void *cookie, const char *buf, size_t size) {
	for(size_t i = 0; i < size; i++) computchar(buf[i]);
	return size;
}


int vfprintf(FILE *stream, const char *fmt, va_list args)
{
	if(stream->fn.write == NULL) return vsnprintf(NULL, 0, fmt, args);
	char buffer[96], *bufptr = buffer;
	va_list copy; va_copy(copy, args);
	int n = vsnprintf(buffer, sizeof buffer, fmt, args);
	if(n >= sizeof buffer) {
		bufptr = alloca(n + 1);
		n = vsnprintf(bufptr, n + 1, fmt, copy);
	}
	va_end(copy);
	return fwrite(bufptr, 1, n, stream);
}


int fprintf(FILE *stream, const char *fmt, ...)
{
	va_list al;
	va_start(al, fmt);
	int n = vfprintf(stream, fmt, al);
	va_end(al);
	return n;
}


int printf(const char *fmt, ...)
{
	va_list al;
	va_start(al, fmt);
	int n = vfprintf(stdout, fmt, al);
	va_end(al);
	return n;
}


int snprintf(char *buf, size_t size, const char *fmt, ...)
{
	va_list al;
	va_start(al, fmt);
	int n = vsnprintf(buf, size, fmt, al);
	va_end(al);
	return n;
}


int puts(const char *s) {
	int n = fputs(s, stdout), m = fputc('\n', stdout);
	return n < 0 || m < 0 ? EOF : n + m;
}


int fputs(const char *s, FILE *stream) {
	return fwrite(s, 1, strlen(s), stream);
}


int putchar(int c) {
	return fputc(c, stdout);
}


int fputc(int c, FILE *stream)
{
	if(stream->fn.write == NULL) return (unsigned char)c;
	if(stream->bpos == sizeof stream->buffer) {
		if(fflush(stream) < 0) return EOF;
		assert(stream->bpos == 0);
	}
	stream->buffer[stream->bpos++] = c;
	return c == '\n' && fflush(stream) < 0 ? EOF : (unsigned char)c;
}


size_t fwrite(const void *ptr, size_t size, size_t nmemb, FILE *stream)
{
	if(stream->fn.write == NULL || size * nmemb == 0) return nmemb;
	/* brute force that works is worth burning a little bit of CPU. */
	const unsigned char *mem = ptr, *end = mem + size * nmemb;
	while(fputc(*mem, stream) >= 0 && ++mem < end) { /* repeat */ }
	size_t done = mem - (const unsigned char *)ptr;
	return size > 1 ? done / size : done;
}


int fflush(FILE *stream)
{
	long n;
	if(stream == NULL) {
		if(stdout_file.bpos > 0) fflush(&stdout_file);
		if(stderr_file.bpos > 0) fflush(&stderr_file);
		n = 0;
	} else if(stream->fn.write == NULL) {
		assert(stream->bpos == 0);
		n = 0;
	} else {
		n = (*stream->fn.write)(stream->cookie, stream->buffer, stream->bpos);
		if(n > 0) {
			if(n < stream->bpos) {
				memmove(stream->buffer, stream->buffer + n,
					sizeof stream->buffer + (stream->bpos - n));
			}
			assert(n <= stream->bpos);
			stream->bpos -= n;
		}
	}
	return n;
}


int fclose(FILE *stream)
{
	int result = fflush(stream);
	if(stream->fn.close != NULL) {
		result = (*stream->fn.close)(stream->cookie);
	}
	if(stream != &stdout_file && stream != &stderr_file) {
		free(stream);
	}
	return result;
}


FILE *fopencookie(void *cookie, const char *mode, cookie_io_functions_t fns)
{
	/* @mode is disregarded. */
	FILE *f = malloc(sizeof *f);
	if(f != NULL) *f = (FILE){ .fn = fns, .cookie = cookie };
	return f;
}
