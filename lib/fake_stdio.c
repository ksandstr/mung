
#include <stdio.h>
#include <stdarg.h>
#include <stdbool.h>
#include <string.h>
#include <assert.h>


static FILE stdout_file, stderr_file;

FILE *stdout = &stdout_file, *stderr = &stderr_file;


int vfprintf(FILE *stream, const char *fmt, va_list args)
{
	char buffer[256];
	int n = vsnprintf(buffer, sizeof(buffer), fmt, args);
	if(n < 0) con_putstr("[vfprintf error]\n"); else con_putstr(buffer);
	return n;
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


int puts(const char *s)
{
	con_putstr(s);
	con_putstr("\n");
	return 0;
}


int fputs(const char *s, FILE *stream) {
	/* ignore @stream entirely. */
	return puts(s);
}


int putchar(char c)
{
	char s[2] = { c, '\0' };
	con_putstr(s);
	return c;
}


int fputc(char c, FILE *stream) {
	/* wheeeeee */
	return putchar(c);
}


size_t fwrite(const void *ptr, size_t size, size_t nmemb, FILE *stream)
{
	assert(stream == stdout || stream == stderr);
	const char *cs = ptr;
	for(size_t i=0; i < size * nmemb; i++) fputc(cs[i], stream);
	return nmemb;
}
