
#include <stdio.h>
#include <stdarg.h>
#include <stdbool.h>
#include <string.h>


static FILE stdout_file, stderr_file;

FILE *stdout = &stdout_file, *stderr = &stderr_file;


int vfprintf(FILE *stream, const char *fmt, va_list args)
{
	char buffer[256];
	int n = vsnprintf(buffer, sizeof(buffer), fmt, args);
	if(stream == stderr && n > 0) {
		static bool is_first = true;
		if(is_first) {
			printf("[ERR]: ");
			is_first = false;
		}
		if(buffer[n - 1] == '\n') is_first = true;
	}
	con_putstr(buffer);
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
