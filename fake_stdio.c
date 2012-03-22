
#include <stdio.h>
#include <stdarg.h>
#include <string.h>


static FILE stdout_file, stderr_file;

FILE *stdout = &stdout_file, *stderr = &stderr_file;


/* from kmain.c */
extern void computchar(unsigned char ch);


void vfprintf(FILE *stream, const char *fmt, va_list args)
{
	if(stream == stderr) {
		printf("[ERR]: ");
	}

	char buffer[256];
	vsnprintf(buffer, sizeof(buffer), fmt, args);
	for(int i=0, len=strlen(buffer); i < len; i++) {
		computchar(buffer[i]);
	}
}


void fprintf(FILE *stream, const char *fmt, ...)
{
	va_list al;
	va_start(al, fmt);
	vfprintf(stream, fmt, al);
	va_end(al);
}


void printf(const char *fmt, ...)
{
	va_list al;
	va_start(al, fmt);
	vfprintf(stdout, fmt, al);
	va_end(al);
}
