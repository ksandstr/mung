
#include <stdio.h>


static FILE stdout_file, stderr_file;

FILE *stdout = &stdout_file, *stderr = &stderr_file;


void fprintf(FILE *stream, const char *fmt, ...)
{
	/* TODO: implement in terms of vprintf()! */
}
