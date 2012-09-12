
/* minimal, nonconforming <ctype.h> */

#ifndef _CTYPE_H
#define _CTYPE_H

#include <stddef.h>


static inline int isspace(int c) {
	switch(c) {
		case ' ': case '\t': case '\n':
			return 1;
		default:
			return 0;
	}
}


static inline int isdigit(int c) {
	return c >= '0' && c <= '9';
}


/* NOTE: none of this shit works!, quoth captain kirk */

extern int isalnum(int c);
extern int isalpha(int c);
extern int isascii(int c);
extern int isblank(int c);
extern int iscntrl(int c);
extern int isgraph(int c);
extern int islower(int c);
extern int isprint(int c);
extern int ispunct(int c);
extern int isupper(int c);
extern int isxdigit(int c);

#endif
