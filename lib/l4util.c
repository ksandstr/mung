
#include <stdlib.h>
#include <stdint.h>

#include <l4/types.h>
#include <l4/message.h>


/* TODO: this needs a "max_words" parameter for safety! */
size_t stritemlen(L4_StringItem_t *si)
{
	size_t len = 0;
	L4_StringItem_t *prev;
	do {
		prev = si;
		len += si->X.string_length * L4_Substrings(si);
		L4_Word_t *wp = (L4_Word_t *)si;
		si = (L4_StringItem_t *)&wp[L4_Substrings(si) + 1];
	} while(L4_CompoundString(prev));
	return len;
}
