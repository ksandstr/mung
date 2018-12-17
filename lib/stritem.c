
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>

#include <l4/types.h>
#include <l4/message.h>

#include <ukernel/util.h>


void stritem_first(
	struct stritem_iter *it,
	L4_StringItem_t *si, int n_words)
{
	assert(L4_IsStringItem(si));
	assert(n_words >= 0 && n_words < 64);

	*it = (struct stritem_iter){
		.words = si->raw, .hdr = 0, .sub = 1, .max = n_words - 1,
		.ptr = (uintptr_t)si->X.str.substring_ptr[0],
		.len = si->X.string_length,
	};
}


bool stritem_next(struct stritem_iter *it)
{
	if(it->hdr + it->sub > it->max) {
		/* out of range */
		return false;
	}

	L4_StringItem_t *si = (L4_StringItem_t *)&it->words[it->hdr];
	assert(L4_IsStringItem(si));
	if(it->sub >= L4_Substrings(si) && !L4_CompoundString(si)) {
		/* last substring of last group. (most commonly at end of simple
		 * string item.)
		 */
		return false;
	} else if(it->sub < L4_Substrings(si)) {
		/* next substring. */
		it->ptr = (uintptr_t)si->X.str.substring_ptr[it->sub++];
		assert(it->len == si->X.string_length);
		return true;
	} else {
		/* next substring group. */
		assert(L4_CompoundString(si));
		L4_StringItem_t *next = (void *)&si->X.str.substring_ptr[it->sub];
		it->hdr = next->raw - it->words;
		it->sub = 0;
		it->len = next->X.string_length;
		return stritem_next(it);
	}
}


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
