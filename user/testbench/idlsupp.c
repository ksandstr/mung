
/* support interface for µIDL */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <ccan/likely/likely.h>

#include "defs.h"


static int muidl_ctx_key = -1;


void muidl_supp_alloc_context(unsigned int length)
{
	if(muidl_ctx_key == -1) {
		tsd_key_create(&muidl_ctx_key, &free);
	}
	void *ptr = tsd_get(muidl_ctx_key);
	if(ptr == NULL) {
		if(length < 64) length = 64;
		ptr = malloc(length);
		if(ptr == NULL) {
			printf("%s: can't alloc length=%u bytes!\n", __func__, length);
			abort();
		}
		memset(ptr, '\0', length);
		tsd_set(muidl_ctx_key, ptr);
		assert(tsd_get(muidl_ctx_key) == ptr);
	}
}


void *muidl_supp_get_context(void) {
	if(unlikely(muidl_ctx_key < 0)) return NULL;
	return tsd_get(muidl_ctx_key);
}
