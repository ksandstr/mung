
/* support interface for µIDL */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <l4/vregs.h>


void muidl_supp_alloc_context(unsigned int length)
{
	void *utcb = __L4_Get_UtcbAddress();
	void **pp = (void *)&L4_VREG(utcb, L4_TCR_THREADWORD0);
	if(*pp == NULL) {
		if(length < 64) length = 64;
		*pp = malloc(length);
		if(*pp == NULL) {
			printf("%s: can't alloc length=%u bytes!\n", __func__, length);
			abort();
		}
		memset(*pp, '\0', length);
	}
}


void *muidl_supp_get_context(void) {
	return (void *)L4_VREG(__L4_Get_UtcbAddress(), L4_TCR_THREADWORD0);
}
