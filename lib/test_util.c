
/* utility routines that're useful for testing. exported
 * in <ukernel/ktest.h>, and hopefully somewhere in the userspace testbench as
 * well (eventually. TODO?)
 */

#include <ukernel/util.h>


static unsigned fac_worker(unsigned acc, unsigned x) {
	if(x == 0) return acc; else return fac_worker(acc * x, x - 1);
}


/* computes x! */
unsigned factorial(unsigned x) {
	if(x == 0) return 0;
	return fac_worker(1, x);
}


/* generates a permutation of n integers [0, n). outputs guaranteed distinct
 * while @perm < n! .
 */
void gen_perm(unsigned *buf, unsigned n, unsigned perm)
{
	if(n == 0) return;
	for(unsigned i=0; i < n; i++) buf[i] = i;
	for(unsigned i=0; i < n - 1; i++) {
		unsigned x = perm % (n - i);
		perm /= (n - i);
		SWAP(unsigned, buf[i], buf[i + x]);
	}
}
