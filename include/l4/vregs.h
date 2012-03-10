
/* <l4/vregs.h> is not seen in L4Ka::Pistachio. */

#ifndef __L4__VREGS_H__
#define __L4__VREGS_H__

#include <l4/types.h>


#define L4_VREG(base, pos) (((L4_Word_t *)(base))[(pos)])


/* names of TCRs and VRs in an UTCB segment, tied to word offsets. */

#define L4_TCR_PAGER (-12)
#define L4_TCR_MR(n) (n)
#define L4_TCR_BR(n) (-(n) - 16)


#endif
