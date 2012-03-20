/* quasi-runtime functions for the microkernel. */

#ifndef SEEN_UKERNEL_MISC_H
#define SEEN_UKERNEL_MISC_H

#include <stdint.h>
#include <ccan/compiler/compiler.h>


#define CHECK_FLAG(mask, bit) (((mask) & (bit)) != 0)

#define PURE __attribute__((pure))

#define MIN(t, a, b) ({ t __a = (a), __b = (b); __a < __b ? (t)__a : (t)__b; })
#define MAX(t, a, b) ({ t __a = (a), __b = (b); __a > __b ? (t)__a : (t)__b; })


struct page;

/* from kmain.c */
extern struct page *kip_page;
extern void NORETURN panic(const char *message);

/* from hash.c */
extern uint32_t int_hash(uint32_t key);
extern uint32_t ptr_hash(const void *ptr);


/* from kip.c */
extern void make_kip(void *mem);

#endif
