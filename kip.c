
/* compose a 32-bit little-endian kernel interface page. */

#include <stdint.h>
#include <string.h>

#include <l4/types.h>
#include <ukernel/mm.h>
#include <ukernel/syscall.h>


/* let's use interrupt 0x8f, for now.
 *
 * TODO: use whatever L4Ka::Pistachio uses when SYSENTER/SYSCALL
 * aren't present.
 */
static void make_syscall_stub(void *start, int *len_p, int sc_num)
{
	int p = 0;
	uint8_t *mem = start;
	mem[p++] = 0xbb;	/* MOV EBX, imm32 */
	*(uint32_t *)&mem[p] = sc_num; p += 4;
	mem[p++] = 0xcd;	/* INT */
	mem[p++] = 0x8f;
	mem[p++] = 0xc3;	/* RET */
	*len_p = p;
}


/* extra clever thing where the syscall stub never enters the kernel.
 * instead, the clock tick is stored in the last 8 bytes of the KIP.
 */
static void make_systemclock_stub(void *start, int *len_p)
{
	int p = 0;
	uint8_t *mem = start;
	mem[p++] = 0xe8;			/* CALL rel32 */
	*(uint32_t *)&mem[p] = 6;	/* past the next instructions */
	p += 4;
	mem[p++] = 0x8b;			/* MOV edx, [eax + 4] */
	mem[p++] = 0x50;
	mem[p++] = 0x04;
	mem[p++] = 0x8b;			/* MOV eax, [eax] */
	mem[p++] = 0x00;
	mem[p++] = 0xc3;			/* RET */

	/* the "read & massage EIP" sequence */
	mem[p++] = 0x8b;			/* MOV eax, [esp] */
	mem[p++] = 0x04;
	mem[p++] = 0x24;
	mem[p++] = 0x0d;			/* OR eax, 0xfff */
	*(uint32_t *)&mem[p] = 0xfffU;
	p += 4;
	mem[p++] = 0x83;			/* AND eax, ~7 */
	mem[p++] = 0xe0;
	mem[p++] = 0xf8;
	mem[p++] = 0xc3;			/* RET */

	*len_p = p;
}


void make_kip(void *mem)
{
	memset(mem, 0, PAGE_SIZE);

	memcpy(mem, "L4\346K", 4);
	/* L4.X2, rev 5 (XXX: what the heck is revision 5?) */
	*(L4_Word_t *)(mem + 0x4) = 0x84 << 24 | 5 << 16;
	*(L4_Word_t *)(mem + 0x8) = 0;	/* API flags: 32-bit, little-endian */
	L4_Word_t *kerndescptr_p = mem + 0xc,
		*procdescptr_p = mem + 0xbc,
		*memoryinfo_p = mem + 0x54;
	*(L4_Word_t *)(mem + 0xa8) = 0; // UtcbInfo;
	*(L4_Word_t *)(mem + 0xac) = 0; // kipareainfo;
	*(L4_Word_t *)(mem + 0xb8) = 0; // bootinfo;

	*(L4_Word_t *)(mem + 0xc0) = 0; // clockinfo;
	*(L4_Word_t *)(mem + 0xc4) = 0; // threadinfo;
	*(L4_Word_t *)(mem + 0xc8) = 0; // pageinfo;
	*(L4_Word_t *)(mem + 0xcc) = 0; // processorinfo;

	/* syscall stubs. */
	static const struct {
		uint16_t sc_num;
		uint16_t offset;
	} syscalls[] = {
		/* not included:
		 * SC_SYSTEMCLOCK, implemented as a soft syscall
		 *
		 * (TODO: SC_EXCHANGEREGISTERS, SC_MEMORYCONTROL; have interrupts of
		 * their very own.)
		 */
		{ SC_IPC, 0xe0 },
		{ SC_LIPC, 0xe4 },
		{ SC_UNMAP, 0xe8 },
		{ SC_EXCHANGEREGISTERS, 0xec },
		{ SC_THREADSWITCH, 0xf4 },
		{ SC_SCHEDULE, 0xf8 },
		{ SC_SPACECONTROL, 0xd0 },
		{ SC_THREADCONTROL, 0xd4 },
		{ SC_PROCESSORCONTROL, 0xd8 },
		{ SC_MEMORYCONTROL, 0xdc },
	};
	const int num_syscalls = sizeof(syscalls) / sizeof(syscalls[0]);
	int kip_pos = 0x100;
	for(int i=0; i < num_syscalls; i++) {
		int len = 0;
		make_syscall_stub(mem + kip_pos, &len, syscalls[i].sc_num);
		*(L4_Word_t *)(mem + syscalls[i].offset) = kip_pos;
		kip_pos += (len + 3) & ~3;
	}
	/* slightly more special ones. */
	int len = 0;
	make_systemclock_stub(mem + kip_pos, &len);
	*(L4_Word_t *)(mem + 0xf0) = kip_pos;
	kip_pos += (len + 3) & ~3;

	void *memdesc_base = mem + kip_pos;
	/* TODO: add # of memory descriptors in the lower 16 bits */
	*memoryinfo_p = (L4_Word_t)(memdesc_base - mem) << 16;

	/* TODO: fill these in */
	*procdescptr_p = 0;
	*kerndescptr_p = 0;
}
