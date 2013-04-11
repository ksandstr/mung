
#include <stdlib.h>
#include <stdbool.h>
#include <stdint.h>
#include <string.h>

#include <l4/types.h>
#include <l4/ipc.h>

#include "defs.h"


bool send_quit(L4_ThreadId_t thread)
{
	L4_LoadMR(0, (L4_MsgTag_t) { .X.label = QUIT_LABEL }.raw);
	return L4_IpcSucceeded(L4_Send_Timeout(thread, TEST_IPC_DELAY));
}


/* actually a call. */
bool send_reset(L4_ThreadId_t thread)
{
	L4_LoadMR(0, (L4_MsgTag_t) { .X.label = RESET_LABEL }.raw);
	return L4_IpcSucceeded(L4_Call_Timeouts(thread, TEST_IPC_DELAY,
		TEST_IPC_DELAY));
}


/* via ccan/bdelta/test/common.h; used because there is no system RNG, and to
 * have consistent output.
 */
/*
 * Finds a pseudorandom 32-bit number from 0 to 2^32-1 .
 * Uses the BCPL linear congruential generator method.
 *
 * Used instead of system RNG to ensure tests are consistent.
 */
uint32_t rand32(uint32_t *state_p)
{
	uint32_t rand32_state = *state_p;
	rand32_state *= (uint32_t)0x7FF8A3ED;
	rand32_state += (uint32_t)0x2AA01D31;
	*state_p = rand32_state;
	return rand32_state;
}


/* generates a bunch of alphanumeric noise from a pre-defined character set,
 * with no null bytes in between. adds terminator at buf[size - 1].
 */
void random_string(char *buf, size_t size, uint32_t *seed_p)
{
	static const char *set = "abcdefghijklmnopqrstuvwxyz0123456789";
	int set_len = strlen(set);
	for(size_t i=0; i < size; i += 4) {
		uint32_t v = rand32(seed_p);
		for(int j=0; j < 4 && i + j < size; j++, v >>= 8) {
			buf[i + j] = set[(v & 0xff) % set_len];
		}
	}
	buf[size - 1] = '\0';
}
