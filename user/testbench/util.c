
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


bool send_delay(
	L4_ThreadId_t thread,
	L4_Time_t delay,
	int repeat,
	bool spin)
{
	L4_LoadMR(0, (L4_MsgTag_t){ .X.label = DELAY_LABEL, .X.u = 3 }.raw);
	L4_LoadMR(1, delay.raw);
	L4_LoadMR(2, repeat);
	L4_LoadMR(3, spin ? 1 : 0);
	return L4_IpcSucceeded(L4_Call_Timeouts(thread, TEST_IPC_DELAY,
		TEST_IPC_DELAY));
}


static void fixture_teardown_common(int pid, L4_ThreadId_t tid)
{
	if(!send_quit(tid)) {
		printf("send_quit() failed, ec %#lx\n", L4_ErrorCode());
		abort();
	}

	/* hacky hacky. provoke an unknown ipc status, and quit of the helper
	 * thread. 0xcbad is seen in IDL_FIXTURE()'s dispatch-calling loop.
	 */
	L4_LoadMR(0, (L4_MsgTag_t){ .X.label = 0xcbad }.raw);
	L4_MsgTag_t tag = L4_Send_Timeout(tid, TEST_IPC_DELAY);
	if(L4_IpcFailed(tag)) {
		printf("%s: ec %#lx\n", __func__, L4_ErrorCode());
		abort();
	}

	if(pid > 0) {
		int st, dead = wait(&st);
		if(dead != pid) {
			printf("%s: failed to wait on p%d (dead=%d)\n",
				__func__, pid, dead);
		}
	} else {
		join_thread(tid);
	}
}


void idl_fixture_teardown(L4_ThreadId_t tid) {
	fixture_teardown_common(-1, tid);
}


void idl_fixture_teardown_fork(int pid, L4_ThreadId_t tid) {
	fixture_teardown_common(pid, tid);
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
