/* BogoMips-based delay loop for the testbench personality. */

#include <stdio.h>
#include <stdint.h>
#include <assert.h>

#include <l4/types.h>
#include <l4/thread.h>

#include "defs.h"


static const int hz = 1000;			/* TODO: get from KIP! */
unsigned long iters_per_tick = 0;


/* from Linux, via Wikipedia */
/* simple loop based delay: */
void delay_loop(unsigned long loops)
{
	int d0;

	__asm__ __volatile__(
		"\tjmp 1f\n"
		".align 16\n"
		"1:\tjmp 2f\n"
		".align 16\n"
		"2:\tdecl %0\n\tjns 2b"
		:"=&a" (d0)
		:"0" (loops));
}


void nsleep(unsigned long nanoseconds)
{
	int nano_per_hz = 1000000000 / hz,
		ticks = nanoseconds / nano_per_hz;
	if(ticks > 2 && false) {
		/* FIXME: this doesn't work, does it... */
		L4_Clock_t start = L4_SystemClock();
		do {
			delay_loop(iters_per_tick / hz);
		} while(start.raw + ticks < L4_SystemClock().raw);
	} else {
		uint64_t t = nanoseconds;
		t *= iters_per_tick;
		t /= nano_per_hz;
		delay_loop(t);
	}
}


void usleep(unsigned long microseconds)
{
	/* TODO: proper implementation */
	nsleep(microseconds * 1000);
}


static bool attempt(unsigned long loops, int ticks)
{
	L4_Clock_t start = L4_SystemClock();
	do {
		delay_loop(100);
	} while(L4_SystemClock().raw < start.raw + 1);

	delay_loop(loops);
	L4_Clock_t end = L4_SystemClock();

	/* this biases the result upward. that's OK; nsleep() and usleep() are
	 * only defined not to wake before the time is up.
	 */
	return start.raw + ticks < end.raw;
}


/* uses a binary search algorithm to find the number of iterations for
 * sleeping for exactly one clock tick.
 */
static uint32_t measure(int ticks)
{
	/* find a lower bound. */
	long lower = 500000 * ticks;
	while(lower > 0 && attempt(lower, ticks)) {
		lower -= 100000 * ticks;
	}
	if(lower < 0) lower = 0;

	/* and an upper bound. */
	unsigned long upper = lower * ticks;
	while(upper < 2000000000 && !attempt(upper, ticks)) {
		upper += 15000000;
	}
	if(upper >= 2000000000) {
		printf("%s: failed to find an upper bound\n", __func__);
		return 0;
	}

	assert(attempt(upper, ticks) != attempt(lower, ticks));
	for(int i=0; i < 100; i++) {
		long mid = (upper + lower) / 2;
		if(upper - lower <= 300 || upper == lower) return mid / ticks;
		bool m = attempt(mid, ticks);
		if(m != attempt(lower, ticks)) {
			upper = mid;
		} else {
			lower = mid;
		}
	}

	return 0;
}


void calibrate_delay_loop(void)
{
	printf("calibrating delay loop...\n");
	iters_per_tick = measure(15);
	assert(iters_per_tick > 0);
	printf("  %lu.%02lu BogoMIPS (%lu iters / tick)\n",
		iters_per_tick / (500000 / hz),
		(iters_per_tick / (5000 / hz)) % 100,
		iters_per_tick);

#if 0
	/* testing. */
	for(int i=1; i<=25; i++) {
		L4_Clock_t start = L4_SystemClock();
		do {
			delay_loop(200);
		} while(L4_SystemClock().raw < start.raw + 1);

		int ticks = i * 2;
		delay_loop(iters_per_tick * ticks);
		L4_Clock_t end = L4_SystemClock();
		if(start.raw + ticks != end.raw) {
			printf("wanted to wait for %d ticks, waited for %d\n",
				ticks, (int)(end.raw - start.raw));
		}
	}
#endif
}
