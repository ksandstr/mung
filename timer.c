
/* code for dealing with the programmable interrupt timer (PIT) present on
 * PC-style x86 systems.
 */

#include <stdio.h>
#include <stdint.h>
#include <ukernel/ioport.h>
#include <ukernel/timer.h>


#define PIT_CH0_DATA 0x40
#define PIT_CH1_DATA 0x41
#define PIT_CH2_DATA 0x42
#define PIT_MODECTRL 0x43

/* MODECTRL bits */
#define MC_CH0 0x00			/* bits 6..7: channel select */
#define MC_CH1 0x40
#define MC_CH2 0x80
#define MC_READBACK 0xc0
#define MC_LATCH_CT 0x00	/* bits 4..5: access mode */
#define MC_LOW_ONLY 0x10
#define MC_HI_ONLY 0x20
#define MC_HI_LOW 0x30
#define MC_ON_COUNT 0x00	/* bits 1..3: operating mode */
#define MC_ONESHOT 0x02
#define MC_RATEGEN 0x04
#define MC_SQ_WAVE 0x06
#define MC_SSTROBE 0x08
#define MC_HSTROBE 0x0a
/* last two map to MC_RATEGEN and MC_SQ_WAVE */
#define MC_BCD_MODE 0x01	/* bit 0: set for four-digit BCD, clear for 16-bit binary */


/* sets a 1000 hz timer on PIT channel 0 (IRQ 0). */
void setup_timer_ch0(void)
{
	const uint16_t reload = 1193;	/* 1193 * 0.8381 µs = 999.85 ms */

	outb(PIT_MODECTRL, MC_CH0 | MC_HI_LOW | MC_SQ_WAVE);
	outb(PIT_CH0_DATA, reload & 0xff);
	outb(PIT_CH0_DATA, reload >> 8);
}
