
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

#include <ukernel/vgacon.h>


#define VGA_WIDTH 80
#define VGA_HEIGHT 24


/* horizontal position on the lowest line */
static int cursor_pos = 0;


static void scroll(uint16_t *vgamem, int lines)
{
	for(int i = 0; i + lines < VGA_HEIGHT; i++) {
		memcpy(vgamem + VGA_WIDTH * i, vgamem + VGA_WIDTH * (i + lines),
			VGA_WIDTH * sizeof *vgamem);
	}
	for(int i = VGA_HEIGHT - lines; i < VGA_HEIGHT; i++) {
		for(int j=0; j < VGA_WIDTH; j++) vgamem[VGA_WIDTH * i + j] = 0;
	}
}


void vgacon_write(void *baseaddr, const char *bytes, size_t count)
{
	uint16_t *vgamem = baseaddr;
	const int fgcolor = 7, bgcolor = 0, attr = fgcolor << 8 | bgcolor << 12;
	for(size_t i=0; i < count; i++) {
		if(bytes[i] == '\n') {
			scroll(vgamem, 1);
			cursor_pos = 0;
		} else {
			vgamem[VGA_WIDTH * (VGA_HEIGHT - 1) + cursor_pos] =
				bytes[i] | attr;
			cursor_pos++;
		}
		if(cursor_pos == VGA_WIDTH) {
			scroll(vgamem, 1);
			cursor_pos = 0;
		}
	}
	/* TODO: set blinking cursor position */
}
