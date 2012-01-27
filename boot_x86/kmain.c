void kmain(void *mbd, unsigned int magic)
{
	if(magic != 0x2BADB002) {
		/* hang! */
		return;
	}

	/* Print a letter to screen to see everything is working: */
	unsigned char *videoram = (unsigned char *)0xb8000;
	videoram[0] = 'A';
	videoram[1] = 0x07;		/* light grey (7) on black (0). */
}
