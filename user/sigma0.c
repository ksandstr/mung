void _start(void) {
	/* this causes a huge ugly page fault. */
	char *ptr = (char *)0xb0a7face;
	*ptr = '@';
}
