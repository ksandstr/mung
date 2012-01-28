
CFLAGS=-O2 -Wall -std=gnu99


all: tags image.bin


clean:
	@rm -f *.o

distclean: clean
	@rm -f image.bin tags

tags: $(wildcard *.[ch])
	@ctags -R .


image.bin: loader.o isr.o kmain.o printf.o
	ld -T linker.ld -o $@ $^


%.o: %.c
	$(CC) -m32 -c -o $@ $< $(CFLAGS) -nostdlib -fno-builtin \
		-nostartfiles -nodefaultlibs


%.o: %-32.S
	as --32 -o $@ $<
