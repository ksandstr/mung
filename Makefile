
CFLAGS=-O2 -Wall -std=gnu99 -I include -fno-builtin -nostdlib


all: tags image.bin


clean:
	@rm -f *.o

distclean: clean
	@rm -f image.bin tags

tags: $(wildcard *.[ch])
	@ctags -R .


image.bin: loader.o isr.o kmain.o printf.o
	@echo "  LD $@"
	@ld -T linker.ld -o $@ $^


%.o: %.c
	@echo "  CC $@"
	@$(CC) -m32 -c -o $@ $< $(CFLAGS) -nostartfiles -nodefaultlibs


%.o: %-32.S
	@echo "  AS $@"
	@as --32 -o $@ $<
