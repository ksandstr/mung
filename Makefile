
CFLAGS=-O2 -Wall -std=gnu99 -I include -I include/fake_clib -fno-builtin -nostdlib


all: tags image.bin


clean:
	@rm -f *.o

distclean: clean
	@rm -f image.bin tags

tags: $(wildcard *.[ch])
	@ctags -R .


image.bin: loader.o isr.o kmain.o printf.o fake_stdio.o string.o dlmalloc.o \
		heap.o
	@echo "  LD $@"
	@ld -T linker.ld -o $@ $^


%.o: %.c
	@echo "  CC $@"
	@$(CC) -m32 -c -o $@ $< $(CFLAGS) -nostartfiles -nodefaultlibs


%.o: %-32.S
	@echo "  AS $@"
	@as --32 -o $@ $<


dlmalloc.o: CFLAGS += -Wno-unused-variable
