
all: image.bin


image.bin: loader.o kmain.o
	ld -T linker.ld -o $@ $^


%.o: %.c
	$(CC) -m32 -c -o $@ $< -Wall -std=gnu99 -nostdlib -fno-builtin \
		-nostartfiles -nodefaultlibs


%.o: %-32.S
	as --32 -o $@ $<
