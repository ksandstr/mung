
CFLAGS=-O2 -Wall -march=native -std=gnu99 -I include -I include/fake_clib \
	-I ~/src/ccan \
	-fno-builtin -nostdlib


all: tags image.bin


clean:
	@rm -f *.o


distclean: clean
	@rm -f image.bin tags
	@rm -rf .deps


tags: $(wildcard *.[ch])
	@ctags -R .


image.bin: linker.ld loader.o isr.o kmain.o printf.o fake_stdio.o string.o \
		dlmalloc.o heap.o slab.o pic.o timer.o thread.o context.o gdt.o
	@echo "  LD $@"
	@ld -T linker.ld -o $@ $(filter %.o,$^)


%.o: %.c
	@echo "  CC $@"
	@$(CC) -m32 -c -o $@ $< $(CFLAGS) -nostartfiles -nodefaultlibs -MMD
	@test -d .deps || mkdir -p .deps
	@mv $(<:.c=.d) .deps/


%.o: %-32.S
	@echo "  AS $@"
	@as --32 -o $@ $<


dlmalloc.o: CFLAGS += -Wno-unused-variable


.deps:
	@mkdir -p .deps


include $(wildcard .deps/*.d)
