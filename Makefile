
export CFGDIR:=$(abspath .)
include config.mk


all: tags image.bin
	+@make -C user all


clean:
	@rm -f *.o
	+@make -C user clean


distclean: clean
	@rm -f image.bin tags
	@rm -rf .deps
	+@make -C user distclean


tags: $(wildcard *.[ch])
	@ctags -R .


image.bin: linker.ld loader.o isr.o kmain.o printf.o fake_stdio.o string.o \
		dlmalloc.o heap.o slab.o pic.o timer.o thread.o context.o \
		gdt.o idt.o exception.o hash.o space.o ipc.o mapdb.o kip.o \
		ccan-htable.o rbtree.o
	@echo "  LD $@"
	@ld -T linker.ld -o $@ $(filter %.o,$^)


# build CCAN sources as though they were in the kernel tree.
ccan-%.o ::
	@echo "  CC $@ <ccan>"
	@$(CC) -m32 -c -o $@ $(CCAN_DIR)/ccan/$*/$*.c $(CFLAGS) -nostartfiles -nodefaultlibs


dlmalloc.o: CFLAGS += -Wno-unused-variable


include $(wildcard .deps/*.d)
