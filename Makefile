
export CFGDIR:=$(abspath .)
include config.mk


all: tags
	+@make -C lib all
	+@make -C user all
	+@make -C mbiloader all
	+@make ia32-kernel

clean:
	@rm -f *.o
	+@make -C user clean
	+@make -C mbiloader clean
	+@make -C lib clean


distclean: clean
	@rm -f ia32-kernel tags
	@rm -rf .deps
	+@make -C user distclean
	+@make -C mbiloader distclean
	+@make -C lib distclean
	@find . -name ".deps" -type d -print|xargs rm -rf


tags: $(shell find . -iname "*.[ch]" -print)
	@ctags -R *


# TODO: remove the GCC-ism for compatiblity with clang. (ha ha ha ha ha ha)
ia32-kernel: linker.ld loader.o isr.o kmain.o kip.o cpu.o \
		dlmalloc.o heap.o pic.o timer.o thread.o context.o trace.o \
		sched.o gdt.o idt.o exception.o irq.o space.o ipc.o mapdb.o \
		ccan-htable.o ccan-list.o
	@echo "  LD $@"
	@ld -T linker.ld -o $@ $(filter %.o,$^) \
		-L lib -lukernel_util \
		$(shell gcc $(CFLAGS) -print-libgcc-file-name)


dlmalloc.o: CFLAGS += -Wno-unused-variable


include $(wildcard .deps/*.d)
