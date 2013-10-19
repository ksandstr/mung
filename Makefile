
export CFGDIR:=$(abspath .)
include config.mk

.PHONY: all clean distclean check qcheck


all: tags
	+@make -C lib all
	+@make -C user all
	+@make -C mbiloader all
	+@make ia32-kernel

clean:
	@rm -f *.o $(CLEAN_PATS)
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


# TODO: skip testbench self-tests only if the perl source, and its test code,
# hasn't changed since "testbench" and "ia32-kernel". (bit of a tall order,
# this.)
qcheck: all
	@user/testbench/report.pl


check: all
	+@make -C user/testbench check
	@user/testbench/report.pl


slowcheck:
	+@make check TEST_SLOW=1


slowercheck:
	+@make check TEST_SLOWER=1


tags: $(shell find . -iname "*.[ch]" -or -iname "*.p[lm]")
	@ctags -R *


ia32-kernel: linker.ld loader.o kmain.o kip.o cpu.o heap.o thread.o kth.o \
		trace.o sched.o exception.o space.o ipc.o ipc_typed.o mapdb.o \
		acpi.o gdt.o idt.o irq.o pic.o apic.o isr.o timer.o context.o \
		dlmalloc.o ccan-htable.o ccan-list.o
	@echo "  LD $@"
	@$(LD) -T linker.ld -o $@ $(filter %.o,$^) \
		-L lib -lukernel_util \
		$(shell gcc $(CFLAGS) -print-libgcc-file-name)


dlmalloc.o: CFLAGS += -Wno-unused-variable


include $(wildcard .deps/*.d)
