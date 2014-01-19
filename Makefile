
export CFGDIR:=$(abspath .)
include config.mk

# the kernel doesn't support use of coprocessors in kernel mode.
CFLAGS+=-mno-mmx -mno-sse -mno-avx

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
	@echo "--- In-kernel tests..."
	@KTEST=1 user/testbench/report.pl
	@echo "--- Userspace tests..."
	@user/testbench/report.pl
	@echo "--- All tests completed!"


check: all
	@echo "--- Framework self-tests..."
	+@make -C user/testbench check
	+@make qcheck


slowcheck:
	+@make check TEST_SLOW=1


slowercheck:
	+@make check TEST_SLOWER=1


tags: $(shell find . -iname "*.[ch]" -or -iname "*.p[lm]")
	@ctags -R *


ia32-kernel: linker.ld loader.o kmain.o kip.o cpu.o heap.o thread.o \
		ktest.o kth.o trace.o \
		sched.o exception.o space.o ipc.o ipc_typed.o mapdb.o arch_x86.o \
		acpi.o gdt.o idt.o irq.o pic.o apic.o isr.o timer.o context.o \
		dlmalloc.o tap.o ccan-htable.o ccan-list.o
	@echo "  LD $@"
	@$(LD) -T linker.ld -o $@ $(filter %.o,$^) \
		-L lib -lukernel_util \
		$(shell gcc $(CFLAGS) -print-libgcc-file-name)


dlmalloc.o: CFLAGS += -Wno-unused-variable

# mostly for tap.o
%.o: user/testbench/%.c
	@echo "  CC $@ <user>"
	@$(CC) -c -o $@ $< $(CFLAGS) -nostartfiles -nodefaultlibs -MMD
	@test -d .deps || mkdir -p .deps
	@mv $(subst user/testbench/,,$(<:.c=.d)) .deps/


include $(wildcard .deps/*.d)
