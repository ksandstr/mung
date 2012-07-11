
export CFGDIR:=$(abspath ../..)
include $(CFGDIR)/config.mk

LDFLAGS=-e_start -N -nostdlib


all: testbench


clean:
	@rm -f *.o


distclean:
	@rm -rf .deps testbench


testbench: testbench.ld crt0.o testbench.o heap.o thread.o delay.o \
		exn-store.o log.o tap.o test.o \
		thread_test.o sched_suite.o \
		syscalls.o ccan-list.o dlmalloc.o
	@echo "  LD $@"
	@$(LD) $(LDFLAGS) -T testbench.ld $(filter %.o,$^) $(LIBS) -o $@ \
		-L $(CFGDIR)/lib -lukernel_util \
		$(shell gcc $(CFLAGS) -print-libgcc-file-name)


%.o: ../../%.c
	@echo "  CC $@ <toplevel>"
	@$(CC) -c -o $@ $< $(CFLAGS) -DUSE_LOCKS=1 -Wno-unused


include $(wildcard .deps/*.d)
