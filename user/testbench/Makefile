
export CFGDIR:=$(abspath ../..)
include $(CFGDIR)/config.mk

LDFLAGS=-e_start -N -nostdlib


all: testbench forkserv


clean:
	@rm -f *.o


distclean:
	@rm -rf .deps testbench forkserv


check:
	@tests/run_all.pl


covercheck:
	@PERL5OPT=-MDevel::Cover tests/run_all.pl


testbench: testbench.ld crt0.o testbench.o dlmalloc.o heap.o thread.o \
		delay.o process.o exn-store.o log.o tap.o test.o util.o \
		pg_stats.o pg_drop.o \
		sched_suite.o space_suite.o thread_suite.o string_suite.o \
		self_suite.o \
		legacy.o thread_test.o \
		syscalls.o ccan-list.o ccan-htable.o ccan-hash.o
	@echo "  LD $@"
	@$(LD) $(LDFLAGS) -T testbench.ld $(filter %.o,$^) $(LIBS) -o $@ \
		-L $(CFGDIR)/lib -lukernel_util \
		$(shell gcc $(CFLAGS) -print-libgcc-file-name)


# NOTE: use of the same heap.o as in testbench
forkserv: forkserv.ld crt0.o forkserv.o syscalls.o heap.o dlmalloc.o \
		ccan-list.o ccan-htable.o
	@echo "  LD $@"
	@$(LD) $(LDFLAGS) -T forkserv.ld $(filter %.o,$^) $(LIBS) -o $@ \
		-L $(CFGDIR)/lib -lukernel_util \
		$(shell gcc $(CFLAGS) -print-libgcc-file-name)


%.o: ../../%.c
	@echo "  CC $@ <toplevel>"
	@$(CC) -c -o $@ $< $(CFLAGS) -DUSE_LOCKS=1 -Wno-unused


include $(wildcard .deps/*.d)
