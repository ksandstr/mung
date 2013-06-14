
export CFGDIR:=$(abspath ../..)
include $(CFGDIR)/config.mk

LDFLAGS=-e_start -N -nostdlib


all: testbench forkserv


clean:
	@rm -f *.o $(CLEAN_PATS)


distclean:
	@rm -rf .deps testbench forkserv


check:
	@tests/run_all.pl


covercheck:
	@PERL5OPT=-MDevel::Cover tests/run_all.pl


testbench: testbench.ld crt0.o testbench.o dlmalloc.o heap.o thread.o tsd.o \
		delay.o process.o exn-store.o log.o tap.o test.o util.o \
		idlsupp.o pg_stats.o pg_drop.o \
		sched_suite.o space_suite.o thread_suite.o string_suite.o \
		ipc_suite.o self_suite.o type_suite.o \
		x86_suite.o interrupt_suite.o \
		legacy.o thread_test.o \
		syscalls.o ccan-list.o ccan-htable.o ccan-hash.o ccan-crc.o \
		ipc-suite-service.o ipc-suite-client.o ipc-suite-common.o \
		forkserv-common.o forkserv-client.o
	@echo "  LD $@"
	@$(LD) $(LDFLAGS) -T testbench.ld $(filter %.o,$^) $(LIBS) -o $@ \
		-L $(CFGDIR)/lib -lukernel_util \
		$(shell gcc $(CFLAGS) -print-libgcc-file-name)

ipc_suite.o: ipc_suite.c ipc-suite-defs.h
# (common dependency between forkserv and testbench, so it takes care of
# forkserv-defs.h for both in the proper order.)
heap.o: heap.c forkserv-defs.h


# NOTE: use of the same heap.o as in testbench
forkserv: forkserv.ld crt0.o forkserv.o syscalls.o heap.o dlmalloc.o \
		idlsupp.o tsd.o \
		forkserv-service.o forkserv-common.o forkserv-client.o \
		ccan-list.o ccan-htable.o
	@echo "  LD $@"
	@$(LD) $(LDFLAGS) -T forkserv.ld $(filter %.o,$^) $(LIBS) -o $@ \
		-L $(CFGDIR)/lib -lukernel_util \
		$(shell gcc $(CFLAGS) -print-libgcc-file-name)


%.o: ../../%.c
	@echo "  CC $@ <toplevel>"
	@$(CC) -c -o $@ $< $(CFLAGS) -DUSE_LOCKS=1 -Wno-unused


include $(wildcard .deps/*.d)
