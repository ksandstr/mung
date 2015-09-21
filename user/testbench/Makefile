
export CFGDIR:=$(abspath ../..)
include $(CFGDIR)/config.mk

LDFLAGS=-e_start -N -nostdlib -L $(CFGDIR)/user/lib/l4
# under -O3, GCC may produce SSE instructions in a runtime that doesn't align
# the stack properly. FIXME: repair that, then remove -mstackrealign.
CFLAGS:=$(subst -D__KERNEL__,,$(CFLAGS)) -mstackrealign

TEST_IDL:=idl/common.idl idl/benchmark.idl \
	$(wildcard idl/*-suite.idl)
TEST_IDL_OBJS:=$(patsubst idl/%.idl,%-service.o,$(TEST_IDL)) \
	$(patsubst idl/%.idl,%-client.o,$(TEST_IDL)) \
	$(patsubst idl/%.idl,%-common.o,$(TEST_IDL))


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
		delay.o process.o exn-store.o log.o tap.o test.o util.o ll.o \
		idlsupp.o pg_stats.o pg_drop.o \
		sched_suite.o space_suite.o thread_suite.o string_suite.o \
		ipc_suite.o self_suite.o type_suite.o redir_suite.o \
		x86_suite.o interrupt_suite.o \
		meta_suite.o legacy.o benchmark.o thread_test.o \
		ccan-list.o ccan-htable.o ccan-hash.o ccan-crc.o \
		ccan-talloc.o ccan-tally.o \
		$(TEST_IDL_OBJS) \
		threadmgr-common.o threadmgr-service.o threadmgr-client.o \
		forkserv-common.o forkserv-client.o
	@echo "  LD $@"
	@$(LD) $(LDFLAGS) -static -o $@ -b elf32-i386 -T testbench.ld \
		$(filter %.o,$^) $(LIBS) \
		-L $(CFGDIR)/lib -lukernel_util -ll4 \
		-L /usr/lib32 -lm \
		$(shell gcc $(CFLAGS) -print-libgcc-file-name)

ipc_suite.o: ipc_suite.c ipc-suite-defs.h
x86_suite.o: x86_suite.c x86-suite-defs.h
thread.o: thread.c threadmgr-defs.h
heap.o: heap.c forkserv-defs.h common-defs.h
benchmark.o: benchmark.c benchmark-defs.h
testbench.o: testbench.c common-defs.h


forkserv: forkserv.ld crt0.o forkserv.o dlmalloc.o idlsupp.o tsd.o ll.o \
		forkserv-service.o forkserv-common.o forkserv-client.o \
		threadmgr-common.o threadmgr-client.o \
		ccan-list.o ccan-htable.o
	@echo "  LD $@"
	@$(LD) $(LDFLAGS) -T forkserv.ld $(filter %.o,$^) $(LIBS) -o $@ \
		-L $(CFGDIR)/lib -lukernel_util -ll4 \
		$(shell gcc $(CFLAGS) -print-libgcc-file-name)


%.o: ../../%.c
	@echo "  CC $@ <toplevel>"
	@$(CC) -c -o $@ $< $(CFLAGS) -DUSE_LOCKS=1 -Wno-unused


# muidl outputs.
# TODO: we don't really need to hear first about the services being generated,
# and then the common bit, the client, and then the defs. there's AS chatter
# about the first three too.
%-service.s: idl/%.idl
	@echo "  IDL $< <service>"
	@$(MUIDL) $(MUIDLFLAGS) --service $<

%-client.s: idl/%.idl
	@echo "  IDL $< <client>"
	@$(MUIDL) $(MUIDLFLAGS) --client $<

%-common.s: idl/%.idl
	@echo "  IDL $< <common>"
	@$(MUIDL) $(MUIDLFLAGS) --common $<

%-defs.h: idl/%.idl
	@echo "  IDL $< <defs>"
	@$(MUIDL) $(MUIDLFLAGS) --defs $<


include $(wildcard .deps/*.d)
