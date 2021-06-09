
export CFGDIR:=$(abspath ../..)
include $(CFGDIR)/config.mk

LDFLAGS += -e_start -N -nostdlib -L $(CFGDIR)/user/lib/l4
# under -O3, GCC may produce SSE instructions in a runtime that doesn't align
# the stack properly. FIXME: repair that, then remove -mstackrealign.
CFLAGS:=$(subst -D__KERNEL__,,$(CFLAGS)) -mstackrealign -ffreestanding

TEST_IDL:=idl/common.idl idl/benchmark.idl \
	$(wildcard idl/*-suite.idl)
TEST_IDL_OBJS:=$(patsubst idl/%.idl,%-service.o,$(TEST_IDL)) \
	$(patsubst idl/%.idl,%-client.o,$(TEST_IDL)) \
	$(patsubst idl/%.idl,%-common.o,$(TEST_IDL))

vpath %.c $(MUIDL_DIR)/share


all: testbench forkserv


clean:
	@rm -f *.o $(CLEAN_PATS)


distclean:
	@rm -rf .deps testbench forkserv


check:
	@tests/run_all.pl


testbench: testbench.ld crt0.o testbench.o dlmalloc.o heap.o \
		process.o thread.o tsd.o mutex.o \
		delay.o exn-store.o log.o tap.o test.o util.o ll.o \
		stritem_support.o idlsupp.o pg_stats.o pg_drop.o \
		sched_suite.o space_suite.o thread_suite.o string_suite.o \
		ipc_suite.o self_suite.o type_suite.o redir_suite.o \
		x86_suite.o interrupt_suite.o meta_suite.o benchmark.o \
		ccan-list.o ccan-htable.o ccan-hash.o ccan-crc32c.o \
		ccan-talloc.o ccan-tally.o ccan-strset.o ccan-autodata.o \
		$(TEST_IDL_OBJS) \
		threadmgr-common.o threadmgr-service.o threadmgr-client.o \
		forkserv-common.o forkserv-client.o
	@echo "  LD $@"
	@$(LD) $(LDFLAGS) -static -o $@ -b elf32-i386 -T testbench.ld \
		$(filter %.o,$^) $(LIBS) \
		-L $(CFGDIR)/lib -lukernel_util -ll4 \
		-L /usr/lib32 -L /usr/lib/i386-linux-gnu -lm \
		$(shell gcc $(CFLAGS) -print-libgcc-file-name)


forkserv: forkserv.ld crt0.o forkserv.o dlmalloc.o idlsupp.o tsd.o ll.o \
		forkserv-service.o forkserv-common.o forkserv-client.o \
		threadmgr-common.o threadmgr-client.o \
		l4x2-common.o l4x2-client.o \
		ccan-list.o ccan-htable.o
	@echo "  LD $@"
	@$(LD) $(LDFLAGS) -T forkserv.ld $(filter %.o,$^) $(LIBS) -o $@ \
		-L $(CFGDIR)/lib -lukernel_util -ll4 \
		$(shell gcc $(CFLAGS) -print-libgcc-file-name)


%.o: ../../%.c
	@echo "  CC $@ <toplevel>"
	@$(CC) -c -o $@ $< $(CFLAGS) -DUSE_LOCKS=2 -Wno-unused


# IDL compiler outputs.

vpath %.idl $(CFGDIR)/idl idl

%-service.s %-client.s %-common.s %-defs.h: %.idl
	@echo "  IDL $<"
	@$(MUIDL) $(MUIDLFLAGS) --service --client --common --defs $<


include $(wildcard .deps/*.d)
