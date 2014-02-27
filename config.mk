
CCAN_DIR=~/src/ccan
MUIDL_DIR=$(CFGDIR)/../muidl

LD=ld.gold

CFLAGS=-O2 -Wall -march=native -std=gnu99 -m32 \
	-I $(CFGDIR)/include -I $(CFGDIR)/include/fake_clib \
	-I $(MUIDL_DIR)/include -I $(CCAN_DIR) \
	-D_GNU_SOURCE -D__KERNEL__ \
	-DENABLE_SELFTEST \
	-mno-avx -mno-sse2 -fuse-ld=gold \
	-fno-builtin -nostdlib #-DCCAN_LIST_DEBUG -DDEBUG_ME_HARDER

MUIDL:=$(abspath $(MUIDL_DIR)/muidl)
MUIDLFLAGS=-I $(MUIDL_DIR)/share/idl -I $(CFGDIR)/idl

CLEAN_PATS=*-service.s *-client.s *-common.s *-defs.h


%.o: %.c
	@echo "  CC $@"
	@$(CC) -c -o $@ $< $(CFLAGS) -nostartfiles -nodefaultlibs -MMD
	@test -d .deps || mkdir -p .deps
	@mv $(<:.c=.d) .deps/


# build CCAN sources as though they were in the kernel tree.
ccan-%.o ::
	@echo "  CC $@ <ccan>"
	@$(CC) -c -o $@ $(CCAN_DIR)/ccan/$*/$*.c $(CFLAGS) -nostartfiles -nodefaultlibs


%.o: %-32.S
	@echo "  AS $@"
	@gcc -c -o $@ $< $(CFLAGS) -DIN_ASM_SOURCE -MMD
	@test -d .deps || mkdir -p .deps
	@mv $(<:-32.S=.d) .deps/


%.o: %.s
	@echo "  AS $@ <generated>"
	@as --32 -o $@ $<


.deps:
	@mkdir -p .deps
