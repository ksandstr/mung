
CCAN_DIR=~/src/ccan
MUIDL_DIR=$(CFGDIR)/../muidl

LD=ld.gold

CFLAGS=-O2 -Wall -march=native -std=gnu11 -m32 \
	-I $(CFGDIR)/include -I $(CFGDIR)/include/fake_clib \
	-I . -I $(MUIDL_DIR)/include -I $(CCAN_DIR) \
	-D_GNU_SOURCE -D__KERNEL__ -DENABLE_SELFTEST \
	-mno-avx -mno-sse2 -fno-pic -fuse-ld=gold -ffunction-sections \
	-fno-builtin -nostdlib \
	-Wno-frame-address -Wno-address-of-packed-member \
	#-DDEBUG_ME_HARDER #-D_L4_DEBUG_ME_HARDER #-DCCAN_LIST_DEBUG

LDFLAGS=--gc-sections

MUIDL:=$(abspath $(MUIDL_DIR)/muidl)
MUIDLFLAGS=-I $(MUIDL_DIR)/share/idl -I $(CFGDIR)/idl

CLEAN_PATS=*-service.s *-client.s *-common.s *-defs.h


%.o: %.c
	@echo "  CC $@"
	@$(CC) -c -o $@ $< $(CFLAGS) -nostartfiles -nodefaultlibs -MMD
	@test -d .deps || mkdir -p .deps
	@mv $(if $(findstring ..,$<),$(notdir $(<:.c=.d)),$(<:.c=.d)) .deps/


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


.deps: $(shell $(CFGDIR)/stuff/find-idl-defs.pl)
	@mkdir -p .deps
