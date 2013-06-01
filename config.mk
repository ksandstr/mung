
CCAN_DIR=~/src/ccan
MUIDL_DIR=$(CFGDIR)/../muidl

CFLAGS=-O2 -Wall -march=native -mno-sse -mno-avx -std=gnu99 -m32 \
	-I $(CFGDIR)/include -I $(CFGDIR)/include/fake_clib \
	-I $(MUIDL_DIR)/include -I $(CCAN_DIR) \
	-D_GNU_SOURCE -D__KERNEL__ \
	-mno-avx -mno-sse -mno-sse2 \
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


# muidl outputs
%-service.s %-client.s %-common.s %-defs.h: %.idl
	@echo "  IDL $<"
	@$(MUIDL) $(MUIDLFLAGS) --service --client --common --defs $<


%.o: %-32.S
	@echo "  AS $@"
	@gcc -c -o $@ $< $(CFLAGS) -DIN_ASM_SOURCE


%.o: %.s
	@echo "  AS $@ <generated>"
	@as --32 -o $@ $<


.deps:
	@mkdir -p .deps
