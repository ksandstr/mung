
export CFGDIR:=$(abspath ../..)
include $(CFGDIR)/config.mk

LDFLAGS=-e_start -N -nostdlib


all: testbench


clean:
	@rm -f *.o


distclean:
	@rm -rf .deps


testbench: testbench.ld crt0.o testbench.o syscalls.o ccan-list.o
	@echo "  LD $@"
	@$(LD) $(LDFLAGS) -T testbench.ld $(filter %.o,$^) $(LIBS) -o $@ \
		-L $(CFGDIR)/lib -lukernel_util \
		$(shell gcc $(CFLAGS) -print-libgcc-file-name)


include $(wildcard .deps/*.d)
