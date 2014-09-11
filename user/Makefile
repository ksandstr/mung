
export CFGDIR:=$(abspath ..)
include ../config.mk

LDFLAGS=-e_start -N -nostdlib
CFLAGS:=$(subst -D__KERNEL__,,$(CFLAGS))


all: sigma0
	+@make -C testbench all


clean:
	+@make -C testbench clean
	@rm -f *.o


distclean:
	+@make -C testbench distclean
	@rm -rf .deps


sigma0: sigma0.ld crt0.o sigma0.o syscalls.o ccan-list.o
	@echo "  LD $@"
	@$(LD) $(LDFLAGS) -T sigma0.ld $(filter %.o,$^) $(LIBS) -o $@ \
		-L $(CFGDIR)/lib -lukernel_util \
		$(shell gcc $(CFLAGS) -print-libgcc-file-name)


include $(wildcard .deps/*.d)
