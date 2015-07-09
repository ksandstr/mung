
export CFGDIR:=$(abspath ..)
include ../config.mk

LDFLAGS=-e_start -N -nostdlib
CFLAGS:=$(subst -D__KERNEL__,,$(CFLAGS))


all: sigma0 lib/l4/libl4.a
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


lib/l4/libl4.a: syscalls.o
	@echo "  AR $@"
	@ar cr $@ $<
	@echo "  RANLIB $@"
	@ranlib $@



include $(wildcard .deps/*.d)
