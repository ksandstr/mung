
export CFGDIR:=$(abspath ..)
include ../config.mk

LDFLAGS=-e_start -N -nostdlib

# mbiloader runs before exception ISRs have been installed. so it absolutely
# doesn't support coprocessor contexts.
CFLAGS:=$(subst -D__KERNEL__,,$(CFLAGS)) -mno-sse -mno-mmx -mno-avx


all: mbiloader


clean:
	@rm -f *.o


distclean:
	@rm -rf .deps


mbiloader: mbiloader.ld crt0.o mbiloader.o
	@echo "  LD $@"
	@$(LD) $(LDFLAGS) -T mbiloader.ld $(filter %.o,$^) $(LIBS) -o $@ \
		-L $(CFGDIR)/lib -lukernel_util \
		$(shell gcc $(CFLAGS) -print-libgcc-file-name)


include $(wildcard .deps/*.d)
