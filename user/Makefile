
include ../config.mk

LDFLAGS=-e_start -N -nostdlib


all: sigma0


clean:
	@rm -f *.o


distclean:
	@rm -rf .deps


sigma0: sigma0.ld sigma0.o
	@echo "  LD $@"
	@$(LD) $(LDFLAGS) -T sigma0.ld $(filter %.o,$^) $(LIBS) -o $@


include $(wildcard .deps/*.d)
