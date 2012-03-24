
export CFGDIR:=$(abspath ..)
include ../config.mk

LDFLAGS=-e_start -N -nostdlib


all: sigma0


clean:
	@rm -f *.o


distclean:
	@rm -rf .deps


sigma0: sigma0.ld crt0.o sigma0.o syscalls.o \
		../vsnprintf.o ../string.o ../strlcpy.o ../strlcat.o
	@echo "  LD $@"
	@$(LD) $(LDFLAGS) -T sigma0.ld $(filter %.o,$^) $(LIBS) -o $@ \
		$(shell gcc $(CFLAGS) -print-libgcc-file-name)


include $(wildcard .deps/*.d)
