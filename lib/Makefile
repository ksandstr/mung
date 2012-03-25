
export CFGDIR:=$(abspath ..)
include ../config.mk


all: libukernel_util.a


clean:
	@rm -f *.o *.a


distclean:
	@rm -rf .deps


libukernel_util.a: hash.o string.o strlcat.o strlcpy.o vsnprintf.o \
		slab.o rbtree.o
	@echo "  AR $@"
	@ar cr $@ $^
	@echo "  RANLIB $@"
	@ranlib $@


include $(wildcard .deps/*.d)
