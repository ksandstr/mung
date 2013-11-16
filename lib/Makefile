
export CFGDIR:=$(abspath ..)
include ../config.mk

TEST_SOURCE:=$(wildcard t_*.c)
TEST_OBJS=$(patsubst %.c,%.o,$(TEST_SOURCE))


all: libukernel_util.a


clean:
	@rm -f *.o *.a


distclean:
	@rm -rf .deps


libukernel_util.a: hash.o string.o strlcat.o strlcpy.o vsnprintf.o \
		slab.o fake_stdio.o ser_io.o rbtree.o inttypes.o bug.o \
		hook.o l4util.o $(TEST_OBJS)
	@echo "  AR $@"
	@ar cr $@ $^
	@echo "  RANLIB $@"
	@ranlib $@


include $(wildcard .deps/*.d)
