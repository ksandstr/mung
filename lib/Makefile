
export CFGDIR:=$(abspath ..)
include ../config.mk

TEST_SOURCE:=$(wildcard t_*.c)
TEST_OBJS=$(patsubst %.c,%.o,$(TEST_SOURCE))

# the library is shared between userspace and kernel, so no MMX/SSE/AVX
# malarkey please. (also, let t_*.c get at user/testbench/test.h .)
CFLAGS+=-mno-mmx -mno-sse -mno-avx \
	-I $(CFGDIR)


all: libukernel_util.a


clean:
	@rm -f *.o *.a


distclean:
	@rm -rf .deps


libukernel_util.a: hash.o string.o strlcat.o strlcpy.o vsnprintf.o \
		slab.o fake_stdio.o ser_io.o rbtree.o inttypes.o \
		bug.o invariant.o hook.o l4util.o memdesc.o sort.o \
		setjmp.o trampoline.o test_util.o $(TEST_OBJS)
	@echo "  AR $@"
	@ar cr $@ $^
	@echo "  RANLIB $@"
	@ranlib $@


include $(wildcard .deps/*.d)
