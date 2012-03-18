CCAN_DIR=~/src/ccan
CFLAGS=-O2 -Wall -march=native -std=gnu99 -I include -I include/fake_clib \
	-I $(CCAN_DIR) \
	-fno-builtin -nostdlib


%.o: %.c
	@echo "  CC $@"
	@$(CC) -m32 -c -o $@ $< $(CFLAGS) -nostartfiles -nodefaultlibs -MMD
	@test -d .deps || mkdir -p .deps
	@mv $(<:.c=.d) .deps/


.deps:
	@mkdir -p .deps
