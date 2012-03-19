
CCAN_DIR=~/src/ccan
CFLAGS=-O2 -Wall -march=native -std=gnu99 \
	-I $(CFGDIR)/include -I $(CFGDIR)/include/fake_clib \
	-I $(CCAN_DIR) \
	-fno-builtin -nostdlib


%.o: %.c
	@echo "  CC $@"
	@$(CC) -m32 -c -o $@ $< $(CFLAGS) -nostartfiles -nodefaultlibs -MMD
	@test -d .deps || mkdir -p .deps
	@mv $(<:.c=.d) .deps/


%.o: %-32.S
	@echo "  AS $@"
	@gcc -m32 -c -o $@ $< $(CFLAGS) -DIN_ASM_SOURCE


.deps:
	@mkdir -p .deps
