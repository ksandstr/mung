#!/bin/sh
# this script is used by user/testbench/report.pl .
KVM=""
if [ -d "/sys/module/kvm" ]; then KVM="-enable-kvm"; fi
exec qemu-system-i386 ${KVM} -serial stdio -no-reboot -net none -kernel mbiloader/mbiloader -initrd "ia32-kernel,user/sigma0,user/testbench/testbench,user/testbench/forkserv $TESTBENCH_OPTS" $@
