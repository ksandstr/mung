#!/bin/sh

exec kvm -serial stdio -no-reboot -net none -kernel mbiloader/mbiloader -initrd "ia32-kernel,user/sigma0,user/testbench/testbench" $@
