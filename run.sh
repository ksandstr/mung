#!/bin/sh

#exec kvm -serial stdio -no-reboot -net none -kernel image.bin -initrd user/sigma0 $@

exec kvm -serial stdio -no-reboot -net none -kernel mbiloader/mbiloader -initrd "image.bin,user/sigma0" $@
