#!/bin/sh
exec kvm -serial stdio -no-reboot -net none -kernel image.bin $@