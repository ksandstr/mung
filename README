
What
====

This is `mung`, an experimental microkernel for the x86 architecture,
following the L4.X2 specification. `mung` is licensed under the GNU GPL
version 3, found in the COPYING file in this directory.


What what?
----------

A microkernel is an operating system component that hides out of sight much of
what makes x86 so _charming_, but little else. This one is written to
implement the L4.X2 specification; the latest version as of 20120601 is
available from L4Ka at

    http://l4ka.org/l4ka/l4-x2-r7.pdf


How?
----

To compile this bullshit you should download the current CCAN repository,
preferably under `~/src/ccan` (or modify `config.mk`'s `CCAN_DIR` to suit your
taste). Use the search engine, Luke. There's also some dependencies I haven't
figured out quite yet, but which can be satisfied from current Debian testing.

To run the testbench application inside a qemu-KVM virtual machine, use the
provided `run.sh` script. To run it inside something else, or to use another
root server besides `testbench`, go chuck some runes of your own.


### Doesn't work. ###


A known issue is that `config.mk`'s default enables all the bells and whistles
in the resulting kernel image that're available on the host system. This leads
to GCC 4.7 emitting AVX instructions on a recent CPU, which require some sort
of extended state in the x86, for which no handler is installed by MultiBoot,
and which therefore causes an octuple 720 inverse crap-out fault right out of
the gate.

So don't compile it with GCC 4.7; I don't, either.


  -- Kalle A. Sandström <ksandstr@iki.fi>
