
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
root server besides `testbench`, go chuck some runes of your own. Proper-ish
test reporting can be had from the `make check' target, which requires a few
Perl packages (names from the Debian repository):

    libmoose-perl
    libtrycatch-perl
    liblist-moreutils-perl


### Doesn't work. ###

Figure it out. File an issue. Or don't.


  -- Kalle A. Sandström <ksandstr@iki.fi>
