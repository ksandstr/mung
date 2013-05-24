
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

To compile this bullshit you need two other git repositories: muidl, and a
recent copy of CCAN. The former goes under `../muidl` relative to where this
`README.md` is at, and CCAN goes under `~/src/ccan`; if these paths are
somehow distasteful to you, modify `config.mk` to make them less so. The
commands to download those packages are

    mung$ (cd .. && git clone git://github.com/ksandstr/muidl.git)
    mung$ (cd ~/src && git clone git://github.com/rustyrussell/ccan.git)

There's also some dependencies I haven't figured out quite yet, but which can
be satisfied from current Debian testing with trial and error.

To run the testbench application inside a qemu-KVM virtual machine, use the
provided `run.sh` script. To run it inside something else, or to use another
root server besides `testbench`, chuck some runes of your own. Brief and
sincere test reporting can be had from the `make check` target, which requires
a few Perl packages (names from the Debian repository):

    libmoose-perl libtrycatch-perl liblist-moreutils-perl libdevel-cover-perl
    libstruct-compare-perl libmodern-perl-perl


### Doesn't work. ###

#### Library was skipped because of format ####

On amd64 Debian this is remedied with the

    binutils-gold

package, along with the relevant 32-bit multiarch GCC and so forth.

#### ... no, not that one ####

Figure it out. File an issue. Or don't.


  -- Kalle A. Sandström <ksandstr@iki.fi>
