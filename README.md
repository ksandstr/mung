
What
====

This is `mung`, a microkernel for the x86 architecture, implementing the L4.X2
specification. `mung` is licensed under the GNU GPL version 3, found in the
COPYING file in this directory.


What what?
----------

A microkernel is an operating system component that hides out of sight much of
the lowest-level detail of the x86 architecture (such as page table management,
task switching, interrupt hardware, inter-process communication, etc.), and
exposes the rest to the operating system personality. This one is written to
implement the L4.X2 specification; the latest version as of 20120601 is
available from L4Ka at

    http://l4ka.org/l4ka/l4-x2-r7.pdf


How?
----

To compile this source tree you need two other git repositories: muidl, and a
recent copy of CCAN. The former goes under `../muidl` relative to where this
`README.md` is at, and CCAN goes under `~/src/ccan`; if these paths are
somehow distasteful to you, modify `config.mk` to make them less so. The
commands to download those packages are

    mung$ (cd .. && git clone git://github.com/ksandstr/muidl.git)
    mung$ (cd ~/src && git clone git://github.com/rustyrussell/ccan.git)

muidl must be built before mung. It'll generate x86 (32-bit) code even when
hosted in a non-x86 environment. (there are also some dependencies I haven't
figured out quite yet, but which can be satisfied from current Debian testing
with trial and error.)

To run the testbench application inside a qemu-KVM virtual machine, use the
provided `run.sh` script. To run it inside something else or to use another
root server besides `testbench`, chuck some runes of your own. Brief (if
sincere) test reporting can be had from the `make check` target, which requires
a few Perl packages (names from the Debian repository):

    libmoose-perl libtrycatch-perl liblist-moreutils-perl libdevel-cover-perl
    libstruct-compare-perl libmodern-perl-perl

To run the benchmark suite, use

    mung$ make && TESTBENCH_OPTS="notest=1 bench=1" ./run.sh -nographic

Note that this doesn't cause qemu to exit once it's done; the user should
supply a ctrl-c at the end.


### Doesn't work. ###

#### Building this thing is hard! ####

Entry fee, unfortunately.

#### Library was skipped because of format ####

Most recently, a fresh `binutils` package solves this problem by providing the
`gold` linker in the same package. Previously a `binutils-gold` package
would've been necessary, but it's since become a transitional dummy.

#### ... no, not that one ####

Figure it out. File an issue. Submit a patch. Whatever.


  -- Kalle A. Sandström <ksandstr@iki.fi>
