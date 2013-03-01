package Mung::Module;
use Modern::Perl '2012';
use Moose::Role;

=head1 NAME

Mung::Module - encapsulation of the test program

=head1 DESCRIPTION

The Mung testbench reporting script accepts output from a test module, which
is started and restarted according to how test programs in the module behave.
The Mung::Module role abstracts away the POSIX process launch & signaling
details, permitting use of canned test logs (for testing the reporting
scripts) or things such as running the testbench on actual hardware over a
serial connection (with a program-controlled hardware reset somewhere).

=cut


requires 'start_test', 'next_line', 'close';


1;
