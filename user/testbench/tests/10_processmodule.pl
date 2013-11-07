#!/usr/bin/perl -w
use Modern::Perl '2012';
use FindBin qw/$Bin/;
use lib "$Bin/lib";

use List::MoreUtils qw/any/;
use Mung::ProcessModule;

use Test::More tests => 6;


my $PROGRAM = "$Bin/../perl5/program.pl";
die "program not found" unless -x $PROGRAM;

# part 1: starting of a program without an environment.
my $pm = Mung::ProcessModule->new(command => $PROGRAM);
print "without environment\n";
$pm->start_test;
like($pm->next_line, qr/^hello/i, "program said hello");
like($pm->next_line, qr/harmless.*benign/,
	"program claims to be harmless");
like($pm->next_line, qr/hate.*vampires/,
	"program cannot possibly be a vampire itself");
$pm->close;

# part 2: starting of a program with an environment variable.
$pm = Mung::ProcessModule->new(
	command => $PROGRAM,
	env => { NIGHTFALL => 1 });
print "with environment\n";
$pm->start_test;
like($pm->next_line, qr/^hello/i, "program said hello");
like($pm->next_line, qr/hand.*flesh/,
	"program talks like a dracula");
like($pm->next_line, qr/called.*oo.+mans.*tribute/,
	"program makes poncy claims");
$pm->close;
