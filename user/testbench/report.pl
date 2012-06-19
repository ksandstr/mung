#!/usr/bin/perl
use strict;
use warnings;
use utf8;

use IO::File;
use IO::Pipe;


my $TOPLEVEL = ".";
-f "$TOPLEVEL/run.sh" || die "can't find run.sh in path `$TOPLEVEL'";

$SIG{PIPE} = "IGNORE";		# and smoke it.
$SIG{INT} = "IGNORE";		# my regards to 007


sub start_test {
	return IO::File->new("./run.sh -display none |");
}


sub is_kmsg {
	shift;
	return /^\[sigma0\]/ || /^sbrk:/;
}


my ($suite, $tcase);
my ($tap_low, $tap_high, $tap_next, $no_plan);

# TODO: skip stderr outputs, log diag() messages, report failed tests
sub tap_line {
	my $line = shift;
	return unless $suite;

	if($line =~ /^(\d+)\.\.(\d+)$/) {
		if(defined $tap_low && !$no_plan) {
			# $tap_high - $tap_next + 1 planned tests went unexecuted.
			# TODO: count and report this.
		}
		$no_plan = 0;
		$tap_low = int($1);
		$tap_high = int($2);
		$tap_next = $tap_low;
		$suite->{planned} += $tap_high - $tap_low + 1;
	} elsif($line =~ /^(not\s+)?ok\s+(\d+)\s+-\s+(.+)$/) {
		my $fail = defined($1);
		my $id = int($2);
		my $desc = $3;
		if($id != $tap_next) {
			if($id == 1) {
				# take this as an implicit start of a new plan_no_plan() style
				# substream
				$tap_low = 0;
				$tap_high = 1;
				$no_plan = 1;
				$tap_next = 1;
			} else {
				# TODO: could invalid streams be handled in a nicer way?
				die "plan expected" unless defined $tap_low;
				die "identifier $id out of order (expected $tap_next, or 1)";
			}
		}
		$suite->{planned}++ if $no_plan;
		$suite->{passed}++ unless $fail;
		if(++$tap_next > $tap_high && !$no_plan) {
			# end of test
			undef $tap_low;
		}
	} else {
		die "unrecognized line";
	}
}

my %begin_table = (
	suite => sub {
		my $name = shift;
		print "Suite $name: ";
		$suite = { name => $name, planned => 0, passed => 0 };
	},
	tcase => sub {
		my $name = shift;
		print "$name ";
		$tcase = { name => $name };
	},
	test => sub {
		# don't report individual tests
	},
);

my %end_table = (
	suite => sub {
		print "[" . $suite->{passed} . "/" . $suite->{planned} . " OK]";
		print "\n";
		undef $suite;
	},
	tcase => sub {
		undef $tcase;
	},
	test => sub { },
);


my $test = start_test();
my $i = 0;
while(<$test>) {
	chomp;
	s/^\s+//;	# apparently sometimes there are carriage returns.

	last if /^\*\*\*.+\bcompleted/;

	if(/^\*\*\*\s+(.+)$/) {
		my $msg = $1;
		if($msg =~ /(begin|end) (suite|tcase|test) [`'](\w+)'/) {
			my $tab = $1 eq 'begin' ? \%begin_table : \%end_table;
			my $fn = $tab->{$2};
			&$fn($3) if $fn;
		} elsif($msg =~ /(.+) follow/) {
			# NOTE: should ignore from here until the next valid control
			# message & possibly log into a hash keyed with $1
		} else {
			print STDERR "unknown control message `$_'\n";
		}
	} else {
		next if is_kmsg($_);
		eval {
			tap_line($_);
		};
		if($@) {
			$@ =~ /^(.+) at/;
			print STDERR  "tap: $1 (line `$_')\n";
		}
	}
}
kill "INT", -getpgrp(0);
$test->close;
