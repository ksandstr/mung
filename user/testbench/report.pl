#!/usr/bin/perl
use strict;
use warnings;
use utf8;

use IO::File;
use IO::Pipe;
use List::Util qw/sum/;


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


my ($suite, $tcase, $test);
my ($tap_low, $tap_high, $tap_next, $no_plan);
my $msg_break;

sub report_msg {
	my $str = shift;
	if(!$msg_break) {
		print "\n";
		$msg_break = 1;
	}

	print "$str\n";
}


# TODO: skip stderr outputs, log diag() messages, report failed tests
sub tap_line {
	return unless $suite;
	my $line = shift;

	$line =~ s/^\[ERR\]: //;
	my $diag = $1 if $line =~ s/#\s*(.+)$//;

	if($line =~ /^(\d+)\.\.(\d+)(.*)$/) {
		if(defined $tap_low && !$no_plan) {
			# $tap_high - $tap_next + 1 planned tests went unexecuted.
			# TODO: count and report this.
		}
		$no_plan = 0;
		$tap_low = int($1);
		$tap_high = int($2);
		$tap_next = $tap_low;
		$test->{planned} += $tap_high - $tap_low + 1;

		if($diag && $diag =~ s/Skip //) {
			die "malformed skip plan" unless $tap_low > $tap_high;
			$tcase->{tests_skipped}++;
			my $tname = test_name();
			report_msg("test plan `$tname' skipped: $diag");
			$diag = '';
		}
	} elsif($line =~ /^(not\s+)?ok\s+(\d+)\s+-\s+(.+)$/) {
		# FIXME: the regexp above is too strict. it should have the test
		# number and description as optional. (though testbench's tap.c
		# doesn't output any other format.)
		my $fail = defined($1);
		my $id = int($2);
		my $desc = $3;
		# (the test boundary resets $tap_next to -1, so that new unplanned
		# streams can be recognized.)
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
		$test->{seen}++;
		$test->{planned}++ if $no_plan;
		$test->{passed}++ unless $fail;
		my $tname = test_name();
		if($fail) {
			report_msg("test `$tname' failed: $desc");
			print "test log: $_\n" foreach @{$test->{log}};
		}
		if(++$tap_next > $tap_high && !$no_plan) {
			# end of test
			undef $tap_low;
		}
	} elsif($line =~ /^ok\s+(\d+)?/ && $diag =~ s/^skip\s+//i) {
		# skip lines.
		my $id = int($1 || '0');
		$test->{skipped}++;
		$test->{seen}++;
		my $tname = test_name();
		report_msg("skip $id - # $diag\t[$tname]");
		undef $diag;
	} elsif($line !~ /^\s*$/) {
		die "unrecognized line";
	}

	report_msg("diag: $diag") if $diag;
}


sub test_name {
	my $n = $test->{name};
	$n .= " {iter $test->{iter}}" if exists $test->{iter};
	return $n;
}


sub restore_header {
	return if !$msg_break;
	print "Suite " . $suite->{name} . ": ";
	$msg_break = 0;
}


my %begin_table = (
	suite => sub {
		my $name = shift;
		print "Suite $name: ";
		$suite = {
			name => $name,
			cases => [],
		};
	},
	tcase => sub {
		my $name = shift;
		restore_header() if $msg_break;
		print "$name ";
		$tcase = {
			name => $name,
			tests_skipped => 0,	# of test runs (incl. loop iters)
			tests => [],
		};
	},
	test => sub {
		my $name = shift;
		my $tag = shift;
		my $value = shift;
		$test = {
			name => $name,
			planned => 0,		# of test points
			seen => 0,
			passed => 0,
			skipped => 0,
			log => [],
		};
		$test->{iter} = int($value) if $tag eq 'iter';
	},
);

my %end_table = (
	suite => sub {
		my @tests = map { @{$_->{tests}} } @{$suite->{cases}};
		my %summary;
		foreach my $key (qw/planned passed skipped/) {
			$summary{$key} = sum(map { $_->{$key} } @tests);
		}
		restore_header() if $msg_break;
		print "[" . $summary{passed} . "/" . $summary{planned} . " OK]";
		print "\n";
		undef $suite;
	},
	tcase => sub {
		my $tskip = $tcase->{tests_skipped};
		if($msg_break) {
			restore_header();
			print $tcase->{name} . " ";
		}
		print "<skipped $tskip plans> " if $tskip > 0;
		push @{$suite->{cases}}, $tcase;
		undef $tcase;
	},
	test => sub {
		my $name = shift;
		my $tag = shift;
		my $val = shift;
		$test->{rc} = $val if $tag eq 'rc';
		if($test->{planned} > $test->{seen}) {
			report_msg("planned " . $test->{planned}
				. " test(s), but executed only " . $test->{seen});
		}
		push @{$tcase->{tests}}, $test;
		undef $test;
		$tap_next = -1;
	},
);


my $test_pipe = start_test();
my $i = 0;
my $status = 0;
while(<$test_pipe>) {
	chomp;
	s/^\s+//;	# apparently sometimes there are carriage returns.

	last if /^\*\*\*.+\bcompleted/;

	if(/^\*\*\*\s+(.+)$/) {
		my $msg = $1;
		if($msg =~ /(begin|end) (suite|tcase|test) [`'](\w+)'(\s+(\w+)\s+(\d+))?/) {
			my $tab = $1 eq 'begin' ? \%begin_table : \%end_table;
			my $fn = $tab->{$2};
			my $tag = $5 || '';
			my $val = $6 && int($6);
			&$fn($3, $tag, $val) if $fn;
		} elsif($msg =~ /test\s+log:\s*(.+)$/) {
			# capture test log output
			push @{$test->{log}}, $1;
		} elsif($msg =~ /(.+) follow/) {
			# NOTE: should ignore from here until the next valid control
			# message & possibly log into a hash keyed with $1
		} else {
			print STDERR "unknown control message `$_'\n";
		}
	} elsif(/^testbench abort.*called/) {
		print "\n";
		print STDERR "premature test abort!\n";
		$status = 1;
		last;
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
$test_pipe->close;
exit $status;
