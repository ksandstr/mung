#!/usr/bin/perl
use strict;
use warnings;
use utf8;
use feature "switch";

use FindBin qw($Bin);
use lib "$Bin/perl5";

use IO::File;
use IO::Pipe;
use TryCatch;
use Moose::Util qw/apply_all_roles/;
use List::Util qw/sum/;
use List::MoreUtils qw/none/;

use Mung::Sink;
use Mung::Ctrl;
use Mung::Test;
use Mung::TestResult;
use Mung::TapError;
use Mung::Restarting;
use Mung::ConsoleReport;
use Mung::Error::TestAbort;
use Mung::Error::TestRestart;


my $TOPLEVEL = ".";
-f "$TOPLEVEL/run.sh" || die "can't find run.sh in path `$TOPLEVEL'";

$SIG{PIPE} = "IGNORE";		# and smoke it.
$| = 1;


sub start_test {
	my %opts = @_;

	my @parms;
	foreach(keys %opts) {
		my $key = $_;
		my $val = $opts{$key};
		$val = join("+", @$val) if ref($val) eq 'ARRAY';
		$key =~ s/_//g;
		push @parms, "$key=$val";
	}
	if(@parms) {
		$ENV{TESTBENCH_OPTS} = join(" ", @parms);
	} else {
		delete $ENV{TESTBENCH_OPTS};
	}
	return IO::File->new("./run.sh -display none 2>/dev/null |");
}


my ($suite, $tcase, $test);
# %stats = (
#   failed => integer, # of failed tests in all suites
#   incorrect => integer, # of failed test points in all suites
#   suites => ref to vector of values in $suite
# )

# printable name of a test. tcase and suite assumed in context.
sub current_test_name {
	my $n = $test->name;
	my $iter = $test->current_result->iter;
	$n .= " {iter $iter}" if $test->low < $test->high;
	return $n;
}


# parameters affecting the test process.
my @roles;
my @ctrl_param = ( max_ids_len => 70 );
if($ENV{TEST_SLOW}) {
	push @ctrl_param, ( allow_all => 0, max_per_run => 1 );
} elsif(!$ENV{TEST_QUICK}) {
	push @roles, 'Mung::Restarting';
}


my $i = 0;
my $status = 0;
my @errors;

my %completed;

my $sink = Mung::Sink->new(output => Mung::ConsoleReport->new);
my $ctrl = Mung::Ctrl->new(@ctrl_param, sink => $sink);
$sink->on_complete_fn(sub { $ctrl->completed($_[0], $_[1]->iter); });

apply_all_roles($sink, @roles) if @roles;

my $prev_restart_id;
my $panic_restart_id;
while(1) {
	my $test_pipe;
	my $test_ids = $ctrl->next_tests || last;
	if($test_ids eq 'ALL') {
		# initial run, without restart. toss old plan.
		$sink->reset;
		$test_pipe = start_test(describe => 1);
	} elsif($test_ids eq 'NEED_PLAN') {
		# initial run, controller wants to run specific tests but has no plan.
		# gather it and try again.
		$test_pipe = start_test(describe => 1, run_only => ['@']);
	} else {
		die "expected arrayref" unless ref($test_ids) eq 'ARRAY';
		$test_pipe = start_test(run_only => $test_ids);
	}

	my $ctl_seen = 0;
	while(<$test_pipe>) {
		chomp;
		s/^\s+//;	# apparently sometimes there are carriage returns.

		if(!$ctl_seen) {
			next unless /^\*\*\*\s/;
			$ctl_seen = 1;
		}

		if(/^\*\*\*.+\bcompleted/) {
			$ctrl->restarted_with() if $test_ids eq 'NEED_PLAN';
			last;
		} elsif(/^PANIC:\s*(.+)$/) {
			# TODO: restart in such a way as to discover the combination of
			# tests that causes the panic, and report that.
			#
			# FIXME: this code should reasonably be in Mung::Sink->tb_line, or
			# something under that, as it duplicates the restart-on-failure
			# function entirely; just the error messages need be specialized.
			my $result;
			try {
				$result = $sink->test_panic($1);
			}
			catch (Mung::Error::TestRestart $exn) {
				$result = $exn->result;
			}
			my $prid = $sink->test->id . ":" . $result->iter;

			my $tlog = $result->test_log;
			if($ENV{VERBOSE} && @$tlog) {
				$sink->print("test log for `$prid' follows.\n");
				foreach (@$tlog) {
					chomp;
					$sink->print("  = $_\n");
				}
			}

			my $path = $sink->test->path;
			$path .= ':' . $result->iter
				if $sink->test->low < $sink->test->high;
			if(($panic_restart_id // '') eq $prid) {
				$sink->print("- double panic, skipping `$path'\n");
				$ctrl->completed($prid);
				$ctrl->restarted_with();	# pretend it's OK.
			} else {
				$sink->print("- program exited with panic in `$path'\n");
				$ctrl->restarted_with($prid);
				$panic_restart_id = $prid;
			}
			last;
		}

		try {
			$sink->tb_line($_);
		}
		catch (Mung::Error::TestRestart $exn) {
			my $test = $exn->test;
			my $result = $exn->result;
			my $rest_id = $test->id . ":" . $result->iter;
			# $sink->print("restarting on `$rest_id'.\n");
			if(defined $prev_restart_id && $prev_restart_id ne $rest_id) {
				# first restart on this ID
				$ctrl->restarted_with($rest_id);
			} else {
				# would be the second, or was otherwise already the first;
				# skip it and restart from the test after this one.
				$ctrl->restarted_with();
			}
			$prev_restart_id = $rest_id;

			# stop processing a TAP result.
			undef $suite;

			last;
		}
		catch (Mung::Error $exn) {
			$sink->print("- test aborted with exception: "
				. $exn->to_string . "\n");
			last;
		}
	}

	{
		# don't drown yourself in the bathwater
		local $SIG{INT} = 'IGNORE';
		kill "INT", -getpgrp(0);
	}
	$test_pipe->close;
}
$sink->done;


# reporting.

# test points that failed, in plan order.
my %notok_suites;
my @out;
foreach my $suite (@{$sink->suites}) {
	foreach my $tcase (@{$suite->tcases}) {
		foreach my $test (@{$tcase->tests}) {
			my @fails = map { $_->failed ? ($_) : () } @{$test->results};
			next unless @fails;

			# there are roughly two kinds of test failures. first are the
			# failed test points, and second is the outright test panic.
			#
			# TODO: for now they're reported one after the other, however,
			# it'd be useful to dump the entire TAP output on panic instead of
			# just the not-ok report log.
			#
			# TODO: there's a third kind, akin to the second, where the test
			# hits a Check-style fail_if() assertion, but that's not handled
			# here.

			my $sum_notok = 0;
			my $num_skipped = 0;
			while(@fails) {
				my $res = shift @fails;
				if(@fails && $res->eqv_to($fails[0])) {
					$num_skipped++;
					next;
				}

				my $tn = $test->name;
				$tn .= ":" . $res->iter if $test->low != $test->high;
				my $nfails = $res->failed;
				$sum_notok += $nfails;

				push @out, "Test `$tn' in case `" . $tcase->name
					. "' had $nfails failed test point(s)";
				push @out, "  " . join(" ", $res->failed);

				if($res->status eq 'FAIL') {
					push @out, "  and exited with failed assertion: " . $res->fail_msg;
				} elsif($res->status eq 'PANIC') {
					push @out, "  and hit panic condition: " . $res->fail_msg;
				}

				if($num_skipped > 0) {
					# (add note to the previous test's output.)
					my $times = $num_skipped + 1;
					push @out, "  (was run $times times total; last reported.)";
					$num_skipped = 0;
				}
			}
			$notok_suites{$suite->{name}} = 1 if $sum_notok > 0;
		}
	}
}
my $num_notok_suites = scalar(keys %notok_suites);
if($num_notok_suites > 0) {
	print "\n" if @out;
	print "$_\n" foreach(@out);
	print "There were failed test points in $num_notok_suites suite(s).\n";
}

# outputs generated by die() while parsing testbench output
if(@errors) {
	my %bywhat;
	my @what_order;
	foreach(@errors) {
		my $key = $_->{error}->text;
		push @what_order, $key unless exists $bywhat{$key};
		push @{$bywhat{$key}}, $_;
	}

	print "There were " . scalar @errors . " errors parsing test output:\n";
	# (and "scalar @what_order" different kinds of error.)
	foreach my $what (@what_order) {
		print "  $what\n";
		foreach my $err (@{$bywhat{$what}}) {
			print "\tline `$err->{line}'\n";
		}
	}
}

if($sink->incorrect || $sink->failed) {
	print "There were " . $sink->incorrect . " incorrect tests";
	my $f = $sink->failed;
	if($f) {
		print ", and $f test(s) failed";
		$status = 2;
	}
	print ".\n";
}


# exit codes: 1 for premature testbench abort, 2 for test failures.
exit $status;
