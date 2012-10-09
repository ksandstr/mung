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
$SIG{INT} = "IGNORE";		# my regards to 007
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
sub report_msg { $sink->print("$_\n") foreach @_; }
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
			# also, the test log isn't very useful -- it's just a whine from
			# some class or another about how it doesn't understand an assert
			# failure message.
			#
			# this case shouldn't even be here today.
			$sink->print("*** test program panic: `$1'\n");
			my $result = $sink->test->end_test;
			my $tlog = $result->test_log;
			if(@$tlog) {
				my $path = $sink->test->path;
				$path .= ":" . $result->iter
					if $sink->test->low < $sink->test->high;
				$sink->print("test log for `$path' follows.\n");
				foreach (@$tlog) {
					chomp;
					$sink->print("  = $_\n");
				}
			}
			my $prid = $sink->test->id . ":" . $result->iter;
			if(($panic_restart_id // '') eq $prid) {
				$sink->print("double panic, skipping `$prid' entirely\n");
				$ctrl->completed->{$prid} = 1;
				$ctrl->restarted_with();	# pretend it's OK.
			} else {
				$ctrl->restarted_with($sink->test->id . ":" . $result->iter);
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
			# report_msg("restarting on `$rest_id'.");
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
			report_msg("test aborted: " . $exn->to_string);
			last;
		}
	}
	kill "INT", -getpgrp(0);
	$test_pipe->close;
}

# terminate suite line
print "\n";

# reporting.

# test points that failed, in plan order.
my %notok_suites;
my @out;
foreach my $suite (@{$sink->suites}) {
	foreach my $tcase (@{$suite->tcases}) {
		foreach my $test (@{$tcase->tests}) {
			my @results = @{$test->results};
			next if none { @{$_->not_ok} > 0 } @results;

			my $res = pop @results;
			my $sum = scalar @{$res->not_ok};
			$notok_suites{$suite->{name}} = 1;

			my $tn = $test->name;
			$tn .= ":" . $res->iter if $test->low != $test->high;
			push @out, "Test `$tn' in case `" . $tcase->name
				. "' had $sum failed test point(s):";
			foreach my $p (@{$res->not_ok}) {
				my $report = $p->{report};
				$report =~ s/\[\w+:\w+\]\s?//;
				push @out, "  $p->{id}:\t$report";
			}

			if(@results) {
				# FIXME: report on differing @{$_->not_ok} lists and so forth
				my $times = scalar(@results) + 1;
				push @out, "  (was run $times times total; last reported.)";
			}
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
