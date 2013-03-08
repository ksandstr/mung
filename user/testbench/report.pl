#!/usr/bin/perl
use Modern::Perl '2012';
use utf8;
use feature "switch";

use FindBin qw($Bin);
use lib "$Bin/perl5";

use TryCatch;
use Moose::Util qw/apply_all_roles/;
use List::Util qw/sum/;
use List::MoreUtils qw/none/;

use Mung::Sink;
use Mung::Ctrl;
use Mung::Loop;
use Mung::ProcessModule;
use Mung::Test;
use Mung::TestResult;
use Mung::TapError;
use Mung::Restarting;
use Mung::ConsoleReport;


my $TOPLEVEL = ".";
-f "$TOPLEVEL/run.sh" || die "can't find run.sh in path `$TOPLEVEL'";

$SIG{PIPE} = "IGNORE";		# and smoke it.
$| = 1;


# parameters affecting the test process.
my @roles;
my @ctrl_param = ( max_ids_len => 70 );
if($ENV{TEST_SLOW}) {
	push @ctrl_param, ( allow_all => 0, max_per_run => 1 );
} elsif(!$ENV{TEST_QUICK}) {
	push @roles, 'Mung::Restarting';
}


my $status = 0;
my @errors;		# FIXME: fill this in

my $module = Mung::ProcessModule->new(command => './run.sh -display none');
my $sink = Mung::Sink->new(output => Mung::ConsoleReport->new);
my $ctrl = Mung::Ctrl->new(@ctrl_param, sink => $sink);
$sink->on_complete_fn(sub { $ctrl->completed($_[0], $_[1]->iter); });

apply_all_roles($sink, @roles) if @roles;
my $loop = Mung::Loop->new(module => $module, sink => $sink, ctrl => $ctrl);
$loop->run;

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
# FIXME: these are never filled in. they should be.
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
