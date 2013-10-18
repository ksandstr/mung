#!/usr/bin/perl -w
use Modern::Perl '2012';
use FindBin qw/$Bin/;
use lib "$Bin/lib";

use IO::File;
use List::MoreUtils qw/any/;
use Mung::ProcessModule;
use Mung::Sink;
use Mung::Ctrl;
use Mung::Loop;

use BufferOutput;

use Test::More tests => 4;

# tests testbench's functions for producing TAP output. this confirms that all
# the TAP primitives that're claimed, are also correctly passed by Loop and
# accepted by Sink.
#
# this is an end-to-end test for all of the test control and reporting
# functionality, with the exception of report-generation which is to be tested
# separately using fabricated inputs.


my $TOPLEVEL = "$Bin/../../..";
my $PROGRAM = "run.sh";
die "program not found" unless -x "$TOPLEVEL/$PROGRAM";
chdir $TOPLEVEL;

my @PMARGS = ( meta => 1 );


# parse meta_suite.c's tests and comments.
sub parse_meta_suite {
	my $file = shift;

	my %tests;
	my $cur;
	my $looped;
	while(<$file>) {
		if(/^START_TEST\((\w+)\)/) {
			$cur = { name => $1 };
			$looped = 0;
			#print STDERR "start test $cur->{name}\n";
		} elsif(/^START_LOOP_TEST\((\w+),\s*\w+,\s*(\d+),\s*(\d+)\)/) {
			$cur = { name => $1, low => $2, high => $3 };
			$looped = 1;
			#print STDERR "start loop test $cur->{name}\n";
		} elsif(/POINT\s+(\d+)\s*:\s*(.*)$/) {
			my $pt = int($1);
			my $desc = $2;
			$desc =~ s/\s*\*\/$//;
			$desc =~ s/\s+/ /g;
			#print STDERR "add point $pt = `$desc'\n";
			$cur->{points}->[$pt] = parse_point($desc);
		} elsif(/^END_TEST/) {
			$cur->{points} //= [];
			$tests{$cur->{name}} = $cur;
			undef $cur;
		}
	}
	return \%tests;
}


sub parse_point {
	my $cap_desc = shift;

	if($cap_desc =~ /^\[/) {
		# iterations, separated
		my @iters;
		while($cap_desc =~ /\[(\d+):\s*([^\]]+)\]/g) {
			$iters[$1] = parse_point($2);
		}
		return \@iters;
	}

	my $orig = $cap_desc;
	my $todo = ($cap_desc =~ s/\s+T//);	# sign of Todo-ness
	my $desc = lc($cap_desc);
	if($desc eq 'ok' || $desc eq 'not ok') {
		# simple ok/notok.
		return { ok => $desc ne 'not ok', todo => $todo };
	} else {
		print STDERR "warning: unrecognized point `$orig'\n";
	}

	return $desc;
}


# TODO: move to utility package
sub btos { $_[0] ? "true" : "false" }


my $meta_src = IO::File->new("< $TOPLEVEL/user/testbench/meta_suite.c")
	or die "can't open meta_suite.c: $!";
my $meta_tests = parse_meta_suite($meta_src);
$meta_src->close;


# returns the sink
sub run_program {
	my @pmargs = @_;

	my $module = Mung::ProcessModule->new(
		command => "./$PROGRAM -display none");
	my $sink = Mung::Sink->new(output => BufferOutput->new);
	my $ctrl = Mung::Ctrl->new(sink => $sink, max_ids_len => 70);
	$sink->on_complete_fn(sub { $ctrl->completed($_[0], $_[1]->iter); });
	my $loop = Mung::Loop->new(module => $module, sink => $sink,
		ctrl => $ctrl, test_args => \@pmargs);
	$loop->run;

	return $sink;
}


# part 1: running the meta suite should not cause the program to hang.
my $sink = run_program(@PMARGS);
pass("program didn't hang");
ok(any { $_ eq 'meta' } (map { $_->name } @{$sink->suites}),
	"meta suite was executed");

# part 2: check for test OKs.
subtest "test OKs" => sub {
	my $n_tests = @{$sink->plan};
	plan tests => $n_tests;

	foreach my $test (sort @{$sink->plan}) {
		my $ann = $meta_tests->{$test->name};
		if(!$ann) {
			pass("test `" . $test->name . "' has no annotations");
			next;
		}

		my $passed = 0;
		my $tested = 0;
		foreach my $res (@{$test->results}) {
			my $iter = $res->iter;
			foreach my $pt (@{$res->results}) {
				next unless $pt->is_test;
				my $n = $pt->number;
				my $expect = $ann->{points}->[$n] // next;
				$expect = ($expect->[$iter] // next) if ref($expect) eq 'ARRAY';
				my $ok = $expect->{ok};
				if($expect->{todo} && ($ok xor $pt->is_actual_ok)
					|| !$expect->{todo} && ($ok xor $pt->is_ok))
				{
					my $e = btos($ok);
					my $todo = $expect->{todo} ? "TODO " : "";
					diag("${todo}test `" . $test->name . "' iter $iter: "
						. "expected " . btos($e)
						. ", got " . btos($pt->is_actual_ok));
					diag($pt->as_string);
				} else {
					$passed++;
				}
				$tested++;
			}
		}
		ok($passed == $tested, "result for " . $test->name
			. " had right OKs (got $passed, wanted $tested)");
	}
};

# part 3: check various kinds of test plan (none, skip, late)
subtest "test plans" => sub {
	plan tests => 7;

	my $tests = $sink->test_by_path;

	my $no = $tests->{"meta:plan:no_plan"}
		|| die "no_plan wasn't run";
	my $res = $no->results->[0];
	ok(!$res->tests_planned, "no_plan had no test plan");
	ok(!$res->is_good_plan, "no_plan wasn't good");

	my $skip = $tests->{"meta:plan:skip_plan"}
		|| die "skip_plan wasn't run";
	$res = $skip->results->[0];
	ok($res->skip_all, "skip_plan skipped all tests");
	ok(!$res->tests_planned, "skip_plan had no test plan");
	ok($res->is_good_plan, "skip_plan was good");

	my $late = $tests->{"meta:plan:late_plan"}
		|| die "late_plan wasn't run";
	$res = $late->results->[0];
	ok($res->tests_planned == 3, "late_plan had 3 test points");
	if(!ok($res->is_good_plan, "late_plan was good")) {
		diag("tests_run = " . $res->tests_run);
	}
};

# TODO: test for bailouts, assert failures, segfaults, and tests taking too
# long.
