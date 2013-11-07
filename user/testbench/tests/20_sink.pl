#!/usr/bin/perl
use Modern::Perl '2012';
use FindBin qw/$Bin/;
use lib "$Bin/lib";

use TryCatch;
use Mung::Sink;
use Mung::Ctrl;
use Mung::Loop;

use ScriptModule;
use BufferOutput;

use Test::More tests => 7;


sub make_loop {
	my $mod = shift;
	my $out = BufferOutput->new;
	my $sink = Mung::Sink->new(output => $out);
	return Mung::Loop->new(
		module => $mod, sink => $sink,
		ctrl => Mung::Ctrl->new(sink => $sink));
}


subtest "pass one, fail one" => sub {
	plan tests => 3;

	my $m = ScriptModule->new;
	$m->add_test('s', 'tc', 't0', failed => 0, rc => 0,
		body => [
			$m->plan(2),
			$m->ok('successful test point'),
			$m->not_ok('failed test point'),
		]);

	my $loop = make_loop($m);
	$loop->run;
	my $sink = $loop->sink;
	$sink->output->dump_to_diag if $ENV{VERBOSE};

	my $test = $sink->suites->[0]->tcases->[0]->tests->[0];
	ok(@{$test->results} == 1, "test was run once");
	my $res = $test->results->[0];
	ok(!$res->status, "test didn't break");
	my @failed = $res->failed;
	ok(@failed > 0 && $failed[0] == 2, "failed the second test point");
};


foreach my $pad_size (0, 1, 2) {
	subtest "assert failure (pad $pad_size)" => sub {
		plan tests => 5;

		my $m = ScriptModule->new;
		my $plan = $m->plan($pad_size + 2);
		my @pad;
		for(my $i = 0; $i < $pad_size; $i++) {
			push @pad, $m->ok('padding test ok');
		}
		$m->add_test('s', 'tc', 't1', failed => 1, rc => -9,
			body => [
				$plan, @pad,
				$m->test_fail("bogon overflow"),
			]);

		my $loop = make_loop($m);
		$loop->run;
		my $sink = $loop->sink;
		$sink->output->dump_to_diag if $ENV{VERBOSE};

		my $test = $sink->suites->[0]->tcases->[0]->tests->[0];
		ok(@{$test->results} == 1, "test was run once");
		my $res = $test->results->[0];
		ok($res->status, "test broke");
		ok($res->has_problems, "result indicates problems");
		my $failrow;
		foreach (@{$res->results}) {
			if($_->is_bailout) {
				$failrow = $_;
				last;
			}
		}
		ok($failrow, "got bailout result");
		like($failrow->as_string, qr/bogon\s+overflow/, "correct fail message");
	};
}


subtest "fail before plan" => sub {
	plan tests => 6;

	my $m = ScriptModule->new;
	$m->plan(666);		# initializes the ok-line counter
	$m->add_test('s', 'tc', 'noplanfail', failed => 1, rc => 666,
		body => [
			# FIXME: the thing with the comma? it eats babies, man
			$m->test_fail("wont even plan this out"),
		]);

	my $loop = make_loop($m);
	$loop->run;
	my $sink = $loop->sink;
	$sink->output->dump_to_diag if $ENV{VERBOSE};

	my $test = $sink->suites->[0]->tcases->[0]->tests->[0];
	ok(@{$test->results} == 1, "module was run once");
	my $res = $test->results->[0];
	my $failrow;
	foreach (@{$res->results}) {
		if($_->is_bailout) {
			$failrow = $_;
			last;
		}
	}
	ok($failrow, "got bailout result");
	like($failrow->as_string, qr/even plan this/, "correct fail message");
	ok($res->status, "test broke");
	ok($res->passed == 0, "no tests passed");
	ok($res->failed == 0, "no tests failed");
};


subtest "test count in progress report (simple)" => sub {
	plan tests => 4;

	my $m = ScriptModule->new;
	foreach (qw/first second/) {
		$m->add_test('s', 'tc', $_, failed => 0, rc => 0,
			body => [
				$m->plan(2),
				$m->ok("fine ($_)"),
				$m->ok("peachy ($_)"),
			]);
	}
	# also a failed test. this should provoke a true ->has_problems for the
	# TestResult object.
	$m->add_test('s', 'tc', "simple_fail", failed => 1, rc => 1,
		body => [
			$m->plan(2),
			$m->ok("no problemo"),
			$m->not_ok("what happen"),
		]);

	my $loop = make_loop($m);
	$loop->run;
	my $sink = $loop->sink;
	$sink->output->dump_to_diag if $ENV{VERBOSE};
	my @stats = grep /Suite\s+\w+:.*\[.*OK\]/, $sink->output->get_full_status;
	ok(@stats > 0, "status lines were produced");
	die "no matching status lines"
		unless @stats && $stats[0] =~ /Suite\s+\w+:.*\[(\d+)\/(\d+)\s+OK\]/;
	my $pass = int($1);
	my $total = int($2);
	is($pass, $total - 1, "one failed");
	is($total, 3, "reported for 3");
	isnt($total, 6, "didn't report 6");
};


subtest "test count in progress report (iterated)" => sub {
	plan tests => 3;
	use constant NUM_ITERS => 5;

	my $m = ScriptModule->new;
	foreach (qw/first second/) {
		for(my $i = 0; $i < NUM_ITERS; $i++) {
			$m->add_test('s', 'tc', $_, iter => $i,
				failed => 0, rc => 0,
				body => [
					$m->plan(2),
					$m->ok("fine ($_)"),
					$m->ok("peachy ($_)"),
				]);
		}
	}

	my $loop = make_loop($m);
	$loop->run;
	my $sink = $loop->sink;
	$sink->output->dump_to_diag if $ENV{VERBOSE};
	my @stats = grep /Suite\s+\w+:.*\[.*OK\]/, $sink->output->get_full_status;
	ok(@stats > 0, "status lines were produced");
	die "no matching status lines"
		unless @stats && $stats[0] =~ /Suite\s+\w+:.*\[(\d+)\/(\d+)\s+OK\]/;
	my $pass = int($1);
	my $total = int($2);
	is($pass, $total, "all passed");
	is($total, 2, "reported for 2");
};
