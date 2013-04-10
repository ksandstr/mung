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

use Test::More tests => 5;


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
