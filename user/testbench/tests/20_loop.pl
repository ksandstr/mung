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

use Test::More tests => 4;


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
		plan tests => 4;

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
		ok($res->has_problems, "result indicates problems");
		ok($res->status eq 'FAIL', "status is FAIL");
		like($res->fail_msg, qr/bogon\s+overflow/, "correct fail message");
	};
}
