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

use Test::More tests => 2;


sub make_loop {
	my $mod = shift;
	my $out = BufferOutput->new;
	my $sink = Mung::Sink->new(output => $out);
	return Mung::Loop->new(
		module => $mod, sink => $sink,
		ctrl => Mung::Ctrl->new(sink => $sink));
}


my $m = ScriptModule->new;
$m->add_test('s', 'tc', 't0', failed => 0, rc => 0,
	body => [
		$m->plan(2),
		$m->ok('successful test point'),
		$m->not_ok('failed test point'),
	]);

if(0) {
	$m->start_test(describe => 1);
	while(my $x = $m->next_line) {
		chomp $x;
		diag("test line: `$x'");
	}
	$m->reset;
}

my $loop = make_loop($m);
$loop->run;
my $sink = $loop->sink;

if(0) {
	diag("sink output follows:");
	my $out = $sink->output;
	foreach (@{$out->lines}) {
		my ($type, $line) = @$_{qw/type line/};
		diag("$type: $line");
	}
}

my $test = $sink->suites->[0]->tcases->[0]->tests->[0];
ok(@{$test->results} == 1, "test was run once");
my $res = $test->results->[0];
my @failed = $res->failed;
ok(@failed > 0 && $failed[0] == 2, "failed the second test point");
