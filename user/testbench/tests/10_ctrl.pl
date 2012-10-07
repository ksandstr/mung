#!/usr/bin/perl
package DiscardOutput;
use Moose;


sub test_status {
	# nothing
}

sub out_of_line {
	# zilch
}


with 'Mung::Output';


package main;
use strict;
use warnings;

use Mung::Ctrl;

use Test::More tests => 3;


my $sink = Mung::Sink->new(output => DiscardOutput->new);
my $ctrl = new_ok('Mung::Ctrl' => [
	sink => $sink,
	completed => $sink->completed_ref,
	# for now, a current failure case.
	allow_all => 0, max_per_run => 1, max_ids_len => 80,
]);

ok($ctrl->next_tests eq 'NEED_PLAN',
	"controller in !allow_all asks for test plan");

# feed a simple test plan to the sink. this could be in a different module.
my @words = qw/foo bar arg wibble zort hurg fart/;
my $id_count = 1;
my @test_names;		# of [ $path, $id ]
foreach my $suite_name (@words) {
	$sink->desc_line('suite', name => $suite_name);
	foreach my $tcase_name (@words) {
		next if $tcase_name eq $suite_name;
		$sink->desc_line('tcase', name => $tcase_name);
		foreach my $test_name (@words) {
			next if $test_name eq $tcase_name || $test_name eq $suite_name;
			my $id = "x" . ($id_count++);
			push @test_names, [ "$suite_name:$tcase_name:$test_name", $id ];
			$sink->desc_line('test', name => $test_name, id => $id,
				low => 0, high => 0);
		}
	}
}

my @observed;	# arrays of $id:$iter
for(my $iters = 0; $iters < scalar(@test_names) * 2; $iters++) {
	my $next = $ctrl->next_tests;
	last unless $next;			# some men just want to watch the world burn.
	push @observed, $next;
	$ctrl->restarted_with();	# plain success.
}
my $n_ix = 0;
OUTER: while(@observed) {
	foreach (@{shift @observed}) {
		my ($p, $id) = @{$test_names[$n_ix++]};
		s/:\d+//;
		next if $_ eq $id;

		diag("expected next test id `$id' (for `$p'), got `$_'");
		last OUTER;
	}
}
ok(!@observed, "controller runs tests in correct order");
# TODO: this should cover the allow_all, max_per_run space far better. also,
# tests for max_ids_len being respected.
#
# so the "build sink, ctrl, plan, run & conditionally restart" part should be
# in a subroutine, returning (\@test_names, \@observed), and passing ($ctrl,
# $sink, $next) to its function parameter.
