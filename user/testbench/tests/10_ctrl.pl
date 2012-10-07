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

use Test::More tests => 7;


# removes capitalized args. counts as one test point due to new_ok.
sub make_ctrl {
	my %args = @_;

	my $sink = Mung::Sink->new(output => DiscardOutput->new);
	my @clean_args = map { uc eq $_ ? () : ($_ => $args{$_}) } keys(%args);
	my $ctrl = new_ok('Mung::Ctrl' => [
		@clean_args, sink => $sink,
		completed => $sink->completed_ref,
	]);

	return $ctrl;
}


# secret control args not passed to Mung::Sink constructor:
# LIMIT => Int, defaults to length of @test_names * 2
# MIDDLE => sub($ctrl), run before plan is added
#
# this counts as one test point due to make_ctrl().
sub ctrl_case {
	my $args = shift;
	my $sub = shift;

	my $ctrl = make_ctrl(%$args);
	&{$args->{MIDDLE}}($ctrl) if $args->{MIDDLE};

	my @test_names = add_plan($ctrl->sink);
	$ctrl->restarted_with();	# produce ->remain
	my @observed;	# arrays of $id:$iter
	my $limit = $args->{LIMIT} // (scalar(@test_names) * 2);
	for(my $iters = 0; $iters < $limit; $iters++) {
		my $next = $ctrl->next_tests;
		# diag("next is `" . ($next // '<undef>') . "'");
		last unless $next;			# some men just want to watch the world burn.
		push @observed, $next;
		my $then = &$sub($ctrl, $next);
		last if $then eq 'LAST';
	}

	return (\@test_names, \@observed);
}


sub add_plan {
	my $sink = shift;

	# feed a simple test plan to the sink. this could be in a different,
	# sink-related module.
	my @words = qw/foo bar wibble zort hurg/;
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

	return @test_names;
}


# regardless of how many tests are run, in the absence of fail-restarts they
# should run in plan order. confirm this for groups of 1 to 7 inclusive.
#
# TODO: also vary allow_all, if that makes any sense from a plan-feeding point
# of view.
foreach my $max_per_run (1..7) {
	subtest "controller obeys plan for max_per_run => $max_per_run" => sub {
		plan tests => 4;
		my ($test_names, $observed) = ctrl_case({
				# for now, a current failure case.
				allow_all => 0, max_per_run => $max_per_run,
				MIDDLE => sub {
					my $ctrl = shift;
					ok($ctrl->next_tests eq 'NEED_PLAN',
						"controller in !allow_all asks for test plan");
				},
			}, sub {
				my ($ctrl, $next) = @_;
				$ctrl->completed->{$_} = 1 foreach (@$next);
				$ctrl->restarted_with();	# plain success.
			});
		my @all = map { @$_ } @$observed;
		ok(scalar(@all) == scalar(@$test_names),
			"correct number of tests were launched");
		my $n_ix = 0;
		OUTER: while(@$observed) {
			foreach (@{shift @$observed}) {
				my ($p, $id) = @{$test_names->[$n_ix++]};
				s/:\d+$//;
				if($_ ne $id) {
					diag("expected next test id `$id' (for `$p'), got `$_'");
					last OUTER;
				}
			}
		}
		ok(!@$observed, "controller runs tests in correct order");
	};
}

# TODO: test for max_ids_len being respected.
