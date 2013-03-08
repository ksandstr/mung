package Mung::Loop;
use Modern::Perl '2012';
use Moose;

use TryCatch;

use Mung::Error;
use Mung::Error::TestAbort;
use Mung::Error::TestRestart;


=head1 NAME

Mung::Loop - execution of tests in a module

=head1 DESCRIPTION

Mung::Loop encapsulates the test loop, i.e. starting and stopping of the test
module, controller/sink interaction, and gathering of statistics and status
information produced thereby.

=cut


has 'module' => ( is => 'ro', does => 'Mung::Module', required => 1 );
has 'sink' => ( is => 'ro', isa => 'Mung::Sink', required => 1 );
has 'ctrl' => ( is => 'ro', isa => 'Mung::Ctrl', required => 1 );


sub run {
	my $self = shift;

	my $module = $self->module;
	my $sink = $self->sink;
	my $ctrl = $self->ctrl;

	my $prev_restart_id;
	my $panic_restart_id;
	for(;;) {
		my $test_ids = $ctrl->next_tests || last;
		if($test_ids eq 'ALL') {
			# initial run, without restart. toss old plan.
			$sink->reset;
			$module->start_test(describe => 1);
		} elsif($test_ids eq 'NEED_PLAN') {
			# initial run, controller wants to run specific tests but has no plan.
			# gather it and try again.
			$module->start_test(describe => 1, run_only => ['@']);
		} else {
			die "expected arrayref" unless ref($test_ids) eq 'ARRAY';
			$module->start_test(run_only => $test_ids);
		}

		my $ctl_seen = 0;
		try {
			while($_ = $module->next_line) {
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

				$sink->tb_line($_);
			}
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
		}
		catch (Mung::Error $exn) {
			$sink->print("- test aborted with exception: "
				. $exn->to_string . "\n");
		}

		$module->close;
	}
	$sink->done;
}


no Moose;
__PACKAGE__->meta->make_immutable;
