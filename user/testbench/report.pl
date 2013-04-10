#!/usr/bin/perl
use Modern::Perl '2012';
use utf8;

use FindBin qw($Bin);
use lib "$Bin/perl5";

use Moose::Util qw/apply_all_roles/;

use Mung::Sink;
use Mung::Ctrl;
use Mung::Loop;
use Mung::ProcessModule;
use Mung::Restarting;
use Mung::TTYOutput;
use Mung::TextReport;


my $TOPLEVEL = ".";
-f "$TOPLEVEL/run.sh" || die "can't find run.sh in path `$TOPLEVEL'";

$SIG{PIPE} = "IGNORE";		# and smoke it.
$| = 1;


# parameters affecting the test process.
my @roles;
my @ctrl_param = ( max_ids_len => 70 );
if($ENV{TEST_SLOWER}) {
	push @ctrl_param, ( allow_all => 0, max_per_run => 1 );
} elsif($ENV{TEST_SLOW}) {
	push @roles, 'Mung::Restarting';
}


my $module = Mung::ProcessModule->new(command => './run.sh -display none');
my $sink = Mung::Sink->new(output => Mung::TTYOutput->new);
my $ctrl = Mung::Ctrl->new(@ctrl_param, sink => $sink);
$sink->on_complete_fn(sub { $ctrl->completed($_[0], $_[1]->iter); });

apply_all_roles($sink, @roles) if @roles;
my $loop = Mung::Loop->new(module => $module, sink => $sink, ctrl => $ctrl);
$loop->run;

my $rept = Mung::TextReport->new;
my $status = $rept->print_report(*STDOUT{IO}, $sink);

# exit codes: 1 for premature testbench abort, 2 for test failures.
# TODO: figure this out somewhere else.
exit $status;
