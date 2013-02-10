package Mung::Test;
use Moose;

use Mung::TestResult;


has [ qw/suite tcase name id/ ] => (is => 'ro', isa => 'Str', required => 1);
has [ qw/low high/ ] => (is => 'rw', isa => 'Int', default => 0);

has 'results' => (
	is => 'rw', isa => 'ArrayRef[Mung::TestResult]',
	default => sub { [] },
	documentation => q{Completed results for this test.});
has 'current_result' => (
	is => 'rw', isa => 'Maybe[Mung::TestResult]',
	handles => [ qw/log tap_line/ ]);



sub path {
	my $self = shift;
	return $self->suite . ":" . $self->tcase . ":" . $self->name;
}


# open a result object on this test.
sub begin {
	my $self = shift;
	die "double begin" if $self->current_result;

	my %args = @_;
	my $iter = $args{iter} // 0;
	my $report_hook = $args{report_msg_hook};

	# TODO: pass prior_tests also!
	my @parms = (test_id => $self->id, iter => $iter);
	if($report_hook) {
		push @parms, (report_msg_hook => $report_hook);
	}
	$self->current_result(Mung::TestResult->new(@parms));

	return $self->current_result;
}


# close the result object. arguments as for Mung::TestResult->done .
sub end {
	my $self = shift;
	my $cr = $self->current_result || die "no active test";
	$cr->done(@_);
	push @{$self->results}, $cr;
	$self->current_result(undef);
	return $cr;
}


no Moose;
__PACKAGE__->meta->make_immutable;
