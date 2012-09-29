package Mung::Test;
use Moose;

use Mung::TestResult;


has [ qw/suite tcase name id/ ] => (is => 'ro', isa => 'Str', required => 1);
has [ qw/low high/ ] => (is => 'rw', isa => 'Int', default => 0);

has 'results' => (
	is => 'rw', isa => 'ArrayRef[Mung::TestResult]',
	default => sub { [] });
has 'current_result' => (is => 'rw', isa => 'Maybe[Mung::TestResult]');



sub path {
	my $self = shift;
	return $self->suite . ":" . $self->tcase . ":" . $self->name;
}


sub begin_test {
	my $self = shift;
	my %args = @_;

	if($self->current_result) {
		warn "test result wasn't ended properly";
		$self->end_test;
	}
	# TODO: pass prior_tests also!
	my @parms = (test_id => $self->id, iter => $args{iter} || 0);
	if($args{report_msg_hook}) {
		push @parms, (report_msg_hook => $args{report_msg_hook});
	}
	$self->current_result(Mung::TestResult->new(@parms));
	push @{$self->results}, $self->current_result;

	return $self->current_result;
}


sub end_test {
	my $self = shift;
	my $cr = $self->current_result || die "no test active";
	my $result = $self->current_result;
	$result->done(@_);
	$self->current_result(undef);
	return $result;
}


no Moose;
__PACKAGE__->meta->make_immutable;
