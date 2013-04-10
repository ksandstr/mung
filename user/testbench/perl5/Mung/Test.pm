package Mung::Test;
use Moose;

use TAP::Parser;
use Mung::TestResult;


=head1 NAME

Mung::Test - encapsulation of each test program in a suite's test case

=head1 DESCRIPTION

This class retains the test program's metadata, i.e. the suite name, tcase
name, test name, and the test ID as communicated by the "describe" protocol.
The "low" and "high" iteration parameters are kept because they were there.

=cut


has [ qw/suite tcase name id/ ] => (is => 'ro', isa => 'Str', required => 1);
has [ qw/low high/ ] => (is => 'rw', isa => 'Int', default => 0);

has 'results' => (
	is => 'rw', isa => 'ArrayRef[Mung::TestResult]',
	default => sub { [] },
	documentation => q{Completed results for this test.});
has 'log_lines' => ( is => 'rw', isa => 'ArrayRef[Str]', default => sub { [] } );
has 'iter' => ( is => 'rw', isa => 'Int' );


# private bits
has 'tap_buf' => ( is => 'rw', isa => 'ArrayRef[Str]', default => sub { [] } );



sub path {
	my $self = shift;
	return $self->suite . ":" . $self->tcase . ":" . $self->name;
}


sub log {
	my $self = shift;
	push @{$self->log_lines}, @_;
}


sub tap_line {
	my $self = shift;
	push @{$self->tap_buf}, @_;
	$self->log(@_);
}


# open a result object on this test.
sub begin {
	my $self = shift;

	my %args = @_;
	$self->iter($args{iter} // 0);
	$self->tap_buf([]);
	$self->log_lines([]);
}


# close the result object. arguments as for Mung::TestResult->done, i.e. panic
# or failmsg where appropriate
sub end {
	my $self = shift;

	# to keep TAP::Parser from becoming confused by a single-line stream (as
	# when the first assert blows right after the plan(), with no test points
	# in between), recognize it and add an all-skip plan.
	my $tap = join("\n", @{$self->tap_buf});
	my $lines = @{$self->tap_buf};
	if($lines == 1) {
		$tap = "1..0\n$tap";
	}

	my $result = Mung::TestResult->new(@_,
		test => $self, iter => $self->iter,
		parser => TAP::Parser->new({ tap => $tap }),
		test_log => $self->log_lines);
	push @{$self->results}, $result;

	$self->begin(iter => $self->iter + 1);	# clear the log bufs

	return $result;
}


no Moose;
__PACKAGE__->meta->make_immutable;
