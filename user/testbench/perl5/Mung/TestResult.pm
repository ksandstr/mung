package Mung::TestResult;
use Moose;

use TryCatch;
use Struct::Compare qw/compare/;
use TAP::Parser;
use TAP::Parser::Result;

use Mung::TapError;


=head1 NAME

Mung::TestResult - container for the results of a single test program
invocation

=head1 DESCRIPTION

Objects of this class are created to retain the TAP::Parser instance, the TAP
parser's output, the test iteration number, and the output log. All accessors
are for reporting code's convenience.

=cut


has 'test' => (is => 'ro', isa => 'Mung::Test', required => 1);
has 'iter' => (is => 'ro', isa => 'Int', required => 0, default => 0);
has 'parser' => (
	is => 'ro',
	isa => 'TAP::Parser',
	required => 1,
	handles => [qw/passed failed actual_passed actual_ok actual_failed/,
		qw/todo todo_passed todo_failed skipped tests_planned tests_run/,
		qw/skip_all has_problems/]);
has 'test_log' => (is => 'ro', isa => 'ArrayRef[Str]');

# FIXME: this is a nasty overload of ->results across the related Mung::Test
# and Mung::TestResult, which leads to confusing callsites.
has 'results' => (
	is => 'ro',
	isa => 'ArrayRef[TAP::Parser::Result]',
	required => 1);

# (for future expansion)
#has 'prior_tests' => (
#	is => 'ro', isa => 'ArrayRef[Str]',		# test IDs
#	required => 0,
#	default => sub { [] });

# either false, or "PANIC", or "FAIL"
has 'status' => (is => 'rw', isa => 'Str');

# defined when $self->status isn't false
has 'fail_msg' => (is => 'rw', isa => 'Str');


sub BUILDARGS {
	my $context = shift;
	my %args = @_;

	my @rs;
	my $bo;
	while(my $result = $args{parser}->next) {
		push @rs, $result;
		if($result->is_bailout) {
			die "unexpected double bailout: " . $result->as_string if $bo;
			$bo = $result;
			$args{status} = 'FAIL';
			$args{fail_msg} = $result->as_string;
		}
	}
	$args{results} = \@rs;

	return \%args;
}


# compares the test results. this predicate controls how double results are
# presented.
sub eqv_to {
	my $self = shift;
	my $other = shift;

	die "type mismatch" unless blessed($other)
		&& $other->isa('Mung::TestResult');
	my $scd = "*** something completely different! ***";
	return 0 unless $self->test->id eq $other->test->id;

	my @fields = (qw/iter actual_passed actual_ok actual_failed/,
		qw/skipped tests_planned tests_run skip_all status fail_msg/);
	foreach my $name (@fields) {
		my $ours = $self->$name // $scd;
		my $theirs = $other->$name // $scd;
		return 0 unless compare($ours, $theirs);
	}

	return 1;
}


no Moose;
__PACKAGE__->meta->make_immutable;
