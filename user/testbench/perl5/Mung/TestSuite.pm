package Mung::TestSuite;
use Moose;
use TryCatch;

has 'name' => (is => 'ro', isa => 'Str', required => 1);
has 'tcases' => (
	is => 'ro',
	isa => 'ArrayRef[Mung::TestCase]',
	default => sub { [] });

no Moose;
__PACKAGE__->meta->make_immutable;
