package Mung::TestCase;
use Moose;
use TryCatch;

has 'name' => (is => 'ro', isa => 'Str', required => 1);

# number of skipped runs of a single test (incl. loop iters)
has 'tests_skipped' => (is => 'rw', isa => 'Int', default => 0);

has 'tests' => (
	is => 'ro',
	isa => 'ArrayRef[Mung::Test]',
	default => sub { [] });

no Moose;
__PACKAGE__->meta->make_immutable;
