package Mung::Error::TestRestart;
use Moose;

extends 'Mung::Error';


has 'test' => (is => 'ro', isa => 'Mung::Test', required => 1);
has 'result' => (is => 'ro', isa => 'Mung::TestResult', required => 1);


no Moose;
__PACKAGE__->meta->make_immutable;
