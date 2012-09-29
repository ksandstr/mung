package Mung::Error::TestRestart;
use Moose;

extends 'Mung::Error';


has 'test' => (is => 'ro', isa => 'Mung::Test', required => 1);


no Moose;
__PACKAGE__->meta->make_immutable;
