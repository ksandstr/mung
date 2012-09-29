package Mung::TapError;
use Moose;

has 'text' => (is => 'ro', isa => 'Str');
has 'value' => (is => 'ro');
has 'object' => (is => 'ro', isa => 'Object');


no Moose;
__PACKAGE__->meta->make_immutable;
