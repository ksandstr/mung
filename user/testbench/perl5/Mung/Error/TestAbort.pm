package Mung::Error::TestAbort;
use Moose;

extends 'Mung::Error';


no Moose;
__PACKAGE__->meta->make_immutable;
