package Mung::Error;
use Moose;

# imitation of the Super Hacky Error module, since deprecated
has 'text' => (is => 'ro', isa => 'Str');
has 'value' => (is => 'ro');
has 'object' => (is => 'ro', isa => 'Object');

# ... no file, line though. (FIXME? how is error formed, how program signal
# abnormal condition.)


sub to_string {
	my $self = shift;
	return ref($self) . " text `" . ($self->text || '') . "'";
}


no Moose;
__PACKAGE__->meta->make_immutable;
