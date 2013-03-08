package BufferOutput;
use Modern::Perl '2012';

use Moose;

with('Mung::Output');


has 'lines' => (
	is => 'ro',
	isa => 'ArrayRef[HashRef]',
	default => sub { [] },
);


sub test_status {
	my $self = shift;
	foreach (@_) {
		push @{$self->lines}, { type => 'status', line => $_ };
	}
}


sub out_of_line {
	my $self = shift;
	foreach (@_) {
		push @{$self->lines}, { type => 'ool', line => $_ };
	}
}


no Moose;
__PACKAGE__->meta->make_immutable;
