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


# TODO: this could try to join consecutive test_status bits and then print
# them out in diags separated by linefeed. as that's what Mung::Sink will
# expect anyway, because of console semantics.
#
# that could be a distinct method, in fact.
sub dump_to_diag {
	my $self = shift;
	Test::More::diag("sink output follows:");
	foreach (@{$self->lines}) {
		my ($type, $line) = @$_{qw/type line/};
		Test::More::diag("$type: $line");
	}
}


no Moose;
__PACKAGE__->meta->make_immutable;
