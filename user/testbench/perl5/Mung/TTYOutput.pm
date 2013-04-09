package Mung::TTYOutput;
use Modern::Perl '2012';
use Moose;


has 'break' => (is => 'rw', isa => 'Bool');
has 'line_buf' => (is => 'rw', isa => 'Str', default => '');


# see Mung::Output
sub test_status {
	my $self = shift;

	$self->repair if @_ && $self->break;
	foreach (@_) {
		print;
		if(/\n/) {
			my @subs = split /\n/;
			$self->line_buf(/\n$/ ? '' : pop @subs);
		} else {
			$self->line_buf($self->line_buf . $_);
		}
	}
}


sub out_of_line {
	my $self = shift;

	if($self->line_buf && !$self->break) {
		$self->break(1);
		print "\n";
	}
	print foreach @_;
}


sub repair {
	my $self = shift;

	die "called when not broken" unless $self->break;
	print $self->line_buf;
	$self->break(0);
}


with 'Mung::Output';

no Moose;
__PACKAGE__->meta->make_immutable;
