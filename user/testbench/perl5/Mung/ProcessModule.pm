package Mung::ProcessModule;
use Modern::Perl '2012';
use Moose;

with 'Mung::Module';

=head1 NAME

Mung::ProcessModule - module interface for receiving stdout from a shell command

=head1 DESCRIPTION

This implementation of Mung::Module launches a program, specified with the
C<command> constructor parameter.

=cut


has 'command' => ( is => 'ro', isa => 'Str', required => 1 );


# private state
has 'file' => ( is => 'rw', isa => 'Maybe[IO::Handle]' );


sub start_test {
	my $self = shift;
	my %opts = @_;

	my @parms;
	foreach(keys %opts) {
		my $key = $_;
		my $val = $opts{$key};
		$val = join("+", @$val) if ref($val) eq 'ARRAY';
		$key =~ s/_//g;
		push @parms, "$key=$val";
	}
	if(@parms) {
		$ENV{TESTBENCH_OPTS} = join(" ", @parms);
	} else {
		delete $ENV{TESTBENCH_OPTS};
	}
	my $file = IO::File->new($self->command . " 2>/dev/null |");
	$self->file($file);
}


sub next_line {
	my $self = shift;

	my $file = $self->file || return;
	return scalar <$file>;
}


# FIXME: this whacks all other subprocesses, too. probably not what we
# actually want, but so far doesn't break shit too wildly either.
sub close {
	my $self = shift;

	my $file = $self->file;
	if($file) {
		{
			# don't drown yourself in the bathwater
			local $SIG{INT} = 'IGNORE';
			kill "INT", -getpgrp(0);
		}
		$file->close;
		$self->file(undef);
	}
}


no Moose;
__PACKAGE__->meta->make_immutable;