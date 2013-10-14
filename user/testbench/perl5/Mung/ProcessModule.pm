package Mung::ProcessModule;
use Modern::Perl '2012';
use Moose;

use IO::Pipe;

with 'Mung::Module';

=head1 NAME

Mung::ProcessModule - module interface for receiving stdout from a shell command

=head1 DESCRIPTION

This implementation of Mung::Module launches a program, specified with the
C<command> constructor parameter. Environment variables can be given in
C<env>.

=cut


has 'command' => ( is => 'ro', isa => 'Str', required => 1 );
has 'env' => ( is => 'ro', isa => 'HashRef[Str]', required => 0,
	default => sub { {} } );

# private state
has 'file' => ( is => 'rw', isa => 'Maybe[IO::Handle]' );
has 'child' => ( is => 'rw', isa => 'Maybe[Int]' );


sub _subprocess {
	my $self = shift;
	my $parms = shift;

	if(@$parms) {
		$ENV{TESTBENCH_OPTS} = join(" ", @$parms);
	} else {
		delete $ENV{TESTBENCH_OPTS};
	}

	my $env = $self->env;
	$ENV{$_} = $env->{$_} for keys %$env;

	close STDOUT;
	open(STDOUT, ">&", $self->file) or die "can't redup STDOUT: $!";
	close STDERR;
	open(STDERR, "> /dev/null") or die "can't reassign STDERR: $!";

	exec($self->command) or die "can't exec command: $!";
}


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

	my $pipe = IO::Pipe->new;
	my $child = fork();
	if($child == 0) {
		$pipe->writer();
		$self->file($pipe);
		my $rc = $self->_subprocess(\@parms);
		exit $rc;
	} else {
		$pipe->reader();
		$self->file($pipe);
		$self->child($child);
	}
}


sub next_line {
	my $self = shift;

	my $file = $self->file || return;
	return scalar <$file>;
}


sub close {
	my $self = shift;

	my $file = $self->file;
	if($file) {
		kill "INT", $self->child;
		$file->close;
		my $dead = wait();
		warn "expected " . $self->child . ", waited for $dead"
			unless $dead == $self->child;

		$self->file(undef);
		$self->child(undef);
	}
}


no Moose;
__PACKAGE__->meta->make_immutable;
