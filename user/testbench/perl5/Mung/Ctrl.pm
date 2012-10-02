package Mung::Ctrl;
use Moose;

use Mung::Sink;

# the test controller. sniffs out and prepares the per-restart test plan.

has 'sink' => (
	is => 'ro', isa => 'Mung::Sink',
	required => 1, handles => [ 'plan' ] );

# set of id:iter of completed tests. completion means that the test shouldn't
# be run again. never modified by instances.
has 'completed' => (
	is => 'ro', isa => 'HashRef[Int]',
	default => sub { {} } );


# configurables.
#
# max_per_run:
#   at most "tests" are returned in next_tests, but perhaps less if their IDs
#   (and a single separator character for each besides the first) only barely
#   fit into "id_len" characters.
has 'max_per_run' => (
	is => 'ro', required => 0, default => sub { {} },
	isa => 'HashRef');


# internals.
has 'remain' => ( is => 'rw', default => sub { [] } );


# return value: ArrayRef['$id:$iter'] | 'ALL'
# termination is indicated by undefined return value.
sub next_tests {
	my $self = shift;

	# deviates from original, which did this per whether @test_remain is empty
	return 'ALL' unless %{$self->plan};

	# do up to 80 characters' worth of id:iter pairs at a time.
	my $ch_lim = $self->max_per_run->{id_len};
	my $test_lim = $self->max_per_run->{tests};
	my @only;
	my $count = 0;		# of characters
	while(!$test_lim || @only < $test_lim) {
		my ($id, $test_inst) = @{shift @{$self->remain} || last};
		next if $self->completed->{$id};
		my $len = length($id) + ($count > 0 ? 1 : 0);
		last if $ch_lim && $count + $len > $ch_lim;
		$count += $len;
		push @only, $id;
	}
	#if(@only) {
	#	$self->sink->print("this run, restarting `" . join(", ", @only) . "'\n");
	#}

	return @only ? \@only : undef;
}


# called when a restart occurred. if parameters (test id:iter strings) exist
# within $self->completed, they are removed.
sub restarted_with {
	my $self = shift;
	delete $self->completed->{$_} foreach @_;

	my @rem;
	while(my ($path, $pt) = each %{$self->plan}) {
		for(my $i = $pt->low; $i <= $pt->high; $i++) {
			my $n = $pt->id . ":$i";
			push @rem, [ $n, $pt ] unless exists $self->completed->{$n};
		}
	}
	# $self->sink->print(scalar(@rem) . " tests remain.\n");
	$self->remain(\@rem);
}


no Moose;
__PACKAGE__->meta->make_immutable;
