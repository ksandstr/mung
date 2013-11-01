package ScriptModule;
use Modern::Perl '2012';
use Moose;

use IO::String;

with('Mung::Module');

=head1 NAME

ScriptModule - a scriptable Mung::Module implementation

=head1 DESCRIPTION

Outputs a stream of testbench output, or indicates end-of-stream, until
->reset is called.

=cut


has 'pos' => ( is => 'rw', isa => 'Int', default => 0 );
has 'lines' => ( is => 'ro', isa => 'ArrayRef[Str]', default => sub { [] } );

# per-run parameters
has 'describe' => ( is => 'rw', isa => 'Bool' );
has 'run_only' => ( is => 'rw', isa => 'Maybe[ArrayRef[Str]]' );

# per-run state
has 'desc_stream' => (
	is => 'ro',
	isa => 'IO::String',
	default => sub { IO::String->new },
);

# private bits
has '_iterated' => (
	is => 'ro',
	isa => 'ArrayRef',
	default => sub { [] },
);
has '_desc_before_iter' => ( is => 'rw', isa => 'Int' );


sub start_test {
	my $self = shift;

	while(@_) {
		my $n = shift;
		my $v = shift;
		$self->$n($v);
	}
	$self->build_iter_desc if @{$self->_iterated};
	$self->desc_stream->pos(0);
}


sub next_line {
	my $self = shift;
	if($self->describe) {
		my $f = $self->desc_stream;
		return <$f> unless $f->eof;
	}

	if($self->pos < @{$self->lines}) {
		my $p = $self->pos;
		$self->pos($p + 1);
		return $self->lines->[$p];
	} elsif($self->pos == @{$self->lines}) {
		$self->pos($self->pos + 1);
		return "*** scripted test completed\n";
	} else {
		return undef;
	}
}


sub close {
	my $self = shift;
	$self->describe(0);
	$self->run_only(undef);
}


sub reset {
	my $self = shift;
	$self->pos(0);
}


sub add_test {
	my $self = shift;
	my $suite = shift;
	my $tcase = shift;
	my $test = shift;
	my %args = @_;

	my $id = $args{id} // ($suite . $tcase . $test);

	$self->add_line(
		"*** begin suite `$suite'\n",
		"*** begin tcase `$tcase'\n");
	if($args{iter}) {
		$self->add_line("*** begin test `$test' iter $args{iter}\n");
	} else {
		$self->add_line("*** begin test `$test'\n");
	}

	$self->add_line(@{$args{body} // []});
	if($args{failed}) {
		$self->add_line("*** test `$test' failed, rc $args{rc}\n");
	} else {
		$self->add_line("*** end test `$test' rc $args{rc}\n");
	}
	$self->add_line(
		"*** end tcase `$tcase'\n",
		"*** end suite `$suite'\n");

	my $desc = $self->desc_stream;
	print $desc "*** desc suite `$suite'\n";
	print $desc "*** desc tcase `$tcase'\n";
	# TODO: die when a test is first entered as non-iterated, and then with an
	# iteration parameter.
	if(!exists $args{iter}) {
		print $desc "*** desc test `$test' low:0 high:0 id:$id\n";
	} else {
		push @{$self->_iterated}, [$suite, $tcase, $test, \%args];
	}
}


# NOTE: this function takes, ehm, liberties with regard to the "describe mode"
# output. as there are no tests, it's not known whether this variant would be
# accepted by Sink just as well as the testbench variant.
#
# writing tests about test support code seems a bit "out there", however.
sub build_iter_desc {
	my $self = shift;

	my $stream = $self->desc_stream;
	if(!defined $self->_desc_before_iter) {
		$self->_desc_before_iter($stream->pos);
	} else {
		$stream->truncate($self->_desc_before_iter);
	}

	my (%lohi, %map);
	foreach (@{$self->_iterated}) {
		my ($suite, $tcase, $test, $args) = @$_;
		my $name = "$suite:$tcase:$test";
		$map{$name} = $_ unless exists $map{$name};

		my $iter = $args->{iter} // die "no iter? what.";
		my ($lo, $hi) = @{$lohi{$name} // [0, 0]};
		$lo = $iter if $iter < $lo;
		$hi = $iter if $iter > $hi;
		$lohi{$name} = [$lo, $hi];
	}
	foreach (sort (keys %map)) {
		my ($suite, $tcase, $test, $args) = @{$map{$_}};
		my ($lo, $hi) = @{$lohi{$_}};
		my $id = $args->{id} // ($suite . $tcase . $test);
		print $stream "*** desc suite `$suite'\n";
		print $stream "*** desc tcase `$tcase'\n";
		print $stream "*** desc test `$test' low:$lo high:$hi id:$id\n";
	}
}


sub add_line {
	my $self = shift;
	push @{$self->lines}, @_;
}


has 'plan_ctr' => ( is => 'rw', isa => 'Int' );

sub plan {
	my $self = shift;
	$self->plan_ctr(1);
	my $num = shift;
	return "1..$num\n";
}


sub ok {
	my $self = shift;
	my $desc = shift;
	my $what = shift // 'ok';
	my $num = $self->plan_ctr || die "plan() wasn't called";
	$self->plan_ctr($num + 1);
	return "$what $num - $desc\n";
}


sub not_ok {
	my $self = shift;
	return $self->ok(@_, 'not ok');
}


sub test_fail {
	my $self = shift;
	my $msg = shift;
	return "Bail out!  $msg\n";
}


no Moose;
__PACKAGE__->meta->make_immutable;
