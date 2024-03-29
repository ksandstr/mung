package Mung::Sink;
use Moose;
use TryCatch;
use List::Util qw/sum/;
use List::MoreUtils qw/any/;

use Mung::TestSuite;
use Mung::TestCase;
use Mung::Test;
use Mung::TapError;

# receiver of the testbench output, i.e. the sink.

has [ qw/suite tcase/ ] => ( is => 'rw' );	# FIXME: specify
has 'test' => (
	is => 'rw', isa => 'Maybe[Mung::Test]',
	handles => [qw/log/] );

# hook. called when a test completes with $test, $result.
has 'on_complete_fn' => ( is => 'rw', isa => 'CodeRef' );

has 'output' => (
	is => 'ro', does => 'Mung::Output', required => 1,
	handles => {
		print => 'out_of_line',
		status => 'test_status',
	});

has 'suites' => (
	is => 'ro', isa => 'ArrayRef[Mung::TestSuite]',
	default => sub { [] });

# map from iterless test name to Mung::Test. writable so it can be ->reset.
has 'test_by_path' => (
	is => 'rw',
	isa => 'HashRef[Mung::Test]',
	default => sub { {} } );

# array of tests in ->desc_line() order.
has 'plan' => (
	is => 'rw',
	isa => 'ArrayRef[Mung::Test]',
	default => sub { [] } );

# output accumulators. should have a group accessor to produce the %stats
# equivalent.
has [ qw/failed incorrect/ ] => ( is => 'rw', isa => 'Int', default => 0 );
has 'errors' => (
	is => 'ro',
	# isa => 'ArrayRef[Dict[error => Mung::TapError, line => Str]]',
	default => sub { [] } );


sub reset {
	my $self = shift;
	$self->plan([]);
	$self->test_by_path({});
}


sub done {
	my $self = shift;
	$self->close_tcase;
	$self->close_suite;
}


# all _{begin,end}_suite functions are called from ctl_line.
sub _begin_suite {
	my $self = shift;
	my $name = shift;

	my $suites = $self->suites;
	my $prev = @$suites ? $suites->[$#$suites] : undef;
	if($prev && $prev->name eq $name) {
		$self->suite($prev);	# resume previous
	} else {
		# new suite.
		#
		# FIXME: tcases don't carry over properly. this fucks with the
		# not_ok reporting and all that. really the tcase should only be a
		# bit of state to remember when looking up instances of Mung::Test
		# from %plan.
		$self->close_tcase;
		$self->close_suite;
		$self->suite(Mung::TestSuite->new(name => $name));
		push @{$self->suites}, $self->suite;
		$self->status("Suite $name: ");
	}
}


sub _begin_tcase {
	my $self = shift;
	my $name = shift;

	if($self->tcase && $self->tcase->name eq $name) {
		# continue
	} else {
		# new tcase
		$self->close_tcase;
		$self->status("$name ");
		$self->tcase(Mung::TestCase->new(name => $name));
	}
}


sub _begin_test {
	my $self = shift;
	my $name = shift;

	my $sn = $self->suite->name;
	my $tcn = $self->tcase->name;
	my $path = "$sn:$tcn:$name";
	$self->test($self->test_by_path->{$path} || die "no test plan for $path");
	return $self->test->begin(@_);
}


# FIXME: what's this doing here? what's it for, anyway?
sub _completed {
	my $self = shift;

	my $fn = $self->on_complete_fn || return;
	&$fn(@_);
}


# args: $name, (as for Mung::SingleResult->new)
# returns the relevant Mung::SingleResult object, mostly for the benefit of
# Moose "around"s.
sub _end_test {
	my $self = shift;
	my $name = shift;

	# TODO: report a stream error when $name ne $self->test->name

	my $first = @{$self->test->results} == 0;
	my $res = $self->test->end(@_);
	if($res->status) {
		$self->failed($self->failed + 1);
	} else {
		$self->_completed($self->test, $res);
		if(!$res->is_good_plan && defined($res->tests_planned)) {
			$self->print("planned " . $res->tests_planned
				. " test(s), but executed only " . $res->tests_run . "\n");
			$self->incorrect($self->incorrect + 1);
		}
	}

	if($first) {
		# record execution. consumed by close_suite()
		push @{$self->tcase->tests}, $self->test;
	}

	return $res;
}


# panic lines are received from outside as they are relevant to process
# restarts, which the Sink doesn't control.
#
# returns Mung::SingleResult, and leaves $self->test as it was before the
# call.
sub test_panic {
	my $self = shift;
	my $msg = shift // '<<no message>>';
	return $self->_end_test($self->test->name,
		status => "PANIC", fail_msg => $msg);
}


sub close_suite {
	my $self = shift;
	my $suite = $self->suite;
	return unless $suite;

	my (@tests, @comment, %seen);
	foreach my $test (map { @{$_->tests} } @{$suite->tcases}) {
		my $path = $test->path;
		next if $seen{$path};
		$seen{$path} = 1;

		# FIXME: pick out the result that's reported in the summary, rather
		# than using the last one.
		my @copy = @{$test->results};
		if(!@copy) {
			push @comment, "test $path wasn't run\n";
			next;
		}
		my $res = pop @copy;
		push @tests, [ $test, $res ];
	}
	$suite->{results} = \@tests;

	my $passed = 0;
	foreach (@tests) {
		$passed++ unless $_->[1]->has_problems;
	}
	my $count = scalar @tests;
	$self->status("[$passed/$count OK]\n");

	$self->status("note: $_\n") foreach @comment;

	$self->suite(undef);
}


sub close_tcase {
	my $self = shift;
	my $tc = $self->tcase;
	return unless $tc;

	my $tskip = $tc->tests_skipped;
	$self->status("<skipped $tskip plans> ") if $tskip > 0;
	push @{$self->suite->tcases}, $tc;
	$self->tcase(undef);
}


sub in_strs {
	my ($haystack, $needle) = @_;
	return any { $needle eq $_ } @$haystack;
}


sub bracket_line {
	my $self = shift;
	my $side = shift;	# begin | end
	my $what = shift;	# suite | tcase | test

	die "invalid bracket for ${side}_${what}"
		unless in_strs([qw/begin end/], $side)
			&& in_strs([qw/suite tcase test/], $what);
	my $method = "_${side}_${what}";
	$self->$method(@_) if $self->can($method);
}


sub ctl_line {
	my $self = shift;
	my $msg = shift;

	if($msg =~ /(begin|end) (suite|tcase|test) [`'](\w+)'(\s+(\w+)\s+(\d+))?/) {
		my $tag = $5 || '';
		my $val = $6 && int($6);
		$self->bracket_line($1, $2, $3, $tag, $val);
	} elsif($msg =~ /test\s+log:\s*/) {
		# capture test log output
		push @{$self->test->{log}}, $';
	} elsif($msg =~ /test [`'](\w+)' failed, rc (-?\d+)/) {
		# this line is generated from the exit_on_fail() path, via
		# run_test()'s return value. it's used for passing the TAP return
		# code.
		$self->_end_test($1, rc => int($2) || 0);
	} elsif($msg =~ /segmentation fault.*at\s+(0x[0-9a-fA-F]+)?/) {
		my $where;
		$where = POSIX::strtoul($1) if $1;
		$self->test->on_segv($where);
	} elsif($msg =~ /desc (test|tcase|suite) `(\w+)'(\s*low:(\d+)\s*high:(\d+)\s*id:([a-zA-Z0-9]+))?/) {
		my @args = (name => $2);
		push @args, (low => int($4), high => int($5), id => $6) if $3;
		$self->desc_line($1, @args);
	} elsif($msg =~ /(.+) follow$/) {
		# NOTE: should ignore from here until the next valid control
		# message & possibly log into a hash keyed with $1
	} else {
		# FIXME: see FIXME above
		print STDERR "unknown control message `$_'\n";
	}
}


has [ qw/desc_suite desc_tcase/ ] => ( is => 'rw', isa => 'Str' );

# output from srunner_describe(). used to construct a Mung::Test for each test
# with the right name, id, low, high attributes.
sub desc_line {
	my $self = shift;
	my $what = shift;
	my %args = @_;

	if($what eq 'suite') {
		$self->desc_suite($args{name});
	} elsif($what eq 'tcase') {
		$self->desc_tcase($args{name});
	} elsif($what eq 'test') {
		my $t = Mung::Test->new(
			suite => $self->desc_suite, tcase => $self->desc_tcase,
			name => $args{name}, id => $args{id},
			low => $args{low}, high => $args{high});
		die "duplicated test " . $t->path
			if exists $self->test_by_path->{$t->path};
		$self->test_by_path->{$t->path} = $t;
		push @{$self->plan}, $t;
	} else {
		die "desc_line undefined over `$what'";
	}
}


sub is_kmsg {
	shift;
	return /^\[(sigma0|forkserv)\]: / || /^sbrk:/;
}


sub tb_line {
	my $self = shift;
	my $tb = shift;
	if($tb =~ /^\*\*\*\s+/) {
		# control lines
		$self->ctl_line($');
	} elsif(/^testbench abort/ && /called$/) {
		$self->status("\n") if $self->suite;
		die Mung::Error::TestAbort->new(text => "premature test abort!");
	} elsif(is_kmsg($_)) {
		return;
	} elsif($self->test) {
		my $input = $_;
		try {
			$self->test->tap_line($input);
		}
		catch (Mung::TapError $err) {
			push @{$self->errors}, { error => $err, line => $input };
		}
	} else {
		# skip everything else when no ongoing test stream.
	}
}


no Moose;
__PACKAGE__->meta->make_immutable;
