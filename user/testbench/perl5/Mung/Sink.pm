package Mung::Sink;
use Moose;
use TryCatch;
use List::Util qw/sum/;

use Mung::TestSuite;
use Mung::TestCase;
use Mung::Test;
use Mung::TapError;

# receiver of the testbench output, i.e. the sink.

has [ qw/suite tcase/ ] => ( is => 'rw' );	# FIXME: specify
has 'test' => ( is => 'rw', isa => 'Maybe[Mung::Test]' );
has 'completed_ref' => (
	is => 'ro', isa => 'HashRef[Int]',
	default => sub { {} });
has 'output' => ( is => 'ro', does => 'Mung::Output', required => 1 );

has 'suites' => (
	is => 'ro', isa => 'ArrayRef[Mung::TestSuite]',
	default => sub { [] });

# this is really just a map from iterless test name to Mung::Test. as a test
# plan it's rather crummy.
has 'plan' => (
	is => 'ro',
	isa => 'HashRef[Mung::Test]',
	default => sub { {} } );

has 'failmsg' => ( is => 'rw', isa => 'Str', default => '' );

# output accumulators. should have a group accessor to produce the %stats
# equivalent.
has [ qw/failed incorrect/ ] => ( is => 'rw', isa => 'Int', default => 0 );
has 'errors' => (
	is => 'ro',
	# isa => 'ArrayRef[Dict[error => Mung::TapError, line => Str]]',
	default => sub { [] } );


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
		$self->output->test_status("Suite $name: ");
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
		$self->output->test_status("$name ");
		$self->tcase(Mung::TestCase->new(name => $name));
	}
}


sub _begin_test {
	my $self = shift;
	my $name = shift;

	my $sn = $self->suite->name;
	my $tcn = $self->tcase->name;
	my $path = "$sn:$tcn:$name";
	$self->test($self->plan->{$path} || die "no test plan for $path");
	$self->test->begin_test(@_,
		report_msg_hook => sub { $self->output->out_of_line(shift . "\n"); });
}


sub _end_test {
	my $self = shift;
	my $name = shift;
	my %args = @_;

	my $res = $self->test->current_result;
	if($args{failmsg}) {
		$self->output->out_of_line("test failed: $args{failmsg}");
		$self->failed($self->failed + 1);
	} else {
		my $comp = $self->test->id . ":" . $res->iter;
		$self->completed_ref->{$comp} = 1;
		# $self->output->out_of_line("noted ID `$comp' as completed.");
	}
	if($res->planned > $res->seen) {
		$self->output->out_of_line("planned " . $res->planned
			. " test(s), but executed only " . $res->seen);
		$self->incorrect($self->incorrect + 1);
	}

	if(@{$self->test->results} == 1) {
		# record execution. consumed by close_suite()
		push @{$self->tcase->tests}, $self->test;
	}

	my $rc;
	# for test's sake, also restart on failed test points.
	if($args{failmsg} || @{$res->not_ok}) {
		die Mung::Error::TestRestart->new(test => $self->test);
	} else {
		$rc = "";
		$self->test->end_test;
	}

	return $rc;
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
	my %summary;
	foreach my $key (qw/planned passed skipped/) {
		$summary{$key} = sum(0, map { $_->[1]->$key || () } @tests);
	}
	$self->output->test_status("[$summary{passed}/$summary{planned} OK]\n");
	$self->output->test_status("note: $_\n") foreach @comment;

	$self->suite(undef);
}


sub close_tcase {
	my $self = shift;
	my $tc = $self->tcase;
	return unless $tc;

	my $tskip = $tc->tests_skipped;
	$self->output->test_status("<skipped $tskip plans> ") if $tskip > 0;
	push @{$self->suite->tcases}, $tc;
	$self->tcase(undef);
}


sub bracket_line {
	my $self = shift;
	my $side = shift;	# begin | end
	my $what = shift;	# suite | tcase | test

	die "invalid bracket for ${side}_${what}"
		unless $side ~~ [qw/begin end/] && $what ~~ [qw/suite tcase test/];
	my $method = "_${side}_${what}";
	return 0 unless $self->can($method);
	return $self->$method(@_);
}


sub ctl_line {
	my $self = shift;
	my $msg = shift;

	if($msg =~ /(begin|end) (suite|tcase|test) [`'](\w+)'(\s+(\w+)\s+(\d+))?/) {
		my $tag = $5 || '';
		my $val = $6 && int($6);
		return $self->bracket_line($1, $2, $3, $tag, $val);
	} elsif($msg =~ /test\s+log:\s*(.+)$/) {
		# capture test log output
		push @{$self->test->{log}}, $1;
	} elsif($msg =~ /test failed:\s+(msg\s+[`'](.+?)')?/) {
		if($self->failmsg) {
			# FIXME: pipe this through $self->output
			print STDERR "WARNING: multiple test failure messages"
				. " (last was `" . $self->failmsg . "')\n";
		}
		$self->failmsg($2 || '');
	} elsif($msg =~ /test [`'](\w+)' failed, rc (\d+)/) {
		# TODO: output something that relates the return code to the test
		# plan.
		# TODO: also do that for tests that didn't fail.
		$self->_end_test($1,
			rc => int($2) || 0,
			failmsg => $self->failmsg);
		$self->failmsg('');
	} elsif($msg =~ /desc (test|tcase|suite) `(\w+)'(\s*low:(\d+)\s*high:(\d+)\s*id:([a-zA-Z0-9]+))?/) {
		my @args = (name => $2);
		push @args, (low => int($4), high => int($5), id => $6) if $3;
		$self->desc_line($1, @args);
	} elsif($msg =~ /(.+) follow/) {
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
		die "duplicated test " . $t->path if exists $self->plan->{$t->path};
		$self->plan->{$t->path} = $t;
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
	if($tb =~ /^\*\*\*\s+(.+)$/) {
		# control lines
		return $self->ctl_line($1);
	} elsif(/^testbench abort.*called/) {
		$self->output->test_status("\n") if $self->suite;
		die Mung::Error::TestAbort->new(text => "premature test abort!");
	} elsif(is_kmsg($_)) {
		return;
	} elsif($self->test->current_result) {
		my $input = $_;
		try {
			$self->test->current_result->tap_line($input);
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
