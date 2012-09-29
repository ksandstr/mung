package Mung::TestResult;
use Moose;

use TryCatch;
use Mung::TapError;


# TAP consumer and result collector.

# information about the result. test mentioned by ID to avoid circular
# reference.
has 'test_id' => (is => 'ro', isa => 'Str', required => 1);
has 'iter' => (is => 'ro', isa => 'Int', required => 0, default => 0);


# (for future expansion)
#has 'prior_tests' => (
#	is => 'ro', isa => 'ArrayRef[Str]',		# test IDs
#	required => 0,
#	default => sub { [] });

has 'report_msg_hook' => (is => 'ro', isa => 'CodeRef', required => 0);

# results
has [qw/planned seen passed skipped/] => (
	is => 'rw', isa => 'Int',
	default => 0);

# could model each test point separately, but for now just record each point
# as a failure.
has 'not_ok' => (
	is => 'rw', isa => 'ArrayRef[HashRef[Str]]',
	default => sub { [] });

has ['test_log', 'report_log'] => (
	is => 'rw', isa => 'ArrayRef[Str]',
	default => sub { [] });


# plan
has 'no_plan' => (is => 'rw', isa => 'Int');
has 'tap_low' => (is => 'rw', isa => 'Maybe[Int]');
has 'tap_high' => (is => 'rw', isa => 'Int');
has 'tap_next' => (is => 'rw', isa => 'Int');


# called at the "*** end test" control line regardless of plan status. does
# nothing for now; should synthesize return code on incomplete TAP stream.
sub done {
	# fnord
}


# accessor: whether the test plan was skipped altogether.
sub plan_skipped {
	my $self = shift;
	return $self->tap_low > $self->tap_high;
}


sub report_msg {
	my $self = shift;
	my $msg = shift;

	my $fn = $self->report_msg_hook;
	&$fn($msg) if $fn;

	push @{$self->report_log}, $msg;
}


# add a line to the test log.
sub log {
	my $self = shift;
	my $line = shift;

	push @{$self->test_log}, $line;
}


# TODO: skip stderr outputs, log diag() messages, report failed tests
# TODO: recognize a deferred plan (i.e. one that occurs at end of stream)
sub tap_line {
	my $self = shift;
	my $line = shift;

	$line =~ s/^\[ERR\]: //;
	my $diag = $1 if $line =~ s/#\s*(.+)$//;

	if($line =~ /^(\d+)\.\.(\d+)(.*)$/) {
		die "double plan" if defined $self->no_plan;
		$self->no_plan(0);
		$self->tap_low($1);
		$self->tap_high($2);
		$self->tap_next($self->tap_low);
		$self->planned($self->tap_high - $self->tap_low + 1);

		if($diag && $diag =~ s/Skip //) {
			die "malformed skip plan" unless $self->tap_low > $self->tap_high;
			$self->report_msg("test plan skipped: $diag");
			$diag = '';
		}
	} elsif($line =~ /^(not\s+)?ok\s+(\d+)\s+-\s+(.+)$/) {
		# FIXME: the regexp above is too strict. it should have the test
		# number and description as optional. (though testbench's tap.c
		# doesn't output any other format.)
		my $fail = defined($1);
		my $id = int($2);
		my $desc = $3;

		# (the test boundary resets $tap_next to -1, so that new unplanned
		# streams can be recognized.)
		#
		# FIXME: this is almost certainly the wrong thing to do.
		if($id != $self->tap_next) {
			if($id == 1) {
				# take this as an implicit start of a new plan_no_plan() style
				# substream
				#
				# FIXME: check that the semantics fit a deferred plan, change
				# comments to call it "deferred"
				$self->tap_low(0);
				$self->tap_high(1);
				$self->no_plan(1);
				$self->tap_next(1);
			} else {
				# TODO: could invalid streams be handled in a nicer way?
				die "plan expected" unless defined $self->no_plan;
				die "identifier $id out of order (expected " . $self->tap_next
					. ", or 1)";
			}
		}

		$self->seen($self->seen + 1);
		$self->planned($self->planned + 1) if $self->no_plan;
		$self->passed($self->passed + 1) unless $fail;
		if($fail) {
			my $report = "not ok $id - $desc";
			$self->report_msg($report);
			push @{$self->not_ok}, {
				report => $report, id => $id,
				test_log => $self->test_log,
			};
			$self->test_log([]);
		}

		$self->tap_next($self->tap_next + 1);
		if($self->tap_next > $self->tap_high && !$self->no_plan) {
			# end of test
			$self->tap_low(undef);
		}
	} elsif($line =~ /^ok\s+(\d+)?/ && $diag =~ s/^skip\s+//i) {
		# skip lines.
		my $id = int($1 || '0');
		$self->seen($self->seen + 1);
		$self->skipped($self->skipped + 1);
		$self->report_msg("skip $id - # $diag");
		undef $diag;
	} elsif($line !~ /^\s*$/) {
		push @{$self->test_log}, "# *** ignored `$line'";
		die Mung::TapError->new(text => "unrecognized");
	}

	$self->report_msg("# $diag") if $diag;
}


no Moose;
__PACKAGE__->meta->make_immutable;
