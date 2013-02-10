package Mung::Restarting;
use Moose::Role;

use Mung::Error::TestRestart;


# changes a Mung::Sink to throw a valid Mung::Error::TestRestart when either
# the test has failed entirely, or has had at least one test point that did.

requires 'test', '_end_test';


around '_end_test' => sub {
	my $orig = shift;
	my $self = shift;

	my $result = $self->$orig(@_);

	my $UNUSED_name = shift;
	my %args = @_;
	if($args{failmsg} || $result->failed) {
		die Mung::Error::TestRestart->new(
			test => $self->test, result => $result);
	} else {
		return $result;
	}
};

1;
