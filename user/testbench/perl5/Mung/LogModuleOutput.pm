package Mung::LogModuleOutput;
use Modern::Perl '2014';
use Moose::Role;

use IO::File;


# changes a Mung::ProcessModule to capture output from next_line, storing them
# into a file opened according to a template pattern when the first line comes
# in.
#
# this has no relation to Mung::TTYOutput. that's a somewhat poor naming
# convention.

requires 'next_line';


has '_log_output_stream' => ( is => "rw", isa => 'Maybe[IO::Handle]' );


sub _generate_log_path {
	my $name = `mktemp -t mung_testlog.XXXXXX`;
	chomp $name;
	return $name;
}


around 'next_line' => sub {
	my $orig = shift;
	my $self = shift;

	my $out = $self->_log_output_stream;
	my $line = $self->$orig(@_);
	if(!$out) {
		my $path = $self->_generate_log_path;
		$out = IO::File->new("> $path")
			or die "can't open logfile $path: $!";
		$self->_log_output_stream($out);
	}
	print $out $line if $line;
	return $line;
};


after 'close' => sub {
	my $self = shift;
	my $out = $self->_log_output_stream;
	if($out) {
		$out->close;
		$self->_log_output_stream(undef);
	}
};

1;
