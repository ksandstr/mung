#!/usr/bin/perl
use warnings;
use strict;

use IO::File;
use Error qw/:try/;
use Bencode qw(bdecode bencode);


# this program started out as a way to remove the "private" flag from a
# bittorrent metafile. turns out that this would make the info_hash differ and
# therefore be useless. as most people don't modify their clients to ignore
# the `private' flag, there won't be many DHT peers for such a torrent anyhow
# once the tracker dies.
#
# so, good intentions, impossible to implement. by design most likely.


sub remove_private_from {
	my $filename = shift;

	my $f = IO::File->new("< $filename")
		or die "can't open `$filename': $!";
	binmode($f);
	my $content;
	for(;;) {
		my $tmp;
		my $n = $f->read($tmp, 16 * 1024);
		last if $n <= 0;
		$content .= $tmp;
	}

	my $torrent = bdecode $content || return;
	die "torrent wasn't a dictionary?" if ref($torrent) ne 'HASH';
	foreach (keys %$torrent) {
		print "key: $_\n";
		my $val = $torrent->{$_};
		if($_ ne 'comment' && !ref($val)) {
			print "  scalar value: `$val'\n";
		}
	}
	foreach (keys %{$torrent->{info}}) {
		print "info key: $_\n";
		my $val = $torrent->{info}->{$_};
		if(!ref($val) && length($val) < 80) {
			print "  scalar value: `$val'\n";
		}
	}
}


my $status = 0;
foreach my $filename (@ARGV) {
	try {
		remove_private_from $filename;
	}
	catch Error with {
		my $err = shift;
		print STDERR "error processing `$filename': " . $err->text;
		$status = 1;
	};
}
exit $status;
