#!/usr/bin/perl
use Modern::Perl '2014';

# script that, given a test name, finds the corresponding test ID for use with
# the "runonly" testbench syntax.

die "gotta have a parameter!" unless @ARGV;
-f 'run.sh' || die "not in the right directory";

my @matched;
$SIG{PIPE} = sub { };		# and smoke it
$ENV{TESTBENCH_OPTS} = "describe=1";
open(DESCRIBE, "./run.sh -display none 2>&1 |")
	|| die "can't open pipe from run.sh!";
while(<DESCRIBE>) {
	chomp;
	my $match = 0;	# of @ARGV patterns matched on this line.
	for my $pat (@ARGV) {
		/desc\s+test\s+\`(\w*$pat\w*)'\s+(.*)$/ or next;
		my %p = map { /(\w+):(\w+)/; ($1, $2) } split /\s+/;
		print "matched `$pat': $1 -> $p{id}";
		my $add = $p{id};
		if(exists $p{low} && exists $p{high} && $p{high} > 0) {
			print " [$p{low}..$p{high}]";
			$add .= ':*';
		}
		print "\n";
		$match++;
		push @matched, $add;
	}
	if(!$match && /\*\*\*\s+begin\s+suite\s+/) {
		# kill the piped child process and finish the loop. the "describe"
		# output has ended so there'll be no further desc lines.
		$SIG{TERM} = sub { };
		kill -TERM, $$;
		delete $SIG{TERM};
		last;
	}
}
close DESCRIBE;

# final output.
if(@matched) {
	print "command: stuff/run-only.pl " . join(' ', @matched) . "\n";
} else {
	print "no matches.\n";
}
