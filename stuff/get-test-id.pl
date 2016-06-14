#!/usr/bin/perl
use Modern::Perl;

# script that, given a test name, finds the corresponding test ID for use with
# the "runonly" testbench syntax.

die "gotta have a parameter!" unless @ARGV;
-f 'run.sh' || die "not in the right directory";

my $matched = 0;
$SIG{PIPE} = sub { };		# and smoke it
$ENV{TESTBENCH_OPTS} = "describe=1";
open(DESCRIBE, "./run.sh -display none 2>&1 |")
	|| die "can't open pipe from run.sh!";
while(<DESCRIBE>) {
	chomp;
	my $match = 0;
	for my $pat (@ARGV) {
		/desc\s+test\s+\`(\w*$pat\w*)'\s+(.*)$/ or next;
		my %p = map { /(\w+):(\w+)/; ($1, $2) } split /\s+/;
		print "matched `$pat': $1 -> $p{id}";
		if(exists $p{low} && exists $p{high} && $p{high} > 0) {
			print " [$p{low}..$p{high}]";
		}
		print "\n";
		$match++;
	}
	$matched += $match;
	if(!$match && /\*\*\*\s+begin\s+suite\s+/) {
		# kill the piped child process.
		$SIG{TERM} = sub { };
		kill -TERM, $$;
		delete $SIG{TERM};
		last;
	}
}
close DESCRIBE;
print "no matches.\n" unless $matched;
