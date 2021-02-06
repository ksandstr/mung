#!/usr/bin/perl
use Modern::Perl '2014';

# script that, given a test name, finds the corresponding test ID for use with
# the "runonly" testbench syntax.

-f 'run.sh' || die "not in the right directory";
die "gotta have a parameter!" unless @ARGV;

my @matched;
$SIG{PIPE} = sub { };		# and smoke it
$ENV{TESTBENCH_OPTS} = "describe=1";
open(DESCRIBE, "./run.sh -display none 2>&1 |")
	|| die "can't open pipe from run.sh!";
my ($suite, $tcase);
LINE: while(my $line = <DESCRIBE>) {
	chomp $line;
	study $line;
	if($line =~ /desc\s+suite\s+\`(\w+)'/) {
		$suite = $1;
		next LINE;
	} elsif($line =~ /desc\s+tcase\s+\`(\w+)'/) {
		$tcase = $1;
		next LINE;
	}
	my $match = 0;	# of @ARGV patterns matched on this line.
	for my $pat (@ARGV) {
		my $test_name;
		if(index($pat, ':') >= 0 && $suite && $tcase
			&& $pat =~ /^(\w+):(\w+)$/)
		{
			# suite:tcase patterns
			next unless $suite eq $1 && $tcase eq $2
				&& $line =~ /desc\s+test\s+\`(\w+)'/;
			$test_name = $1;
		} else {
			# patterns on the test name
			$line =~ /desc\s+test\s+\`(\w*$pat\w*)'/ or next;
			$test_name = $1;
		}
		my %p = map { /(\w+):(\w+)/ ? ($1, $2) : () } split(/\s+/, $line);
		print "matched `$pat': $test_name -> $p{id}";
		my $add = $p{id};
		if(exists $p{low} && exists $p{high} && $p{high} > 0) {
			print " [$p{low}..$p{high}]";
			$add .= ':*';
		}
		print "\n";
		$match++;
		push @matched, $add;
	}
	if(!$match && ($line =~ /\*\*\*\s+begin\s+suite\s+/
		|| $line =~ /\*\*\* .+ completed/))
	{
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
