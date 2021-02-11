#!/usr/bin/perl
use Modern::Perl '2014';

# script that, given a partial test name or a 'suite:testcase' pattern (w/
# asterisk wildcards), finds the matching test IDs for use with the "runonly"
# testbench syntax.

-f 'run.sh' || die "not in the right directory";
die "gotta have a parameter!" unless @ARGV;

# returns a closure that matches ($suite, $tcase, $testname) against a parsed
# pattern and returns a printable UI name on a match, or undef otherwise.
sub parse_pattern {
	my $pat = shift;
	if(index($pat, ':') >= 0 && $pat =~ /^(\w+|\*):(\w+|\*)$/) {
		# suite:tcase patterns w/ wildcards
		my $p_s = $1;
		my $p_tc = $2;
		return sub {
			my ($suite, $tcase, $name) = @_;
			return undef unless ($p_s eq '*' || $suite eq $p_s)
				&& ($p_tc eq '*' || $tcase eq $p_tc);
			# tell what was actually matched for a wildcard
			$name .= " {$tcase}" if $p_tc eq '*';
			$name .= " {$suite:$tcase}" if $p_s eq '*';
			return $name;
		};
	} elsif(index($pat, '*') < 0) {
		# patterns on the test name
		return sub {
			my ($suite, $tcase, $name) = @_;
			return $name =~ /\w*$pat\w*/ && $name;
		};
	} else {
		# bad pattern
		print "warning: pattern `$pat' is no good\n";
		return sub { return undef; };
	}
}

my @pats = map { { fn => parse_pattern($_), pat => $_ } } @ARGV;
my @matched;
$SIG{PIPE} = sub { };		# and smoke it
$ENV{TESTBENCH_OPTS} = "describe=1";
open(DESCRIBE, "./run.sh -display none 2>&1 |")
	|| die "can't open pipe from run.sh!";
my %st = (suite => '', tcase => '');
LINE: while(my $line = <DESCRIBE>) {
	chomp $line;
	my $match = 0;	# of patterns matched on this line.
	if($line =~ /desc\s+(suite|tcase)\s+\`(\w+)'/) {
		$st{$1} = $2;
		next LINE;
	} elsif($line =~ /desc\s+test\s+\`(\w+)'/) {
		my $test_name = $1;
		for my $pat (@pats) {
			my $pn = $pat->{fn}->($st{suite}, $st{tcase}, $test_name) or next;
			my %p = map { /(\w+):(\w+)/ ? ($1, $2) : () } split(/\s+/, $line);
			if($match == 0) {
				print "matched `$pat->{pat}': $pn -> $p{id}";
				my $add = $p{id};
				if(exists $p{low} && exists $p{high} && $p{high} > 0) {
					print " [$p{low}..$p{high}]";
					$add .= ':*';
				}
				print "\n";
				push @matched, $add;
			}
			$match++;
		}
	}
	if(!$match && ($line =~ /\*\*\*\s+begin\s+suite\s+/
		|| $line =~ /\*\*\* .+ completed/))
	{
		# kill the piped child process and finish the loop. the "describe"
		# output has ended so there'll be no further desc lines.
		$SIG{TERM} = sub { };
		kill -TERM, $$;
		delete $SIG{TERM};
		last LINE;
	}
}
close DESCRIBE;

# final output.
if(@matched) {
	print "command: stuff/run-only.pl " . join(' ', @matched) . "\n";
} else {
	print "no matches.\n";
}
