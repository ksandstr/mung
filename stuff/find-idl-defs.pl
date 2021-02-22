#!/usr/bin/perl
use Modern::Perl '2020';
use IO::File;


# files that shouldn't be examined. they're guaranteed not to have IDL
# dependencies, aren't found in the directory this script was run in (i.e.
# would require -I handling, which we don't yet do), or are otherwise a priori
# uninteresting.
my @exempt_files = qw/muidl.h/;


my %header_deps;
my %exempt = map { $_ => 1 } @exempt_files;

sub find_idl_defs {
	my $filename = shift;
	my $fh = IO::File->new("< $filename") || die "can't open $filename: $!";
	my (@direct, @indirect);
	while(<$fh>) {
		last if /^[{}]/; # stops at end of first function, struct, or union
		next unless /^#include\s+"(?<file>(\w|-)+\.h)"/;
		my $inc = $+{file};
		next if $exempt{$inc};
		if($inc =~ /-defs\.h$/) {
			push @direct, $inc;
		} else {
			if(not exists $header_deps{$inc}) {
				$header_deps{$inc} = [];	# recursion blocker
				$header_deps{$inc} = [ find_idl_defs($inc) ];
			}
			push @indirect, $inc if @{$header_deps{$inc}} > 0;
		}
	}
	$fh->close;

	return uniq(@direct, map { @{$header_deps{$_}} } @indirect);
}


sub uniq {
	my @result;
	my %ex;
	for (@_) {
		next if exists $ex{$_};
		$ex{$_} = 1;
		push @result, $_;
	}
	return @result;
}


my $dir_present = -d '.deps';
for my $filename (<*.c>) {
	# don't crawl the file if deps already exist, such as when the tree was
	# already built.
	my $depsname = ".deps/$filename";
	$depsname =~ s/\.c$/.d/;
	next if -f $depsname;

	my @defs = find_idl_defs($filename);
	next unless @defs;
	if(!$dir_present) {
		mkdir '.deps' || die "can't mkdir .deps: $!";
		$dir_present = 1;
	}
	my $dfh = IO::File->new("> $depsname") || die "can't open $depsname: $!";
	my $objname = $filename;
	$objname =~ s/\.c$/.o/;
	# output a rudimentary dependency spec for this.
	print $dfh "$objname: $filename @defs\n";
	$dfh->close;
}
