package Mung::TextReport;
use Modern::Perl '2012';
use Moose;

use IO::String;
use Term::ANSIColor;


sub in_color {
	my $color = shift;
	return color($color) . join("", @_) . color('reset');
}


# return a report for a single test's results.
sub report_result {
	my $self = shift;
	my %args = @_;
	my ($test, $suite, $tcase, $res) = @args{qw/test suite tcase result/};

	my $out = IO::String->new;

	my $tn = $test->name;
	$tn .= ":" . $res->iter if $test->low != $test->high;

	my @conds;
	if($res->failed > 0) {
		push @conds, "failed " . scalar $res->failed . " test point(s): "
			. "{ " . in_color('bold', join(" ", $res->failed)) . " }";
	}
	my $st = $res->status // "";
	if($st eq 'FAIL') {
		push @conds, "bailed out: " . $res->fail_msg;
	} elsif($st eq 'PANIC') {
		push @conds, "panic()ed: " . $res->fail_msg;
	}

	if(@conds > 0) {
		# TODO: sprinkle this, too, with more colours.
		print $out "Test `$tn' [" . $test->id . "] in "
			. $suite->name . "/" . $tcase->name . " ";
		my $fst = 1;
		foreach (@conds) {
			if(!$fst) {
				print $out "  and ";
			} else {
				$fst = 0;
			}
			print $out "$_\n";
		}
		print $out "  ---\n";
	}

	# add the complete test log.
	foreach (@{$res->results}) {
		next if $_->is_pragma || $_->is_version;
		my @si = ('reset', " ");
		my $pref = "";
		# TODO: recognize to-do lines, use a bright_yellow minus
		# sigil
		if(!$_->is_ok) {
			@si = ('bold bright_red', '!');
		} elsif($_->is_test) {
			# mark successful test points
			@si = ('bright_green', '+');
		} elsif($_->is_bailout) {
			# frightening to all nethack & dwarf fortress players
			@si = ('reverse bright_red', '&');
			$pref = "Bail out! ";
		}
		print $out in_color(@si) . " $pref" . $_->as_string . "\n";
	}

	return ${$out->string_ref};
}


sub print_report {
	my $self = shift;
	my $out = shift;
	my $sink = shift;

	# test points that failed, in plan order.
	my @errors;		# FIXME: should be filled in, or removed
	my $status = 0;
	my %notok_suites;
	foreach my $suite (@{$sink->suites}) {
		foreach my $tcase (@{$suite->tcases}) {
			foreach my $test (@{$tcase->tests}) {
				my @fails = map { $_->failed || $_->status ? ($_) : () } @{$test->results};
				next unless @fails;

				# there are roughly two kinds of test failures. first are the
				# failed test points, and second is the outright test panic.
				#
				# TODO: for now they're reported one after the other, however,
				# it'd be useful to dump the entire TAP output on panic
				# instead of just the not-ok report log.
				#
				# TODO: there's a third kind, akin to the second, where the
				# test hits a Check-style fail_if() assertion, but that's not
				# handled here.

				my $sum_notok = 0;
				my $num_skipped = 0;
				my $fst = 1;
				while(@fails) {
					my $res = shift @fails;
					if(@fails && $res->eqv_to($fails[0])) {
						$num_skipped++;
						next;
					}

					$sum_notok += scalar $res->failed;

					if($num_skipped > 0) {
						# (add note to the previous test's output.)
						my $times = $num_skipped + 1;
						print $out "  (was run $times times total; last reported.)\n";
						$num_skipped = 0;
					}

					if($fst) {
						print $out "\n";
						$fst = 0;
					}
					print $out $self->report_result(
						suite => $suite, tcase => $tcase, test => $test,
						result => $res);
				}
				$notok_suites{$suite->{name}} = 1 if $sum_notok > 0;
			}
		}
	}
	my $num_notok_suites = scalar(keys %notok_suites);
	if($num_notok_suites > 0) {
		print $out "There were failed test points in $num_notok_suites suite(s).\n";
	}

	# outputs generated by die() while parsing testbench output
	# FIXME: these are never filled in. they should be.
	if(@errors) {
		my %bywhat;
		my @what_order;
		foreach(@errors) {
			my $key = $_->{error}->text;
			push @what_order, $key unless exists $bywhat{$key};
			push @{$bywhat{$key}}, $_;
		}

		print $out "There were " . scalar @errors . " errors parsing test output:\n";
		# (and "scalar @what_order" different kinds of error.)
		foreach my $what (@what_order) {
			print $out "  $what\n";
			foreach my $err (@{$bywhat{$what}}) {
				print $out "\tline `$err->{line}'\n";
			}
		}
	}

	# (what the fuck does this mean?)
	if($sink->incorrect || $sink->failed) {
		print $out "\n";
		print $out "There were " . $sink->incorrect . " incorrect tests";
		my $f = $sink->failed;
		if($f) {
			print $out ", and $f test(s) failed";
			$status = 2;
		}
		print $out ".\n";
	}

	return $status;
}


no Moose;
__PACKAGE__->meta->make_immutable;
