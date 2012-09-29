package Mung::Output;
use Moose::Role;


# interface for handling running output during test processing. the default
# methods print to STDOUT assuming it's a terminal emulator or equivalent
# (i.e. not a file).
#
# something like a web session interface could use this to produce running
# output via a JavaScript push mechanism.
#
# more generally this could just handle multiple streams and break those as
# appropriate.


# this is just an interface, now. both methods accept a list of things a'la
# print, and do not add linefeeds or anything.
requires 'test_status';		# test progress report (suitelines)
requires 'out_of_line';		# dianostic output & failure reports

1;
