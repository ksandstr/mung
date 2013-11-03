package Mung::TestResult;
use Moose::Role;


# data access
requires 'test';	# Mung::Test reference
requires 'iters';	# ArrayRef[Maybe[Mung::SingleResult]], index by iteration

# the list of parser items for this test. if different iterations had a
# different parser item at a position, then the value may be an
# ArrayRef[Maybe[TAP::Parser::Result]], indexed by iteration. (such lines can
# be had in a grouped fashion, i.e. as an array of ([iters], result), from a
# different method.)
requires 'parser_items';

# as in TAP::Parser
requires qw(
	passed failed actual_passed actual_ok actual_failed
	todo todo_passed todo_failed skipped tests_planned tests_run
	skip_all has_problems is_good_plan);

1;
