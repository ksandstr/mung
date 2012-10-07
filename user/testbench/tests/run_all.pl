#!/usr/bin/perl
use strict;
use warnings;

use FindBin qw/$Bin/;
use lib "$Bin/../perl5";

use Test::Harness;

runtests(<$Bin/[0-9][0-9]*.pl>);
