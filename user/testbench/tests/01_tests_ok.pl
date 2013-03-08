#!/usr/bin/perl
use Modern::Perl '2012';
use FindBin qw/$Bin/;
use lib "$Bin/lib";

use Test::More tests => 2;

use_ok('ScriptModule');
use_ok('BufferOutput');
