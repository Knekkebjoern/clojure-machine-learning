#!/usr/local/bin/perl

use strict;

my $rows = 10000000;
my $dimensions = 5;
my $range = 10000;

for my $x (1..$rows) {
  my @arr;
  for my $y (1..$dimensions) {
    push(@arr, int(rand($range)));
  }
  print join(' ', @arr)."\n";
}
