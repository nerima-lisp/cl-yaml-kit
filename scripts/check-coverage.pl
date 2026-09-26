#!/usr/bin/env perl
use strict;
use warnings;
my ($report, $threshold) = @ARGV;
die "usage: $0 REPORT THRESHOLD\n" unless defined $threshold;
open my $fh, '<', $report or die "cannot read $report: $!\n";
local $/;
my $html = <$fh>;
close $fh or die "cannot close $report: $!\n";
my ($covered, $total) = $html =~ /Total.*?(\d+)\s*\/\s*(\d+)/s;
die "no coverage total found\n" unless defined $total && $total > 0;
my $percent = 100 * $covered / $total;
printf "coverage: %d/%d (%.2f%%), required: %.2f%%\n", $covered, $total,
  $percent, $threshold;
die "coverage is below threshold\n" if $percent < $threshold;
