#!/usr/bin/env perl
use strict;
use warnings;
use Cwd qw(abs_path);

my ($report, $source_directory, $expression_threshold, $branch_threshold, @source_files) = @ARGV;
die "usage: $0 REPORT SOURCE_DIRECTORY EXPRESSION_THRESHOLD BRANCH_THRESHOLD\n"
  unless defined $branch_threshold;
open my $handle, '<', $report or die "cannot read $report: $!\n";
local $/;
my $html = <$handle>;
close $handle or die "cannot close $report: $!\n";
$source_directory = abs_path($source_directory)
  // die "cannot resolve source directory $source_directory\n";
$source_directory =~ s{/*$}{/};
my %counts = (expression => [0, 0], branch => [0, 0]);
my $current_directory = '';
my %wanted_files = map { $_ => 1 } @source_files;
my %seen_files;
while ($html =~ m{
  <tr\s+class='subheading'>\s*<td\s+colspan='7'>([^<]+)</td>\s*</tr>
  (.*?)(?=<tr\s+class='subheading'>|\z)
}gsx) {
  $current_directory = $1;
  next unless index($current_directory, $source_directory) == 0;
  my $section = $2;
  while ($section =~ m{
    <tr\s+class='(?:odd|even)'>\s*
    <td\s+class='text-cell'>\s*<a\s+href='[^']+'>([^<]+)</a>\s*</td>\s*
    <td>(\d+|-)</td>\s*<td>(\d+|-)</td>\s*<td>[^<]+</td>\s*
    <td>(\d+|-)</td>\s*<td>(\d+|-)</td>\s*<td>[^<]+</td>\s*
    </tr>
  }gsx) {
    next if @source_files && !$wanted_files{$1};
    $seen_files{$1} = 1;
    my @values = ($2, $3, $4, $5);
    for (@values) { $_ = 0 if $_ eq '-'; }
    $counts{expression}[0] += $values[0];
    $counts{expression}[1] += $values[1];
    $counts{branch}[0] += $values[2];
    $counts{branch}[1] += $values[3];
  }
}
if (@source_files) {
  my @missing = grep { !$seen_files{$_} } @source_files;
  die "no coverage data found for: @missing\n" if @missing;
}
my @failures;
for my $kind (qw(expression branch)) {
  my ($covered, $total) = @{$counts{$kind}};
  die "no $kind coverage data found below $source_directory\n" unless $total;
  my $percent = 100 * $covered / $total;
  my $threshold = $kind eq 'expression' ? $expression_threshold : $branch_threshold;
  printf "%s coverage: %d/%d (%.2f%%), required: %.2f%%\n",
    $kind, $covered, $total, $percent, $threshold;
  push @failures, "$kind coverage is below $threshold%" if $percent < $threshold;
}
die join("\n", @failures), "\n" if @failures;
