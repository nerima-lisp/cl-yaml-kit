#!/usr/bin/env perl
use strict;
use warnings;

# Exit 0 when every file meets both thresholds, 1 when a report was produced but
# a file is below one, 2 when no report could be produced at all.
my $EXIT_CANNOT_RUN = 2;

my @COLUMNS = qw(file line-covered line-total branch-covered branch-total
                 uncovered-lines);
my %ENVIRONMENT = (
  input        => 'COVERAGE_SUMMARY',
  'min-line'   => 'CL_YAML_COVERAGE_MIN_LINE',
  'min-branch' => 'CL_YAML_COVERAGE_MIN_BRANCH',
);
my %DEFAULT = (
  input        => '/tmp/cl-yaml-kit-coverage/per-file.tsv',
  'min-line'   => 100,
  'min-branch' => 100,
);
# Each pair names a kind and the option holding its minimum. Index 0 selects the
# line columns 2 and 3 of a summary row, index 1 the branch columns 4 and 5.
my @KINDS = (['line', 'min-line'], ['branch', 'min-branch']);

sub fail {
  my ($message) = @_;
  print STDERR "$0: $message\n";
  exit $EXIT_CANNOT_RUN;
}

sub usage {
  print STDERR <<"USAGE";
usage: $0 [--input TSV] [--min-line PERCENT] [--min-branch PERCENT]

  --input TSV          per-file coverage summary (default: \$COVERAGE_SUMMARY,
                       else /tmp/cl-yaml-kit-coverage/per-file.tsv)
  --min-line PERCENT   required line coverage (default:
                       \$CL_YAML_COVERAGE_MIN_LINE, else 100)
  --min-branch PERCENT required branch coverage (default:
                       \$CL_YAML_COVERAGE_MIN_BRANCH, else 100)

exit 0 when every file meets both thresholds, 1 when a file is below one, and 2
when the settings are invalid or the summary cannot be read.
USAGE
  exit $EXIT_CANNOT_RUN;
}

sub fail_usage {
  my ($message) = @_;
  print STDERR "$0: $message\n\n";
  usage();
}

# A set-but-empty environment variable is how a shell spells "unset", while an
# empty option value is a mistake and stays visible to the validator.
sub resolve {
  my ($setting, $given) = @_;
  my $environment = $ENV{$ENVIRONMENT{$setting}};
  return ($given, 'option') if defined $given;
  return ($environment, $ENVIRONMENT{$setting})
    if defined $environment && length $environment;
  return ($DEFAULT{$setting}, 'default');
}

sub resolve_percent {
  my ($setting, $given) = @_;
  my ($raw, $origin) = resolve($setting, $given);
  my $number = $raw =~ /\A(?:\d+(?:\.\d+)?|\.\d+)\z/ ? $raw : undef;
  fail("--$setting ($origin) must be a percentage in 0..100, got '$raw'")
    if !defined $number || $number > 100;
  return 0 + $number;
}

sub parse_arguments {
  my %given;
  while (@ARGV) {
    my $argument = shift @ARGV;
    my ($setting) = $argument =~ /\A--([a-z][a-z-]*)\z/;
    fail_usage("unexpected argument '$argument'") unless defined $setting;
    fail_usage("unknown option --$setting") unless exists $DEFAULT{$setting};
    fail_usage("option --$setting requires a value") unless @ARGV;
    $given{$setting} = shift @ARGV;
  }
  return %given;
}

sub read_rows {
  my ($path) = @_;
  open my $handle, '<', $path or fail("cannot read $path: $!");
  my (@rows, $number, $header);
  while (my $line = <$handle>) {
    $number++;
    next if $line =~ /\A#/;
    chomp $line;
    my @fields = split /\t/, $line, -1;
    unless ($header) {
      fail("$path line $number: expected header '"
           . join("\t", @COLUMNS) . "', got '$line'")
        unless "@fields" eq "@COLUMNS";
      $header = 1;
      next;
    }
    fail("$path line $number: expected " . scalar(@COLUMNS)
         . " tab-separated columns, got " . scalar(@fields))
      if @fields != @COLUMNS;
    my @counts;
    for my $column (1 .. 4) {
      fail("$path line $number: $COLUMNS[$column] must be a non-negative"
           . " integer, got '$fields[$column]'")
        unless $fields[$column] =~ /\A\d+\z/;
      push @counts, 0 + $fields[$column];
    }
    push @rows, { file => $fields[0], counts => \@counts,
                  uncovered => $fields[5] };
  }
  close $handle or fail("cannot read $path: $!");
  fail("$path is empty") unless $number;
  fail("$path has no header row") unless $header;
  fail("$path has no data rows") unless @rows;
  return \@rows;
}

sub percentage {
  my ($covered, $total) = @_;
  return $total ? 100 * $covered / $total : undef;
}

sub percent_text {
  my ($value) = @_;
  return 'n/a' unless defined $value;
  return sprintf '%.1f%%', $value;
}

my %given = parse_arguments();
my ($input) = resolve('input', $given{input});
my @minimum
  = map { resolve_percent($KINDS[$_][1], $given{ $KINDS[$_][1] }) } 0 .. $#KINDS;
my $rows = read_rows($input);

my (@failures, @totals);
for my $row (@$rows) {
  my @values;
  for my $index (0 .. $#KINDS) {
    my ($covered, $total) = @{$row->{counts}}[2 * $index, 2 * $index + 1];
    push @values, percentage($covered, $total);
    $totals[2 * $index] += $covered;
    $totals[2 * $index + 1] += $total;
  }
  $row->{values} = \@values;
  my @reasons;
  for my $index (0 .. $#KINDS) {
    next unless defined $values[$index];
    next if $values[$index] >= $minimum[$index];
    push @reasons, sprintf '%s %s, required %s', $KINDS[$index][0],
      percent_text($values[$index]), percent_text($minimum[$index]);
  }
  push @failures, { file => $row->{file}, reasons => \@reasons } if @reasons;
}

print "| file | line | branch | uncovered lines |\n";
print "|---|---:|---:|---|\n";
for my $row (@$rows) {
  print sprintf "| %s | %s | %s | %s |\n", $row->{file},
    percent_text($row->{values}[0]), percent_text($row->{values}[1]),
    $row->{uncovered};
}
print sprintf "| total | %s | %s |  |\n",
  percent_text(percentage($totals[0], $totals[1])),
  percent_text(percentage($totals[2], $totals[3]));

if (@failures) {
  print "\n";
  for my $failure (@failures) {
    print "  $failure->{file}: ", join('; ', @{$failure->{reasons}}), "\n";
  }
  printf "coverage gate: FAIL (%d of %d files below threshold)\n",
    scalar @failures, scalar @$rows;
  exit 1;
}
printf "coverage gate: PASS (all %d files at or above threshold)\n",
  scalar @$rows;
exit 0;
