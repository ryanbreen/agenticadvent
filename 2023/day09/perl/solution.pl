#!/usr/bin/env perl
use strict;
use warnings;
use List::Util qw(all);

# Read input from ../input.txt
my $input_file = '../input.txt';
open(my $fh, '<', $input_file) or die "Cannot open $input_file: $!";
my @lines = <$fh>;
close($fh);

# Parse input: each line is a sequence of integers
my @histories;
for my $line (@lines) {
    chomp $line;
    next if $line eq '';
    my @nums = split /\s+/, $line;
    push @histories, \@nums;
}

# Compute successive differences of a sequence
sub get_differences {
    my @seq = @{$_[0]};
    my @diffs;
    for my $i (0 .. $#seq - 1) {
        push @diffs, $seq[$i + 1] - $seq[$i];
    }
    return \@diffs;
}

# Part 1: Extrapolate the next value by adding last values bottom-up
sub extrapolate_next {
    my @seq = @{$_[0]};
    my @sequences = (\@seq);
    my $current = \@seq;

    # Build difference sequences until all zeros
    while (!all { $_ == 0 } @$current) {
        $current = get_differences($current);
        push @sequences, $current;
    }

    # Work backwards: add last element of lower sequence to last element of current
    for my $i (reverse 0 .. $#sequences - 1) {
        my $new_val = $sequences[$i][-1] + $sequences[$i + 1][-1];
        push @{$sequences[$i]}, $new_val;
    }

    return $sequences[0][-1];
}

# Part 2: Extrapolate the previous value by subtracting first values bottom-up
sub extrapolate_prev {
    my @seq = @{$_[0]};
    my @sequences = (\@seq);
    my $current = \@seq;

    # Build difference sequences until all zeros
    while (!all { $_ == 0 } @$current) {
        $current = get_differences($current);
        push @sequences, $current;
    }

    # Work backwards: subtract first element of lower sequence from first element of current
    for my $i (reverse 0 .. $#sequences - 1) {
        my $new_val = $sequences[$i][0] - $sequences[$i + 1][0];
        unshift @{$sequences[$i]}, $new_val;
    }

    return $sequences[0][0];
}

# Part 1: Sum of all next extrapolated values
my $part1 = 0;
for my $history (@histories) {
    my @copy = @$history;
    $part1 += extrapolate_next(\@copy);
}

# Part 2: Sum of all previous extrapolated values
my $part2 = 0;
for my $history (@histories) {
    my @copy = @$history;
    $part2 += extrapolate_prev(\@copy);
}

print "Part 1: $part1\n";
print "Part 2: $part2\n";
