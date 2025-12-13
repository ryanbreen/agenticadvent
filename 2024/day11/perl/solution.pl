#!/usr/bin/env perl
use strict;
use warnings;
use v5.10;

# Read input file
my $input_file = '../input.txt';
open my $fh, '<', $input_file or die "Cannot open $input_file: $!";
my $input = do { local $/; <$fh> };
close $fh;

# Parse space-separated numbers
chomp $input;
my @stones = split /\s+/, $input;

# Memoization hash for count_stones function
my %memo;

sub count_stones {
    my ($value, $blinks) = @_;

    # Base case: no more blinks
    return 1 if $blinks == 0;

    # Check memoization cache
    my $key = "$value,$blinks";
    return $memo{$key} if exists $memo{$key};

    my $result;

    # Rule 1: 0 becomes 1
    if ($value == 0) {
        $result = count_stones(1, $blinks - 1);
    }
    # Rule 2: Even number of digits -> split
    elsif (length($value) % 2 == 0) {
        my $s = "$value";
        my $mid = length($s) / 2;
        my $left = int(substr($s, 0, $mid));
        my $right = int(substr($s, $mid));
        $result = count_stones($left, $blinks - 1) + count_stones($right, $blinks - 1);
    }
    # Rule 3: Multiply by 2024
    else {
        $result = count_stones($value * 2024, $blinks - 1);
    }

    # Store in cache
    $memo{$key} = $result;
    return $result;
}

sub part1 {
    my $total = 0;
    for my $stone (@stones) {
        $total += count_stones($stone, 25);
    }
    return $total;
}

sub part2 {
    my $total = 0;
    for my $stone (@stones) {
        $total += count_stones($stone, 75);
    }
    return $total;
}

# Run both parts
say "Part 1: " . part1();
say "Part 2: " . part2();
