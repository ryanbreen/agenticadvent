#!/usr/bin/env perl
use strict;
use warnings;
use List::Util qw(max);
use FindBin qw($RealBin);

# Read input file
my $input_file = "$RealBin/../input.txt";
open(my $fh, '<', $input_file) or die "Cannot open $input_file: $!";
my @lines = <$fh>;
chomp @lines;
close($fh);

sub parse_input {
    my @lines = @_;

    # Find the blank line separator
    my $blank_idx = -1;
    for my $i (0..$#lines) {
        if ($lines[$i] eq "") {
            $blank_idx = $i;
            last;
        }
    }

    # Parse ranges from the first section
    my @ranges;
    for my $i (0..$blank_idx-1) {
        my ($start, $end) = split(/-/, $lines[$i]);
        push @ranges, [$start, $end];
    }

    # Parse ingredient IDs from the second section
    my @ingredient_ids;
    for my $i ($blank_idx+1..$#lines) {
        next if $lines[$i] eq "";
        push @ingredient_ids, int($lines[$i]);
    }

    return (\@ranges, \@ingredient_ids);
}

sub part1 {
    my @lines = @_;

    my ($ranges, $ingredient_ids) = parse_input(@lines);

    # Count how many ingredient IDs fall within any range
    my $fresh_count = 0;
    for my $ingredient_id (@$ingredient_ids) {
        for my $range (@$ranges) {
            my ($start, $end) = @$range;
            if ($ingredient_id >= $start && $ingredient_id <= $end) {
                $fresh_count++;
                last;  # Found a match, no need to check other ranges
            }
        }
    }

    return $fresh_count;
}

sub part2 {
    my @lines = @_;

    my ($ranges, $ingredient_ids) = parse_input(@lines);

    # Sort ranges by start position
    my @sorted_ranges = sort { $a->[0] <=> $b->[0] } @$ranges;

    # Merge overlapping ranges
    my @merged;
    for my $range (@sorted_ranges) {
        my ($start, $end) = @$range;
        if (@merged && $start <= $merged[-1][1] + 1) {
            # Overlapping or adjacent - merge with the last range
            $merged[-1][1] = max($end, $merged[-1][1]);
        } else {
            # No overlap - add as new range
            push @merged, [$start, $end];
        }
    }

    # Count total unique IDs covered by merged ranges
    my $total_count = 0;
    for my $range (@merged) {
        my ($start, $end) = @$range;
        $total_count += ($end - $start + 1);
    }

    return $total_count;
}

print "Part 1: " . part1(@lines) . "\n";
print "Part 2: " . part2(@lines) . "\n";
