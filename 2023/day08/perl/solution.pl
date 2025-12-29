#!/usr/bin/env perl
use strict;
use warnings;
use Math::BigInt;

# Read input file
my $input_file = '../input.txt';
open my $fh, '<', $input_file or die "Cannot open $input_file: $!";
my @lines = <$fh>;
close $fh;
chomp @lines;

# Parse input
my $instructions = shift @lines;
shift @lines;  # Skip blank line

my %network;
for my $line (@lines) {
    next unless $line =~ /\S/;
    # Parse: AAA = (BBB, CCC)
    if ($line =~ /^(\w+)\s*=\s*\((\w+),\s*(\w+)\)/) {
        $network{$1} = [$2, $3];
    }
}

# Part 1: Navigate from AAA to ZZZ
sub part1 {
    my $current = 'AAA';
    my $steps = 0;
    my $instruction_len = length($instructions);

    while ($current ne 'ZZZ') {
        my $instruction = substr($instructions, $steps % $instruction_len, 1);
        if ($instruction eq 'L') {
            $current = $network{$current}[0];
        } else {
            $current = $network{$current}[1];
        }
        $steps++;
    }

    return $steps;
}

# GCD function
sub gcd {
    my ($a, $b) = @_;
    while ($b) {
        ($a, $b) = ($b, $a % $b);
    }
    return $a;
}

# LCM function using Math::BigInt for large numbers
sub lcm {
    my ($a, $b) = @_;
    $a = Math::BigInt->new($a) unless ref($a) eq 'Math::BigInt';
    $b = Math::BigInt->new($b) unless ref($b) eq 'Math::BigInt';
    return $a * $b / gcd($a, $b);
}

# Part 2: Navigate all nodes ending in A simultaneously to nodes ending in Z
sub part2 {
    my @starting_nodes = grep { /A$/ } keys %network;
    my $instruction_len = length($instructions);

    my @cycle_lengths;

    for my $node (@starting_nodes) {
        my $current = $node;
        my $steps = 0;

        while ($current !~ /Z$/) {
            my $instruction = substr($instructions, $steps % $instruction_len, 1);
            if ($instruction eq 'L') {
                $current = $network{$current}[0];
            } else {
                $current = $network{$current}[1];
            }
            $steps++;
        }
        push @cycle_lengths, $steps;
    }

    # Find LCM of all cycle lengths
    my $result = Math::BigInt->new($cycle_lengths[0]);
    for my $i (1 .. $#cycle_lengths) {
        $result = lcm($result, $cycle_lengths[$i]);
    }

    return $result;
}

print "Part 1: ", part1(), "\n";
print "Part 2: ", part2(), "\n";
