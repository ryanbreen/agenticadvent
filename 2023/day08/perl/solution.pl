#!/usr/bin/env perl
use strict;
use warnings;
use List::Util qw(reduce);

# Direction mapping: L -> 0 (left), R -> 1 (right)
my %dir = (L => 0, R => 1);

# Read and parse input
my $input_file = '../input.txt';
open my $fh, '<', $input_file or die "Cannot open $input_file: $!";
my @lines = <$fh>;
close $fh;
chomp @lines;

my @instructions = map { $dir{$_} } split //, shift @lines;
shift @lines;  # Skip blank line

my %network;
for my $line (@lines) {
    if ($line =~ /^(\w+)\s*=\s*\((\w+),\s*(\w+)\)/) {
        $network{$1} = [$2, $3];
    }
}

# Navigate from a starting node until reaching a node matching the end pattern
sub navigate {
    my ($start, $end_pattern) = @_;
    my $current = $start;
    my $steps = 0;
    my $num_instructions = @instructions;

    while ($current !~ $end_pattern) {
        $current = $network{$current}[$instructions[$steps % $num_instructions]];
        $steps++;
    }

    return $steps;
}

# GCD using Euclidean algorithm
sub gcd {
    my ($a, $b) = @_;
    ($a, $b) = ($b, $a % $b) while $b;
    return $a;
}

# LCM of two numbers
sub lcm {
    my ($a, $b) = @_;
    return $a / gcd($a, $b) * $b;
}

# Part 1: Navigate from AAA to ZZZ
sub part1 {
    return navigate('AAA', qr/^ZZZ$/);
}

# Part 2: Find when all ghost paths align at nodes ending in Z
sub part2 {
    my @starting_nodes = grep { /A$/ } keys %network;
    my @cycle_lengths = map { navigate($_, qr/Z$/) } @starting_nodes;
    return reduce { lcm($a, $b) } @cycle_lengths;
}

print "Part 1: ", part1(), "\n";
print "Part 2: ", part2(), "\n";
