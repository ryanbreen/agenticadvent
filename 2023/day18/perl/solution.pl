#!/usr/bin/env perl
# Day 18: Lavaduct Lagoon - Polygon area with Shoelace formula and Pick's theorem.

use strict;
use warnings;
use File::Basename;

# Parse dig plan instructions
sub parse_input {
    my ($filename) = @_;
    my @instructions;

    open my $fh, '<', $filename or die "Cannot open $filename: $!";
    while (my $line = <$fh>) {
        chomp $line;
        next unless $line;
        my ($direction, $distance, $color) = $line =~ /^(\w)\s+(\d+)\s+\(#([0-9a-f]+)\)$/;
        push @instructions, [$direction, int($distance), $color];
    }
    close $fh;

    return \@instructions;
}

# Calculate total area using Shoelace formula and Pick's theorem
sub calculate_area {
    my ($vertices, $perimeter) = @_;

    my $n = scalar @$vertices;
    my $area = 0;

    # Shoelace formula for polygon area
    for my $i (0 .. $n - 1) {
        my $j = ($i + 1) % $n;
        $area += $vertices->[$i][0] * $vertices->[$j][1];
        $area -= $vertices->[$j][0] * $vertices->[$i][1];
    }
    $area = abs($area) / 2;

    # Total points = interior + boundary
    # From Pick's theorem: interior = area - boundary/2 + 1
    # Total = interior + boundary = area + boundary/2 + 1
    return $area + $perimeter / 2 + 1;
}

# Part 1: Follow the dig plan directions
sub part1 {
    my ($instructions) = @_;

    my %direction_map = (
        'R' => [0, 1],
        'D' => [1, 0],
        'L' => [0, -1],
        'U' => [-1, 0]
    );

    my @vertices = ([0, 0]);
    my $perimeter = 0;
    my ($r, $c) = (0, 0);

    for my $inst (@$instructions) {
        my ($direction, $distance, undef) = @$inst;
        my ($dr, $dc) = @{$direction_map{$direction}};
        $r += $dr * $distance;
        $c += $dc * $distance;
        push @vertices, [$r, $c];
        $perimeter += $distance;
    }

    return calculate_area(\@vertices, $perimeter);
}

# Part 2: Decode instructions from hex color codes
sub part2 {
    my ($instructions) = @_;

    # Last digit of hex: 0=R, 1=D, 2=L, 3=U
    # First 5 digits: distance in hex
    my %direction_map = (
        '0' => [0, 1],   # R
        '1' => [1, 0],   # D
        '2' => [0, -1],  # L
        '3' => [-1, 0]   # U
    );

    my @vertices = ([0, 0]);
    my $perimeter = 0;
    my ($r, $c) = (0, 0);

    for my $inst (@$instructions) {
        my (undef, undef, $color) = @$inst;
        my $distance = hex(substr($color, 0, 5));
        my $direction = substr($color, 5, 1);
        my ($dr, $dc) = @{$direction_map{$direction}};
        $r += $dr * $distance;
        $c += $dc * $distance;
        push @vertices, [$r, $c];
        $perimeter += $distance;
    }

    return calculate_area(\@vertices, $perimeter);
}

# Main
my $script_dir = dirname(__FILE__);
my $input_file = "$script_dir/../input.txt";
my $instructions = parse_input($input_file);

printf "Part 1: %.0f\n", part1($instructions);
printf "Part 2: %.0f\n", part2($instructions);
