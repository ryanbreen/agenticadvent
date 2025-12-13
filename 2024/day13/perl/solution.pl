#!/usr/bin/env perl
use strict;
use warnings;
use v5.10;

# Read input file
my $input_file = '../input.txt';
open my $fh, '<', $input_file or die "Cannot open $input_file: $!";
my $input_text = do { local $/; <$fh> };
close $fh;

sub parse_machines {
    my ($text) = @_;
    my @machines;

    # Split by blank lines
    my @blocks = split /\n\n/, $text;

    foreach my $block (@blocks) {
        my @lines = split /\n/, $block;

        # Button A: X+ax, Y+ay
        my ($ax, $ay) = $lines[0] =~ /Button A: X\+(\d+), Y\+(\d+)/;
        # Button B: X+bx, Y+by
        my ($bx, $by) = $lines[1] =~ /Button B: X\+(\d+), Y\+(\d+)/;
        # Prize: X=px, Y=py
        my ($px, $py) = $lines[2] =~ /Prize: X=(\d+), Y=(\d+)/;

        push @machines, [$ax, $ay, $bx, $by, $px, $py];
    }

    return @machines;
}

sub solve_machine {
    my ($ax, $ay, $bx, $by, $px, $py, $max_presses) = @_;

    # Cramer's rule for system of linear equations:
    # a*ax + b*bx = px
    # a*ay + b*by = py

    my $det = $ax * $by - $ay * $bx;

    # No unique solution
    return undef if $det == 0;

    # Calculate using integer arithmetic
    my $a_num = $px * $by - $py * $bx;
    my $b_num = $ax * $py - $ay * $px;

    # Check if solutions are integers
    return undef if $a_num % $det != 0 || $b_num % $det != 0;

    my $a = int($a_num / $det);
    my $b = int($b_num / $det);

    # Check non-negative
    return undef if $a < 0 || $b < 0;

    # Check max presses constraint (Part 1)
    if (defined $max_presses && ($a > $max_presses || $b > $max_presses)) {
        return undef;
    }

    return 3 * $a + $b;
}

sub part1 {
    my @machines = parse_machines($input_text);
    my $total = 0;

    foreach my $machine (@machines) {
        my $cost = solve_machine(@$machine, 100);
        $total += $cost if defined $cost;
    }

    return $total;
}

sub part2 {
    my @machines = parse_machines($input_text);
    my $offset = 10_000_000_000_000;
    my $total = 0;

    foreach my $machine (@machines) {
        my ($ax, $ay, $bx, $by, $px, $py) = @$machine;

        # Shift prize coordinates
        my $cost = solve_machine($ax, $ay, $bx, $by, $px + $offset, $py + $offset, undef);
        $total += $cost if defined $cost;
    }

    return $total;
}

say "Part 1: ", part1();
say "Part 2: ", part2();
