#!/usr/bin/env perl
use strict;
use warnings;
use v5.20;
use feature 'signatures';
no warnings 'experimental::signatures';

use List::Util qw(min sum);

sub parse_input($text) {
    return [ map { [ split /\n/, $_ ] } split /\n\n/, $text ];
}

sub count_differences($s1, $s2) {
    my $len = min(length($s1), length($s2));
    return sum(map { substr($s1, $_, 1) ne substr($s2, $_, 1) ? 1 : 0 } 0 .. $len - 1) // 0;
}

sub find_vertical_reflection($pattern, $target_diff = 0) {
    return 0 unless @$pattern;

    my $width = length($pattern->[0]);

    for my $col (1 .. $width - 1) {
        my $total_diff = 0;

        for my $row (@$pattern) {
            my $left  = reverse(substr($row, 0, $col));
            my $right = substr($row, $col);
            my $len   = min(length($left), length($right));

            $total_diff += count_differences(substr($left, 0, $len), substr($right, 0, $len));
            last if $total_diff > $target_diff;
        }

        return $col if $total_diff == $target_diff;
    }

    return 0;
}

sub find_horizontal_reflection($pattern, $target_diff = 0) {
    return 0 unless @$pattern;

    my $height = @$pattern;

    for my $row (1 .. $height - 1) {
        my @top    = reverse @$pattern[0 .. $row - 1];
        my @bottom = @$pattern[$row .. $height - 1];
        my $len    = min(scalar @top, scalar @bottom);

        my $total_diff = 0;
        for my $i (0 .. $len - 1) {
            $total_diff += count_differences($top[$i], $bottom[$i]);
            last if $total_diff > $target_diff;
        }

        return $row if $total_diff == $target_diff;
    }

    return 0;
}

sub summarize_pattern($pattern, $target_diff = 0) {
    my $v = find_vertical_reflection($pattern, $target_diff);
    return $v if $v > 0;

    return find_horizontal_reflection($pattern, $target_diff) * 100;
}

sub part1($patterns) {
    return sum(map { summarize_pattern($_, 0) } @$patterns);
}

sub part2($patterns) {
    return sum(map { summarize_pattern($_, 1) } @$patterns);
}

# Main execution
my $input_file = '../input.txt';
open(my $fh, '<', $input_file) or die "Cannot open $input_file: $!";
my $text = do { local $/; <$fh> };
close($fh);

chomp($text);
my $patterns = parse_input($text);

say "Part 1: " . part1($patterns);
say "Part 2: " . part2($patterns);
