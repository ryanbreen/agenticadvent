#!/usr/bin/env perl
use strict;
use warnings;
use v5.10;

sub parse_input {
    my ($text) = @_;
    my @blocks = split /\n\n/, $text;
    my @patterns;
    foreach my $block (@blocks) {
        my @lines = split /\n/, $block;
        push @patterns, \@lines;
    }
    return \@patterns;
}

sub find_vertical_reflection {
    my ($pattern) = @_;
    return 0 unless @$pattern;

    my $width = length($pattern->[0]);

    for (my $col = 1; $col < $width; $col++) {
        my $is_reflection = 1;

        foreach my $row (@$pattern) {
            # Get left and right parts
            my $left = substr($row, 0, $col);
            my $right = substr($row, $col);

            # Reverse left side
            $left = reverse($left);

            # Compare overlapping parts
            my $min_len = length($left) < length($right) ? length($left) : length($right);

            if (substr($left, 0, $min_len) ne substr($right, 0, $min_len)) {
                $is_reflection = 0;
                last;
            }
        }

        return $col if $is_reflection;
    }

    return 0;
}

sub find_horizontal_reflection {
    my ($pattern) = @_;
    return 0 unless @$pattern;

    my $height = scalar(@$pattern);

    for (my $row = 1; $row < $height; $row++) {
        my $is_reflection = 1;

        # Get top and bottom parts
        my @top = reverse(@$pattern[0..$row-1]);
        my @bottom = @$pattern[$row..$height-1];

        my $min_len = scalar(@top) < scalar(@bottom) ? scalar(@top) : scalar(@bottom);

        for (my $i = 0; $i < $min_len; $i++) {
            if ($top[$i] ne $bottom[$i]) {
                $is_reflection = 0;
                last;
            }
        }

        return $row if $is_reflection;
    }

    return 0;
}

sub summarize_pattern {
    my ($pattern) = @_;

    my $v = find_vertical_reflection($pattern);
    return $v if $v > 0;

    my $h = find_horizontal_reflection($pattern);
    return $h * 100;
}

sub part1 {
    my ($patterns) = @_;
    my $total = 0;

    foreach my $pattern (@$patterns) {
        $total += summarize_pattern($pattern);
    }

    return $total;
}

sub count_differences {
    my ($s1, $s2) = @_;
    my $count = 0;
    my $len = length($s1) < length($s2) ? length($s1) : length($s2);

    for (my $i = 0; $i < $len; $i++) {
        $count++ if substr($s1, $i, 1) ne substr($s2, $i, 1);
    }

    return $count;
}

sub find_vertical_reflection_with_smudge {
    my ($pattern) = @_;
    return 0 unless @$pattern;

    my $width = length($pattern->[0]);

    for (my $col = 1; $col < $width; $col++) {
        my $total_diff = 0;

        foreach my $row (@$pattern) {
            my $left = substr($row, 0, $col);
            my $right = substr($row, $col);

            $left = reverse($left);

            my $min_len = length($left) < length($right) ? length($left) : length($right);

            $total_diff += count_differences(substr($left, 0, $min_len), substr($right, 0, $min_len));

            last if $total_diff > 1;
        }

        return $col if $total_diff == 1;
    }

    return 0;
}

sub find_horizontal_reflection_with_smudge {
    my ($pattern) = @_;
    return 0 unless @$pattern;

    my $height = scalar(@$pattern);

    for (my $row = 1; $row < $height; $row++) {
        my $total_diff = 0;

        my @top = reverse(@$pattern[0..$row-1]);
        my @bottom = @$pattern[$row..$height-1];

        my $min_len = scalar(@top) < scalar(@bottom) ? scalar(@top) : scalar(@bottom);

        for (my $i = 0; $i < $min_len; $i++) {
            $total_diff += count_differences($top[$i], $bottom[$i]);
            last if $total_diff > 1;
        }

        return $row if $total_diff == 1;
    }

    return 0;
}

sub summarize_pattern_with_smudge {
    my ($pattern) = @_;

    my $v = find_vertical_reflection_with_smudge($pattern);
    return $v if $v > 0;

    my $h = find_horizontal_reflection_with_smudge($pattern);
    return $h * 100;
}

sub part2 {
    my ($patterns) = @_;
    my $total = 0;

    foreach my $pattern (@$patterns) {
        $total += summarize_pattern_with_smudge($pattern);
    }

    return $total;
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
