#!/usr/bin/env perl
use strict;
use warnings;
use v5.10;

sub parse_input {
    my ($text) = @_;
    my @equations;

    for my $line (split /\n/, $text) {
        next unless $line =~ /\S/;
        my ($target, $nums_str) = split /: /, $line;
        my @nums = split / /, $nums_str;
        push @equations, [$target, \@nums];
    }

    return \@equations;
}

sub evaluate {
    my ($nums, $ops) = @_;
    my $result = $nums->[0];

    for my $i (0 .. $#$ops) {
        my $op = $ops->[$i];
        if ($op eq '+') {
            $result += $nums->[$i + 1];
        } elsif ($op eq '*') {
            $result *= $nums->[$i + 1];
        } elsif ($op eq '||') {
            $result = $result . $nums->[$i + 1];
        }
    }

    return $result;
}

sub generate_operator_combinations {
    my ($operators, $count) = @_;
    return [[]] if $count == 0;

    my @combinations;
    my $subcombinations = generate_operator_combinations($operators, $count - 1);

    for my $op (@$operators) {
        for my $subcomb (@$subcombinations) {
            push @combinations, [$op, @$subcomb];
        }
    }

    return \@combinations;
}

sub can_make_target {
    my ($target, $nums, $operators) = @_;
    my $n_ops = scalar(@$nums) - 1;

    my $combinations = generate_operator_combinations($operators, $n_ops);

    for my $ops (@$combinations) {
        if (evaluate($nums, $ops) == $target) {
            return 1;
        }
    }

    return 0;
}

sub part1 {
    my ($equations) = @_;
    my @operators = ('+', '*');
    my $total = 0;

    for my $eq (@$equations) {
        my ($target, $nums) = @$eq;
        if (can_make_target($target, $nums, \@operators)) {
            $total += $target;
        }
    }

    return $total;
}

sub part2 {
    my ($equations) = @_;
    my @operators = ('+', '*', '||');
    my $total = 0;

    for my $eq (@$equations) {
        my ($target, $nums) = @$eq;
        if (can_make_target($target, $nums, \@operators)) {
            $total += $target;
        }
    }

    return $total;
}

# Main
my $input_file = '../input.txt';
open my $fh, '<', $input_file or die "Cannot open $input_file: $!";
my $text = do { local $/; <$fh> };
close $fh;

my $equations = parse_input($text);

say 'Part 1: ', part1($equations);
say 'Part 2: ', part2($equations);
