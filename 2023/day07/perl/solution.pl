#!/usr/bin/env perl
use v5.16;
use strict;
use warnings;
use File::Basename;
use File::Spec;
use List::Util qw(sum);

# Read input file
my $dir = dirname(__FILE__);
my $input_path = File::Spec->catfile($dir, '..', 'input.txt');
open(my $fh, '<', $input_path) or die "Cannot open $input_path: $!";
my @lines = <$fh>;
close($fh);
chomp(@lines);

# Card strength order (higher index = stronger)
my $CARD_STRENGTH = "23456789TJQKA";
my $CARD_STRENGTH_JOKER = "J23456789TQKA";  # J is weakest in Part 2

# Pattern-to-type mapping (shared between both hand type functions)
my %PATTERN_TYPE = (
    "5"       => 6,  # Five of a kind
    "4,1"     => 5,  # Four of a kind
    "3,2"     => 4,  # Full house
    "3,1,1"   => 3,  # Three of a kind
    "2,2,1"   => 2,  # Two pair
    "2,1,1,1" => 1,  # One pair
    "1,1,1,1,1" => 0,  # High card
);

sub pattern_to_type {
    my ($pattern) = @_;
    return $PATTERN_TYPE{$pattern} // 0;
}

sub get_hand_type {
    my ($hand) = @_;

    # Count cards
    my %counts;
    $counts{$_}++ for split //, $hand;

    # Sort counts in descending order and convert to pattern
    my $pattern = join(",", sort { $b <=> $a } values %counts);

    return pattern_to_type($pattern);
}

sub get_hand_type_with_jokers {
    my ($hand) = @_;

    my $joker_count = ($hand =~ tr/J//);

    return get_hand_type($hand) if $joker_count == 0;
    return 6 if $joker_count == 5;  # Five of a kind

    # Count non-joker cards
    my %counts;
    for my $c (split //, $hand) {
        $counts{$c}++ if $c ne 'J';
    }

    # Sort counts in descending order
    my @sorted_counts = sort { $b <=> $a } values %counts;

    # Add jokers to the highest count
    $sorted_counts[0] += $joker_count;

    # Convert to pattern and look up type
    my $pattern = join(",", @sorted_counts);

    return pattern_to_type($pattern);
}

sub hand_key {
    my ($hand, $use_jokers) = @_;

    my $hand_type = $use_jokers ? get_hand_type_with_jokers($hand) : get_hand_type($hand);
    my $strength = $use_jokers ? $CARD_STRENGTH_JOKER : $CARD_STRENGTH;
    my @card_values = map { index($strength, $_) } split //, $hand;

    return ($hand_type, @card_values);
}

sub compare_hands {
    my ($a_hand, $b_hand, $use_jokers) = @_;

    my @a_key = hand_key($a_hand, $use_jokers);
    my @b_key = hand_key($b_hand, $use_jokers);

    for my $i (0 .. $#a_key) {
        return $a_key[$i] <=> $b_key[$i] if $a_key[$i] != $b_key[$i];
    }
    return 0;
}

sub solve {
    my ($use_jokers) = @_;

    my @hands;
    for my $line (@lines) {
        next unless $line =~ /\S/;
        my ($hand, $bid) = split /\s+/, $line;
        push @hands, [$hand, $bid];
    }

    # Sort by hand strength
    my @sorted = sort { compare_hands($a->[0], $b->[0], $use_jokers) } @hands;

    # Calculate total winnings using sum from List::Util
    return sum map { ($_ + 1) * $sorted[$_][1] } 0 .. $#sorted;
}

sub part1 { solve(0) }
sub part2 { solve(1) }

say "Part 1: " . part1();
say "Part 2: " . part2();
