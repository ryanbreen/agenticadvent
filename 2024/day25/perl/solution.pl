#!/usr/bin/env perl
use strict;
use warnings;
use v5.10;

# Day 25: Code Chronicle - Lock and key matching

sub parse_input {
    my ($text) = @_;
    my @locks;
    my @keys;

    # Split input into schematics (separated by blank lines)
    my @schematics = split /\n\n/, $text;

    foreach my $schematic (@schematics) {
        my @lines = split /\n/, $schematic;

        # Lock: top row is all #, bottom is all .
        # Key: top row is all ., bottom is all #
        if ($lines[0] eq '#####') {
            # It's a lock - count # from top (excluding top row)
            my @heights;
            for my $col (0..4) {
                my $height = 0;
                for my $row (1..6) {  # rows 1-6
                    if (substr($lines[$row], $col, 1) eq '#') {
                        $height++;
                    } else {
                        last;
                    }
                }
                push @heights, $height;
            }
            push @locks, \@heights;
        } else {
            # It's a key - count # from bottom (excluding bottom row)
            my @heights;
            for my $col (0..4) {
                my $height = 0;
                for (my $row = 5; $row >= 0; $row--) {  # rows 5 down to 0
                    if (substr($lines[$row], $col, 1) eq '#') {
                        $height++;
                    } else {
                        last;
                    }
                }
                push @heights, $height;
            }
            push @keys, \@heights;
        }
    }

    return (\@locks, \@keys);
}

sub fits {
    my ($lock, $key) = @_;
    # Check if a key fits a lock (no column exceeds 5)
    for my $i (0..4) {
        if ($lock->[$i] + $key->[$i] > 5) {
            return 0;
        }
    }
    return 1;
}

sub part1 {
    my ($locks, $keys) = @_;
    # Count unique lock/key pairs that fit together
    my $count = 0;
    foreach my $lock (@$locks) {
        foreach my $key (@$keys) {
            if (fits($lock, $key)) {
                $count++;
            }
        }
    }
    return $count;
}

# Main
my $input_file = "../input.txt";
open my $fh, '<', $input_file or die "Cannot open $input_file: $!";
my $text = do { local $/; <$fh> };
close $fh;

my ($locks, $keys) = parse_input($text);

my $answer1 = part1($locks, $keys);
say "Part 1: $answer1";

# Day 25 typically only has Part 1
say "Part 2: Merry Christmas!";
