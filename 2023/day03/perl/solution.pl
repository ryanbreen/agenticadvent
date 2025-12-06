#!/usr/bin/env perl
use strict;
use warnings;
use File::Basename;
use File::Spec;

# Read input file
my $dir = dirname(__FILE__);
my $input_file = File::Spec->catfile($dir, '..', 'input.txt');
open(my $fh, '<', $input_file) or die "Cannot open $input_file: $!";
my @lines = <$fh>;
close($fh);
chomp @lines;

# Helper function to check if a character is a symbol (not digit, not period)
sub is_symbol {
    my $char = shift;
    return 0 if !defined $char;
    return 0 if $char eq '.';
    return 0 if $char =~ /\d/;
    return 1;
}

# Helper function to check if position is within grid bounds
sub in_bounds {
    my ($row, $col, $rows, $cols) = @_;
    return $row >= 0 && $row < $rows && $col >= 0 && $col < $cols;
}

# Part 1: Find all numbers adjacent to symbols
sub part1 {
    my $sum = 0;
    my $rows = scalar @lines;

    for my $r (0 .. $rows - 1) {
        my $line = $lines[$r];
        my $cols = length($line);

        # Find all numbers in this line
        while ($line =~ /(\d+)/g) {
            my $number = $1;
            my $end_pos = pos($line) - 1;
            my $start_pos = $end_pos - length($number) + 1;

            # Check if this number is adjacent to any symbol
            my $is_part_number = 0;

            # Check all positions around this number
            for my $col ($start_pos .. $end_pos) {
                # Check 8 directions around each digit
                for my $dr (-1, 0, 1) {
                    for my $dc (-1, 0, 1) {
                        next if $dr == 0 && $dc == 0;  # Skip the digit itself

                        my $nr = $r + $dr;
                        my $nc = $col + $dc;

                        if (in_bounds($nr, $nc, $rows, $cols)) {
                            my $char = substr($lines[$nr], $nc, 1);
                            if (is_symbol($char)) {
                                $is_part_number = 1;
                                last;
                            }
                        }
                    }
                    last if $is_part_number;
                }
                last if $is_part_number;
            }

            $sum += $number if $is_part_number;
        }
    }

    return $sum;
}

# Part 2: Find all gears (stars with exactly 2 adjacent numbers)
sub part2 {
    my $rows = scalar @lines;
    my %gear_numbers;  # Hash of "row,col" => [adjacent numbers]

    # Find all numbers and track which stars they're adjacent to
    for my $r (0 .. $rows - 1) {
        my $line = $lines[$r];
        my $cols = length($line);

        # Find all numbers in this line
        while ($line =~ /(\d+)/g) {
            my $number = $1;
            my $end_pos = pos($line) - 1;
            my $start_pos = $end_pos - length($number) + 1;

            # Track which stars this number is adjacent to
            my %adjacent_stars;

            # Check all positions around this number
            for my $col ($start_pos .. $end_pos) {
                # Check 8 directions around each digit
                for my $dr (-1, 0, 1) {
                    for my $dc (-1, 0, 1) {
                        next if $dr == 0 && $dc == 0;

                        my $nr = $r + $dr;
                        my $nc = $col + $dc;

                        if (in_bounds($nr, $nc, $rows, $cols)) {
                            my $char = substr($lines[$nr], $nc, 1);
                            if ($char eq '*') {
                                my $key = "$nr,$nc";
                                $adjacent_stars{$key} = 1;
                            }
                        }
                    }
                }
            }

            # Add this number to all adjacent stars
            for my $star_key (keys %adjacent_stars) {
                push @{$gear_numbers{$star_key}}, $number;
            }
        }
    }

    # Calculate sum of gear ratios
    my $sum = 0;
    for my $star_key (keys %gear_numbers) {
        my @numbers = @{$gear_numbers{$star_key}};
        # A gear must be adjacent to exactly 2 numbers
        if (scalar @numbers == 2) {
            $sum += $numbers[0] * $numbers[1];
        }
    }

    return $sum;
}

# Run both parts
print "Part 1: " . part1() . "\n";
print "Part 2: " . part2() . "\n";
