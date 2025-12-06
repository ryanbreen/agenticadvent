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
chomp @lines;
close($fh);

sub part1 {
    my @lines = @_;
    my $sum = 0;

    # Bag contains: 12 red, 13 green, 14 blue
    my %max_cubes = (
        'red' => 12,
        'green' => 13,
        'blue' => 14
    );

    foreach my $line (@lines) {
        # Parse: Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
        if ($line =~ /^Game (\d+): (.+)$/) {
            my $game_id = $1;
            my $game_data = $2;

            my $possible = 1;

            # Split by semicolons to get each draw
            my @draws = split(/;\s*/, $game_data);

            foreach my $draw (@draws) {
                # Split by commas to get each color count
                my @cubes = split(/,\s*/, $draw);

                foreach my $cube (@cubes) {
                    # Parse "3 blue" or "4 red"
                    if ($cube =~ /(\d+)\s+(\w+)/) {
                        my $count = $1;
                        my $color = $2;

                        # Check if this exceeds the bag limit
                        if ($count > $max_cubes{$color}) {
                            $possible = 0;
                            last;
                        }
                    }
                }
                last unless $possible;
            }

            $sum += $game_id if $possible;
        }
    }

    return $sum;
}

sub part2 {
    my @lines = @_;
    my $sum = 0;

    foreach my $line (@lines) {
        # Parse: Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
        if ($line =~ /^Game (\d+): (.+)$/) {
            my $game_id = $1;
            my $game_data = $2;

            # Track minimum cubes needed for each color
            my %min_cubes = (
                'red' => 0,
                'green' => 0,
                'blue' => 0
            );

            # Split by semicolons to get each draw
            my @draws = split(/;\s*/, $game_data);

            foreach my $draw (@draws) {
                # Split by commas to get each color count
                my @cubes = split(/,\s*/, $draw);

                foreach my $cube (@cubes) {
                    # Parse "3 blue" or "4 red"
                    if ($cube =~ /(\d+)\s+(\w+)/) {
                        my $count = $1;
                        my $color = $2;

                        # Track the maximum seen for each color
                        if ($count > $min_cubes{$color}) {
                            $min_cubes{$color} = $count;
                        }
                    }
                }
            }

            # Calculate power (red * green * blue)
            my $power = $min_cubes{'red'} * $min_cubes{'green'} * $min_cubes{'blue'};
            $sum += $power;
        }
    }

    return $sum;
}

# Main execution
print "Part 1: " . part1(@lines) . "\n";
print "Part 2: " . part2(@lines) . "\n";
