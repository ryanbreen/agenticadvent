#!/usr/bin/env perl
use strict;
use warnings;
use File::Basename;
use Cwd 'abs_path';

# Read input file
my $script_dir = dirname(abs_path($0));
my $input_file = "$script_dir/../input.txt";
open(my $fh, '<', $input_file) or die "Cannot open $input_file: $!";
my @lines = <$fh>;
close($fh);
chomp @lines;

# Parse grid
my @grid;
foreach my $line (@lines) {
    push @grid, [split(//, $line)];
}

my $rows = scalar @grid;
my $cols = scalar @{$grid[0]};

# Directions for 8 neighbors (including diagonals)
my @directions = (
    [-1, -1], [-1, 0], [-1, 1],
    [0, -1],           [0, 1],
    [1, -1],  [1, 0],  [1, 1]
);

# Count neighbors that are '@'
sub count_neighbors {
    my ($grid_ref, $r, $c) = @_;
    my $count = 0;

    foreach my $dir (@directions) {
        my $nr = $r + $dir->[0];
        my $nc = $c + $dir->[1];

        if ($nr >= 0 && $nr < $rows && $nc >= 0 && $nc < $cols) {
            if ($grid_ref->[$nr][$nc] eq '@') {
                $count++;
            }
        }
    }

    return $count;
}

# Part 1: Count rolls with fewer than 4 adjacent rolls
sub part1 {
    my ($grid_ref) = @_;
    my $accessible_count = 0;

    for (my $r = 0; $r < $rows; $r++) {
        for (my $c = 0; $c < $cols; $c++) {
            if ($grid_ref->[$r][$c] eq '@') {
                my $neighbors = count_neighbors($grid_ref, $r, $c);
                if ($neighbors < 4) {
                    $accessible_count++;
                }
            }
        }
    }

    return $accessible_count;
}

# Part 2: Repeatedly remove accessible rolls until none remain
sub part2 {
    my ($grid_ref) = @_;

    # Deep copy the grid
    my @grid_copy;
    for (my $i = 0; $i < $rows; $i++) {
        push @grid_copy, [@{$grid_ref->[$i]}];
    }

    my $total_removed = 0;

    while (1) {
        # Find all accessible rolls
        my @accessible;

        for (my $r = 0; $r < $rows; $r++) {
            for (my $c = 0; $c < $cols; $c++) {
                if ($grid_copy[$r][$c] eq '@') {
                    my $neighbors = count_neighbors(\@grid_copy, $r, $c);
                    if ($neighbors < 4) {
                        push @accessible, [$r, $c];
                    }
                }
            }
        }

        # If no accessible rolls found, we're done
        if (scalar @accessible == 0) {
            last;
        }

        # Remove all accessible rolls
        foreach my $pos (@accessible) {
            my ($r, $c) = @$pos;
            $grid_copy[$r][$c] = '.';
            $total_removed++;
        }
    }

    return $total_removed;
}

# Run both parts
my $answer1 = part1(\@grid);
my $answer2 = part2(\@grid);

print "Part 1: $answer1\n";
print "Part 2: $answer2\n";
