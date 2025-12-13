#!/usr/bin/env perl
use strict;
use warnings;
use v5.10;

# Read input file
my $input_file = "../input.txt";
open(my $fh, '<', $input_file) or die "Cannot open $input_file: $!";
my @lines = <$fh>;
close($fh);
chomp(@lines);

# Parse input into grid
my @grid;
foreach my $line (@lines) {
    my @row = split //, $line;
    push @grid, [map { int($_) } @row];
}

my $rows = scalar @grid;
my $cols = scalar @{$grid[0]};

# Directions: up, down, left, right
my @DIRS = ([-1, 0], [1, 0], [0, -1], [0, 1]);

sub find_trailheads {
    my @trailheads;
    for my $r (0 .. $rows - 1) {
        for my $c (0 .. $cols - 1) {
            if ($grid[$r][$c] == 0) {
                push @trailheads, [$r, $c];
            }
        }
    }
    return @trailheads;
}

sub count_reachable_nines {
    my ($start_r, $start_c) = @_;

    # BFS to find all 9s reachable from a trailhead
    my %visited;
    my @queue = ([$start_r, $start_c]);
    $visited{"$start_r,$start_c"} = 1;
    my %nines;

    while (@queue) {
        my $current = shift @queue;
        my ($r, $c) = @$current;
        my $current_height = $grid[$r][$c];

        if ($current_height == 9) {
            $nines{"$r,$c"} = 1;
            next;
        }

        # Try all four directions
        foreach my $dir (@DIRS) {
            my ($dr, $dc) = @$dir;
            my $nr = $r + $dr;
            my $nc = $c + $dc;

            if ($nr >= 0 && $nr < $rows && $nc >= 0 && $nc < $cols) {
                my $key = "$nr,$nc";
                if (!exists $visited{$key}) {
                    if ($grid[$nr][$nc] == $current_height + 1) {
                        $visited{$key} = 1;
                        push @queue, [$nr, $nc];
                    }
                }
            }
        }
    }

    return scalar keys %nines;
}

sub count_distinct_trails {
    my ($start_r, $start_c) = @_;

    # DFS to count all distinct trails from a trailhead to any 9
    my $dfs;
    $dfs = sub {
        my ($r, $c) = @_;
        my $current_height = $grid[$r][$c];

        if ($current_height == 9) {
            return 1;
        }

        my $total = 0;
        foreach my $dir (@DIRS) {
            my ($dr, $dc) = @$dir;
            my $nr = $r + $dr;
            my $nc = $c + $dc;

            if ($nr >= 0 && $nr < $rows && $nc >= 0 && $nc < $cols) {
                if ($grid[$nr][$nc] == $current_height + 1) {
                    $total += $dfs->($nr, $nc);
                }
            }
        }

        return $total;
    };

    return $dfs->($start_r, $start_c);
}

sub part1 {
    my @trailheads = find_trailheads();
    my $total_score = 0;
    foreach my $th (@trailheads) {
        my ($r, $c) = @$th;
        $total_score += count_reachable_nines($r, $c);
    }
    return $total_score;
}

sub part2 {
    my @trailheads = find_trailheads();
    my $total_rating = 0;
    foreach my $th (@trailheads) {
        my ($r, $c) = @$th;
        $total_rating += count_distinct_trails($r, $c);
    }
    return $total_rating;
}

# Main execution
say "Part 1: " . part1();
say "Part 2: " . part2();
