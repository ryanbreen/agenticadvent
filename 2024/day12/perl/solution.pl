#!/usr/bin/env perl
use strict;
use warnings;
use v5.10;
use List::Util qw(sum);

# Read input
my $input_file = "../input.txt";
open my $fh, '<', $input_file or die "Cannot open $input_file: $!";
my @grid = map { chomp; [split //] } <$fh>;
close $fh;

my $rows = scalar @grid;
my $cols = scalar @{$grid[0]};

# Direction vectors (up, down, left, right)
my @DIRECTIONS = ([0, 1], [0, -1], [1, 0], [-1, 0]);

sub coord {
    my ($r, $c) = @_;
    return "$r,$c";
}

sub find_regions {
    my %visited;
    my @regions;

    for my $r (0 .. $rows - 1) {
        for my $c (0 .. $cols - 1) {
            my $key = coord($r, $c);
            next if exists $visited{$key};

            # BFS to find all cells in this region
            my $plant = $grid[$r][$c];
            my %region;
            my @queue = ([$r, $c]);

            while (@queue) {
                my $cell = shift @queue;
                my ($cr, $cc) = @$cell;
                my $cell_key = coord($cr, $cc);

                next if exists $visited{$cell_key};
                next if $cr < 0 || $cr >= $rows || $cc < 0 || $cc >= $cols;
                next if $grid[$cr][$cc] ne $plant;

                $visited{$cell_key} = 1;
                $region{$cell_key} = 1;

                # Add neighbors
                for my $dir (@DIRECTIONS) {
                    my ($dr, $dc) = @$dir;
                    my $nr = $cr + $dr;
                    my $nc = $cc + $dc;
                    my $next_key = coord($nr, $nc);
                    push @queue, [$nr, $nc] unless exists $visited{$next_key};
                }
            }

            push @regions, \%region;
        }
    }

    return @regions;
}

sub calculate_perimeter {
    my ($region) = @_;
    my $perimeter = 0;

    for my $cell (keys %$region) {
        my ($r, $c) = split /,/, $cell;
        for my $dir (@DIRECTIONS) {
            my ($dr, $dc) = @$dir;
            my $nr = $r + $dr;
            my $nc = $c + $dc;
            my $neighbor = coord($nr, $nc);
            $perimeter++ unless exists $region->{$neighbor};
        }
    }

    return $perimeter;
}

sub count_sides {
    my ($region) = @_;
    my $corners = 0;

    for my $cell (keys %$region) {
        my ($r, $c) = split /,/, $cell;

        # Check all 4 corners of this cell
        # Each corner is defined by checking two orthogonal neighbors and the diagonal
        # Convex: both orthogonal out
        # Concave: both orthogonal in, diagonal out

        my $up = exists $region->{coord($r - 1, $c)};
        my $down = exists $region->{coord($r + 1, $c)};
        my $left = exists $region->{coord($r, $c - 1)};
        my $right = exists $region->{coord($r, $c + 1)};
        my $up_left = exists $region->{coord($r - 1, $c - 1)};
        my $up_right = exists $region->{coord($r - 1, $c + 1)};
        my $down_left = exists $region->{coord($r + 1, $c - 1)};
        my $down_right = exists $region->{coord($r + 1, $c + 1)};

        # Top-left corner
        if (!$up && !$left) {  # convex
            $corners++;
        } elsif ($up && $left && !$up_left) {  # concave
            $corners++;
        }

        # Top-right corner
        if (!$up && !$right) {  # convex
            $corners++;
        } elsif ($up && $right && !$up_right) {  # concave
            $corners++;
        }

        # Bottom-left corner
        if (!$down && !$left) {  # convex
            $corners++;
        } elsif ($down && $left && !$down_left) {  # concave
            $corners++;
        }

        # Bottom-right corner
        if (!$down && !$right) {  # convex
            $corners++;
        } elsif ($down && $right && !$down_right) {  # concave
            $corners++;
        }
    }

    return $corners;
}

# Compute regions once and share between parts
my @regions = find_regions();

sub part1 {
    return sum map {
        my $area = keys %$_;
        my $perimeter = calculate_perimeter($_);
        $area * $perimeter;
    } @regions;
}

sub part2 {
    return sum map {
        my $area = keys %$_;
        my $sides = count_sides($_);
        $area * $sides;
    } @regions;
}

say "Part 1: " . part1();
say "Part 2: " . part2();
