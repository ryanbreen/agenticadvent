#!/usr/bin/env perl
use strict;
use warnings;
use feature 'say';

# Read input
my $input_file = '../input.txt';
open(my $fh, '<', $input_file) or die "Cannot open $input_file: $!";
my @lines = <$fh>;
close($fh);
chomp @lines;

# Parse points - each line is "x,y"
my @points;
for my $line (@lines) {
    my ($x, $y) = split /,/, $line;
    push @points, [$x, $y];
}

sub part1 {
    my $max_area = 0;
    my $n = scalar @points;

    # Check all pairs of points as opposite corners
    for my $i (0 .. $n-1) {
        my ($x1, $y1) = @{$points[$i]};
        for my $j ($i+1 .. $n-1) {
            my ($x2, $y2) = @{$points[$j]};
            # Rectangle area = width * height (inclusive of both corners)
            my $width = abs($x2 - $x1) + 1;
            my $height = abs($y2 - $y1) + 1;
            my $area = $width * $height;
            $max_area = $area if $area > $max_area;
        }
    }

    return $max_area;
}

# Helper functions for part2
sub is_inside_polygon_p2 {
    my ($x, $y, $vert_by_x_ref) = @_;
    my $crossings = 0;

    # Cast ray to the right
    for my $vx (sort {$a <=> $b} keys %$vert_by_x_ref) {
        next if $vx <= $x;
        for my $edge (@{$vert_by_x_ref->{$vx}}) {
            my ($y_min, $y_max) = @$edge;
            if ($y > $y_min && $y < $y_max) {
                $crossings++;
            } elsif ($y == $y_min || $y == $y_max) {
                $crossings += 0.5;
            }
        }
    }

    return ($crossings % 2) == 1;
}

sub rectangle_valid_p2 {
    my ($x1, $y1, $x2, $y2, $vert_by_x_ref, $horiz_by_y_ref) = @_;
    my $min_x = $x1 < $x2 ? $x1 : $x2;
    my $max_x = $x1 > $x2 ? $x1 : $x2;
    my $min_y = $y1 < $y2 ? $y1 : $y2;
    my $max_y = $y1 > $y2 ? $y1 : $y2;

    # Check if any vertical edge crosses through rectangle interior
    for my $vx (keys %$vert_by_x_ref) {
        if ($vx > $min_x && $vx < $max_x) {
            for my $edge (@{$vert_by_x_ref->{$vx}}) {
                my ($y_min, $y_max) = @$edge;
                # Check if edge overlaps with rectangle's y range
                unless ($y_max <= $min_y || $y_min >= $max_y) {
                    return 0;
                }
            }
        }
    }

    # Check if any horizontal edge crosses through rectangle interior
    for my $hy (keys %$horiz_by_y_ref) {
        if ($hy > $min_y && $hy < $max_y) {
            for my $edge (@{$horiz_by_y_ref->{$hy}}) {
                my ($x_min, $x_max) = @$edge;
                # Check if edge overlaps with rectangle's x range
                unless ($x_max <= $min_x || $x_min >= $max_x) {
                    return 0;
                }
            }
        }
    }

    # Check that center is inside polygon
    my $center_x = ($min_x + $max_x) / 2;
    my $center_y = ($min_y + $max_y) / 2;
    return is_inside_polygon_p2($center_x, $center_y, $vert_by_x_ref);
}

sub part2 {
    my $n = scalar @points;
    my @horizontal_edges;  # [y, x_min, x_max]
    my @vertical_edges;    # [x, y_min, y_max]

    # Build edges from consecutive points
    for my $i (0 .. $n-1) {
        my ($x1, $y1) = @{$points[$i]};
        my ($x2, $y2) = @{$points[($i + 1) % $n]};

        if ($y1 == $y2) {  # Horizontal edge
            my $min_x = $x1 < $x2 ? $x1 : $x2;
            my $max_x = $x1 > $x2 ? $x1 : $x2;
            push @horizontal_edges, [$y1, $min_x, $max_x];
        } else {  # Vertical edge
            my $min_y = $y1 < $y2 ? $y1 : $y2;
            my $max_y = $y1 > $y2 ? $y1 : $y2;
            push @vertical_edges, [$x1, $min_y, $max_y];
        }
    }

    # Build maps for efficient lookup
    my %vert_by_x;
    for my $edge (@vertical_edges) {
        my ($x, $y_min, $y_max) = @$edge;
        push @{$vert_by_x{$x}}, [$y_min, $y_max];
    }

    my %horiz_by_y;
    for my $edge (@horizontal_edges) {
        my ($y, $x_min, $x_max) = @$edge;
        push @{$horiz_by_y{$y}}, [$x_min, $x_max];
    }

    # Find largest valid rectangle with red corners
    my $max_area = 0;

    for my $i (0 .. $n-1) {
        my ($x1, $y1) = @{$points[$i]};
        for my $j ($i+1 .. $n-1) {
            my ($x2, $y2) = @{$points[$j]};

            if (rectangle_valid_p2($x1, $y1, $x2, $y2, \%vert_by_x, \%horiz_by_y)) {
                my $width = abs($x2 - $x1) + 1;
                my $height = abs($y2 - $y1) + 1;
                my $area = $width * $height;
                $max_area = $area if $area > $max_area;
            }
        }
    }

    return $max_area;
}

say "Part 1: ", part1();
say "Part 2: ", part2();
