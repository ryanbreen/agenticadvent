#!/usr/bin/env perl
# Day 22: Sand Slabs - 3D falling bricks simulation
use strict;
use warnings;

# Parse input - read bricks as arrays of 6 coordinates
sub parse_input {
    my ($filename) = @_;
    my @bricks;

    open my $fh, '<', $filename or die "Cannot open $filename: $!";
    while (my $line = <$fh>) {
        chomp $line;
        next unless $line;

        my ($left, $right) = split /~/, $line;
        my ($x1, $y1, $z1) = split /,/, $left;
        my ($x2, $y2, $z2) = split /,/, $right;

        # Ensure z1 <= z2 for consistent processing
        if ($z1 > $z2) {
            ($x1, $y1, $z1, $x2, $y2, $z2) = ($x2, $y2, $z2, $x1, $y1, $z1);
        }

        push @bricks, [$x1, $y1, $z1, $x2, $y2, $z2];
    }
    close $fh;

    return \@bricks;
}

# Settle bricks and build support relationships
sub settle_bricks {
    my ($bricks) = @_;
    my $n = scalar @$bricks;

    # Create indexed list and sort by minimum z coordinate
    my @indexed;
    for my $i (0 .. $n - 1) {
        push @indexed, [$i, $bricks->[$i]];
    }
    @indexed = sort { $a->[1][2] <=> $b->[1][2] } @indexed;

    # Track occupied cells: "x,y,z" -> brick index
    my %occupied;
    my @settled;
    $settled[$_] = undef for 0 .. $n - 1;

    # supports{i} = set of brick indices that brick i supports (bricks above)
    # supporters{i} = set of brick indices that support brick i (bricks below)
    my %supports;
    my %supporters;

    for my $item (@indexed) {
        my ($orig_idx, $brick) = @$item;
        my ($x1, $y1, $z1, $x2, $y2, $z2) = @$brick;

        # Find the maximum drop (to z=1)
        my $drop = $z1 - 1;

        # Get xy footprint and find how far we can drop
        my $min_x = $x1 < $x2 ? $x1 : $x2;
        my $max_x = $x1 > $x2 ? $x1 : $x2;
        my $min_y = $y1 < $y2 ? $y1 : $y2;
        my $max_y = $y1 > $y2 ? $y1 : $y2;

        for my $x ($min_x .. $max_x) {
            for my $y ($min_y .. $max_y) {
                # Check each z level below the brick
                for my $z (reverse 1 .. $z1 - 1) {
                    my $key = "$x,$y,$z";
                    if (exists $occupied{$key}) {
                        my $possible_drop = $z1 - $z - 1;
                        $drop = $possible_drop if $possible_drop < $drop;
                        last;
                    }
                }
            }
        }

        # Drop the brick
        my $new_z1 = $z1 - $drop;
        my $new_z2 = $z2 - $drop;
        $settled[$orig_idx] = [$x1, $y1, $new_z1, $x2, $y2, $new_z2];

        # Initialize sets
        $supports{$orig_idx} //= {};
        $supporters{$orig_idx} //= {};

        # Mark cells as occupied and find supporters
        for my $x ($min_x .. $max_x) {
            for my $y ($min_y .. $max_y) {
                # Check if there's a brick directly below
                my $below_key = "$x,$y," . ($new_z1 - 1);
                if (exists $occupied{$below_key}) {
                    my $supporter_idx = $occupied{$below_key};
                    $supporters{$orig_idx}{$supporter_idx} = 1;
                    $supports{$supporter_idx}{$orig_idx} = 1;
                }

                # Mark all cells of this brick as occupied
                for my $z ($new_z1 .. $new_z2) {
                    $occupied{"$x,$y,$z"} = $orig_idx;
                }
            }
        }
    }

    return (\@settled, \%supports, \%supporters);
}

# Part 1: Count bricks that can be safely disintegrated
sub part1 {
    my ($bricks) = @_;
    my ($settled, $supports, $supporters) = settle_bricks($bricks);
    my $n = scalar @$bricks;

    my $safe_count = 0;
    for my $i (0 .. $n - 1) {
        # Brick i can be safely removed if every brick it supports
        # has at least one other supporter
        my $can_remove = 1;

        my $supported_set = $supports->{$i} // {};
        for my $supported (keys %$supported_set) {
            my $supporter_count = scalar keys %{$supporters->{$supported}};
            if ($supporter_count == 1) {
                $can_remove = 0;
                last;
            }
        }

        $safe_count++ if $can_remove;
    }

    return $safe_count;
}

# Part 2: Count total bricks that would fall for each disintegration
sub part2 {
    my ($bricks) = @_;
    my ($settled, $supports, $supporters) = settle_bricks($bricks);
    my $n = scalar @$bricks;

    my $total_falls = 0;

    for my $i (0 .. $n - 1) {
        # Simulate removing brick i and count chain reaction using BFS
        my %falling = ($i => 1);
        my @queue = ($i);

        while (@queue) {
            my $brick = shift @queue;

            # Check all bricks that this brick supports
            my $supported_set = $supports->{$brick} // {};
            for my $supported (keys %$supported_set) {
                next if exists $falling{$supported};

                # This brick falls if all its supporters have fallen
                my $all_supporters_fallen = 1;
                my $supporter_set = $supporters->{$supported} // {};
                for my $sup (keys %$supporter_set) {
                    unless (exists $falling{$sup}) {
                        $all_supporters_fallen = 0;
                        last;
                    }
                }

                if ($all_supporters_fallen) {
                    $falling{$supported} = 1;
                    push @queue, $supported;
                }
            }
        }

        # Don't count the initial brick we removed
        $total_falls += (scalar keys %falling) - 1;
    }

    return $total_falls;
}

# Main
my $input_path = $0;
$input_path =~ s/[^\/]+$//;
$input_path .= "../input.txt";

my $bricks = parse_input($input_path);
print "Part 1: ", part1($bricks), "\n";
print "Part 2: ", part2($bricks), "\n";
