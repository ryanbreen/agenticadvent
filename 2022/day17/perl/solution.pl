#!/usr/bin/env perl
use strict;
use warnings;
use File::Basename;

# Rock shapes as list of [dx, dy] offsets from bottom-left
my @ROCKS = (
    [[0, 0], [1, 0], [2, 0], [3, 0]],           # Horizontal line
    [[1, 0], [0, 1], [1, 1], [2, 1], [1, 2]],   # Plus
    [[0, 0], [1, 0], [2, 0], [2, 1], [2, 2]],   # L shape
    [[0, 0], [0, 1], [0, 2], [0, 3]],           # Vertical line
    [[0, 0], [1, 0], [0, 1], [1, 1]]            # Square
);

my $WIDTH = 7;

sub simulate {
    my ($jets, $num_rocks) = @_;
    my %occupied;
    my $height = 0;
    my $jet_idx = 0;
    my $jet_len = length($jets);

    # For cycle detection
    my %states;
    my @heights;

    for (my $rock_num = 0; $rock_num < $num_rocks; $rock_num++) {
        my $rock_type = $rock_num % 5;
        my $rock = $ROCKS[$rock_type];

        # Starting position: left edge at x=2, bottom at y=height+3
        my $x = 2;
        my $y = $height + 3;

        while (1) {
            # Jet push
            my $jet = substr($jets, $jet_idx, 1);
            $jet_idx = ($jet_idx + 1) % $jet_len;

            my $dx = ($jet eq '>') ? 1 : -1;

            # Check if can move horizontally
            my $can_move = 1;
            for my $cell (@$rock) {
                my ($rx, $ry) = @$cell;
                my $nx = $x + $rx + $dx;
                my $ny = $y + $ry;
                if ($nx < 0 || $nx >= $WIDTH || exists $occupied{"$nx,$ny"}) {
                    $can_move = 0;
                    last;
                }
            }

            if ($can_move) {
                $x += $dx;
            }

            # Fall down
            my $can_fall = 1;
            for my $cell (@$rock) {
                my ($rx, $ry) = @$cell;
                my $nx = $x + $rx;
                my $ny = $y + $ry - 1;
                if ($ny < 0 || exists $occupied{"$nx,$ny"}) {
                    $can_fall = 0;
                    last;
                }
            }

            if ($can_fall) {
                $y--;
            } else {
                # Rock stops
                for my $cell (@$rock) {
                    my ($rx, $ry) = @$cell;
                    my $fx = $x + $rx;
                    my $fy = $y + $ry;
                    $occupied{"$fx,$fy"} = 1;
                    if ($fy + 1 > $height) {
                        $height = $fy + 1;
                    }
                }
                last;
            }
        }

        push @heights, $height;

        # Cycle detection for Part 2
        if ($num_rocks > 10000) {
            # Create state key from surface profile
            my $profile_depth = 30;
            my @profile;
            for my $col (0 .. $WIDTH - 1) {
                my $found = 0;
                for my $row (0 .. $profile_depth - 1) {
                    my $check_y = $height - 1 - $row;
                    if (exists $occupied{"$col,$check_y"}) {
                        push @profile, "$col,$row";
                        $found = 1;
                        last;
                    }
                }
                if (!$found) {
                    push @profile, "$col,$profile_depth";
                }
            }

            my $state = "$rock_type,$jet_idx," . join(";", @profile);

            if (exists $states{$state}) {
                # Found cycle
                my $cycle_start = $states{$state};
                my $cycle_len = $rock_num - $cycle_start;
                my $cycle_height = $height - $heights[$cycle_start];

                # Calculate final height
                my $remaining = $num_rocks - $rock_num - 1;
                my $full_cycles = int($remaining / $cycle_len);
                my $leftover = $remaining % $cycle_len;

                my $final_height = $height + $full_cycles * $cycle_height;
                if ($leftover > 0) {
                    $final_height += $heights[$cycle_start + $leftover] - $heights[$cycle_start];
                }

                return $final_height;
            }

            $states{$state} = $rock_num;
        }
    }

    return $height;
}

sub part1 {
    my ($jets) = @_;
    return simulate($jets, 2022);
}

sub part2 {
    my ($jets) = @_;
    return simulate($jets, 1000000000000);
}

sub main {
    my $script_dir = dirname(__FILE__);
    my $input_file = "$script_dir/../input.txt";

    open my $fh, '<', $input_file or die "Cannot open $input_file: $!";
    my $jets = do { local $/; <$fh> };
    close $fh;

    $jets =~ s/\s+//g;

    print "Part 1: ", part1($jets), "\n";
    print "Part 2: ", part2($jets), "\n";
}

main();
