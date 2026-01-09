#!/usr/bin/env perl
use strict;
use warnings;
use File::Basename;
use Cwd 'abs_path';

# Parse the input file into grid and instructions
sub parse_input {
    my ($text) = @_;

    my @parts = split /\n\n/, $text;
    my @grid_lines = split /\n/, $parts[0];
    my $path = $parts[1];
    $path =~ s/^\s+|\s+$//g;

    # Find dimensions
    my $height = scalar @grid_lines;
    my $width = 0;
    for my $line (@grid_lines) {
        my $len = length($line);
        $width = $len if $len > $width;
    }

    # Create grid (pad lines to consistent width)
    my @grid;
    for my $line (@grid_lines) {
        my $padded = $line . (' ' x ($width - length($line)));
        push @grid, $padded;
    }

    # Parse path into moves and turns
    my @instructions;
    while ($path =~ /(\d+|[LR])/g) {
        my $token = $1;
        if ($token =~ /^\d+$/) {
            push @instructions, int($token);
        } else {
            push @instructions, $token;
        }
    }

    return (\@grid, \@instructions);
}

# Part 1: Navigate the map with 2D flat wrapping
sub part1 {
    my ($text) = @_;
    my ($grid_ref, $instr_ref) = parse_input($text);
    my @grid = @$grid_ref;
    my @instructions = @$instr_ref;

    my $height = scalar @grid;
    my $width = length($grid[0]);

    # Directions: 0=right, 1=down, 2=left, 3=up
    my @DR = (0, 1, 0, -1);
    my @DC = (1, 0, -1, 0);

    # Find starting position (leftmost open tile on top row)
    my $row = 0;
    my $col = index($grid[0], '.');
    my $facing = 0;  # Start facing right

    for my $instr (@instructions) {
        if ($instr =~ /^\d+$/) {
            # Move forward
            for (1..$instr) {
                my $dr = $DR[$facing];
                my $dc = $DC[$facing];
                my $nr = $row + $dr;
                my $nc = $col + $dc;

                # Wrap around if needed
                if ($facing == 0) {  # Right
                    if ($nc >= $width || substr($grid[$nr], $nc, 1) eq ' ') {
                        $nc = 0;
                        while (substr($grid[$nr], $nc, 1) eq ' ') {
                            $nc++;
                        }
                    }
                } elsif ($facing == 2) {  # Left
                    if ($nc < 0 || substr($grid[$nr], $nc, 1) eq ' ') {
                        $nc = $width - 1;
                        while (substr($grid[$nr], $nc, 1) eq ' ') {
                            $nc--;
                        }
                    }
                } elsif ($facing == 1) {  # Down
                    if ($nr >= $height || substr($grid[$nr], $nc, 1) eq ' ') {
                        $nr = 0;
                        while (substr($grid[$nr], $nc, 1) eq ' ') {
                            $nr++;
                        }
                    }
                } elsif ($facing == 3) {  # Up
                    if ($nr < 0 || substr($grid[$nr], $nc, 1) eq ' ') {
                        $nr = $height - 1;
                        while (substr($grid[$nr], $nc, 1) eq ' ') {
                            $nr--;
                        }
                    }
                }

                # Check if we hit a wall
                if (substr($grid[$nr], $nc, 1) eq '#') {
                    last;  # Stop moving
                }

                # Move to new position
                $row = $nr;
                $col = $nc;
            }
        } else {
            # Turn
            if ($instr eq 'R') {
                $facing = ($facing + 1) % 4;
            } else {
                $facing = ($facing + 3) % 4;  # -1 mod 4 = 3
            }
        }
    }

    # Calculate password: 1000*row + 4*col + facing (1-indexed)
    return 1000 * ($row + 1) + 4 * ($col + 1) + $facing;
}

# Get cube face and local coordinates
sub get_cube_face_and_local {
    my ($row, $col, $face_size) = @_;

    my $face_row = int($row / $face_size);
    my $face_col = int($col / $face_size);
    my $local_r = $row % $face_size;
    my $local_c = $col % $face_size;

    # Map face_row, face_col to face number
    # Layout:
    #   12
    #   3
    #  45
    #  6
    my $face;
    if ($face_row == 0 && $face_col == 1) {
        $face = 1;
    } elsif ($face_row == 0 && $face_col == 2) {
        $face = 2;
    } elsif ($face_row == 1 && $face_col == 1) {
        $face = 3;
    } elsif ($face_row == 2 && $face_col == 0) {
        $face = 4;
    } elsif ($face_row == 2 && $face_col == 1) {
        $face = 5;
    } elsif ($face_row == 3 && $face_col == 0) {
        $face = 6;
    } else {
        $face = -1;
    }

    return ($face, $local_r, $local_c);
}

# Handle cube wrapping for the input layout
sub wrap_cube {
    my ($row, $col, $facing, $face_size) = @_;
    my $S = $face_size;
    my ($face, $lr, $lc) = get_cube_face_and_local($row, $col, $S);

    # Based on the direction and which edge we're leaving
    if ($face == 1) {
        if ($facing == 3) {  # Up: goes to face 6, from left, facing right
            return (3*$S + $lc, 0, 0);
        } elsif ($facing == 2) {  # Left: goes to face 4, from left, facing right (inverted)
            return (3*$S - 1 - $lr, 0, 0);
        }
    } elsif ($face == 2) {
        if ($facing == 0) {  # Right: goes to face 5, from right, facing left (inverted)
            return (3*$S - 1 - $lr, 2*$S - 1, 2);
        } elsif ($facing == 1) {  # Down: goes to face 3, from right, facing left
            return ($S + $lc, 2*$S - 1, 2);
        } elsif ($facing == 3) {  # Up: goes to face 6, from bottom, facing up
            return (4*$S - 1, $lc, 3);
        }
    } elsif ($face == 3) {
        if ($facing == 0) {  # Right: goes to face 2, from bottom, facing up
            return ($S - 1, 2*$S + $lr, 3);
        } elsif ($facing == 2) {  # Left: goes to face 4, from top, facing down
            return (2*$S, $lr, 1);
        }
    } elsif ($face == 4) {
        if ($facing == 3) {  # Up: goes to face 3, from left, facing right
            return ($S + $lc, $S, 0);
        } elsif ($facing == 2) {  # Left: goes to face 1, from left, facing right (inverted)
            return ($S - 1 - $lr, $S, 0);
        }
    } elsif ($face == 5) {
        if ($facing == 0) {  # Right: goes to face 2, from right, facing left (inverted)
            return ($S - 1 - $lr, 3*$S - 1, 2);
        } elsif ($facing == 1) {  # Down: goes to face 6, from right, facing left
            return (3*$S + $lc, $S - 1, 2);
        }
    } elsif ($face == 6) {
        if ($facing == 0) {  # Right: goes to face 5, from bottom, facing up
            return (3*$S - 1, $S + $lr, 3);
        } elsif ($facing == 1) {  # Down: goes to face 2, from top, facing down
            return (0, 2*$S + $lc, 1);
        } elsif ($facing == 2) {  # Left: goes to face 1, from top, facing down
            return (0, $S + $lr, 1);
        }
    }

    # Shouldn't reach here
    return ($row, $col, $facing);
}

# Part 2: Navigate the map with cube wrapping
sub part2 {
    my ($text) = @_;
    my ($grid_ref, $instr_ref) = parse_input($text);
    my @grid = @$grid_ref;
    my @instructions = @$instr_ref;

    my $height = scalar @grid;
    my $width = length($grid[0]);

    # Determine face size
    my $face_size = ($height > 50) ? 50 : 4;

    # Directions: 0=right, 1=down, 2=left, 3=up
    my @DR = (0, 1, 0, -1);
    my @DC = (1, 0, -1, 0);

    # Find starting position
    my $row = 0;
    my $col = index($grid[0], '.');
    my $facing = 0;

    for my $instr (@instructions) {
        if ($instr =~ /^\d+$/) {
            for (1..$instr) {
                my $dr = $DR[$facing];
                my $dc = $DC[$facing];
                my $nr = $row + $dr;
                my $nc = $col + $dc;
                my $nf = $facing;

                # Check if we need to wrap (off grid or hitting a space)
                my $need_wrap = 0;
                if ($nr < 0 || $nr >= $height || $nc < 0 || $nc >= $width) {
                    $need_wrap = 1;
                } elsif (substr($grid[$nr], $nc, 1) eq ' ') {
                    $need_wrap = 1;
                }

                if ($need_wrap) {
                    ($nr, $nc, $nf) = wrap_cube($row, $col, $facing, $face_size);
                }

                # Check for wall
                if (substr($grid[$nr], $nc, 1) eq '#') {
                    last;
                }

                $row = $nr;
                $col = $nc;
                $facing = $nf;
            }
        } else {
            if ($instr eq 'R') {
                $facing = ($facing + 1) % 4;
            } else {
                $facing = ($facing + 3) % 4;
            }
        }
    }

    return 1000 * ($row + 1) + 4 * ($col + 1) + $facing;
}

# Main
my $script_dir = dirname(abs_path($0));
my $input_file = "$script_dir/../input.txt";

open my $fh, '<', $input_file or die "Cannot open $input_file: $!";
my $text = do { local $/; <$fh> };
close $fh;

print "Part 1: ", part1($text), "\n";
print "Part 2: ", part2($text), "\n";
