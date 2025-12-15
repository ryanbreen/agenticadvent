#!/usr/bin/env perl
use strict;
use warnings;
use File::Basename;
use File::Spec;

my $input_path = File::Spec->catfile(dirname(__FILE__), '..', 'input.txt');
my $input_text = do {
    local $/;
    open my $fh, '<', $input_path or die "Cannot open $input_path: $!";
    my $text = <$fh>;
    close $fh;
    $text =~ s/\s+$//;
    $text;
};

sub parse_input {
    my ($text) = @_;
    my ($grid_text, $moves_text) = split /\n\n/, $text, 2;

    my @grid = map { [split //] } split /\n/, $grid_text;
    my $moves = $moves_text;
    $moves =~ s/\s+//g;

    return (\@grid, $moves);
}

sub find_robot {
    my ($grid) = @_;
    for my $r (0 .. $#{$grid}) {
        for my $c (0 .. $#{$grid->[$r]}) {
            return ($r, $c) if $grid->[$r][$c] eq '@';
        }
    }
    return (undef, undef);
}

sub move_robot {
    my ($grid, $robot_r, $robot_c, $direction) = @_;

    my %dirs = (
        '<' => [0, -1],
        '>' => [0, 1],
        '^' => [-1, 0],
        'v' => [1, 0]
    );

    my ($dr, $dc) = @{$dirs{$direction}};
    my ($nr, $nc) = ($robot_r + $dr, $robot_c + $dc);

    # Hit a wall
    return ($robot_r, $robot_c) if $grid->[$nr][$nc] eq '#';

    # Empty space
    if ($grid->[$nr][$nc] eq '.') {
        $grid->[$robot_r][$robot_c] = '.';
        $grid->[$nr][$nc] = '@';
        return ($nr, $nc);
    }

    # Box
    if ($grid->[$nr][$nc] eq 'O') {
        my ($check_r, $check_c) = ($nr, $nc);
        while ($grid->[$check_r][$check_c] eq 'O') {
            $check_r += $dr;
            $check_c += $dc;
        }

        # Wall blocking
        return ($robot_r, $robot_c) if $grid->[$check_r][$check_c] eq '#';

        # Move the chain
        $grid->[$check_r][$check_c] = 'O';
        $grid->[$robot_r][$robot_c] = '.';
        $grid->[$nr][$nc] = '@';
        return ($nr, $nc);
    }

    return ($robot_r, $robot_c);
}

sub calculate_gps {
    my ($grid, $box_char) = @_;
    $box_char //= 'O';

    my $total = 0;
    for my $r (0 .. $#{$grid}) {
        for my $c (0 .. $#{$grid->[$r]}) {
            if ($grid->[$r][$c] eq $box_char) {
                $total += 100 * $r + $c;
            }
        }
    }
    return $total;
}

sub part1 {
    my ($grid, $moves) = parse_input($input_text);
    my ($robot_r, $robot_c) = find_robot($grid);

    for my $move (split //, $moves) {
        ($robot_r, $robot_c) = move_robot($grid, $robot_r, $robot_c, $move);
    }

    return calculate_gps($grid);
}

sub scale_grid {
    my ($grid) = @_;
    my @new_grid;

    for my $row (@$grid) {
        my @new_row;
        for my $cell (@$row) {
            if ($cell eq '#') {
                push @new_row, '#', '#';
            } elsif ($cell eq 'O') {
                push @new_row, '[', ']';
            } elsif ($cell eq '.') {
                push @new_row, '.', '.';
            } elsif ($cell eq '@') {
                push @new_row, '@', '.';
            }
        }
        push @new_grid, \@new_row;
    }

    return \@new_grid;
}

sub can_move_box_vertical {
    my ($grid, $box_left_c, $r, $dr) = @_;

    my $nr = $r + $dr;
    my ($left_c, $right_c) = ($box_left_c, $box_left_c + 1);

    my $left_target = $grid->[$nr][$left_c];
    my $right_target = $grid->[$nr][$right_c];

    # Wall blocks
    return 0 if $left_target eq '#' || $right_target eq '#';

    # Check for boxes in the way
    my %boxes_to_check;

    if ($left_target eq '[') {
        $boxes_to_check{"$nr,$left_c"} = [$nr, $left_c];
    } elsif ($left_target eq ']') {
        $boxes_to_check{"$nr," . ($left_c - 1)} = [$nr, $left_c - 1];
    }

    if ($right_target eq '[') {
        $boxes_to_check{"$nr,$right_c"} = [$nr, $right_c];
    } elsif ($right_target eq ']') {
        $boxes_to_check{"$nr," . ($right_c - 1)} = [$nr, $right_c - 1];
    }

    # Recursively check
    for my $key (keys %boxes_to_check) {
        my ($box_r, $box_c) = @{$boxes_to_check{$key}};
        return 0 unless can_move_box_vertical($grid, $box_c, $box_r, $dr);
    }

    return 1;
}

sub collect_boxes_vertical {
    my ($grid, $box_left_c, $r, $dr, $collected) = @_;

    my $key = "$r,$box_left_c";
    return if exists $collected->{$key};
    $collected->{$key} = [$r, $box_left_c];

    my $nr = $r + $dr;
    my ($left_c, $right_c) = ($box_left_c, $box_left_c + 1);

    my $left_target = $grid->[$nr][$left_c];
    my $right_target = $grid->[$nr][$right_c];

    my %boxes_to_check;

    if ($left_target eq '[') {
        $boxes_to_check{"$nr,$left_c"} = [$nr, $left_c];
    } elsif ($left_target eq ']') {
        $boxes_to_check{"$nr," . ($left_c - 1)} = [$nr, $left_c - 1];
    }

    if ($right_target eq '[') {
        $boxes_to_check{"$nr,$right_c"} = [$nr, $right_c];
    } elsif ($right_target eq ']') {
        $boxes_to_check{"$nr," . ($right_c - 1)} = [$nr, $right_c - 1];
    }

    for my $key (keys %boxes_to_check) {
        my ($box_r, $box_c) = @{$boxes_to_check{$key}};
        collect_boxes_vertical($grid, $box_c, $box_r, $dr, $collected);
    }
}

sub move_robot_wide {
    my ($grid, $robot_r, $robot_c, $direction) = @_;

    my %dirs = (
        '<' => [0, -1],
        '>' => [0, 1],
        '^' => [-1, 0],
        'v' => [1, 0]
    );

    my ($dr, $dc) = @{$dirs{$direction}};
    my ($nr, $nc) = ($robot_r + $dr, $robot_c + $dc);

    my $target = $grid->[$nr][$nc];

    # Hit a wall
    return ($robot_r, $robot_c) if $target eq '#';

    # Empty space
    if ($target eq '.') {
        $grid->[$robot_r][$robot_c] = '.';
        $grid->[$nr][$nc] = '@';
        return ($nr, $nc);
    }

    # Handle box pushing
    if ($target eq '[' || $target eq ']') {
        if ($dc != 0) {  # Horizontal movement
            my $check_c = $nc;
            while ($grid->[$robot_r][$check_c] eq '[' || $grid->[$robot_r][$check_c] eq ']') {
                $check_c += $dc;
            }

            # Wall blocking
            return ($robot_r, $robot_c) if $grid->[$robot_r][$check_c] eq '#';

            # Shift all boxes
            if ($dc > 0) {  # Moving right
                for (my $col = $check_c; $col > $nc; $col--) {
                    $grid->[$robot_r][$col] = $grid->[$robot_r][$col - 1];
                }
            } else {  # Moving left
                for (my $col = $check_c; $col < $nc; $col++) {
                    $grid->[$robot_r][$col] = $grid->[$robot_r][$col + 1];
                }
            }

            $grid->[$robot_r][$robot_c] = '.';
            $grid->[$nr][$nc] = '@';
            return ($nr, $nc);

        } else {  # Vertical movement
            # Find the left edge of the box
            my $box_left_c = ($target eq '[') ? $nc : $nc - 1;

            # Check if we can move
            return ($robot_r, $robot_c) unless can_move_box_vertical($grid, $box_left_c, $nr, $dr);

            # Collect all boxes
            my %boxes_to_move;
            collect_boxes_vertical($grid, $box_left_c, $nr, $dr, \%boxes_to_move);

            # Sort boxes by row
            my @sorted_boxes = sort {
                ($dr > 0) ? $b->[0] <=> $a->[0] : $a->[0] <=> $b->[0]
            } values %boxes_to_move;

            # Move all boxes
            for my $box (@sorted_boxes) {
                my ($box_r, $box_c) = @$box;
                $grid->[$box_r][$box_c] = '.';
                $grid->[$box_r][$box_c + 1] = '.';
                $grid->[$box_r + $dr][$box_c] = '[';
                $grid->[$box_r + $dr][$box_c + 1] = ']';
            }

            # Move robot
            $grid->[$robot_r][$robot_c] = '.';
            $grid->[$nr][$nc] = '@';
            return ($nr, $nc);
        }
    }

    return ($robot_r, $robot_c);
}

sub part2 {
    my ($grid, $moves) = parse_input($input_text);
    $grid = scale_grid($grid);
    my ($robot_r, $robot_c) = find_robot($grid);

    for my $move (split //, $moves) {
        ($robot_r, $robot_c) = move_robot_wide($grid, $robot_r, $robot_c, $move);
    }

    return calculate_gps($grid, '[');
}

print "Part 1: " . part1() . "\n";
print "Part 2: " . part2() . "\n";
