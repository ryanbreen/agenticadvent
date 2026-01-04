#!/usr/bin/env perl
use strict;
use warnings;
use Math::BigRat;

# Parse input file
sub parse_input {
    my ($filename) = @_;
    my @hailstones;

    open my $fh, '<', $filename or die "Cannot open $filename: $!";
    while (my $line = <$fh>) {
        chomp $line;
        my ($pos, $vel) = split /\s*@\s*/, $line;
        my @p = split /\s*,\s*/, $pos;
        my @v = split /\s*,\s*/, $vel;
        push @hailstones, [[@p], [@v]];
    }
    close $fh;

    return \@hailstones;
}

# Find intersection of two hailstone paths in 2D (XY plane)
sub find_intersection_2d {
    my ($h1, $h2) = @_;

    my ($px1, $py1) = @{$h1->[0]}[0, 1];
    my ($vx1, $vy1) = @{$h1->[1]}[0, 1];
    my ($px2, $py2) = @{$h2->[0]}[0, 1];
    my ($vx2, $vy2) = @{$h2->[1]}[0, 1];

    # Using Cramer's rule to solve:
    # vx1*t1 - vx2*t2 = px2 - px1
    # vy1*t1 - vy2*t2 = py2 - py1

    my $det = $vx1 * (-$vy2) - (-$vx2) * $vy1;
    return () if $det == 0;  # Parallel lines

    my $dx = $px2 - $px1;
    my $dy = $py2 - $py1;

    my $t1 = ($dx * (-$vy2) - (-$vx2) * $dy) / $det;
    my $t2 = ($vx1 * $dy - $dx * $vy1) / $det;

    my $x = $px1 + $vx1 * $t1;
    my $y = $py1 + $vy1 * $t1;

    return ($x, $y, $t1, $t2);
}

# Part 1: Count intersections within test area in the future
sub part1 {
    my ($hailstones, $min_coord, $max_coord) = @_;
    $min_coord //= 200000000000000;
    $max_coord //= 400000000000000;

    my $count = 0;
    my $n = scalar @$hailstones;

    for my $i (0 .. $n - 2) {
        for my $j ($i + 1 .. $n - 1) {
            my @result = find_intersection_2d($hailstones->[$i], $hailstones->[$j]);
            next unless @result;

            my ($x, $y, $t1, $t2) = @result;

            # Check if intersection is in the future for both
            next if $t1 < 0 || $t2 < 0;

            # Check if within test area
            if ($x >= $min_coord && $x <= $max_coord &&
                $y >= $min_coord && $y <= $max_coord) {
                $count++;
            }
        }
    }

    return $count;
}

# Solve system of linear equations using Gaussian elimination with BigRat
sub solve_system {
    my ($matrix, $rhs) = @_;
    my $n = scalar @$matrix;

    # Create augmented matrix - already has BigRat values
    my @aug;
    for my $i (0 .. $n - 1) {
        my @row;
        for my $j (0 .. $n - 1) {
            push @row, $matrix->[$i][$j]->copy();
        }
        push @row, $rhs->[$i]->copy();
        push @aug, \@row;
    }

    # Forward elimination
    for my $col (0 .. $n - 1) {
        # Find pivot (row with largest absolute value in this column)
        my $max_row = $col;
        for my $row ($col + 1 .. $n - 1) {
            if ($aug[$row][$col]->copy()->babs() > $aug[$max_row][$col]->copy()->babs()) {
                $max_row = $row;
            }
        }
        # Swap rows
        ($aug[$col], $aug[$max_row]) = ($aug[$max_row], $aug[$col]);

        next if $aug[$col][$col]->is_zero();

        # Eliminate column
        for my $row ($col + 1 .. $n - 1) {
            if (!$aug[$row][$col]->is_zero()) {
                my $factor = $aug[$row][$col] / $aug[$col][$col];
                for my $j ($col .. $n) {
                    $aug[$row][$j] = $aug[$row][$j] - $factor * $aug[$col][$j];
                }
            }
        }
    }

    # Back substitution
    my @solution = map { Math::BigRat->new(0) } (0 .. $n - 1);
    for my $i (reverse 0 .. $n - 1) {
        $solution[$i] = $aug[$i][$n]->copy();
        for my $j ($i + 1 .. $n - 1) {
            $solution[$i] = $solution[$i] - $aug[$i][$j] * $solution[$j];
        }
        $solution[$i] = $solution[$i] / $aug[$i][$i];
    }

    return @solution;
}

# Part 2: Find rock position that hits all hailstones
sub part2 {
    my ($hailstones) = @_;

    # Take first 5 hailstones to get 4 pairs of equations
    my @h = @{$hailstones}[0..4];

    # Build system for XY plane (4 equations, 4 unknowns: rx, ry, rvx, rvy)
    # Use BigRat from the start to avoid overflow
    my @matrix_xy;
    my @rhs_xy;

    for my $i (0 .. 3) {
        my $px1 = Math::BigRat->new($h[$i][0][0]);
        my $py1 = Math::BigRat->new($h[$i][0][1]);
        my $vx1 = Math::BigRat->new($h[$i][1][0]);
        my $vy1 = Math::BigRat->new($h[$i][1][1]);
        my $px2 = Math::BigRat->new($h[$i+1][0][0]);
        my $py2 = Math::BigRat->new($h[$i+1][0][1]);
        my $vx2 = Math::BigRat->new($h[$i+1][1][0]);
        my $vy2 = Math::BigRat->new($h[$i+1][1][1]);

        # Coefficients for rx, ry, rvx, rvy
        my $a = $vy1 - $vy2;
        my $b = $vx2 - $vx1;
        my $c = $py2 - $py1;
        my $d = $px1 - $px2;
        my $e = $px1*$vy1 - $py1*$vx1 - ($px2*$vy2 - $py2*$vx2);

        push @matrix_xy, [$a, $b, $c, $d];
        push @rhs_xy, $e;
    }

    my ($rx, $ry, $rvx, $rvy) = solve_system(\@matrix_xy, \@rhs_xy);

    # Build system for XZ plane (4 equations, 4 unknowns: rx, rz, rvx, rvz)
    my @matrix_xz;
    my @rhs_xz;

    for my $i (0 .. 3) {
        my $px1 = Math::BigRat->new($h[$i][0][0]);
        my $pz1 = Math::BigRat->new($h[$i][0][2]);
        my $vx1 = Math::BigRat->new($h[$i][1][0]);
        my $vz1 = Math::BigRat->new($h[$i][1][2]);
        my $px2 = Math::BigRat->new($h[$i+1][0][0]);
        my $pz2 = Math::BigRat->new($h[$i+1][0][2]);
        my $vx2 = Math::BigRat->new($h[$i+1][1][0]);
        my $vz2 = Math::BigRat->new($h[$i+1][1][2]);

        # Same structure as XY but with Z instead of Y
        my $a = $vz1 - $vz2;
        my $b = $vx2 - $vx1;
        my $c = $pz2 - $pz1;
        my $d = $px1 - $px2;
        my $e = $px1*$vz1 - $pz1*$vx1 - ($px2*$vz2 - $pz2*$vx2);

        push @matrix_xz, [$a, $b, $c, $d];
        push @rhs_xz, $e;
    }

    my ($rx2, $rz, $rvx2, $rvz) = solve_system(\@matrix_xz, \@rhs_xz);

    # Return sum as integer
    my $sum = $rx + $ry + $rz;
    return $sum->as_int();
}

# Main
my $input_file = $ARGV[0] // '../input.txt';
my $hailstones = parse_input($input_file);

print "Part 1: ", part1($hailstones), "\n";
print "Part 2: ", part2($hailstones), "\n";
