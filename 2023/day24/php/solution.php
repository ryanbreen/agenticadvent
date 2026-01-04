<?php
/**
 * Advent of Code 2023 - Day 24: Never Tell Me The Odds
 *
 * Part 1: Find 2D intersections of hailstone trajectories in XY plane
 * Part 2: Find rock position and velocity that hits all hailstones
 */

/**
 * Parse input file and return array of hailstones.
 * Each hailstone is [[px, py, pz], [vx, vy, vz]]
 */
function parseInput(string $filename): array {
    $hailstones = [];
    $lines = file($filename, FILE_IGNORE_NEW_LINES | FILE_SKIP_EMPTY_LINES);

    foreach ($lines as $line) {
        [$pos, $vel] = explode('@', $line);
        $posCoords = array_map('trim', explode(',', $pos));
        $velCoords = array_map('trim', explode(',', $vel));

        $hailstones[] = [
            [(int)$posCoords[0], (int)$posCoords[1], (int)$posCoords[2]],
            [(int)$velCoords[0], (int)$velCoords[1], (int)$velCoords[2]]
        ];
    }

    return $hailstones;
}

/**
 * Find intersection of two hailstone paths in 2D (XY plane).
 * Returns [x, y, t1, t2] or null if parallel.
 */
function findIntersection2D(array $h1, array $h2): ?array {
    [$px1, $py1] = [$h1[0][0], $h1[0][1]];
    [$vx1, $vy1] = [$h1[1][0], $h1[1][1]];
    [$px2, $py2] = [$h2[0][0], $h2[0][1]];
    [$vx2, $vy2] = [$h2[1][0], $h2[1][1]];

    // Using Cramer's rule to solve:
    // vx1*t1 - vx2*t2 = px2 - px1
    // vy1*t1 - vy2*t2 = py2 - py1
    $det = $vx1 * (-$vy2) - (-$vx2) * $vy1;

    if ($det == 0) {
        return null; // Parallel lines
    }

    $dx = $px2 - $px1;
    $dy = $py2 - $py1;

    $t1 = ($dx * (-$vy2) - (-$vx2) * $dy) / $det;
    $t2 = ($vx1 * $dy - $dx * $vy1) / $det;

    // Calculate intersection point
    $x = $px1 + $vx1 * $t1;
    $y = $py1 + $vy1 * $t1;

    return [$x, $y, $t1, $t2];
}

/**
 * Part 1: Count intersections within test area, in the future for both hailstones.
 */
function part1(array $hailstones, float $minCoord = 200000000000000, float $maxCoord = 400000000000000): int {
    $count = 0;
    $n = count($hailstones);

    for ($i = 0; $i < $n; $i++) {
        for ($j = $i + 1; $j < $n; $j++) {
            $result = findIntersection2D($hailstones[$i], $hailstones[$j]);

            if ($result === null) {
                continue;
            }

            [$x, $y, $t1, $t2] = $result;

            // Check if intersection is in the future for both hailstones
            if ($t1 < 0 || $t2 < 0) {
                continue;
            }

            // Check if intersection is within test area
            if ($x >= $minCoord && $x <= $maxCoord && $y >= $minCoord && $y <= $maxCoord) {
                $count++;
            }
        }
    }

    return $count;
}

/**
 * Solve system of linear equations using Gaussian elimination with GMP for exact arithmetic.
 * Matrix is an array of GMP arrays, rhs is a GMP array.
 * Returns array of GMP solutions.
 */
function solveSystem(array $matrix, array $rhs): array {
    $n = count($matrix);

    // Create augmented matrix with GMP rationals (numerator, denominator pairs)
    $aug = [];
    for ($i = 0; $i < $n; $i++) {
        $aug[$i] = [];
        for ($j = 0; $j < $n; $j++) {
            // Store as [numerator, denominator]
            $aug[$i][$j] = [gmp_init($matrix[$i][$j]), gmp_init(1)];
        }
        $aug[$i][$n] = [gmp_init($rhs[$i]), gmp_init(1)];
    }

    // Helper to add two fractions: a/b + c/d = (ad + bc) / bd
    $addFrac = function($f1, $f2) {
        $num = gmp_add(gmp_mul($f1[0], $f2[1]), gmp_mul($f2[0], $f1[1]));
        $den = gmp_mul($f1[1], $f2[1]);
        $gcd = gmp_gcd($num, $den);
        return [gmp_div_q($num, $gcd), gmp_div_q($den, $gcd)];
    };

    // Helper to subtract two fractions: a/b - c/d = (ad - bc) / bd
    $subFrac = function($f1, $f2) {
        $num = gmp_sub(gmp_mul($f1[0], $f2[1]), gmp_mul($f2[0], $f1[1]));
        $den = gmp_mul($f1[1], $f2[1]);
        $gcd = gmp_gcd($num, $den);
        if (gmp_sign($gcd) != 0) {
            return [gmp_div_q($num, $gcd), gmp_div_q($den, $gcd)];
        }
        return [$num, $den];
    };

    // Helper to multiply two fractions: (a/b) * (c/d) = ac/bd
    $mulFrac = function($f1, $f2) {
        $num = gmp_mul($f1[0], $f2[0]);
        $den = gmp_mul($f1[1], $f2[1]);
        $gcd = gmp_gcd($num, $den);
        if (gmp_sign($gcd) != 0) {
            return [gmp_div_q($num, $gcd), gmp_div_q($den, $gcd)];
        }
        return [$num, $den];
    };

    // Helper to divide two fractions: (a/b) / (c/d) = ad/bc
    $divFrac = function($f1, $f2) {
        $num = gmp_mul($f1[0], $f2[1]);
        $den = gmp_mul($f1[1], $f2[0]);
        // Handle negative denominator
        if (gmp_sign($den) < 0) {
            $num = gmp_neg($num);
            $den = gmp_neg($den);
        }
        $gcd = gmp_gcd($num, $den);
        if (gmp_sign($gcd) != 0) {
            return [gmp_div_q($num, $gcd), gmp_div_q($den, $gcd)];
        }
        return [$num, $den];
    };

    // Helper to compare absolute values of fractions
    $absGreater = function($f1, $f2) {
        $v1 = gmp_abs(gmp_mul($f1[0], $f2[1]));
        $v2 = gmp_abs(gmp_mul($f2[0], $f1[1]));
        return gmp_cmp($v1, $v2) > 0;
    };

    // Forward elimination
    for ($col = 0; $col < $n; $col++) {
        // Find pivot
        $maxRow = $col;
        for ($row = $col + 1; $row < $n; $row++) {
            if ($absGreater($aug[$row][$col], $aug[$maxRow][$col])) {
                $maxRow = $row;
            }
        }

        // Swap rows
        $temp = $aug[$col];
        $aug[$col] = $aug[$maxRow];
        $aug[$maxRow] = $temp;

        if (gmp_sign($aug[$col][$col][0]) == 0) {
            continue;
        }

        // Eliminate column
        for ($row = $col + 1; $row < $n; $row++) {
            if (gmp_sign($aug[$row][$col][0]) != 0) {
                $factor = $divFrac($aug[$row][$col], $aug[$col][$col]);
                for ($j = $col; $j <= $n; $j++) {
                    $aug[$row][$j] = $subFrac($aug[$row][$j], $mulFrac($factor, $aug[$col][$j]));
                }
            }
        }
    }

    // Back substitution
    $solution = array_fill(0, $n, [gmp_init(0), gmp_init(1)]);
    for ($i = $n - 1; $i >= 0; $i--) {
        $solution[$i] = $aug[$i][$n];
        for ($j = $i + 1; $j < $n; $j++) {
            $solution[$i] = $subFrac($solution[$i], $mulFrac($aug[$i][$j], $solution[$j]));
        }
        $solution[$i] = $divFrac($solution[$i], $aug[$i][$i]);
    }

    // Convert to integers (should all be whole numbers for this problem)
    $result = [];
    for ($i = 0; $i < $n; $i++) {
        $result[$i] = gmp_div_q($solution[$i][0], $solution[$i][1]);
    }

    return $result;
}

/**
 * Part 2: Find rock position and velocity that hits all hailstones.
 * Uses linearization by taking differences between pairs of hailstones.
 */
function part2(array $hailstones): string {
    // Take first 5 hailstones to get 4 pairs
    $h = array_slice($hailstones, 0, 5);

    // Build system for XY plane (4 equations, 4 unknowns: rx, ry, rvx, rvy)
    $matrixXY = [];
    $rhsXY = [];

    for ($i = 0; $i < 4; $i++) {
        $px1 = $h[$i][0][0];
        $py1 = $h[$i][0][1];
        $vx1 = $h[$i][1][0];
        $vy1 = $h[$i][1][1];

        $px2 = $h[$i+1][0][0];
        $py2 = $h[$i+1][0][1];
        $vx2 = $h[$i+1][1][0];
        $vy2 = $h[$i+1][1][1];

        // Coefficients for rx, ry, rvx, rvy
        $a = $vy1 - $vy2;
        $b = $vx2 - $vx1;
        $c = $py2 - $py1;
        $d = $px1 - $px2;
        $e = $px1 * $vy1 - $py1 * $vx1 - ($px2 * $vy2 - $py2 * $vx2);

        $matrixXY[] = [$a, $b, $c, $d];
        $rhsXY[] = $e;
    }

    $xyResult = solveSystem($matrixXY, $rhsXY);
    $rx = $xyResult[0];
    $ry = $xyResult[1];

    // Build system for XZ plane to get rz
    $matrixXZ = [];
    $rhsXZ = [];

    for ($i = 0; $i < 4; $i++) {
        $px1 = $h[$i][0][0];
        $pz1 = $h[$i][0][2];
        $vx1 = $h[$i][1][0];
        $vz1 = $h[$i][1][2];

        $px2 = $h[$i+1][0][0];
        $pz2 = $h[$i+1][0][2];
        $vx2 = $h[$i+1][1][0];
        $vz2 = $h[$i+1][1][2];

        // Same structure as XY but with Z instead of Y
        $a = $vz1 - $vz2;
        $b = $vx2 - $vx1;
        $c = $pz2 - $pz1;
        $d = $px1 - $px2;
        $e = $px1 * $vz1 - $pz1 * $vx1 - ($px2 * $vz2 - $pz2 * $vx2);

        $matrixXZ[] = [$a, $b, $c, $d];
        $rhsXZ[] = $e;
    }

    $xzResult = solveSystem($matrixXZ, $rhsXZ);
    $rz = $xzResult[1]; // rz is the second element (rx, rz, rvx, rvz)

    // Return sum of coordinates
    $sum = gmp_add(gmp_add($rx, $ry), $rz);
    return gmp_strval($sum);
}

// Main
$inputFile = $argv[1] ?? '../input.txt';
$hailstones = parseInput($inputFile);

echo "Part 1: " . part1($hailstones) . "\n";
echo "Part 2: " . part2($hailstones) . "\n";
