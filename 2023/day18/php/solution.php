<?php
/**
 * Day 18: Lavaduct Lagoon - Polygon area with Shoelace formula and Pick's theorem.
 */

/**
 * Parse dig plan instructions from input file.
 *
 * @param string $filename Path to input file
 * @return array List of [direction, distance, color] tuples
 */
function parseInput(string $filename): array
{
    $text = trim(file_get_contents($filename));
    $instructions = [];

    foreach (explode("\n", $text) as $line) {
        $parts = preg_split('/\s+/', $line);
        $direction = $parts[0];
        $distance = (int)$parts[1];
        // Remove (# and ) from color code
        $color = substr($parts[2], 2, -1);
        $instructions[] = [$direction, $distance, $color];
    }

    return $instructions;
}

/**
 * Calculate total area using Shoelace formula and Pick's theorem.
 *
 * Shoelace gives us twice the signed area of the polygon.
 * Pick's theorem: A = i + b/2 - 1, where i = interior points, b = boundary points
 * We want: Total = i + b = A + b/2 + 1
 *
 * @param array $vertices List of [row, col] coordinates
 * @param int $perimeter Total boundary length
 * @return string The area as a string (for GMP compatibility)
 */
function calculateArea(array $vertices, string $perimeter): string
{
    $n = count($vertices);
    $area = gmp_init(0);

    for ($i = 0; $i < $n; $i++) {
        $j = ($i + 1) % $n;
        // area += vertices[i][0] * vertices[j][1]
        $area = gmp_add($area, gmp_mul($vertices[$i][0], $vertices[$j][1]));
        // area -= vertices[j][0] * vertices[i][1]
        $area = gmp_sub($area, gmp_mul($vertices[$j][0], $vertices[$i][1]));
    }

    $area = gmp_abs($area);
    $area = gmp_div_q($area, 2);

    // Total = area + perimeter/2 + 1
    $result = gmp_add($area, gmp_div_q($perimeter, 2));
    $result = gmp_add($result, 1);

    return gmp_strval($result);
}

/**
 * Part 1: Follow the dig plan directions.
 *
 * @param array $instructions Parsed dig instructions
 * @return string The total area
 */
function part1(array $instructions): string
{
    $directionMap = [
        'R' => [0, 1],
        'D' => [1, 0],
        'L' => [0, -1],
        'U' => [-1, 0],
    ];

    $vertices = [[gmp_init(0), gmp_init(0)]];
    $perimeter = gmp_init(0);
    $r = gmp_init(0);
    $c = gmp_init(0);

    foreach ($instructions as [$direction, $distance, $_]) {
        [$dr, $dc] = $directionMap[$direction];
        $r = gmp_add($r, gmp_mul($dr, $distance));
        $c = gmp_add($c, gmp_mul($dc, $distance));
        $vertices[] = [$r, $c];
        $perimeter = gmp_add($perimeter, $distance);
    }

    return calculateArea($vertices, $perimeter);
}

/**
 * Part 2: Decode instructions from hex color codes.
 * Last digit of hex: 0=R, 1=D, 2=L, 3=U
 * First 5 digits: distance in hex
 *
 * @param array $instructions Parsed dig instructions
 * @return string The total area
 */
function part2(array $instructions): string
{
    $directionMap = [
        '0' => [0, 1],   // R
        '1' => [1, 0],   // D
        '2' => [0, -1],  // L
        '3' => [-1, 0],  // U
    ];

    $vertices = [[gmp_init(0), gmp_init(0)]];
    $perimeter = gmp_init(0);
    $r = gmp_init(0);
    $c = gmp_init(0);

    foreach ($instructions as [, , $color]) {
        // First 5 hex digits = distance
        $distance = hexdec(substr($color, 0, 5));
        // Last digit = direction code
        $dirCode = $color[5];
        [$dr, $dc] = $directionMap[$dirCode];

        $r = gmp_add($r, gmp_mul($dr, $distance));
        $c = gmp_add($c, gmp_mul($dc, $distance));
        $vertices[] = [$r, $c];
        $perimeter = gmp_add($perimeter, $distance);
    }

    return calculateArea($vertices, $perimeter);
}

// Main execution
$instructions = parseInput(__DIR__ . '/../input.txt');
echo "Part 1: " . part1($instructions) . "\n";
echo "Part 2: " . part2($instructions) . "\n";
