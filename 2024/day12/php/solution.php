<?php

declare(strict_types=1);

$input = file_get_contents(__DIR__ . '/../input.txt');
$input = trim($input);

// Parse input
$lines = explode("\n", $input);
$grid = array_map('str_split', $lines);
$rows = count($grid);
$cols = count($grid[0]);

/**
 * Find all connected regions in the grid using BFS.
 *
 * @param array<int, array<int, string>> $grid
 * @param int $rows
 * @param int $cols
 * @return array<int, array<string, bool>>
 */
function find_regions(array $grid, int $rows, int $cols): array {
    $visited = [];
    $regions = [];

    for ($r = 0; $r < $rows; $r++) {
        for ($c = 0; $c < $cols; $c++) {
            $key = "$r,$c";
            if (isset($visited[$key])) {
                continue;
            }

            // BFS to find all cells in this region
            $plant = $grid[$r][$c];
            $region = [];
            $queue = new SplQueue();
            $queue->enqueue([$r, $c]);

            while (!$queue->isEmpty()) {
                [$cr, $cc] = $queue->dequeue();
                $ckey = "$cr,$cc";

                if (isset($visited[$ckey])) {
                    continue;
                }
                if ($cr < 0 || $cr >= $rows || $cc < 0 || $cc >= $cols) {
                    continue;
                }
                if ($grid[$cr][$cc] !== $plant) {
                    continue;
                }

                $visited[$ckey] = true;
                $region[$ckey] = true;

                $directions = [[0, 1], [0, -1], [1, 0], [-1, 0]];
                foreach ($directions as [$dr, $dc]) {
                    $nr = $cr + $dr;
                    $nc = $cc + $dc;
                    $nkey = "$nr,$nc";
                    if (!isset($visited[$nkey])) {
                        $queue->enqueue([$nr, $nc]);
                    }
                }
            }

            $regions[] = $region;
        }
    }

    return $regions;
}

/**
 * Calculate perimeter of a region (edges not touching same region).
 *
 * @param array<string, bool> $region
 * @return int
 */
function calculate_perimeter(array $region): int {
    $perimeter = 0;
    $directions = [[0, 1], [0, -1], [1, 0], [-1, 0]];

    foreach ($region as $pos => $val) {
        [$r, $c] = explode(',', $pos);
        $r = (int)$r;
        $c = (int)$c;

        foreach ($directions as [$dr, $dc]) {
            $nr = $r + $dr;
            $nc = $c + $dc;
            $nkey = "$nr,$nc";
            if (!isset($region[$nkey])) {
                $perimeter++;
            }
        }
    }

    return $perimeter;
}

/**
 * Count number of sides (corners) in a region.
 *
 * @param array<string, bool> $region
 * @return int
 */
function count_sides(array $region): int {
    $corners = 0;

    foreach ($region as $pos => $val) {
        [$r, $c] = explode(',', $pos);
        $r = (int)$r;
        $c = (int)$c;

        // Check all 4 corners of this cell
        // Each corner is defined by checking two orthogonal neighbors and the diagonal
        // Convex: both orthogonal out
        // Concave: both orthogonal in, diagonal out

        $up = isset($region[($r - 1) . ",$c"]);
        $down = isset($region[($r + 1) . ",$c"]);
        $left = isset($region["$r," . ($c - 1)]);
        $right = isset($region["$r," . ($c + 1)]);
        $up_left = isset($region[($r - 1) . "," . ($c - 1)]);
        $up_right = isset($region[($r - 1) . "," . ($c + 1)]);
        $down_left = isset($region[($r + 1) . "," . ($c - 1)]);
        $down_right = isset($region[($r + 1) . "," . ($c + 1)]);

        // Top-left corner
        if (!$up && !$left) {  // convex
            $corners++;
        } elseif ($up && $left && !$up_left) {  // concave
            $corners++;
        }

        // Top-right corner
        if (!$up && !$right) {  // convex
            $corners++;
        } elseif ($up && $right && !$up_right) {  // concave
            $corners++;
        }

        // Bottom-left corner
        if (!$down && !$left) {  // convex
            $corners++;
        } elseif ($down && $left && !$down_left) {  // concave
            $corners++;
        }

        // Bottom-right corner
        if (!$down && !$right) {  // convex
            $corners++;
        } elseif ($down && $right && !$down_right) {  // concave
            $corners++;
        }
    }

    return $corners;
}

/**
 * Calculate total fencing cost: sum of area * perimeter for each region.
 *
 * @param array<int, array<int, string>> $grid
 * @param int $rows
 * @param int $cols
 * @return int
 */
function part1(array $grid, int $rows, int $cols): int {
    $regions = find_regions($grid, $rows, $cols);
    $total = 0;

    foreach ($regions as $region) {
        $area = count($region);
        $perimeter = calculate_perimeter($region);
        $total += $area * $perimeter;
    }

    return $total;
}

/**
 * Calculate total fencing cost using sides instead of perimeter.
 *
 * @param array<int, array<int, string>> $grid
 * @param int $rows
 * @param int $cols
 * @return int
 */
function part2(array $grid, int $rows, int $cols): int {
    $regions = find_regions($grid, $rows, $cols);
    $total = 0;

    foreach ($regions as $region) {
        $area = count($region);
        $sides = count_sides($region);
        $total += $area * $sides;
    }

    return $total;
}

echo "Part 1: " . part1($grid, $rows, $cols) . "\n";
echo "Part 2: " . part2($grid, $rows, $cols) . "\n";
