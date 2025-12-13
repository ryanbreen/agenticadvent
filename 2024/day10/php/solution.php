<?php

// Read input file
$input = trim(file_get_contents(__DIR__ . '/../input.txt'));
$lines = explode("\n", $input);

// Parse input into grid
$grid = [];
foreach ($lines as $line) {
    $row = [];
    for ($i = 0; $i < strlen($line); $i++) {
        $row[] = (int)$line[$i];
    }
    $grid[] = $row;
}

$rows = count($grid);
$cols = count($grid[0]);

// Directions: up, down, left, right
const DIRS = [[-1, 0], [1, 0], [0, -1], [0, 1]];

/**
 * Find all positions with height 0 (trailheads).
 *
 * @param array<int, array<int, int>> $grid The height map grid
 * @param int $rows Number of rows
 * @param int $cols Number of columns
 * @return array<int, array{int, int}> Array of trailhead positions
 */
function findTrailheads(array $grid, int $rows, int $cols): array {
    $trailheads = [];
    for ($r = 0; $r < $rows; $r++) {
        for ($c = 0; $c < $cols; $c++) {
            if ($grid[$r][$c] === 0) {
                $trailheads[] = [$r, $c];
            }
        }
    }
    return $trailheads;
}

/**
 * BFS to find all 9s reachable from a trailhead.
 *
 * @param array<int, array<int, int>> $grid The height map grid
 * @param int $rows Number of rows
 * @param int $cols Number of columns
 * @param int $startR Starting row
 * @param int $startC Starting column
 * @return int Number of distinct 9s reachable
 */
function countReachableNines(array $grid, int $rows, int $cols, int $startR, int $startC): int {
    $visited = [];
    $visited["$startR,$startC"] = true;
    $queue = [[$startR, $startC]];
    $nines = [];

    while (count($queue) > 0) {
        [$r, $c] = array_shift($queue);
        $currentHeight = $grid[$r][$c];

        if ($currentHeight === 9) {
            $nines["$r,$c"] = true;
            continue;
        }

        // Try all four directions
        foreach (DIRS as $dir) {
            [$dr, $dc] = $dir;
            $nr = $r + $dr;
            $nc = $c + $dc;

            if ($nr >= 0 && $nr < $rows && $nc >= 0 && $nc < $cols) {
                $key = "$nr,$nc";
                if (!isset($visited[$key])) {
                    if ($grid[$nr][$nc] === $currentHeight + 1) {
                        $visited[$key] = true;
                        $queue[] = [$nr, $nc];
                    }
                }
            }
        }
    }

    return count($nines);
}

/**
 * Part 1: Sum of trailhead scores.
 *
 * @param array<int, array<int, int>> $grid The height map grid
 * @param int $rows Number of rows
 * @param int $cols Number of columns
 * @return int Total score of all trailheads
 */
function part1(array $grid, int $rows, int $cols): int {
    $trailheads = findTrailheads($grid, $rows, $cols);
    $totalScore = 0;

    foreach ($trailheads as $th) {
        [$r, $c] = $th;
        $totalScore += countReachableNines($grid, $rows, $cols, $r, $c);
    }

    return $totalScore;
}

/**
 * DFS to count all distinct trails from current position to any 9.
 *
 * @param array<int, array<int, int>> $grid The height map grid
 * @param int $rows Number of rows
 * @param int $cols Number of columns
 * @param int $r Current row
 * @param int $c Current column
 * @return int Number of distinct trails from this position
 */
function dfsCountTrails(array $grid, int $rows, int $cols, int $r, int $c): int {
    $currentHeight = $grid[$r][$c];

    if ($currentHeight === 9) {
        return 1;
    }

    $total = 0;
    foreach (DIRS as $dir) {
        [$dr, $dc] = $dir;
        $nr = $r + $dr;
        $nc = $c + $dc;

        if ($nr >= 0 && $nr < $rows && $nc >= 0 && $nc < $cols) {
            if ($grid[$nr][$nc] === $currentHeight + 1) {
                $total += dfsCountTrails($grid, $rows, $cols, $nr, $nc);
            }
        }
    }

    return $total;
}

/**
 * Part 2: Sum of trailhead ratings.
 *
 * @param array<int, array<int, int>> $grid The height map grid
 * @param int $rows Number of rows
 * @param int $cols Number of columns
 * @return int Total rating of all trailheads
 */
function part2(array $grid, int $rows, int $cols): int {
    $trailheads = findTrailheads($grid, $rows, $cols);
    $totalRating = 0;

    foreach ($trailheads as $th) {
        [$r, $c] = $th;
        $totalRating += dfsCountTrails($grid, $rows, $cols, $r, $c);
    }

    return $totalRating;
}

// Run both parts
echo "Part 1: " . part1($grid, $rows, $cols) . "\n";
echo "Part 2: " . part2($grid, $rows, $cols) . "\n";
