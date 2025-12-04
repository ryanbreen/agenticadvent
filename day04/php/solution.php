<?php

// Read input file
$input_text = file_get_contents(__DIR__ . '/../input.txt');
$input_text = rtrim($input_text);

// Parse input
$lines = explode("\n", $input_text);

// Directions for 8 neighbors (including diagonals)
const DIRECTIONS = [
    [-1, -1], [-1, 0], [-1, 1],
    [0, -1],           [0, 1],
    [1, -1],  [1, 0],  [1, 1]
];

/**
 * Count the number of adjacent rolls ('@') around position ($r, $c).
 */
function count_adjacent_rolls($grid, $r, $c) {
    $rows = is_array($grid) && count($grid) > 0 ? (is_array($grid[0]) ? count($grid) : count($grid)) : 0;
    $cols = is_array($grid) && count($grid) > 0 ? (is_array($grid[0]) ? count($grid[0]) : strlen($grid[0])) : 0;
    $count = 0;

    foreach (DIRECTIONS as [$dr, $dc]) {
        $nr = $r + $dr;
        $nc = $c + $dc;
        // Check bounds
        if ($nr >= 0 && $nr < $rows && $nc >= 0 && $nc < $cols) {
            $cell = is_array($grid[$nr]) ? $grid[$nr][$nc] : $grid[$nr][$nc];
            if ($cell === '@') {
                $count++;
            }
        }
    }

    return $count;
}

function part1($lines) {
    /**
     * Count rolls of paper that can be accessed by a forklift.
     *
     * A roll can be accessed if it has fewer than 4 adjacent rolls
     * in the 8 surrounding positions (including diagonals).
     */
    $grid = $lines;
    $rows = count($grid);
    $cols = $rows > 0 ? strlen($grid[0]) : 0;

    $accessible_count = 0;

    for ($r = 0; $r < $rows; $r++) {
        for ($c = 0; $c < $cols; $c++) {
            if ($grid[$r][$c] === '@') {
                $adjacent_rolls = count_adjacent_rolls($grid, $r, $c);
                // Accessible if fewer than 4 adjacent rolls
                if ($adjacent_rolls < 4) {
                    $accessible_count++;
                }
            }
        }
    }

    return $accessible_count;
}

function part2($lines) {
    /**
     * Count total rolls removed by iteratively removing accessible rolls.
     *
     * A roll can be removed if it has fewer than 4 adjacent rolls.
     * After removal, check again for newly accessible rolls.
     * Repeat until no more rolls can be removed.
     */
    // Create a mutable copy of the grid
    $grid = [];
    foreach ($lines as $line) {
        $grid[] = str_split($line);
    }

    $rows = count($grid);
    $cols = $rows > 0 ? count($grid[0]) : 0;

    $total_removed = 0;

    while (true) {
        // Find all rolls that can be removed in this iteration
        $removable = [];

        for ($r = 0; $r < $rows; $r++) {
            for ($c = 0; $c < $cols; $c++) {
                if ($grid[$r][$c] === '@') {
                    $adjacent_rolls = count_adjacent_rolls($grid, $r, $c);
                    // Can be removed if fewer than 4 adjacent rolls
                    if ($adjacent_rolls < 4) {
                        $removable[] = [$r, $c];
                    }
                }
            }
        }

        // If no rolls can be removed, we're done
        if (empty($removable)) {
            break;
        }

        // Remove all accessible rolls
        foreach ($removable as [$r, $c]) {
            $grid[$r][$c] = '.';
        }

        $total_removed += count($removable);
    }

    return $total_removed;
}

// Run both parts
echo "Part 1: " . part1($lines) . "\n";
echo "Part 2: " . part2($lines) . "\n";
