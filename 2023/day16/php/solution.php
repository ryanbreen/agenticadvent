<?php
declare(strict_types=1);

/**
 * Advent of Code 2023 - Day 16: The Floor Will Be Lava
 * Light beam simulation through a grid with mirrors and splitters.
 */

function parseInput(string $text): array
{
    return array_filter(explode("\n", trim($text)), fn($line) => $line !== '');
}

/**
 * Count energized tiles starting from given position and direction.
 * Directions: 0=right, 1=down, 2=left, 3=up
 */
function countEnergized(array $grid, int $startRow, int $startCol, int $startDir): int
{
    $rows = count($grid);
    $cols = strlen($grid[0]);

    // Direction deltas: 0=right, 1=down, 2=left, 3=up
    $dr = [0, 1, 0, -1];
    $dc = [1, 0, -1, 0];

    $visited = [];
    $queue = [[$startRow, $startCol, $startDir]];

    while (!empty($queue)) {
        [$r, $c, $d] = array_shift($queue);

        // Check bounds
        if ($r < 0 || $r >= $rows || $c < 0 || $c >= $cols) {
            continue;
        }

        // Check if already visited this state
        $stateKey = "$r,$c,$d";
        if (isset($visited[$stateKey])) {
            continue;
        }
        $visited[$stateKey] = true;

        $cell = $grid[$r][$c];
        $nextDirs = [];

        switch ($cell) {
            case '.':
                $nextDirs = [$d];
                break;
            case '/':
                // right->up, down->left, left->down, up->right
                $reflectMap = [3, 2, 1, 0];
                $nextDirs = [$reflectMap[$d]];
                break;
            case '\\':
                // right->down, down->right, left->up, up->left
                $reflectMap = [1, 0, 3, 2];
                $nextDirs = [$reflectMap[$d]];
                break;
            case '|':
                // Vertical splitter: horizontal beams split, vertical pass through
                if ($d === 0 || $d === 2) {
                    $nextDirs = [1, 3]; // Split to down and up
                } else {
                    $nextDirs = [$d];
                }
                break;
            case '-':
                // Horizontal splitter: vertical beams split, horizontal pass through
                if ($d === 1 || $d === 3) {
                    $nextDirs = [0, 2]; // Split to right and left
                } else {
                    $nextDirs = [$d];
                }
                break;
        }

        foreach ($nextDirs as $nd) {
            $queue[] = [$r + $dr[$nd], $c + $dc[$nd], $nd];
        }
    }

    // Count unique positions (ignoring direction)
    $positions = [];
    foreach (array_keys($visited) as $state) {
        [$r, $c] = explode(',', $state);
        $positions["$r,$c"] = true;
    }

    return count($positions);
}

function part1(array $grid): int
{
    // Beam starts at (0,0) heading right (direction 0)
    return countEnergized($grid, 0, 0, 0);
}

function part2(array $grid): int
{
    $rows = count($grid);
    $cols = strlen($grid[0]);
    $maxEnergized = 0;

    // Top row, heading down
    for ($c = 0; $c < $cols; $c++) {
        $maxEnergized = max($maxEnergized, countEnergized($grid, 0, $c, 1));
    }

    // Bottom row, heading up
    for ($c = 0; $c < $cols; $c++) {
        $maxEnergized = max($maxEnergized, countEnergized($grid, $rows - 1, $c, 3));
    }

    // Left column, heading right
    for ($r = 0; $r < $rows; $r++) {
        $maxEnergized = max($maxEnergized, countEnergized($grid, $r, 0, 0));
    }

    // Right column, heading left
    for ($r = 0; $r < $rows; $r++) {
        $maxEnergized = max($maxEnergized, countEnergized($grid, $r, $cols - 1, 2));
    }

    return $maxEnergized;
}

function main(): void
{
    $inputFile = __DIR__ . '/../input.txt';
    $text = file_get_contents($inputFile);
    $grid = parseInput($text);

    echo "Part 1: " . part1($grid) . "\n";
    echo "Part 2: " . part2($grid) . "\n";
}

main();
