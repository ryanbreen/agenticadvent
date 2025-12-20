<?php

function parseGrid(string $input): array {
    $grid = [];
    $start = $end = null;
    $lines = explode("\n", trim($input));

    foreach ($lines as $r => $line) {
        $row = str_split($line);
        $grid[] = $row;
        foreach ($row as $c => $ch) {
            if ($ch === 'S') {
                $start = [$r, $c];
            } elseif ($ch === 'E') {
                $end = [$r, $c];
            }
        }
    }

    return [$grid, $start, $end];
}

function tracePath(array $grid, array $start, array $end): array {
    $rows = count($grid);
    $cols = count($grid[0]);
    $dist = [];
    $key = $start[0] . ',' . $start[1];
    $dist[$key] = 0;
    $queue = [$start];
    $front = 0;

    $directions = [[-1, 0], [1, 0], [0, -1], [0, 1]];

    while ($front < count($queue)) {
        [$r, $c] = $queue[$front++];

        if ($r === $end[0] && $c === $end[1]) {
            break;
        }

        $currentKey = $r . ',' . $c;

        foreach ($directions as [$dr, $dc]) {
            $nr = $r + $dr;
            $nc = $c + $dc;

            if ($nr >= 0 && $nr < $rows && $nc >= 0 && $nc < $cols && $grid[$nr][$nc] !== '#') {
                $nkey = $nr . ',' . $nc;
                if (!isset($dist[$nkey])) {
                    $dist[$nkey] = $dist[$currentKey] + 1;
                    $queue[] = [$nr, $nc];
                }
            }
        }
    }

    return $dist;
}

function countCheats(array $dist, int $maxCheatTime, int $minSavings): int {
    $count = 0;
    $trackPositions = [];

    // Convert dist keys back to coordinates
    foreach ($dist as $key => $d) {
        $parts = explode(',', $key);
        $trackPositions[] = [(int)$parts[0], (int)$parts[1]];
    }

    foreach ($trackPositions as [$r1, $c1]) {
        $d1 = $dist[$r1 . ',' . $c1];

        foreach ($trackPositions as [$r2, $c2]) {
            // Manhattan distance is the cheat cost
            $cheatCost = abs($r2 - $r1) + abs($c2 - $c1);

            if ($cheatCost <= $maxCheatTime) {
                $d2 = $dist[$r2 . ',' . $c2];
                $savings = $d2 - $d1 - $cheatCost;

                if ($savings >= $minSavings) {
                    $count++;
                }
            }
        }
    }

    return $count;
}

function part1(array $grid, array $start, array $end): int {
    $dist = tracePath($grid, $start, $end);
    return countCheats($dist, 2, 100);
}

function part2(array $grid, array $start, array $end): int {
    $dist = tracePath($grid, $start, $end);
    return countCheats($dist, 20, 100);
}

function main(): void {
    $input = file_get_contents(__DIR__ . '/../input.txt');
    [$grid, $start, $end] = parseGrid($input);

    echo 'Part 1: ' . part1($grid, $start, $end) . PHP_EOL;
    echo 'Part 2: ' . part2($grid, $start, $end) . PHP_EOL;
}

main();
