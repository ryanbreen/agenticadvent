#!/usr/bin/env php
<?php

/**
 * Advent of Code 2023 - Day 11: Cosmic Expansion
 *
 * Calculates the sum of Manhattan distances between all pairs of galaxies,
 * accounting for cosmic expansion in empty rows and columns.
 */

function parseGrid(array $lines): array {
    $galaxies = [];
    foreach ($lines as $r => $line) {
        $len = strlen($line);
        for ($c = 0; $c < $len; $c++) {
            if ($line[$c] === '#') {
                $galaxies[] = [$r, $c];
            }
        }
    }
    return $galaxies;
}

function findEmptyRowsAndCols(array $lines): array {
    $rows = count($lines);
    $cols = $rows > 0 ? strlen($lines[0]) : 0;

    $emptyRows = [];
    $emptyCols = [];

    // Find empty rows
    foreach ($lines as $r => $line) {
        if (strpos($line, '#') === false) {
            $emptyRows[$r] = true;
        }
    }

    // Find empty columns
    for ($c = 0; $c < $cols; $c++) {
        $hasGalaxy = false;
        for ($r = 0; $r < $rows; $r++) {
            if (isset($lines[$r][$c]) && $lines[$r][$c] === '#') {
                $hasGalaxy = true;
                break;
            }
        }
        if (!$hasGalaxy) {
            $emptyCols[$c] = true;
        }
    }

    return [$emptyRows, $emptyCols];
}

function calculateDistances(array $galaxies, array $emptyRows, array $emptyCols, int $expansionFactor): int {
    $total = 0;
    $n = count($galaxies);

    // Iterate over all pairs
    for ($i = 0; $i < $n - 1; $i++) {
        for ($j = $i + 1; $j < $n; $j++) {
            list($r1, $c1) = $galaxies[$i];
            list($r2, $c2) = $galaxies[$j];

            // Calculate row distance with expansion
            $minR = min($r1, $r2);
            $maxR = max($r1, $r2);
            $rowDist = $maxR - $minR;
            for ($r = $minR; $r < $maxR; $r++) {
                if (isset($emptyRows[$r])) {
                    $rowDist += $expansionFactor - 1;
                }
            }

            // Calculate column distance with expansion
            $minC = min($c1, $c2);
            $maxC = max($c1, $c2);
            $colDist = $maxC - $minC;
            for ($c = $minC; $c < $maxC; $c++) {
                if (isset($emptyCols[$c])) {
                    $colDist += $expansionFactor - 1;
                }
            }

            $total += $rowDist + $colDist;
        }
    }

    return $total;
}

function part1(array $lines): int {
    $galaxies = parseGrid($lines);
    list($emptyRows, $emptyCols) = findEmptyRowsAndCols($lines);
    return calculateDistances($galaxies, $emptyRows, $emptyCols, 2);
}

function part2(array $lines): int {
    $galaxies = parseGrid($lines);
    list($emptyRows, $emptyCols) = findEmptyRowsAndCols($lines);
    return calculateDistances($galaxies, $emptyRows, $emptyCols, 1000000);
}

function main(): void {
    global $argv;

    $inputFile = $argv[1] ?? __DIR__ . '/../input.txt';
    $content = file_get_contents($inputFile);
    $lines = explode("\n", rtrim($content, "\n"));

    // Remove any trailing empty lines
    while (!empty($lines) && $lines[count($lines) - 1] === '') {
        array_pop($lines);
    }

    echo "Part 1: " . part1($lines) . "\n";
    echo "Part 2: " . part2($lines) . "\n";
}

main();
