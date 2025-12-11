#!/usr/bin/env php
<?php

function parseInput(string $filename): array
{
    $grid = array_map('rtrim', file($filename));

    $rows = count($grid);
    $cols = $rows > 0 ? strlen($grid[0]) : 0;

    // Group antenna positions by frequency
    $antennas = [];
    for ($r = 0; $r < $rows; $r++) {
        for ($c = 0; $c < $cols; $c++) {
            $ch = $grid[$r][$c];
            if ($ch !== '.') {
                if (!isset($antennas[$ch])) {
                    $antennas[$ch] = [];
                }
                $antennas[$ch][] = [$r, $c];
            }
        }
    }

    return [$rows, $cols, $antennas];
}

function part1(): int
{
    [$rows, $cols, $antennas] = parseInput('../input.txt');

    $antinodes = [];

    foreach ($antennas as $freq => $positions) {
        $count = count($positions);

        // For each pair of antennas with same frequency
        for ($i = 0; $i < $count; $i++) {
            for ($j = $i + 1; $j < $count; $j++) {
                [$r1, $c1] = $positions[$i];
                [$r2, $c2] = $positions[$j];

                // Calculate the two antinodes
                // Antinode beyond antenna 1 (away from antenna 2)
                $ar1 = 2 * $r1 - $r2;
                $ac1 = 2 * $c1 - $c2;

                // Antinode beyond antenna 2 (away from antenna 1)
                $ar2 = 2 * $r2 - $r1;
                $ac2 = 2 * $c2 - $c1;

                // Add if within bounds
                if ($ar1 >= 0 && $ar1 < $rows && $ac1 >= 0 && $ac1 < $cols) {
                    $antinodes["$ar1,$ac1"] = true;
                }
                if ($ar2 >= 0 && $ar2 < $rows && $ac2 >= 0 && $ac2 < $cols) {
                    $antinodes["$ar2,$ac2"] = true;
                }
            }
        }
    }

    return count($antinodes);
}

function part2(): int
{
    [$rows, $cols, $antennas] = parseInput('../input.txt');

    $antinodes = [];

    foreach ($antennas as $freq => $positions) {
        $count = count($positions);

        // For each pair of antennas with same frequency
        for ($i = 0; $i < $count; $i++) {
            for ($j = $i + 1; $j < $count; $j++) {
                [$r1, $c1] = $positions[$i];
                [$r2, $c2] = $positions[$j];

                $dr = $r2 - $r1;
                $dc = $c2 - $c1;

                // Extend in both directions along the line
                // Direction 1: from antenna 1 towards and beyond antenna 2
                $r = $r1;
                $c = $c1;
                while ($r >= 0 && $r < $rows && $c >= 0 && $c < $cols) {
                    $antinodes["$r,$c"] = true;
                    $r += $dr;
                    $c += $dc;
                }

                // Direction 2: from antenna 1 away from antenna 2
                $r = $r1 - $dr;
                $c = $c1 - $dc;
                while ($r >= 0 && $r < $rows && $c >= 0 && $c < $cols) {
                    $antinodes["$r,$c"] = true;
                    $r -= $dr;
                    $c -= $dc;
                }
            }
        }
    }

    return count($antinodes);
}

echo "Part 1: " . part1() . "\n";
echo "Part 2: " . part2() . "\n";
