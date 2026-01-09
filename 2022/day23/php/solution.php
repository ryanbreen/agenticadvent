#!/usr/bin/env php
<?php

function parseInput(string $text): array {
    $elves = [];
    $lines = explode("\n", trim($text));
    foreach ($lines as $r => $line) {
        for ($c = 0; $c < strlen($line); $c++) {
            if ($line[$c] === '#') {
                $elves["$r,$c"] = [$r, $c];
            }
        }
    }
    return $elves;
}

function simulateRound(array $elves, array $directions): array {
    // Direction checks: [check positions, move delta]
    $dirChecks = [
        'N' => [[[-1, -1], [-1, 0], [-1, 1]], [-1, 0]],
        'S' => [[[1, -1], [1, 0], [1, 1]], [1, 0]],
        'W' => [[[-1, -1], [0, -1], [1, -1]], [0, -1]],
        'E' => [[[-1, 1], [0, 1], [1, 1]], [0, 1]],
    ];

    // All 8 neighbors
    $allNeighbors = [[-1, -1], [-1, 0], [-1, 1], [0, -1], [0, 1], [1, -1], [1, 0], [1, 1]];

    // Phase 1: Each elf proposes a move
    $proposals = [];  // elf key -> proposed position key
    $proposalCounts = [];  // position key -> count

    foreach ($elves as $elfKey => $elf) {
        [$r, $c] = $elf;

        // Check if any neighbors
        $hasNeighbor = false;
        foreach ($allNeighbors as [$dr, $dc]) {
            $nKey = ($r + $dr) . "," . ($c + $dc);
            if (isset($elves[$nKey])) {
                $hasNeighbor = true;
                break;
            }
        }

        if (!$hasNeighbor) {
            continue;  // Don't move
        }

        // Try each direction
        foreach ($directions as $d) {
            [$checks, $delta] = $dirChecks[$d];
            [$dr, $dc] = $delta;

            $canMove = true;
            foreach ($checks as [$cr, $cc]) {
                $checkKey = ($r + $cr) . "," . ($c + $cc);
                if (isset($elves[$checkKey])) {
                    $canMove = false;
                    break;
                }
            }

            if ($canMove) {
                $newPos = ($r + $dr) . "," . ($c + $dc);
                $proposals[$elfKey] = $newPos;
                if (!isset($proposalCounts[$newPos])) {
                    $proposalCounts[$newPos] = 0;
                }
                $proposalCounts[$newPos]++;
                break;
            }
        }
    }

    // Phase 2: Execute moves (only if unique proposal)
    $newElves = [];
    $moved = false;

    foreach ($elves as $elfKey => $elf) {
        if (isset($proposals[$elfKey])) {
            $newPos = $proposals[$elfKey];
            if ($proposalCounts[$newPos] === 1) {
                $parts = explode(",", $newPos);
                $newElves[$newPos] = [(int)$parts[0], (int)$parts[1]];
                $moved = true;
            } else {
                $newElves[$elfKey] = $elf;
            }
        } else {
            $newElves[$elfKey] = $elf;
        }
    }

    return [$newElves, $moved];
}

function boundingRectEmpty(array $elves): int {
    $minR = PHP_INT_MAX;
    $maxR = PHP_INT_MIN;
    $minC = PHP_INT_MAX;
    $maxC = PHP_INT_MIN;

    foreach ($elves as [$r, $c]) {
        $minR = min($minR, $r);
        $maxR = max($maxR, $r);
        $minC = min($minC, $c);
        $maxC = max($maxC, $c);
    }

    $area = ($maxR - $minR + 1) * ($maxC - $minC + 1);
    return $area - count($elves);
}

function part1(string $text): int {
    $elves = parseInput($text);
    $directions = ['N', 'S', 'W', 'E'];

    for ($i = 0; $i < 10; $i++) {
        [$elves, $_] = simulateRound($elves, $directions);
        // Rotate directions
        $first = array_shift($directions);
        $directions[] = $first;
    }

    return boundingRectEmpty($elves);
}

function part2(string $text): int {
    $elves = parseInput($text);
    $directions = ['N', 'S', 'W', 'E'];

    $roundNum = 0;
    while (true) {
        $roundNum++;
        [$elves, $moved] = simulateRound($elves, $directions);
        if (!$moved) {
            return $roundNum;
        }
        // Rotate directions
        $first = array_shift($directions);
        $directions[] = $first;
    }
}

function main(): void {
    $scriptDir = dirname(__FILE__);
    $inputFile = $scriptDir . '/../input.txt';

    $text = file_get_contents($inputFile);

    echo "Part 1: " . part1($text) . "\n";
    echo "Part 2: " . part2($text) . "\n";
}

main();
