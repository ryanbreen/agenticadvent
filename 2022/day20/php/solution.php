#!/usr/bin/env php
<?php

/**
 * Day 20: Grove Positioning System
 *
 * Circular list mixing where numbers move by their value.
 */

function parseInput(string $text): array {
    return array_map('intval', array_filter(explode("\n", trim($text)), 'strlen'));
}

function mix(array $numbers, int $times = 1): array {
    $n = count($numbers);

    // Store [original_index, value] pairs
    $indexed = [];
    foreach ($numbers as $i => $val) {
        $indexed[] = [$i, $val];
    }

    for ($round = 0; $round < $times; $round++) {
        for ($origIdx = 0; $origIdx < $n; $origIdx++) {
            // Find current position of this element
            $currPos = 0;
            foreach ($indexed as $pos => $pair) {
                if ($pair[0] === $origIdx) {
                    $currPos = $pos;
                    break;
                }
            }

            // Get the value
            $val = $indexed[$currPos][1];

            // Remove from current position
            array_splice($indexed, $currPos, 1);

            // Calculate new position (modulo n-1 because we removed the element)
            // PHP modulo can return negative, so we need to handle that
            $newPos = ($currPos + $val) % ($n - 1);
            if ($newPos < 0) {
                $newPos += ($n - 1);
            }

            // Insert at new position
            array_splice($indexed, $newPos, 0, [[[$origIdx, $val]]]);
            // Fix the nested array from array_splice
            $indexed[$newPos] = [$origIdx, $val];
        }
    }

    return array_map(function($pair) { return $pair[1]; }, $indexed);
}

function groveCoordinates(array $mixed): int {
    $n = count($mixed);
    $zeroIdx = array_search(0, $mixed, true);

    $sum = 0;
    foreach ([1000, 2000, 3000] as $offset) {
        $sum += $mixed[($zeroIdx + $offset) % $n];
    }
    return $sum;
}

function part1(string $text): int {
    $numbers = parseInput($text);
    $mixed = mix($numbers, 1);
    return groveCoordinates($mixed);
}

function part2(string $text): int {
    $numbers = parseInput($text);
    $decryptionKey = 811589153;
    $numbers = array_map(function($n) use ($decryptionKey) {
        return $n * $decryptionKey;
    }, $numbers);
    $mixed = mix($numbers, 10);
    return groveCoordinates($mixed);
}

function main(): void {
    $scriptDir = dirname(__FILE__);
    $inputFile = $scriptDir . '/../input.txt';
    $text = file_get_contents($inputFile);

    echo 'Part 1: ' . part1($text) . PHP_EOL;
    echo 'Part 2: ' . part2($text) . PHP_EOL;
}

main();
