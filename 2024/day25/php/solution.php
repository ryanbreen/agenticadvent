#!/usr/bin/env php
<?php
/**
 * Day 25: Code Chronicle - Lock and key matching
 */

/**
 * Parse schematics into locks and keys.
 *
 * @param string $text Input text
 * @return array Array with two elements: [locks, keys]
 */
function parseInput(string $text): array {
    $locks = [];
    $keys = [];

    $schematics = array_filter(explode("\n\n", trim($text)));

    foreach ($schematics as $schematic) {
        $lines = explode("\n", trim($schematic));

        // Lock: top row is all #, bottom is all .
        // Key: top row is all ., bottom is all #
        if ($lines[0] === '#####') {
            // It's a lock - count # from top (excluding top row)
            $heights = [];
            for ($col = 0; $col < 5; $col++) {
                $height = 0;
                for ($row = 1; $row < 7; $row++) {  // rows 1-6
                    if ($lines[$row][$col] === '#') {
                        $height++;
                    } else {
                        break;
                    }
                }
                $heights[] = $height;
            }
            $locks[] = $heights;
        } else {
            // It's a key - count # from bottom (excluding bottom row)
            $heights = [];
            for ($col = 0; $col < 5; $col++) {
                $height = 0;
                for ($row = 5; $row >= 0; $row--) {  // rows 5 down to 0
                    if ($lines[$row][$col] === '#') {
                        $height++;
                    } else {
                        break;
                    }
                }
                $heights[] = $height;
            }
            $keys[] = $heights;
        }
    }

    return [$locks, $keys];
}

/**
 * Check if a key fits a lock (no column exceeds 5).
 *
 * @param array $lock Lock heights
 * @param array $key Key heights
 * @return bool True if fits, false otherwise
 */
function fits(array $lock, array $key): bool {
    for ($i = 0; $i < 5; $i++) {
        if ($lock[$i] + $key[$i] > 5) {
            return false;
        }
    }
    return true;
}

/**
 * Count unique lock/key pairs that fit together.
 *
 * @param array $locks Array of locks
 * @param array $keys Array of keys
 * @return int Count of fitting pairs
 */
function part1(array $locks, array $keys): int {
    $count = 0;
    foreach ($locks as $lock) {
        foreach ($keys as $key) {
            if (fits($lock, $key)) {
                $count++;
            }
        }
    }
    return $count;
}

// Main execution
$inputFile = __DIR__ . '/../input.txt';
$text = file_get_contents($inputFile);

[$locks, $keys] = parseInput($text);

$answer1 = part1($locks, $keys);
echo "Part 1: $answer1\n";

// Day 25 typically only has Part 1
echo "Part 2: Merry Christmas!\n";
