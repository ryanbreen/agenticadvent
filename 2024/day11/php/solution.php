<?php

// Read and parse input
$inputFile = __DIR__ . '/../input.txt';
$input = trim(file_get_contents($inputFile));
$stones = array_map('intval', explode(' ', $input));

// Memoization cache
$cache = [];

/**
 * Count how many stones result from a single stone after N blinks.
 * Uses memoization to avoid redundant calculations.
 */
function countStones($value, $blinks) {
    global $cache;

    // Base case: no more blinks
    if ($blinks === 0) {
        return 1;
    }

    // Check cache
    $cacheKey = "$value,$blinks";
    if (isset($cache[$cacheKey])) {
        return $cache[$cacheKey];
    }

    $result = 0;

    // Rule 1: 0 becomes 1
    if ($value === 0) {
        $result = countStones(1, $blinks - 1);
    }
    // Rule 2: Even number of digits -> split
    else {
        $s = (string)$value;
        $len = strlen($s);

        if ($len % 2 === 0) {
            $mid = $len / 2;
            $left = (int)substr($s, 0, $mid);
            $right = (int)substr($s, $mid);
            $result = countStones($left, $blinks - 1) + countStones($right, $blinks - 1);
        }
        // Rule 3: Multiply by 2024
        else {
            $result = countStones($value * 2024, $blinks - 1);
        }
    }

    // Store in cache
    $cache[$cacheKey] = $result;
    return $result;
}

function part1($stones) {
    $total = 0;
    foreach ($stones as $stone) {
        $total += countStones($stone, 25);
    }
    return $total;
}

function part2($stones) {
    $total = 0;
    foreach ($stones as $stone) {
        $total += countStones($stone, 75);
    }
    return $total;
}

// Run solutions
echo "Part 1: " . part1($stones) . "\n";
echo "Part 2: " . part2($stones) . "\n";
