<?php
/**
 * Advent of Code 2024 - Day 19: Linen Layout
 *
 * Part 1: Count designs that CAN be formed by concatenating towel patterns
 * Part 2: Sum the NUMBER OF WAYS each design can be formed
 * Uses dynamic programming with memoization
 */

$input = trim(file_get_contents(__DIR__ . '/../input.txt'));
$parts = explode("\n\n", $input);

$patterns = array_map('trim', explode(',', $parts[0]));
$designs = explode("\n", trim($parts[1]));

/**
 * Check if a design can be formed by concatenating patterns.
 */
function canForm(string $design, array $patterns): bool {
    $len = strlen($design);
    $dp = array_fill(0, $len + 1, false);
    $dp[0] = true;

    for ($i = 0; $i < $len; $i++) {
        if (!$dp[$i]) {
            continue;
        }
        foreach ($patterns as $pattern) {
            $plen = strlen($pattern);
            if ($i + $plen <= $len && substr($design, $i, $plen) === $pattern) {
                $dp[$i + $plen] = true;
            }
        }
    }

    return $dp[$len];
}

/**
 * Count the number of ways to form design from patterns.
 */
function countWays(string $design, array $patterns): int {
    $len = strlen($design);
    $dp = array_fill(0, $len + 1, 0);
    $dp[0] = 1;

    for ($i = 0; $i < $len; $i++) {
        if ($dp[$i] === 0) {
            continue;
        }
        foreach ($patterns as $pattern) {
            $plen = strlen($pattern);
            if ($i + $plen <= $len && substr($design, $i, $plen) === $pattern) {
                $dp[$i + $plen] += $dp[$i];
            }
        }
    }

    return $dp[$len];
}

function part1(array $designs, array $patterns): int {
    $count = 0;
    foreach ($designs as $design) {
        if (canForm($design, $patterns)) {
            $count++;
        }
    }
    return $count;
}

function part2(array $designs, array $patterns): int {
    $total = 0;
    foreach ($designs as $design) {
        $total += countWays($design, $patterns);
    }
    return $total;
}

echo "Part 1: " . part1($designs, $patterns) . "\n";
echo "Part 2: " . part2($designs, $patterns) . "\n";
