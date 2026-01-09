<?php

/**
 * Compare two values recursively.
 * Returns: -1 if left < right (correct order)
 *           1 if left > right (wrong order)
 *           0 if equal (continue)
 */
function compare($left, $right) {
    // Both integers
    if (is_int($left) && is_int($right)) {
        if ($left < $right) {
            return -1;
        } elseif ($left > $right) {
            return 1;
        }
        return 0;
    }

    // Both lists (arrays)
    if (is_array($left) && is_array($right)) {
        $minLen = min(count($left), count($right));
        for ($i = 0; $i < $minLen; $i++) {
            $result = compare($left[$i], $right[$i]);
            if ($result !== 0) {
                return $result;
            }
        }
        // Check lengths
        if (count($left) < count($right)) {
            return -1;
        } elseif (count($left) > count($right)) {
            return 1;
        }
        return 0;
    }

    // Mixed types - convert integer to list
    if (is_int($left)) {
        return compare([$left], $right);
    } else {
        return compare($left, [$right]);
    }
}

/**
 * Part 1: Sum indices of pairs in correct order.
 */
function part1($text) {
    $pairs = preg_split('/\n\n/', trim($text));
    $total = 0;

    foreach ($pairs as $index => $pair) {
        $lines = explode("\n", trim($pair));
        $left = json_decode($lines[0], true);
        $right = json_decode($lines[1], true);

        if (compare($left, $right) === -1) {
            $total += ($index + 1);  // 1-indexed
        }
    }

    return $total;
}

/**
 * Part 2: Sort all packets with divider packets, find decoder key.
 */
function part2($text) {
    $lines = array_filter(explode("\n", trim($text)), function($line) {
        return strlen(trim($line)) > 0;
    });

    $packets = array_map(function($line) {
        return json_decode($line, true);
    }, $lines);

    // Add divider packets
    $divider1 = [[2]];
    $divider2 = [[6]];
    $packets[] = $divider1;
    $packets[] = $divider2;

    // Sort using comparison function
    usort($packets, 'compare');

    // Find positions of dividers (1-indexed)
    $pos1 = 0;
    $pos2 = 0;
    foreach ($packets as $i => $packet) {
        if ($packet === $divider1) {
            $pos1 = $i + 1;
        }
        if ($packet === $divider2) {
            $pos2 = $i + 1;
        }
    }

    return $pos1 * $pos2;
}

function main() {
    $scriptDir = dirname(__FILE__);
    $inputFile = $scriptDir . '/../input.txt';
    $text = file_get_contents($inputFile);

    echo 'Part 1: ' . part1($text) . "\n";
    echo 'Part 2: ' . part2($text) . "\n";
}

main();
