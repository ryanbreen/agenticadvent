#!/usr/bin/env php
<?php
// Run: php solution.php

function is_invalid_id_part1($num) {
    $s = strval($num);
    $length = strlen($s);

    // Must have even length to be repeated twice
    if ($length % 2 !== 0) {
        return false;
    }

    // Check if it starts with 0 (leading zeros not allowed)
    if ($s[0] === '0') {
        return false;
    }

    // Split in half and check if both halves are identical
    $mid = intdiv($length, 2);
    $first_half = substr($s, 0, $mid);
    $second_half = substr($s, $mid);

    return $first_half === $second_half;
}

function is_invalid_id_part2($num) {
    $s = strval($num);
    $length = strlen($s);

    // Check if it starts with 0 (leading zeros not allowed)
    if ($s[0] === '0') {
        return false;
    }

    // Try all possible pattern lengths from 1 to length//2
    for ($pattern_length = 1; $pattern_length <= intdiv($length, 2); $pattern_length++) {
        // Check if the string length is divisible by pattern_length
        if ($length % $pattern_length === 0) {
            $pattern = substr($s, 0, $pattern_length);
            $repetitions = intdiv($length, $pattern_length);

            // Check if repeating the pattern gives us the original string
            if (str_repeat($pattern, $repetitions) === $s) {
                return true;
            }
        }
    }

    return false;
}

function part1() {
    $input_text = file_get_contents(__DIR__ . '/../input.txt');

    // Parse ranges from input
    $ranges = [];
    $parts = explode(',', trim($input_text));
    foreach ($parts as $part) {
        $part = trim($part);
        if (strpos($part, '-') !== false) {
            $range_parts = explode('-', $part);
            if (count($range_parts) === 2) {
                $start = intval($range_parts[0]);
                $end = intval($range_parts[1]);
                $ranges[] = [$start, $end];
            }
        }
    }

    $total = 0;
    foreach ($ranges as list($start, $end)) {
        for ($num = $start; $num <= $end; $num++) {
            if (is_invalid_id_part1($num)) {
                $total += $num;
            }
        }
    }

    return $total;
}

function part2() {
    $input_text = file_get_contents(__DIR__ . '/../input.txt');

    // Parse ranges from input
    $ranges = [];
    $parts = explode(',', trim($input_text));
    foreach ($parts as $part) {
        $part = trim($part);
        if (strpos($part, '-') !== false) {
            $range_parts = explode('-', $part);
            if (count($range_parts) === 2) {
                $start = intval($range_parts[0]);
                $end = intval($range_parts[1]);
                $ranges[] = [$start, $end];
            }
        }
    }

    $total = 0;
    foreach ($ranges as list($start, $end)) {
        for ($num = $start; $num <= $end; $num++) {
            if (is_invalid_id_part2($num)) {
                $total += $num;
            }
        }
    }

    return $total;
}

echo "Part 1: " . part1() . "\n";
echo "Part 2: " . part2() . "\n";
