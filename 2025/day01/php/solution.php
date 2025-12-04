#!/usr/bin/env php
<?php
// Run: php solution.php

function part1() {
    $input_text = file_get_contents(__DIR__ . '/../input.txt');
    $lines = array_filter(array_map('trim', explode("\n", trim($input_text))));

    $position = 50; // Starting position
    $zero_count = 0;

    foreach ($lines as $line) {
        if (empty($line)) {
            continue;
        }

        $direction = $line[0];
        $distance = intval(substr($line, 1));

        if ($direction === 'L') {
            $position = ($position - $distance) % 100;
            if ($position < 0) {
                $position += 100;
            }
        } else { // direction === 'R'
            $position = ($position + $distance) % 100;
        }

        if ($position === 0) {
            $zero_count++;
        }
    }

    return $zero_count;
}

function part2() {
    $input_text = file_get_contents(__DIR__ . '/../input.txt');
    $lines = array_filter(array_map('trim', explode("\n", trim($input_text))));

    $position = 50; // Starting position
    $zero_count = 0;

    foreach ($lines as $line) {
        if (empty($line)) {
            continue;
        }

        $direction = $line[0];
        $distance = intval(substr($line, 1));

        if ($direction === 'L') {
            // Moving left (toward lower numbers)
            if ($position > 0 && $distance >= $position) {
                $zero_count += 1 + intdiv($distance - $position, 100);
            } elseif ($position === 0 && $distance >= 100) {
                $zero_count += intdiv($distance, 100);
            }
        } else { // direction === 'R'
            // Moving right (toward higher numbers)
            if ($position > 0) {
                $steps_to_zero = 100 - $position;
                if ($distance >= $steps_to_zero) {
                    $zero_count += 1 + intdiv($distance - $steps_to_zero, 100);
                }
            } else { // position === 0
                if ($distance >= 100) {
                    $zero_count += intdiv($distance, 100);
                }
            }
        }

        // Update position
        if ($direction === 'L') {
            $position = ($position - $distance) % 100;
            if ($position < 0) {
                $position += 100;
            }
        } else {
            $position = ($position + $distance) % 100;
        }
    }

    return $zero_count;
}

echo "Part 1: " . part1() . "\n";
echo "Part 2: " . part2() . "\n";
